# HaXPath
HaXPath is a library and embedded domain-specifc language which uses strongly-typed Haskell expressions to represent
XPaths.

## Motivation
In many contexts when querying XML documents in Haskell we often need to use `String` values to represent the
XPaths we want to use. These `String` expressions can quickly become hard to manage as they do not take advantage of
Haskell's type system, particularly for more complex XPaths. We may not know until run-time whether the XPath is even
syntactically valid. HaXPath does not have its own XPath engine to run the queries, rather it is expected to be used
in combination with other libraries which have such functionality. Instead, we can simply convert the strongly-typed
XPath expressions to `String` or `Text` and send them to our favourite APIs.

## HaXPath API
HaXPath provides two core APIs: the standard API (`HaXPath` module) allows for expressing generic XPaths, while
the schematic API (`HaXPath.Schematic` module) is a layer of abstraction built upon the standard API which constrains
XPath expressions so they must follow a specifc document schema. An example of a schema is the HTML standard. There is
an (incomplete) implementation of the schematic API for HTML in the `HaXPath.Schematic.HTML` module.

### Standard API
`HaXPath` modules are expected to be imported qualified as otherwise you will get name conflicts with the Prelude. The
operators however need not be qualified, and can conveniently be imported directly from `HaXPath.Operators`. All
operators are suffixed with `.`.

Some basic examples:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified HaXPath           as X
import           HaXPath.Operators

-- Create XPath nodes <a> and <b>
a :: X.Node
a = X.namedNode "a"

b :: X.Node
b = X.namedNode "b"

-- The XPath "/descendant-or-self::node()/child::a/child::b"
-- root is a virtual node, and can be used only at the beginning of a path to indicate it is an absolute path
p1 :: X.Path
p1 = X.root /. X.descendantOrSelf X.node /. X.child a /. X.child b

-- The same XPath as above but in abbreviated form
p2 :: X.Path
p2 = X.root //. a /. b

-- Convert paths to `Text`:
X.show p1 == "/descendant-or-self::node()/child::a/child::b"
X.show p2 == "/descendant-or-self::node()/child::a/child::b"
```

You can add qualifiers to filter node sets using the `#` operator:

```haskell
X.show (p1 # X.position =. 1) == "(/descendant-or-self::node()/child::a/child::b)[position() = 1]"
X.show (X.root //. a /. b # X.position =. 1) == "/descendant-or-self::node()/child::a/child::b[position() = 1]"
X.show (X.root //. a # X.at "id" =. "abc") == "/descendant-or-self::node()/child::a[@id = 'abc']"
```

Note the second argument to `#` must represent an XPath boolean value, otherwise it will not type check.

`test/HaXPath/Test.hs` provides a variety of examples showing what can be done.

### Schematic API
The schematic API provides further constraints than the standard API by only allowing paths that are valid with respect
to some custom schema. Take for example the following XML document for a restaurant menu:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<menu>
  <item name="Belgian Waffles" price="$5.95"></item>
  <item name="Strawberry Waffles" price="$7.95"></item>
  <item name="French Toast" price="$4.50"></item>
</menu>
```
It should be fairly intuitive that there is an underlying schema to the above document. We can express this using the
`HaXPath.Schematic` module:

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Data.Proxy                  (Proxy(Proxy))
import qualified HaXPath.Schematic           as S
import           HaXPath.Schematic.Operators

-- Empty data type for our schema
data MenuSchema

root :: S.DocumentRoot MenuSchema
root = S.root

-- Type of the <menu> node.
data Menu

instance IsNode Menu where
  nodeName _ = "menu"

-- A <menu> node.
menu :: S.Node Menu
menu = S.namedNode

-- Type of the <item> node.
data Item

instance IsNode Item where
  nodeName _ = "item"

-- An <item> node.
item :: S.Node Item
item = S.namedNode "item"

-- Type of the "name" attribute
data Name

instance IsAttribute Name where
  attributeName _ = "name"

-- @name
-- "as" is a type-level list of attributes used within the expression.
-- The "Member" constraint is used to show that the type Name is a member of "as".
-- This constraint can then be used to verify that it is only used within the context of a node that can actually have
-- the name attribute.
name :: S.Member Name as => S.Text as
name = S.at (Proxy :: Proxy Name)

-- Type of the "price" attribute
data Price

instance IsAttribute Price where
  attributeName _ = "price"

-- @price
price :: S.Member Price as => S.Text as
price = S.at (Proxy :: Proxy Price)

-- Menu is the only possible node at the top level of the document
type instance S.Relatives (S.DocumentRoot MenuSchema) S.Child '[Menu]

-- The only possible child node of a menu is item
type instance S.Relatives Menu S.Child '[Item]

-- An <item> may have "name" and "price" attributes.
type instance S.Attributes Item '[Name, Price]

-- Select all the waffles items
S.show (root /. menu /. item # name `S.contains` "Waffle") == "/child::menu/child::item[contains(@name, 'Waffle')]"

-- The following will not type check because <menu> does not have a price
root /. menu # price =. "$7.50"

-- The following will not type check because <item> cannot exist at the top level of the document
root /. item
```
