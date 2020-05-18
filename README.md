# HaXPath
HaXPath is a library and embedded domain-specifc language which enables expression of strongly-typed XPath expressions
within Haskell.

## Motivation
In many contexts when querying XML documents in Haskell we often need to use `String` values to represent the
XPaths we want to use. These `String` expressions can quickly become hard to manage as they do not take advantage of
Haskell's type system, particularly for more complex XPaths. We may not know until run-time whether the XPath is even
syntactically valid. HaXPath does not have its own XPath engine to run the queries, rather it is expected to be used
in combination with other libraries which have such functionality. Instead, we can simply convert the typeful XPath
expressions to `String` or `Text` and send them to our favourite APIs.

## HaXPath API
HaXPath provides two core APIs: the standard API (`HaXPath` module) allows for expressing generic XPaths, while
the schematic API (`HaXPath.Schematic` module) is a layer of abstraction built upon the standard API which constrains
XPath expressions so they must follow a specifc document schema. Such a schema could include HTML for exmaple. In
future an implementation of the schematic API for HTML will be provided. The API Haddock can be found here under
`doc/index.html`. 

### Standard API
`HaXPath` modules are expected to be imported qualified as otherwise you will get name conflicts with the Prelude. The
operators however need not be qualified, and can conveniently be imported directly from `HaXPath.Operators`.

Some basic examples:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified HaXPath as X
import           HaXPath.Operators

-- Create XPath nodes "a" and "b"
let a = X.namedNode "a"
let b = X.namedNode "b"

-- The XPath "/descendant-or-self::node()/child::a/child::b"
let p1 = X.fromRoot $ X.descendantOrSelf X.node ./. X.child a ./. X.child b

-- The same XPath as above but in abbreviated form
let p2 = X.fromRoot $ X.doubleSlash a /. b

-- Convert paths to `Text`:
X.show p1 == "/descendant-or-self::node()/child::a/child::b"
X.show p2 == "/descendant-or-self::node()/child::a/child::b"
```

You can add qualifiers to filter node sets using the `#` operator:

```haskell
X.show (p1 # X.position =. 1) == "(/descendant-or-self::node()/child::a/child::b)[position() = 1]"
X.show (X.doubleSlash a /. b # X.position =. 1) === "/descendant-or-self::node()/child::a/child::b[position() = 1]"
X.show (X.doubleSlash a # X.at "id" =. "abc") === "/descendant-or-self::node()/child::a[@id = 'abc']"
```

Note the second argument to `#` must represent a boolean value, otherwise it will not type check.

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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified HaXPath.Schematic as S
import           HaXPath.Schematic.Operators

-- Empty data type for our schema
data MenuSchema

-- Type of the <menu> node.
data Menu

-- A <menu> node.
menu :: S.Node MenuSchema Menu
menu = S.namedNode "a"

-- Type of the <item> node.
data Item

-- An <item> node.
item :: S.Node MenuSchema Item
item = S.namedNode "item"

-- Type of the "name" attribute
data Name

-- An <item> may have a "name" attribute.
instance S.NodeAttribute Item Name

-- @name
name :: S.Attribute Name
name = S.at "name"

-- Type of the "price" attribute
data Price

-- An <item> may have a "price" attribute.
instance S.NodeAttribute Item Price

-- @price
price :: S.Attribute Price
price = S.at "price"

-- Our menu schema may only have <menu> and <item> nodes.
instance S.SchemaNodes MenuSchema '[Menu, Item]

-- Select all the waffles items
p = S.fromRoot $ S.child menu ./ item # name `S.contains` ("Waffle" :: S.Text)
S.show p == "/child::menu/child::item[contains(@name, 'Waffle')]"

-- The follwing will not type check because <menu> does not have a price
S.fromRoot $ S.child menu # price =. ("$7.50" :: S.Text)
```

Currently there are no constraints on the axis relationships bewteen nodes. For example, there is no constraint that
`<item>` must be a child of `<menu>` in the above example. This will be added in future.
