{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaXPath.Schematic.Examples where

import           Data.Proxy                  (Proxy(Proxy))
import qualified HaXPath.Schematic           as S
import           HaXPath.Schematic.Operators

-- Empty data type for our schema
data MenuSchema

-- Type of the document root in our schema
type MenuRoot = S.DocumentRoot MenuSchema

-- Type of absolute paths in our schema which return nodes of type rn
type AbsolutePath rn = S.AbsolutePath MenuSchema rn

root :: MenuRoot
root = S.root

-- Type of the <menu> node.
data Menu

-- We need to provide an XML identifier for the node, otherwise it won't compile if we try to use it as a node
instance S.IsNode Menu where
  nodeName _ = "menu"

-- A <menu> node.
menu :: S.Node Menu
menu = S.namedNode

-- Type of the <item> node.
data Item

instance S.IsNode Item where
  nodeName _ = "item"

-- An <item> node.
item :: S.Node Item
item = S.namedNode

-- Type of the "name" attribute
data Name

instance S.IsAttribute Name where
  attributeName _ = "name"

-- @name attribute
-- "as" is a type-level list of attributes used within the expression.
-- The "Member" constraint is used to show that the type Name is a member of "as".
-- This constraint can then be used to verify that it is only used within the context of a node that can actually have
-- the name attribute.
name :: S.Member Name as => S.Text as
name = S.at (Proxy :: Proxy Name)

-- Type of the "price" attribute
data Price

instance S.IsAttribute Price where
  attributeName _ = "price"

-- @price
price :: S.Member Price as => S.Text as
price = S.at (Proxy :: Proxy Price)

-- Menu is the only possible node at the top level of the document
type instance S.Relatives (S.DocumentRoot MenuSchema) S.Child = '[Menu]

-- The only possible child node of a menu is item
type instance S.Relatives Menu S.Child = '[Item]

-- An <item> may have "name" and "price" attributes.
type instance S.Attributes Item = '[Name, Price]

-- Select all the waffles items with a certain price
-- The equivalent of "/menu/item[contains(@name, 'Waffle') and @price = 7.50]"
p0 :: AbsolutePath Item
p0 = root /. menu /. item # [name `S.contains` "Waffle" &&. price =. "$7.50"]

-- The following will not type check because <menu> does not have a price
-- root /. menu # price =. "$7.50"

-- The following will not type check because <item> cannot exist at the top level of the document
-- root /. item
