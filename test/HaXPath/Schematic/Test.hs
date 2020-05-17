{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Schematic.Test (suite) where

import qualified HaXPath.Schematic as S
import HaXPath.Schematic.Operators
import qualified Test.HUnit as H

data Schema

data A

a :: S.Node Schema A
a = S.namedNode "a"

data B

b :: S.Node Schema B
b = S.namedNode "b"

data C

c :: S.Node Schema B
c = S.namedNode "c"

instance S.SchemaNodes Schema '[A, B, C]

data Id

id' :: S.Attribute Id
id' = S.unsafeAt "id"

instance S.NodeAttribute A Id
instance S.NodeAttribute B Id
instance S.NodeAttribute C Id

abc :: S.Text
abc = "abc"

def :: S.Text
def = "def"

hello :: S.Text
hello = "hello"

testAppend :: H.Test
testAppend = H.TestLabel "append" . H.TestCase $ do
  H.assertEqual
    "Child"
    "/descendant-or-self::node()/child::a/child::b" 
    (S.show . S.fromRoot $ S.descendantOrSelf S.node ./. S.child a ./. S.child b)
  H.assertEqual
    "Child(abbrev)"
    "/descendant-or-self::node()/child::a/child::b" 
    (S.show $ S.doubleSlash a /. b)
  H.assertEqual
    "Child(abbrev) with brackets"
    "child::a/child::b/child::c" 
    (S.show $ S.child a ./. (S.child b /. c))
  H.assertEqual
    "Descendent or self"
    "/descendant-or-self::node()/child::a/descendant-or-self::node()/child::b"
    (S.show $ S.doubleSlash a //. b)

testAttribute :: H.Test
testAttribute = H.TestLabel "attribute" . H.TestCase $
  H.assertEqual "Attribute equality" "child::a[@id = 'hello']" (S.show $ S.child a # id' =. hello)

testBool :: H.Test
testBool = H.TestLabel "bool" . H.TestCase $ do
  H.assertEqual
    "and"
    "child::a[(text() = 'abc') and contains(@id, 'def')]"
    (S.show $ S.child a # (S.text =. abc &&. id' `S.contains` def))
  H.assertEqual
    "or"
    "child::a[(text() = 'abc') or contains(@id, 'def')]"
    (S.show $ S.child a # (S.text =. abc ||. id' `S.contains` def))
  H.assertEqual
    "not"
    "child::a[(text() = 'abc') or contains(@id, 'def')]"
    (S.show $ S.child a # (S.text =. abc ||. id' `S.contains` def))
  H.assertEqual
    "!="
    "child::a[text() != 'abc']"
    (S.show $ S.child a # S.text /=. abc)
--  H.assertEqual
--    "true"
--    "child::a[true()]"
--    (S.show $ S.child a # True)
--  H.assertEqual
--    "false"
--    "child::a[false()]"
--    (S.show $ S.child a # False)
--  H.assertEqual
--    "false"
--    "child::a[false() and (text() != 'abc')]"
--    (S.show $ S.child a # (False &&. S.text /=. abc))

testContext :: H.Test
testContext = H.TestLabel "context" . H.TestCase $ do
  H.assertEqual "//" "/descendant-or-self::node()/child::a" (S.show $ S.doubleSlash a)
  H.assertEqual "/" "/child::a" (S.show . S.fromRoot $ S.child a)

testFunction :: H.Test
testFunction = H.TestLabel "function" . H.TestCase $ do
  H.assertEqual "text()" "child::a[text() = 'hello']" (S.show $ S.child a # S.text =. hello)
  H.assertEqual
    "contains()"
    "child::a[contains(text(), 'hello')]"
    (S.show $ S.child a # S.text `S.contains` hello)
--  H.assertEqual
--    "count() [relative]"
--    "child::a[count(child::b/child::c[@id = 'id']) = 3]"
--    (S.show $ S.child a # S.count (S.child b /. c # id' =. ("id" :: S.Text)) =. 3)
--  H.assertEqual
--    "count() [absolute]"
--    "child::a[count(/child::b/child::c[@id = 'id']) = 3]"
--    (S.show $ S.child a # S.count (S.fromRoot $ S.child b /. c # id' =. ("id" :: S.Text)) =. 3)
  H.assertEqual "not()" "child::a[not(@id = 'id')]" (S.show $ S.child a # S.not (id' =. ("id" :: S.Text)))

suite :: H.Test
suite = H.TestLabel "HaXPath.Schematic" $ H.TestList [
    testAppend,
    testAttribute,
    testBool,
    testContext,
    testFunction
  ]
