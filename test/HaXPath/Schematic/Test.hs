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
  H.assertEqual "Attribute equality" "child::a[@id = 'hello']" (S.show $ S.child a # id' =. ("hello" :: S.Text))

suite :: H.Test
suite = H.TestLabel "HaXPath.Schematic" $ H.TestList [
    testAppend,
    testAttribute
  ]
