{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Test (suite) where

import qualified Test.HUnit as H
import HaXPath ((/.), (//.), (=.), (#))
import qualified HaXPath as X

testAppend :: H.Test
testAppend = H.TestLabel "append" . H.TestCase $ do
  H.assertEqual "Child" "/descendant-or-self::node()/child::a/child::b"  (X.showPath $ X.fromAnywhere "a" /. "b")
  H.assertEqual
    "Descendent or self"
    "/descendant-or-self::node()/child::a/descendant-or-self::node()/child::b"
    (X.showPath $ X.fromAnywhere "a" //. "b")

testAttribute :: H.Test
testAttribute = H.TestLabel "attribute" . H.TestCase $
  H.assertEqual "Attribute equality" "(a)[@id = 'hello']" (X.showPath $ X.fromCurrent "a" # X.at "id" =. "hello")

testContext :: H.Test
testContext = H.TestLabel "context" . H.TestCase $ do
  H.assertEqual "//" "/descendant-or-self::node()/child::a" (X.showPath $ X.fromAnywhere "a")
  H.assertEqual "/" "/child::a" (X.showPath $ X.fromRoot "a")
  H.assertEqual "Implicit" "a" (X.showPath $ X.fromCurrent "a")

testFunction :: H.Test
testFunction = H.TestLabel "function" . H.TestCase $ do
  H.assertEqual "text()" "(a)[text() = 'hello']" (X.showPath $ X.fromCurrent "a" # X.text =. "hello")
  H.assertEqual
    "contans()" "(a)[contains(text(), 'hello')]"
    (X.showPath $ X.fromCurrent "a" # X.text `X.contains` "hello")

suite :: H.Test
suite = H.TestLabel "HaXPath" $ H.TestList [testAppend, testAttribute, testContext, testFunction]
