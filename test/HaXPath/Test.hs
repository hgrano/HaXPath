{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Test (suite) where

import qualified Test.HUnit as H
import HaXPath ((/.), (//.), (=.), (/=.), (<.), (<=.), (>.), (>=.), (#))
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
  H.assertEqual "Attribute equality" "(a)[@id = 'hello']" (X.showRelativePath $ "a" # X.at "id" =. "hello")

testBool :: H.Test
testBool = H.TestLabel "bool" . H.TestCase $ do
  H.assertEqual
    "and"
    "(a)[(text() = 'abc') and contains(@id, 'def')]"
    (X.showRelativePath $ "a" # X.text =. "abc" `X.and` X.contains (X.at "id") "def")
  H.assertEqual
    "or"
    "(a)[(text() = 'abc') or contains(@id, 'def')]"
    (X.showRelativePath $ "a" # X.text =. "abc" `X.or` X.contains (X.at "id") "def")
  H.assertEqual
    "not"
    "(a)[(text() = 'abc') or contains(@id, 'def')]"
    (X.showRelativePath $ "a" # X.text =. "abc" `X.or` X.contains (X.at "id") "def")
  H.assertEqual
    "!="
    "(a)[text() != 'abc']"
    (X.showRelativePath $ "a" # X.text /=. "abc")
  H.assertEqual
    "true"
    "(a)[true()]"
    (X.showRelativePath $ "a" # True)
  H.assertEqual
    "false"
    "(a)[false()]"
    (X.showRelativePath $ "a" # False)

testContext :: H.Test
testContext = H.TestLabel "context" . H.TestCase $ do
  H.assertEqual "//" "/descendant-or-self::node()/child::a" (X.showPath $ X.fromAnywhere "a")
  H.assertEqual "/" "/child::a" (X.showPath $ X.fromRoot "a")

testFunction :: H.Test
testFunction = H.TestLabel "function" . H.TestCase $ do
  H.assertEqual "text()" "(a)[text() = 'hello']" (X.showRelativePath $ "a" # X.text =. "hello")
  H.assertEqual
    "contans()"
    "(a)[contains(text(), 'hello')]"
    (X.showRelativePath $ "a" # X.text `X.contains` "hello")

testNum :: H.Test
testNum = H.TestLabel "num" . H.TestCase $ do
  H.assertEqual "+" "(a)[(position() + 1) = 2]" (X.showRelativePath $ "a" # X.position + 1 =. 2)
  H.assertEqual "+" "(a)[(position() - 1) = 2]" (X.showRelativePath $ "a" # X.position - 1 =. 2)
  H.assertEqual "*" "(a)[(position() * 2) = 4]" (X.showRelativePath $ "a" # X.position * 2 =. 4)
  H.assertEqual
    "signum"
    "(a)[position() = (((0 - 4) > 0) - ((0 - 4) < 0))]"
    (X.showRelativePath $ "a" # X.position =. signum (-4))
  H.assertEqual
    "abs" "(a)[position() = ((0 - 4) * (((0 - 4) > 0) - ((0 - 4) < 0)))]"
    (X.showRelativePath $ "a" # X.position =. abs (-4))

testOrd :: H.Test
testOrd = H.TestLabel "ord" . H.TestCase $ do
  H.assertEqual "<" "(a)[2 < position()]" (X.showRelativePath $ "a" # 2 <. X.position)
  H.assertEqual "<" "(a)[2 <= position()]" (X.showRelativePath $ "a" # 2 <=. X.position)
  H.assertEqual ">" "(a)[2 > position()]" (X.showRelativePath $ "a" # 2 >. X.position)
  H.assertEqual ">=" "(a)[2 >= position()]" (X.showRelativePath $ "a" # 2 >=. X.position)

testPath :: H.Test
testPath = H.TestLabel "path" . H.TestCase $ do
  H.assertEqual
    "no bracket"
    "/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id']"
    (X.showIsPath $ X.fromAnywhere "a" /. "b" /. "c" # X.at "id" =. "id")

  H.assertEqual
    "bracket"
    "(/descendant-or-self::node()/child::a/child::b/child::c)[@id = 'id']"
    (X.showIsPath $ (X.fromAnywhere "a" /. "b" /. "c") # X.at "id" =. "id")

  H.assertEqual
    "bracket"
    "(/descendant-or-self::node()/child::a/child::b/child::c)[@id = 'id']"
    (X.showIsPath $ (X.fromAnywhere "a" /. "b" /. "c") # X.at "id" =. "id")

suite :: H.Test
suite = H.TestLabel "HaXPath" $ H.TestList [
    testAppend,
    testAttribute,
    testBool,
    testContext,
    testFunction,
    testNum,
    testOrd,
    testPath
  ]
