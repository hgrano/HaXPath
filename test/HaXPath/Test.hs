{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Test (suite) where

import qualified Test.HUnit as H
import HaXPath ((/.), (./.), (//.), (=.), (/=.), (<.), (<=.), (>.), (>=.), (#))
import qualified HaXPath as X

a :: X.Node
a = X.namedNode "a"

b :: X.Node
b = X.namedNode "b"

c :: X.Node
c = X.namedNode "c"

testAppend :: H.Test
testAppend = H.TestLabel "append" . H.TestCase $ do
  H.assertEqual
    "Child"
    "/descendant-or-self::node()/child::a/child::b" 
    (X.show . X.fromRoot $ X.descendantOrSelf X.node ./. X.child a ./. X.child b)
  H.assertEqual
    "Child(abbrev)"
    "/descendant-or-self::node()/child::a/child::b" 
    (X.show $ X.doubleSlash a /. b)
  H.assertEqual
    "Descendent or self"
    "/descendant-or-self::node()/child::a/descendant-or-self::node()/child::b"
    (X.show $ X.doubleSlash a //. b)

testAttribute :: H.Test
testAttribute = H.TestLabel "attribute" . H.TestCase $
  H.assertEqual "Attribute equality" "child::a[@id = 'hello']" (X.show $ X.child a # X.at "id" =. "hello")

testBool :: H.Test
testBool = H.TestLabel "bool" . H.TestCase $ do
  H.assertEqual
    "and"
    "child::a[(text() = 'abc') and contains(@id, 'def')]"
    (X.show $ X.child a # X.text =. "abc" `X.and` X.contains (X.at "id") "def")
  H.assertEqual
    "or"
    "child::a[(text() = 'abc') or contains(@id, 'def')]"
    (X.show $ X.child a # X.text =. "abc" `X.or` X.contains (X.at "id") "def")
  H.assertEqual
    "not"
    "child::a[(text() = 'abc') or contains(@id, 'def')]"
    (X.show $ X.child a # X.text =. "abc" `X.or` X.contains (X.at "id") "def")
  H.assertEqual
    "!="
    "child::a[text() != 'abc']"
    (X.show $ X.child a # X.text /=. "abc")
  H.assertEqual
    "true"
    "child::a[true()]"
    (X.show $ X.child a # True)
  H.assertEqual
    "false"
    "child::a[false()]"
    (X.show $ X.child a # False)

testContext :: H.Test
testContext = H.TestLabel "context" . H.TestCase $ do
  H.assertEqual "//" "/descendant-or-self::node()/child::a" (X.show $ X.doubleSlash a)
  H.assertEqual "/" "/child::a" (X.show . X.fromRoot $ X.child a)

testFunction :: H.Test
testFunction = H.TestLabel "function" . H.TestCase $ do
  H.assertEqual "text()" "child::a[text() = 'hello']" (X.show $ X.child a # X.text =. "hello")
  H.assertEqual
    "contains()"
    "child::a[contains(text(), 'hello')]"
    (X.show $ X.child a # X.text `X.contains` "hello")
  H.assertEqual
    "count() [relative]"
    "child::a[count(child::b/child::c[@id = 'id']) = 3]"
    (X.show $ X.child a # X.count (X.child b /. (c # X.at "id" =. "id")) =. 3)
  H.assertEqual
    "count() [absolute]"
    "child::a[count(/child::b/child::c[@id = 'id']) = 3]"
    (X.show $ X.child a # X.count (X.fromRoot $ X.child b /. (c # X.at "id" =. "id")) =. 3)

testNum :: H.Test
testNum = H.TestLabel "num" . H.TestCase $ do
  H.assertEqual "+" "child::a[(position() + 1) = 2]" (X.show $ X.child a # X.position + 1 =. 2)
  H.assertEqual "+" "child::a[(position() - 1) = 2]" (X.show $ X.child a # X.position - 1 =. 2)
  H.assertEqual "*" "child::a[(position() * 2) = 4]" (X.show $ X.child a # X.position * 2 =. 4)
  H.assertEqual
    "signum"
    "child::a[position() = (((0 - 4) > 0) - ((0 - 4) < 0))]"
    (X.show $ X.child a # X.position =. signum (-4))
  H.assertEqual
    "abs" "child::a[position() = ((0 - 4) * (((0 - 4) > 0) - ((0 - 4) < 0)))]"
    (X.show $ X.child a # X.position =. abs (-4))

testOrd :: H.Test
testOrd = H.TestLabel "ord" . H.TestCase $ do
  H.assertEqual "<" "child::a[2 < position()]" (X.show $ X.child a # 2 <. X.position)
  H.assertEqual "<" "child::a[2 <= position()]" (X.show $ X.child a # 2 <=. X.position)
  H.assertEqual ">" "child::a[2 > position()]" (X.show $ X.child a # 2 >. X.position)
  H.assertEqual ">=" "child::a[2 >= position()]" (X.show $ X.child a # 2 >=. X.position)

testPath :: H.Test
testPath = H.TestLabel "path" . H.TestCase $ do
  H.assertEqual
    "filter node"
    "/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id']"
    (X.show $ X.doubleSlash a /. b /. (c # X.at "id" =. "id"))

  H.assertEqual
    "filter absolute"
    "(/descendant-or-self::node()/child::a/child::b/child::c)[@id = 'id']"
    (X.show $ (X.doubleSlash a /. b /. c) # X.at "id" =. "id")

  H.assertEqual
    "double filter"
    "(/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id'])[@id = 'id']"
     (X.show $ (X.doubleSlash a /. b /. (c # X.at "id" =. "id")) # X.at "id" =. "id")

  H.assertEqual
    "filter in middle"
    "/descendant-or-self::node()/child::a/(child::b)[@id = 'id']/child::c"
    (X.show $ X.doubleSlash a ./. (X.child b # X.at "id" =. "id") /. c)

  H.assertEqual
    "filter in middle (abbrev)"
    "/descendant-or-self::node()/child::a/(child::b)[@id = 'id']/child::c"
    (X.show $ X.doubleSlash a /. (b # X.at "id" =. "id") /. c)

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
