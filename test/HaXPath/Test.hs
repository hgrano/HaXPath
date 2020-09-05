{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Test (suite) where

import qualified HaXPath           as X
import           HaXPath.Operators
import qualified Test.HUnit        as H

a :: X.Node
a = X.namedNode "a"

b :: X.Node
b = X.namedNode "b"

c :: X.Node
c = X.namedNode "c"

d :: X.Node
d = X.namedNode "d"

testAppend :: H.Test
testAppend = H.TestLabel "append" . H.TestCase $ do
  H.assertEqual "ancestor" "/ancestor::a" (X.show . X.fromRoot $ X.ancestor a)
  H.assertEqual
    "Child"
    "/descendant-or-self::node()/child::a/child::b"
    (X.show . X.fromRoot $ X.descendantOrSelf X.node ./. X.child a ./. X.child b)
  H.assertEqual
    "Child(abbrev)"
    "/descendant-or-self::node()/child::a/child::b"
    (X.show $ X.doubleSlash a /. b)
  H.assertEqual
    "Child(abbrev) with brackets"
    "child::a/child::b/child::c"
    (X.show $ X.child a ./. (X.child b /. c))
  H.assertEqual "descendant" "/descendant::a" (X.show . X.fromRoot $ X.descendant a)
  H.assertEqual
    "Descendent or self"
    "/descendant-or-self::node()/child::a/descendant-or-self::node()/child::b"
    (X.show $ X.doubleSlash a //. b)
  H.assertEqual "following" "/following::a" (X.show . X.fromRoot $ X.following a)
  H.assertEqual "following" "/following-sibling::a" (X.show . X.fromRoot $ X.followingSibling a)
  H.assertEqual "parent" "/parent::a" (X.show . X.fromRoot $ X.parent a)

testAttribute :: H.Test
testAttribute = H.TestLabel "attribute" . H.TestCase $
  H.assertEqual "Attribute equality" "child::a[@id = 'hello']" (X.show $ X.child a # X.at "id" =. "hello")

testBool :: H.Test
testBool = H.TestLabel "bool" . H.TestCase $ do
  H.assertEqual
    "and"
    "child::a[(text() = 'abc') and contains(@id, 'def')]"
    (X.show $ X.child a # (X.text =. "abc" &&. X.at "id" `X.contains` "def"))
  H.assertEqual
    "or"
    "child::a[(text() = 'abc') or contains(@id, 'def')]"
    (X.show $ X.child a # (X.text =. "abc" ||. X.at "id" `X.contains` "def"))
  H.assertEqual
    "not"
    "child::a[(text() = 'abc') or contains(@id, 'def')]"
    (X.show $ X.child a # (X.text =. "abc" ||. X.at "id" `X.contains` "def"))
  H.assertEqual
    "!="
    "child::a[text() != 'abc']"
    (X.show $ X.child a # X.text /=. "abc")
  H.assertEqual
    "true"
    "child::a[true()]"
    (X.show $ X.child a # X.lit True)
  H.assertEqual
    "false"
    "child::a[false()]"
    (X.show $ X.child a # X.lit False)
  H.assertEqual
    "false"
    "child::a[false() and (text() != 'abc')]"
    (X.show $ X.child a # (X.lit False &&. X.text /=. "abc"))

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
    (X.show $ X.child a # X.count (X.child b /. c # X.at "id" =. "id") =. 3)
  H.assertEqual
    "count() [absolute]"
    "child::a[count(/child::b/child::c[@id = 'id']) = 3]"
    (X.show $ X.child a # X.count (X.fromRoot $ X.child b /. c # X.at "id" =. "id") =. 3)
  H.assertEqual "not()" "child::a[not(@id = 'id')]" (X.show $ X.child a # X.not (X.at "id" =. "id"))

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
  H.assertEqual "<=" "child::a[2 <= position()]" (X.show $ X.child a # 2 <=. X.position)
  H.assertEqual ">" "child::a[2 > position()]" (X.show $ X.child a # 2 >. X.position)
  H.assertEqual ">=" "child::a[2 >= position()]" (X.show $ X.child a # 2 >=. X.position)

testPredicate :: H.Test
testPredicate = H.TestLabel "path" . H.TestCase $ do
  H.assertEqual
    "filter node"
    "/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id']"
    (X.show $ X.doubleSlash a /. b /. c # X.at "id" =. "id")

  H.assertEqual
    "filter absolute"
    "(/descendant-or-self::node()/child::a/child::b/child::c)[@id = 'id']"
    (X.show $ (X.doubleSlash a /. b /. c) # X.at "id" =. "id")

  H.assertEqual
    "double filter"
    "(/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id'])[@id = 'id']"
     (X.show $ (X.doubleSlash a /. b /. c # X.at "id" =. "id") # X.at "id" =. "id")

  H.assertEqual
    "filter in middle"
    "/descendant-or-self::node()/child::a/child::b[@id = 'id']/child::c"
    (X.show $ X.doubleSlash a ./. (X.child b # X.at "id" =. "id") /. c)

  H.assertEqual
    "filter in middle (abbrev)"
    "/descendant-or-self::node()/child::a/child::b[@id = 'id']/child::c"
    (X.show $ X.doubleSlash a /. (b # X.at "id" =. "id") /. c)

  H.assertEqual
    "filter in middle with 2 nodes"
    "/descendant-or-self::node()/child::a/(child::b/child::c)[@id = 'id']/child::d"
    (X.show $ X.doubleSlash a ./. ((X.child b /. c) # X.at "id" =. "id") /. d)

  H.assertEqual
    "brackets + brackets"
    "child::a/child::b/(child::c/child::d)[@id = 'id2']"
    (X.show $ X.child a ./. (X.child b ./. ((X.child c /. d) # X.at "id" =. "id2")))

  H.assertEqual
    "filtering bracketed expression"
    "(child::a/child::b)[@id = 'id'][position() = 2]"
    (X.show $ ((X.child a /. b) # X.at "id" =. "id") # X.position =. 2)

  H.assertEqual
    "filtering bracketed expression with prev"
    "(child::a/(child::b/child::c)[@id = 'id'])[position() = 2]"
    (X.show $ (X.child a ./. ((X.child b /. c) # X.at "id" =. "id")) # X.position =. 2)

  H.assertEqual
    "Two filters"
    "(child::a/child::b)[@id = 'id'][position() = 2]"
    (X.show $ (X.child a /. b) # X.at "id" =. "id" # X.position =. 2)

suite :: H.Test
suite = H.TestLabel "HaXPath" $ H.TestList [
    testAppend,
    testAttribute,
    testBool,
    testContext,
    testFunction,
    testNum,
    testOrd,
    testPredicate
  ]
