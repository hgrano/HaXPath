{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Test (suite) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Builder as BSBuilder
import           Data.String             (IsString)
import           Data.Text               (Text)
import qualified Data.Text.Lazy.Builder  as TBuilder
import qualified HaXPath                 as X
import           HaXPath.Operators
import qualified Test.HUnit              as H

a :: IsString s => X.Node' s
a = X.namedNode "a"

b :: IsString s => X.Node' s
b = X.namedNode "b"

c :: IsString s => X.Node' s
c = X.namedNode "c"

d :: IsString s => X.Node' s
d = X.namedNode "d"

testAppend :: H.Test
testAppend = H.TestLabel "append" . H.TestCase $ do
  H.assertEqual "ancestor" "/ancestor::a" (X.show $ X.root /. X.ancestor a)
  H.assertEqual
    "Child"
    "/(descendant-or-self::node()/child::a)/child::b"
    (X.show $ X.root /. X.descendantOrSelf X.node /. X.child a /. X.child b)
  H.assertEqual
    "Child(abbrev)"
    "/(descendant-or-self::node()/child::a)/child::b"
    (X.show $ X.root //. a /. b)
  H.assertEqual
    "Child(abbrev) with brackets"
    "child::a/(child::b/child::c)"
    (X.show $ a /. (b /. c))
  H.assertEqual "descendant" "/descendant::a" (X.show $ X.root /. X.descendant a)
  H.assertEqual
    "Descendent or self"
    "/((descendant-or-self::node()/child::a)/descendant-or-self::node())/child::b"
    (X.show $ X.root //. a //. b)
  H.assertEqual "following" "/following::a" (X.show $ X.root /. X.following a)
  H.assertEqual "following" "/following-sibling::a" (X.show $ X.root /. X.followingSibling a)
  H.assertEqual "parent" "/parent::a" (X.show $ X.root /. X.parent a)

testAttribute :: H.Test
testAttribute = H.TestLabel "attribute" . H.TestCase $
  H.assertEqual "Attribute equality" "a[@id = \"hello\"]" (X.show $ a # [X.at "id" =. "hello"])

testBool :: H.Test
testBool = H.TestLabel "bool" . H.TestCase $ do
  H.assertEqual
    "and"
    "a[(text() = \"abc\") and contains(@id, \"def\")]"
    (X.show $ a # [X.text =. "abc" &&. X.at "id" `X.contains` "def"])
  H.assertEqual
    "or"
    "a[(text() = \"abc\") or contains(@id, \"def\")]"
    (X.show $ a # [X.text =. "abc" ||. X.at "id" `X.contains` "def"])
  H.assertEqual
    "not"
    "a[(text() = \"abc\") or contains(@id, \"def\")]"
    (X.show $ a # [X.text =. "abc" ||. X.at "id" `X.contains` "def"])
  H.assertEqual
    "!="
    "a[text() != \"abc\"]"
    (X.show $ a # [X.text /=. "abc"])
  H.assertEqual
    "true"
    "a[true()]"
    (X.show $ a # [X.true])
  H.assertEqual
    "false"
    "a[false()]"
    (X.show $ a # [X.false])
  H.assertEqual
    "false"
    "a[false() and (text() != \"abc\")]"
    (X.show $ a # [X.false &&. X.text /=. "abc"])

testContext :: H.Test
testContext = H.TestLabel "context" . H.TestCase $ do
  H.assertEqual "//" "/descendant-or-self::node()/child::a" (X.show $ X.root //. a)
  H.assertEqual "/" "/child::a" (X.show $ X.root /. a)
  H.assertEqual "/ - using operator" "/((a) | (b))" (X.show $ X.root /. (a |. b))

testFunction :: H.Test
testFunction = H.TestLabel "function" . H.TestCase $ do
  H.assertEqual "text()" "a[text() = \"hello\"]" (X.show $ a # [X.text =. "hello"])
  H.assertEqual
    "contains()"
    "a[contains(text(), \"hello\")]"
    (X.show $ a # [X.text `X.contains` "hello"])
  H.assertEqual
    "count() [relative]"
    "a[count(child::b/child::c[@id = \"id\"]) = 3]"
    (X.show $ a # [X.count (b /. c # [X.at "id" =. "id"]) =. 3])
  H.assertEqual
    "count() [absolute]"
    "a[count(/child::b/child::c[@id = \"id\"]) = 3]"
    (X.show $ a # [X.count (X.root /. b /. c # [X.at "id" =. "id"]) =. 3])
  H.assertEqual
    "doesNotContain()"
    "a[not(contains(text(), \"hello\"))]"
    (X.show $ a # [X.text `X.doesNotContain` "hello"])
  H.assertEqual "not()" "a[not(@id = \"id\")]" (X.show $ a # [X.not (X.at "id" =. "id")])

testNum :: H.Test
testNum = H.TestLabel "num" . H.TestCase $ do
  H.assertEqual "+" "a[(position() + 1) = 2]" (X.show $ a # [X.position + 1 =. 2])
  H.assertEqual "+" "a[(position() - 1) = 2]" (X.show $ a # [X.position - 1 =. 2])
  H.assertEqual "*" "a[(position() * 2) = 4]" (X.show $ a # [X.position * 2 =. 4])
  H.assertEqual
    "signum"
    "a[position() = (((0 - 4) > 0) - ((0 - 4) < 0))]"
    (X.show $ a # [X.position =. signum (-4)])
  H.assertEqual
    "abs" "a[position() = ((0 - 4) * (((0 - 4) > 0) - ((0 - 4) < 0)))]"
    (X.show $ a # [X.position =. abs (-4)])

testOrd :: H.Test
testOrd = H.TestLabel "ord" . H.TestCase $ do
  H.assertEqual "<" "a[2 < position()]" (X.show $ a # [2 <. X.position])
  H.assertEqual "<=" "a[2 <= position()]" (X.show $ a # [2 <=. X.position])
  H.assertEqual ">" "a[2 > position()]" (X.show $ a # [2 >. X.position])
  H.assertEqual ">=" "a[2 >= position()]" (X.show $ a # [2 >=. X.position])

testPredicate :: H.Test
testPredicate = H.TestLabel "predicates" . H.TestCase $ do
  H.assertEqual
    "filter node"
    "/((descendant-or-self::node()/child::a)/child::b)/child::c[@id = \"id\"]"
    (X.show $ X.root //. a /. b /. c # [X.at "id" =. "id"])

  H.assertEqual
    "filter absolute"
    "(/((descendant-or-self::node()/child::a)/child::b)/child::c)[@id = \"id\"]"
    (X.show $ (X.root //. a /. b /. c) # [X.at "id" =. "id"])

  H.assertEqual
    "double filter"
    "(/((descendant-or-self::node()/child::a)/child::b)/child::c[@id = \"id\"])[@id = \"id\"]"
     (X.show $ (X.root //. a /. b /. c # [X.at "id" =. "id"]) # [X.at "id" =. "id"])

  H.assertEqual
    "filter in middle"
    "/((descendant-or-self::node()/child::a)/child::b[@id = \"id\"])/child::c"
    (X.show $ X.root //. a /. (b # [X.at "id" =. "id"]) /. c)

  H.assertEqual
    "filter in middle (abbrev)"
    "/((descendant-or-self::node()/child::a)/child::b[@id = \"id\"])/child::c"
    (X.show $ X.root //. a /. (b # [X.at "id" =. "id"]) /. c)

  H.assertEqual
    "filter in middle with 2 nodes"
    "/((descendant-or-self::node()/child::a)/((child::b/child::c)[@id = \"id\"]))/child::d"
    (X.show $ X.root //. a /. ((b /. c) # [X.at "id" =. "id"]) /. d)

  H.assertEqual
    "brackets + brackets"
    "child::a/(child::b/((child::c/child::d)[@id = \"id2\"]))"
    (X.show $ a /. (b /. ((c /. d) # [X.at "id" =. "id2"])))

  H.assertEqual
    "filtering bracketed expression"
    "(child::a/child::b)[@id = \"id\"][position() = 2]"
    (X.show $ ((a /. b) # [X.at "id" =. "id"]) # [X.position =. 2])

  H.assertEqual
    "filtering bracketed expression with prev"
    "(child::a/((child::b/child::c)[@id = \"id\"]))[position() = 2]"
    (X.show $ (a /. ((b /. c) # [X.at "id" =. "id"])) # [X.position =. 2])

  H.assertEqual
    "Two filters"
    "(child::a/child::b)[@id = \"id\"][position() = 2]"
    (X.show $ (a /. b) # [X.at "id" =. "id", X.position =. 2])

  H.assertEqual
    "Issue #8"
    "/(descendant-or-self::node()/child::a[position() = 2])/child::b"
    (X.show $ X.root //. a # [X.position =. 2] /. b)

testShowGeneric :: H.Test
testShowGeneric = H.TestLabel "show generic" . H.TestCase $ do
  H.assertEqual "Show Text" (expectedShow :: Text) (X.show' path)
  H.assertEqual "Show ByteString" (expectedShow :: ByteString) (X.show' path)
  H.assertEqual
    "Show ByteString from builder"
    (BSBuilder.toLazyByteString expectedShow)
    (BSBuilder.toLazyByteString $ X.show' path)
  H.assertEqual
    "Show Text from builder"
    (TBuilder.toLazyText expectedShow)
    (TBuilder.toLazyText $ X.show' path)

  where
    expectedShow :: IsString s => s
    expectedShow = "child::a/child::b[@id = \"hello \\\"world\\\"\"]"

    path :: IsString s => X.Path' X.CurrentContext s
    path = a /. b # [X.at "id" =. "hello \"world\""]

testUnion :: H.Test
testUnion = H.TestLabel "union" . H.TestCase $ do
  H.assertEqual "Union absolute paths" "(/child::a/child::b) | (/child::c)" (X.show $ X.root /. a /. b |. X.root /. c)
  H.assertEqual "Union relative paths" "(child::a/child::b) | (c)" (X.show $ a /. b |. c)

suite :: H.Test
suite = H.TestLabel "HaXPath" $ H.TestList [
    testAppend,
    testAttribute,
    testBool,
    testContext,
    testFunction,
    testNum,
    testOrd,
    testPredicate,
    testShowGeneric,
    testUnion
  ]
