{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module HaXPath.Schematic.Test (suite) where

import           Data.Proxy                  (Proxy (Proxy))
import qualified HaXPath.Schematic           as S
import           HaXPath.Schematic.Operators
import qualified Test.HUnit                  as H

data Schema

data A

instance S.IsNode A where
  nodeName _ = "a"

a :: S.Node A
a = S.namedNode

data B

instance S.IsNode B where
  nodeName _ = "b"

b :: S.Node B
b = S.namedNode

data C

instance S.IsNode C where
  nodeName _ = "c"

c :: S.Node C
c = S.namedNode

data D

instance S.IsNode D where
  nodeName _ = "d"

d :: S.Node D
d = S.namedNode

type Root = S.DocumentRoot Schema

root :: Root
root = S.root

type instance S.Relatives Root S.Child = '[A]

type instance S.Relatives Root S.Descendant = '[A, B, C, D]

type instance S.Relatives A S.Child = '[B]

type instance S.Relatives A S.Descendant = '[B, C]

type instance S.Relatives A S.Following = '[A, B, C, D]

type instance S.Relatives A S.FollowingSibling = '[A]

type instance S.Relatives B S.Ancestor = '[A]

type instance S.Relatives B S.Child = '[C]

type instance S.Relatives B S.Descendant = '[C]

type instance S.Relatives B S.Parent = '[A]

type instance S.Relatives C S.Child = '[D]

data Id

instance S.IsAttribute Id where
  attributeName _ = "id"

id' :: S.Member Id as => S.Text as
id' = S.at (Proxy :: Proxy Id)

data Attr2

instance S.IsAttribute Attr2 where
  attributeName _ = "attr2"

attr2 :: S.Member Attr2 as => S.Text as
attr2 = S.at (Proxy :: Proxy Attr2)

type instance S.Attributes A = '[Attr2, Id]
type instance S.Attributes B = '[Id]
type instance S.Attributes C = '[Id]
type instance S.Attributes D = '[Id]

testAppend :: H.Test
testAppend = H.TestLabel "append" . H.TestCase $ do
  H.assertEqual "ancestor" "/(child::a/child::b)/ancestor::a" (S.show $ root /. a /. b /. S.ancestor a)
  H.assertEqual
    "Child"
    "/(descendant-or-self::node()/child::a)/child::b"
    (S.show $ root //. S.child a /. S.child b)
  H.assertEqual
    "Child(abbrev)"
    "/(descendant-or-self::node()/child::a)/child::b"
    (S.show $ root //. a /. b)
  H.assertEqual
    "Child(abbrev) with brackets"
    "child::a/(child::b/child::c)"
    (S.show $ a /. (b /. c))
  H.assertEqual "descendant" "/descendant::a" (S.show $ root /. S.descendant a)
  H.assertEqual
    "Descendent or self"
    "/((descendant-or-self::node()/child::a)/descendant-or-self::node())/child::b"
    (S.show $ root //. a //. b)
  H.assertEqual "following" "/child::a/following::a" (S.show $ root /. a /. S.following a)
  H.assertEqual "following" "/child::a/following-sibling::a" (S.show $ root /. a /. S.followingSibling a)
  H.assertEqual "parent" "/(child::a/child::b)/parent::a" (S.show $ root /. a /. b /. S.parent a)

testAttribute :: H.Test
testAttribute = H.TestLabel "attribute" . H.TestCase $ do
  H.assertEqual "Attribute equality" "a[@id = \"hello\"]" (S.show $ a # [id' =. "hello"])
  H.assertEqual "Attribute equality" "a[@attr2 = \"hello\"]" (S.show $ a # [attr2 =. "hello"])

testBool :: H.Test
testBool = H.TestLabel "bool" . H.TestCase $ do
  H.assertEqual
    "and"
    "a[(text() = \"abc\") and contains(@id, \"def\")]"
    (S.show $ a # [S.text =. "abc" &&. id' `S.contains` "def"])
  H.assertEqual
    "or"
    "a[(text() = \"abc\") or contains(@id, \"def\")]"
    (S.show $ a # [S.text =. "abc" ||. id' `S.contains` "def"])
  H.assertEqual
    "not"
    "a[(text() = \"abc\") or not(contains(@id, \"def\"))]"
    (S.show $ a # [S.text =. "abc" ||. id' `S.doesNotContain` "def"])
  H.assertEqual
    "!="
    "a[text() != \"abc\"]"
    (S.show $ a # [S.text /=. "abc"])
  H.assertEqual
    "true"
    "a[true()]"
    (S.show $ a # [S.true])
  H.assertEqual
    "false"
    "a[false()]"
    (S.show $ a # [S.false])
  H.assertEqual
    "false"
    "a[false() and (text() != \"abc\")]"
    (S.show $ a # [S.false &&. S.text /=. "abc"])

testContext :: H.Test
testContext = H.TestLabel "context" . H.TestCase $ do
  H.assertEqual "//" "/descendant-or-self::node()/child::a" (S.show $ root //. a)
  H.assertEqual "/" "/child::a" (S.show $ root /. a)

testFunction :: H.Test
testFunction = H.TestLabel "function" . H.TestCase $ do
  H.assertEqual "text()" "a[text() = \"hello\"]" (S.show $ a # [S.text =. "hello"])
  H.assertEqual
    "contains()"
    "a[contains(text(), \"hello\")]"
    (S.show $ a # [S.text `S.contains` "hello"])
  H.assertEqual
    "count() [relative]"
    "a[count(child::b/child::c[@id = \"id\"]) = 3]"
    (S.show $ a # [S.count (b /. c # [id' =. "id"]) =. 3])
  H.assertEqual
    "count() [absolute]"
    "a[count(/(child::a/child::b)/child::c[@id = \"id\"]) = 3]"
    (S.show $ a # [S.count (root /. a /. b /. c # [id' =. "id"]) =. 3])
  H.assertEqual
    "doesNotContain()"
    "a[not(contains(text(), \"hello\"))]"
    (S.show $ a # [S.text `S.doesNotContain` "hello"])
  H.assertEqual "not()" "a[not(@id = \"id\")]" (S.show $ a # [S.not (id' =. "id")])

testNum :: H.Test
testNum = H.TestLabel "num" . H.TestCase $ do
  H.assertEqual "+" "a[(position() + 1) = 2]" (S.show $ a # [S.position + 1 =. 2])
  H.assertEqual "+" "a[(position() + (0 - 1)) = 2]" (S.show $ a # [S.position - 1 =. 2])
  H.assertEqual "*" "a[(position() * 2) = 4]" (S.show $ a # [S.position * 2 =. 4])
  H.assertEqual
    "signum"
    "a[position() = (((0 - 4) > 0) - ((0 - 4) < 0))]"
    (S.show $ a # [S.position =. signum (-4)])
  H.assertEqual
    "abs" "a[position() = ((0 - 4) * (((0 - 4) > 0) - ((0 - 4) < 0)))]"
    (S.show $ a # [S.position =. abs (-4)])

testOrd :: H.Test
testOrd = H.TestLabel "ord" . H.TestCase $ do
  H.assertEqual "<" "a[2 < position()]" (S.show $ a # [2 <. S.position])
  H.assertEqual "<=" "a[2 <= position()]" (S.show $ a # [2 <=. S.position])
  H.assertEqual ">" "a[2 > position()]" (S.show $ a # [2 >. S.position])
  H.assertEqual ">=" "a[2 >= position()]" (S.show $ a # [2 >=. S.position])

testPredicate :: H.Test
testPredicate = H.TestLabel "path" . H.TestCase $ do
  H.assertEqual
    "filter node"
    "/((descendant-or-self::node()/child::a)/child::b)/child::c[@id = \"id\"]"
    (S.show $ root //. a /. b /. c # [id' =. "id"])

  H.assertEqual
    "filter absolute"
    "(/((descendant-or-self::node()/child::a)/child::b)/child::c)[@id = \"id\"]"
    (S.show $ (root //. a /. b /. c) # [id' =. "id"])

  H.assertEqual
    "double filter"
    "(/((descendant-or-self::node()/child::a)/child::b)/child::c[@id = \"id\"])[@id = \"id\"]"
     (S.show $ (root //. a /. b /. c # [id' =. "id"]) # [id' =. "id"])

  H.assertEqual
    "filter in middle"
    "/((descendant-or-self::node()/child::a)/child::b[@id = \"id\"])/child::c"
    (S.show $ root //. a /. (b # [id' =. "id"]) /. c)

  H.assertEqual
    "filter in middle (abbrev)"
    "/((descendant-or-self::node()/child::a)/child::b[@id = \"id\"])/child::c"
    (S.show $ root //. a /. (b # [id' =. "id"]) /. c)

  H.assertEqual
    "filter in middle with 2 nodes"
    "/((descendant-or-self::node()/child::a)/((child::b/child::c)[@id = \"id\"]))/child::d"
    (S.show $ root //. a /. ((b /. c) # [id' =. "id"]) /. d)

  H.assertEqual
    "brackets + brackets"
    "child::a/(child::b/((child::c/child::d)[@id = \"id2\"]))"
    (S.show $ a /. (b /. ((c /. d) # [id' =. "id2"])))

  H.assertEqual
    "filtering bracketed expression"
    "(child::a/child::b)[@id = \"id\"][position() = 2]"
    (S.show $ ((a /. b) # [id' =. "id"]) # [S.position =. 2])

  H.assertEqual
    "filtering bracketed expression with prev"
    "(child::a/((child::b/child::c)[@id = \"id\"]))[position() = 2]"
    (S.show $ (a /. ((b /. c) # [id' =. "id"])) # [S.position =. 2])

  H.assertEqual
    "Two filters"
    "(child::a/child::b)[@id = \"id\"][position() = 2]"
    (S.show $ (a /. b) # [id' =. "id"] # [S.position =. 2])

  H.assertEqual
    "Issue #8"
    "/((descendant-or-self::node()/child::a)[position() = 2])/child::b"
    (S.show $ (root //. a) # [S.position =. 2] /. b)

suite :: H.Test
suite = H.TestLabel "HaXPath.Schematic" $ H.TestList [
    testAppend,
    testAttribute,
    testBool,
    testContext,
    testFunction,
    testNum,
    testOrd,
    testPredicate
  ]
