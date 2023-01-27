{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaXPath.Schematic.Test where --(suite) where

-- import qualified HaXPath.Schematic           as S
-- import           HaXPath.Schematic.Operators
-- import qualified Test.HUnit                  as H

-- data Schema

-- data A

-- a :: S.Node Schema A
-- a = S.namedNode "a"

-- data B

-- b :: S.Node Schema B
-- b = S.namedNode "b"

-- data C

-- c :: S.Node Schema B
-- c = S.namedNode "c"

-- data D

-- d :: S.Node Schema D
-- d = S.namedNode "d"

-- instance S.SchemaNodes Schema '[A, B, C, D]

-- data Id

-- id' :: S.Attribute Id
-- id' = S.at "id"

-- instance S.NodeAttribute A Id
-- instance S.NodeAttribute B Id
-- instance S.NodeAttribute C Id
-- instance S.NodeAttribute D Id

-- abc :: S.Text
-- abc = "abc"

-- def :: S.Text
-- def = "def"

-- hello :: S.Text
-- hello = "hello"

-- one :: S.Number
-- one = 1

-- two :: S.Number
-- two = 2

-- three :: S.Number
-- three = 3

-- four :: S.Number
-- four = 4

-- testAppend :: H.Test
-- testAppend = H.TestLabel "append" . H.TestCase $ do
--   H.assertEqual "ancestor" "/ancestor::a" (S.show . S.fromRoot $ S.ancestor a)
--   H.assertEqual
--     "Child"
--     "/descendant-or-self::node()/child::a/child::b"
--     (S.show . S.fromRoot $ S.descendantOrSelf S.node ./. S.child a ./. S.child b)
--   H.assertEqual
--     "Child(abbrev)"
--     "/descendant-or-self::node()/child::a/child::b"
--     (S.show $ S.doubleSlash a /. b)
--   H.assertEqual
--     "Child(abbrev) with brackets"
--     "child::a/child::b/child::c"
--     (S.show $ S.child a ./. (S.child b /. c))
--   H.assertEqual "descendant" "/descendant::a" (S.show . S.fromRoot $ S.descendant a)
--   H.assertEqual
--     "Descendent or self"
--     "/descendant-or-self::node()/child::a/descendant-or-self::node()/child::b"
--     (S.show $ S.doubleSlash a //. b)
--   H.assertEqual "following" "/following::a" (S.show . S.fromRoot $ S.following a)
--   H.assertEqual "following" "/following-sibling::a" (S.show . S.fromRoot $ S.followingSibling a)
--   H.assertEqual "parent" "/parent::a" (S.show . S.fromRoot $ S.parent a)

-- testAttribute :: H.Test
-- testAttribute = H.TestLabel "attribute" . H.TestCase $
--   H.assertEqual "Attribute equality" "child::a[@id = 'hello']" (S.show $ S.child a # id' =. hello)

-- testBool :: H.Test
-- testBool = H.TestLabel "bool" . H.TestCase $ do
--   H.assertEqual
--     "and"
--     "child::a[(text() = 'abc') and contains(@id, 'def')]"
--     (S.show $ S.child a # (S.text =. abc &&. id' `S.contains` def))
--   H.assertEqual
--     "or"
--     "child::a[(text() = 'abc') or contains(@id, 'def')]"
--     (S.show $ S.child a # (S.text =. abc ||. id' `S.contains` def))
--   H.assertEqual
--     "not"
--     "child::a[(text() = 'abc') or contains(@id, 'def')]"
--     (S.show $ S.child a # (S.text =. abc ||. id' `S.contains` def))
--   H.assertEqual
--     "!="
--     "child::a[text() != 'abc']"
--     (S.show $ S.child a # S.text /=. abc)
--   H.assertEqual
--     "true"
--     "child::a[true()]"
--     (S.show $ S.child a # S.lit True)
--   H.assertEqual
--     "false"
--     "child::a[false()]"
--     (S.show $ S.child a # S.lit False)
--   H.assertEqual
--     "false"
--     "child::a[false() and (text() != 'abc')]"
--     (S.show $ S.child a # (S.lit False &&. S.text /=. abc))

-- testContext :: H.Test
-- testContext = H.TestLabel "context" . H.TestCase $ do
--   H.assertEqual "//" "/descendant-or-self::node()/child::a" (S.show $ S.doubleSlash a)
--   H.assertEqual "/" "/child::a" (S.show . S.fromRoot $ S.child a)

-- testFunction :: H.Test
-- testFunction = H.TestLabel "function" . H.TestCase $ do
--   H.assertEqual "text()" "child::a[text() = 'hello']" (S.show $ S.child a # S.text =. hello)
--   H.assertEqual
--     "contains()"
--     "child::a[contains(text(), 'hello')]"
--     (S.show $ S.child a # S.text `S.contains` hello)
--   H.assertEqual
--     "count() [relative]"
--     "child::a[count(child::b/child::c[@id = 'id']) = 3]"
--     (S.show $ S.child a # S.count (S.child b /. c # id' =. ("id" :: S.Text)) =. three)
--   H.assertEqual
--     "count() [absolute]"
--     "child::a[count(/child::b/child::c[@id = 'id']) = 3]"
--     (S.show $ S.child a # S.count (S.fromRoot $ S.child b /. c # id' =. ("id" :: S.Text)) =. three)
--   H.assertEqual
--     "doesNotContain()"
--     "child::a[not(contains(text(), 'hello'))]"
--     (S.show $ S.child a # S.text `S.doesNotContain` ("hello" :: S.Text))
--   H.assertEqual "not()" "child::a[not(@id = 'id')]" (S.show $ S.child a # S.not (id' =. ("id" :: S.Text)))

-- testNum :: H.Test
-- testNum = H.TestLabel "num" . H.TestCase $ do
--   H.assertEqual "+" "child::a[(position() + 1) = 2]" (S.show $ S.child a # S.position +. one =. two)
--   H.assertEqual "+" "child::a[(position() - 1) = 2]" (S.show $ S.child a # S.position -. one =. two)
--   H.assertEqual "*" "child::a[(position() * 2) = 4]" (S.show $ S.child a # S.position *. two =. four)
--   H.assertEqual
--     "signum"
--     "child::a[position() = (((0 - 4) > 0) - ((0 - 4) < 0))]"
--     (S.show $ S.child a # S.position =. S.signum (-four))
--   H.assertEqual
--     "abs" "child::a[position() = ((0 - 4) * (((0 - 4) > 0) - ((0 - 4) < 0)))]"
--     (S.show $ S.child a # S.position =. S.abs (-four))

-- testOrd :: H.Test
-- testOrd = H.TestLabel "ord" . H.TestCase $ do
--   H.assertEqual "<" "child::a[2 < position()]" (S.show $ S.child a # two <. S.position)
--   H.assertEqual "<=" "child::a[2 <= position()]" (S.show $ S.child a # two <=. S.position)
--   H.assertEqual ">" "child::a[2 > position()]" (S.show $ S.child a # two >. S.position)
--   H.assertEqual ">=" "child::a[2 >= position()]" (S.show $ S.child a # two >=. S.position)

-- testPredicate :: H.Test
-- testPredicate = H.TestLabel "path" . H.TestCase $ do
--   H.assertEqual
--     "filter node"
--     "/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id']"
--     (S.show $ S.doubleSlash a /. b /. c # id' =. ("id" :: S.Text))

--   H.assertEqual
--     "filter absolute"
--     "(/descendant-or-self::node()/child::a/child::b/child::c)[@id = 'id']"
--     (S.show $ (S.doubleSlash a /. b /. c) # id' =. ("id" :: S.Text))

--   H.assertEqual
--     "double filter"
--     "(/descendant-or-self::node()/child::a/child::b/child::c[@id = 'id'])[@id = 'id']"
--      (S.show $ (S.doubleSlash a /. b /. c # id' =. ("id" :: S.Text)) # id' =. ("id" :: S.Text))

--   H.assertEqual
--     "filter in middle"
--     "/descendant-or-self::node()/child::a/child::b[@id = 'id']/child::c"
--     (S.show $ S.doubleSlash a ./. (S.child b # id' =. ("id" :: S.Text)) /. c)

--   H.assertEqual
--     "filter in middle (abbrev)"
--     "/descendant-or-self::node()/child::a/child::b[@id = 'id']/child::c"
--     (S.show $ S.doubleSlash a /. (b # id' =. ("id" :: S.Text)) /. c)

--   H.assertEqual
--     "filter in middle with 2 nodes"
--     "/descendant-or-self::node()/child::a/(child::b/child::c)[@id = 'id']/child::d"
--     (S.show $ S.doubleSlash a ./. ((S.child b /. c) # id' =. ("id" :: S.Text)) /. d)

--   H.assertEqual
--     "brackets + brackets"
--     "child::a/child::b/(child::c/child::d)[@id = 'id2']"
--     (S.show $ S.child a ./. (S.child b ./. ((S.child c /. d) # id' =. ("id2" :: S.Text))))

--   H.assertEqual
--     "filtering bracketed expression"
--     "(child::a/child::b)[@id = 'id'][position() = 2]"
--     (S.show $ ((S.child a /. b) # id' =. ("id" :: S.Text)) # S.position =. two)

--   H.assertEqual
--     "filtering bracketed expression with prev"
--     "(child::a/(child::b/child::c)[@id = 'id'])[position() = 2]"
--     (S.show $ (S.child a ./. ((S.child b /. c) # id' =. ("id" :: S.Text))) # S.position =. two)

--   H.assertEqual
--     "Two filters"
--     "(child::a/child::b)[@id = 'id'][position() = 2]"
--     (S.show $ (S.child a /. b) # id' =. ("id" :: S.Text) # S.position =. two)

--   H.assertEqual
--     "Issue #8"
--     "(/descendant-or-self::node()/child::a)[position() = 2]/child::b"
--     (S.show $ S.doubleSlash a # S.position =. two /. b)

-- suite :: H.Test
-- suite = H.TestLabel "HaXPath.Schematic" $ H.TestList [
--     testAppend,
--     testAttribute,
--     testBool,
--     testContext,
--     testFunction,
--     testNum,
--     testOrd,
--     testPredicate
--   ]
