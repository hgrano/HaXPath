{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module HaXPath.Typed(
  AttributeUnion,
  Bool,
  Expression,
  fromAnywhere,
  fromRoot,
  il,
  Int,
  IsPath(..),
  NodeAttribute,
  NodeAttributes,
  not,
  or,
  Path,
  RelativePath,
  SchemaNode,
  text,
  Text,
  tl,
  unsafeAttribute,
  unsafeNode,
  (.=),
  (./),
  (.//)
) where

import Data.Maybe (isJust)
import qualified Data.Text as T
import Prelude ((&&), (.), ($), (<>), (<$>))
import qualified Prelude as P

data Text
data Int
data Bool

class AttributeUnion (l :: [*]) (m :: [*]) (lm :: [*]) | l m -> lm

instance AttributeUnion '[] m m

instance AttributeUnion l' m lm => AttributeUnion (l ': l') m (l ': lm)

data Expression' = Function T.Text [Expression'] |
                   Operator T.Text Expression' Expression' |
                   Attribute T.Text |
                   TextLiteral T.Text |
                   IntegerLiteral P.Integer deriving (P.Eq, P.Ord)

showExpression :: Expression' -> T.Text
showExpression (Function f exps) = f <> "(" <> args <> ")"
  where
    args = T.intercalate ", " $ showExpression <$> exps
showExpression (Operator o a b) = "(" <> showExpression a <> ") " <> o <> " (" <> showExpression b <> ")"
showExpression (Attribute a) = "@" <> a
showExpression (TextLiteral t) = "'" <> t <> "'"
showExpression (IntegerLiteral i) = T.pack $ P.show i

showExpressionBracketed :: Expression' -> T.Text
showExpressionBracketed e = "[" <> showExpression e <> "]"

showExpressions :: [Expression'] -> T.Text
showExpressions = T.concat . P.fmap showExpressionBracketed . P.reverse

newtype Expression (t :: *) (a :: [*]) = Expression Expression' deriving (P.Eq, P.Ord)

-- | Takes a 'Text' and creates a literal text value.
tl :: T.Text -> Expression Text '[]
tl = Expression . TextLiteral

-- | Takes an 'Integer' and creates a literal integer value.
il :: P.Integer -> Expression Int '[]
il = Expression . IntegerLiteral

-- | The XPath 'text()' function.
text :: Expression Text '[]
text = Expression $ Function "text" []

or :: AttributeUnion a b c => Expression Bool a -> Expression Bool b -> Expression Bool c
Expression x `or` Expression y = Expression $ Operator "or" x y
infixr 4 `or`

(.=) :: AttributeUnion a b c => Expression t a -> Expression t b -> Expression Bool c
Expression x .= Expression y = Expression $ Operator "=" x y
infixr 6 .=

not :: Expression Bool a -> Expression Bool a
not (Expression x) = Expression $ Function "not" [x]

unsafeAttribute :: T.Text -> Expression Text a
unsafeAttribute = Expression . Attribute

data Axis = Ancestor |
            Child |
            Descendant |
            DescendantOrSelf |
            Parent deriving (P.Eq, P.Ord)

showAxis :: Axis -> T.Text
showAxis axis =
  let a = case axis of
        Ancestor -> "ancestor"
        Child -> "child"
        Descendant -> "descendant"
        DescendantOrSelf -> "descendant-or-self::node()/child"
        Parent -> "parent"
  in
  a <> "::"

unsafeNode :: T.Text -> RelativePath s n
unsafeNode name = RelativePath $ RelativePath' name P.Nothing []

class NodeAttribute n a

class NodeAttributes (n :: *) (a :: [*])

instance (NodeAttribute n h, NodeAttributes n t) => NodeAttributes n (h ': t)

instance NodeAttributes n '[]

data RelativePath' = RelativePath' T.Text (P.Maybe (Axis, RelativePath')) [Expression'] deriving (P.Eq, P.Ord)

showRelativePath :: RelativePath' -> T.Text
showRelativePath (RelativePath' n nextMay es) =
  let esStr = showExpressions es
      nextStr = case nextMay of
        P.Just (axis, p) -> "/" <> showAxis axis <> showRelativePath p
        P.Nothing -> ""
  in
  let unqual = n <> nextStr in
  if P.not (P.null es) && isJust nextMay then
    "(" <> unqual <> ")" <> esStr
  else
    unqual <> esStr

-- | Relative path for some schema s returning a node of type n
newtype RelativePath s n = RelativePath RelativePath' deriving (P.Eq, P.Ord)

-- | Witnesses that for some schema 's' we can use nodes of type 'n'.
class SchemaNode s n

append' :: Axis -> RelativePath' -> RelativePath' -> RelativePath'
append' axis (RelativePath' n P.Nothing es) u = RelativePath' n (P.Just (axis, u)) es
append' axis (RelativePath' n (P.Just (a, p)) es) u = RelativePath' n (P.Just (a, append' axis p u)) es

append :: SchemaNode s n => Axis -> RelativePath s n' -> RelativePath s n -> RelativePath s n
append axis (RelativePath s) (RelativePath u) = RelativePath $ append' axis s u

(./) :: SchemaNode s n => RelativePath s m -> RelativePath s n -> RelativePath s n
(./) = append Child
infixr 2 ./

(.//) :: SchemaNode s n => RelativePath s m -> RelativePath s n -> RelativePath s n
(.//) = append DescendantOrSelf
infixr 2 .//

data Context = Anywhere | Root deriving (P.Eq, P.Ord)

data Path s n = Path Context (RelativePath s n) [Expression'] deriving (P.Eq, P.Ord)

fromAnywhere :: RelativePath s n -> Path s n
fromAnywhere r = Path Anywhere r []

fromRoot :: RelativePath s n -> Path s n
fromRoot r = Path Root r []

class IsPath t where
  (#) :: NodeAttributes n a => t s n -> Expression Bool a -> t s n
  infixr 3 #

  showPath :: t s n -> T.Text

instance IsPath RelativePath where
  RelativePath (RelativePath' n p es) # (Expression e) = RelativePath (RelativePath' n p (e : es))
  
  showPath (RelativePath p) = showAxis Child <> showRelativePath p

instance IsPath Path where
  Path c r es # (Expression e) = Path c r (e : es)

  showPath (Path c (RelativePath p) es) =
    let prefix = case c of
          Anywhere -> showAxis DescendantOrSelf
          Root -> showAxis Child
    in
    let s = "/" <> prefix <> showRelativePath p in
    if P.not (P.null es) then
      "(" <> s <> ")" <> showExpressions es
    else
      s
