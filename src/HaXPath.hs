{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HaXPath where

import Data.Maybe (isJust)
import qualified Data.String as S
import qualified Data.Text as T
import Prelude ((&&), (+), (-), (*), (.), ($), (<>), (<$>))
import qualified Prelude as P

data Text
data Int
data Bool
data NodeSet

data Expression' = Function T.Text [Expression'] |
                   Operator T.Text Expression' Expression' |
                   Attribute T.Text |
                   TextLiteral T.Text |
                   IntegerLiteral P.Integer |
                   Path Context RelativePath [Expression'] deriving (P.Eq, P.Ord)

showExpression :: Expression' -> T.Text
showExpression (Function f es) = f <> "(" <> args <> ")"
  where
    args = T.intercalate ", " $ showExpression <$> es
showExpression (Operator o a b) = "(" <> showExpression a <> ") " <> o <> " (" <> showExpression b <> ")"
showExpression (Attribute a) = "@" <> a
showExpression (TextLiteral t) = "'" <> t <> "'"
showExpression (IntegerLiteral i) = T.pack $ P.show i
showExpression (Path c p es) =
  let prefix = case c of
        Anywhere -> "/" <> showAxis DescendantOrSelf
        Root -> "/" <> showAxis Child
        Implicit -> ""
  in
  let s = prefix <> showRelativePath p in
  if P.not (P.null es) then
    "(" <> s <> ")" <> showExpressions es
  else
    s

showExpressionBracketed :: Expression' -> T.Text
showExpressionBracketed e = "[" <> showExpression e <> "]"

showExpressions :: [Expression'] -> T.Text
showExpressions = T.concat . P.fmap showExpressionBracketed . P.reverse

newtype Expression t = Expression Expression' deriving (P.Eq, P.Ord)

unsafeCast :: Expression t -> Expression u
unsafeCast (Expression e) = Expression e

boolToInt :: Expression Bool -> Expression Int
boolToInt = unsafeCast

(.=) :: Expression t -> Expression t -> Expression Bool
Expression x .= Expression y = Expression $ Operator "=" x y

(.<) :: Expression t -> Expression t -> Expression Bool
Expression x .< Expression y = Expression $ Operator "<" x y

(.<=) :: Expression t -> Expression t -> Expression Bool
Expression x .<= Expression y = Expression $ Operator "<=" x y

(.>) :: Expression t -> Expression t -> Expression Bool
Expression x .> Expression y = Expression $ Operator ">" x y

(.>=) :: Expression t -> Expression t -> Expression Bool
Expression x .>= Expression y = Expression $ Operator ">=" x y

instance S.IsString (Expression Text) where
  fromString = Expression . TextLiteral . T.pack

instance P.Num (Expression Int) where
  Expression x + Expression y = Expression $ Operator "+" x y
  
  Expression x - Expression y = Expression $ Operator "-" x y
  
  Expression x * Expression y = Expression $ Operator "*" x y
  
  abs x = x * P.signum x

  signum x = boolToInt (x .> 0) - boolToInt (x .< 0)

  fromInteger x = Expression $ IntegerLiteral x

-- | The XPath 'text()' function.
text :: Expression Text
text = Expression $ Function "text" []

or :: Expression Bool -> Expression Bool -> Expression Bool
Expression x `or` Expression y = Expression $ Operator "or" x y
infixr 4 `or`

-- | The XPath not(.)' function.
not :: Expression Bool -> Expression Bool
not (Expression x) = Expression $ Function "not" [x]

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

data RelativePath = RelativePath T.Text (P.Maybe (Axis, RelativePath)) [Expression'] deriving (P.Eq, P.Ord)

showRelativePath :: RelativePath -> T.Text
showRelativePath (RelativePath n nextMay es) =
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

data Context = Anywhere | Root | Implicit deriving (P.Eq, P.Ord)

class IsPath t where
  append :: Axis -> t -> RelativePath -> t

  (#) :: t -> Expression Bool -> t
  infixr 3 #

instance IsPath RelativePath where
  append axis (RelativePath n P.Nothing es) u = RelativePath n (P.Just (axis, u)) es
  append axis (RelativePath n (P.Just (a, p)) es) u = RelativePath n (P.Just (a, append axis p u)) es
  
  RelativePath n p es # Expression e = RelativePath n p (e : es)

(./) :: IsPath p => p -> RelativePath -> p
(./) = append Child
infixr 2 ./

(.//) :: IsPath p => p -> RelativePath -> p
(.//) = append DescendantOrSelf
infixr 2 .//

type Path = Expression NodeSet

showPath :: Path -> T.Text
showPath (Expression e) = showExpression e

instance IsPath Path where
  append axis (Expression (Path context p es)) u = Expression $ Path context (append axis p u) es
  append _ _ _ = P.error "HaXPath internal error: unexpected non-Path expression"
  
  Expression (Path context p es) # Expression e = Expression $ Path context p (e : es)
  _ # _ = P.error "HaXPath internal error: unexpected non-Path expression"

fromAnywhere :: RelativePath -> Path
fromAnywhere p = Expression $ Path Anywhere p []

fromRoot :: RelativePath -> Path
fromRoot p = Expression $ Path Root p []

fromCurrent :: RelativePath -> Path
fromCurrent p = Expression $ Path Implicit p []
