{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HaXPath(
  and,
  at,
  Bool,
  contains,
  count,
  Expression,
  Eq,
  fromAnywhere,
  fromRoot,
  IsExpression(..),
  IsPath,
  NodeSet,
  not,
  Number,
  or,
  Ord,
  Path,
  position,
  RelativePath,
  showIsPath,
  showPath,
  showRelativePath,
  text,
  Text,
  (=.),
  (/=.),
  (<.),
  (<=.),
  (>.),
  (>=.),
  (/.),
  (//.),
  (#)
) where

import Data.Maybe (isJust)
import qualified Data.String as S
import qualified Data.Text as T
import Prelude ((&&), (+), (-), (*), (.), ($), (<>), (<$>))
import qualified Prelude as P

-- | XPath textual (string) data type.
data Text

-- | XPath numeric data type.
data Number

-- | XPath boolean data type.
data Bool

-- | XPath type representing an unordered set of nodes.
data NodeSet

data Expression' = Function T.Text [Expression'] |
                   Operator T.Text Expression' Expression' |
                   Attribute T.Text |
                   TextLiteral T.Text |
                   IntegerLiteral P.Integer |
                   Path Context RelativePath [Expression']

showExpression :: Expression' -> T.Text
showExpression (Function f es) = f <> "(" <> args <> ")"
  where
    args = T.intercalate ", " $ showExpression <$> es
showExpression (Operator o a b) = showWithBracket a <> " " <> o <> " " <> showWithBracket b
  where
    needsBracket (Operator _ _ _) = P.True
    needsBracket (Path _ _ _) = P.True
    needsBracket _ = P.False

    showWithBracket e = if needsBracket e then "(" <> showExpression e <> ")" else showExpression e

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

-- | Opaque representation of an XPath expression.
newtype Expression t = Expression { unExpression :: Expression' }

instance S.IsString (Expression Text) where
  fromString = Expression . TextLiteral . T.pack

-- | The type of XPaths.
type Path = Expression NodeSet

class IsExpression a t | a -> t where
  toExpression :: a -> Expression  t

unIsExpression :: IsExpression a t => a -> Expression'
unIsExpression = unExpression . toExpression

instance IsExpression P.Bool Bool where
  toExpression x = Expression $ Function (if x then "true" else "false") []

instance IsExpression (Expression t) t where
  toExpression = P.id

unsafeCast :: Expression t -> Expression u
unsafeCast (Expression e) = Expression e

boolToInt :: Expression Bool -> Expression Number
boolToInt = unsafeCast

-- | Access the value of a node's attribute in text form (equivalent to XPath's @\@@).
at :: T.Text -> Expression Text
at = Expression . Attribute

-- | Type class of XPath types that can be compared for equality.
class Eq t

instance Eq Text
instance Eq Number
instance Eq Bool

-- | The XPath @=@ operator.
(=.) :: (Eq t, IsExpression a t) => a -> a -> Expression Bool
x =. y = Expression $ Operator "=" (unIsExpression x) (unIsExpression y)
infixr 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (Eq t, IsExpression a t) => a -> a -> Expression Bool
x /=. y = Expression $ Operator "!="(unIsExpression x) (unIsExpression y)
infixr 4 /=.

-- | Type class of XPath types that can be ordered.
class Eq t => Ord t

instance Ord Text
instance Ord Number
instance Ord Bool

-- | The XPath @<@ operator.
(<.) :: (Ord t, IsExpression a t) => a -> a -> Expression Bool
x <. y = Expression $ Operator "<" (unIsExpression x) (unIsExpression y)

-- | The XPath @<=@ operator.
(<=.) :: (Ord t, IsExpression a t) => a -> a -> Expression Bool
x <=. y = Expression $ Operator "<=" (unIsExpression x) (unIsExpression y)

-- | The XPath @>@ operator.
(>.) :: (Ord t, IsExpression a t) => a -> a -> Expression Bool
x >. y = Expression $ Operator ">" (unIsExpression x) (unIsExpression y)

-- | The XPath @>=@ operator.
(>=.) :: (Ord t, IsExpression a t) => a -> a -> Expression Bool
x >=. y = Expression $ Operator ">=" (unIsExpression x) (unIsExpression y)

instance P.Num (Expression Number) where
  Expression x + Expression y = Expression $ Operator "+" x y

  Expression x - Expression y = Expression $ Operator "-" x y

  Expression x * Expression y = Expression $ Operator "*" x y

  abs x = x * P.signum x

  signum x = boolToInt (x >. 0) - boolToInt (x <. 0)

  fromInteger = Expression . IntegerLiteral

-- | The XPath @position()@ function.
position :: Expression Number
position = Expression $ Function "position" []

-- | The XPath @text()@ function.
text :: Expression Text
text = Expression $ Function "text" []

-- | The XPath @contains()@ function.
contains :: IsExpression a Text => a -> a -> Expression Bool
contains x y = Expression . Function "contains" $ [unIsExpression x, unIsExpression y]

-- | The XPath @count()@ function.
count :: IsExpression a NodeSet => a -> Expression Number
count p = Expression $ Function "count" [unIsExpression p]

-- | The XPath @and@ operator.
and :: (IsExpression a Bool, IsExpression b Bool) => a -> b -> Expression Bool
x `and` y = Expression $ Operator "and" (unIsExpression x) (unIsExpression y)
infixr 3 `and`

-- | The XPath @or@ operator.
or :: (IsExpression a Bool, IsExpression b Bool) => a -> b -> Expression Bool
x `or` y = Expression $ Operator "or" (unIsExpression x) (unIsExpression y)
infixr 2 `or`

-- | The XPath @not(.)@ function.
not :: IsExpression a Bool => a -> Expression Bool
not x = Expression $ Function "not" [unIsExpression x]

data Axis = Ancestor |
            Child |
            Descendant |
            DescendantOrSelf |
            Parent

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

-- | A relative XPath, i.e. an XPath that is relative to the current node.
data RelativePath = RelativePath T.Text (P.Maybe (Axis, RelativePath)) [Expression']

instance IsExpression RelativePath NodeSet where
  toExpression p = Expression $ Path Implicit p []

instance S.IsString RelativePath where
  fromString n = RelativePath (T.pack n) P.Nothing []

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

data Context = Anywhere | Root | Implicit

-- | Type class for allowing XPath-like operations. Do not create instances of this class.
class IsExpression t NodeSet => IsPath t where
  append :: Axis -> t -> RelativePath -> t

  -- | Filter a node set by the given predicate. Equivalent to the use of square brackets in XPath.
  (#) :: IsExpression b Bool => t -> b -> t
  infixr 2 #

instance IsPath RelativePath where
  append axis (RelativePath n P.Nothing es) u = RelativePath n (P.Just (axis, u)) es
  append axis (RelativePath n (P.Just (a, p)) es) u = RelativePath n (P.Just (a, append axis p u)) es
  
  RelativePath n p es # e = RelativePath n p $ unIsExpression e : es

-- | The XPath @/@ operator.
(/.) :: IsPath p => p -> RelativePath -> p
(/.) = append Child
infixr 2 /.

-- | The XPath @//@ operator.
(//.) :: IsPath p => p -> RelativePath -> p
(//.) = append DescendantOrSelf
infixr 2 //.

-- | Display an XPath expression. This is useful to sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
showIsPath :: IsPath p => p -> T.Text
showIsPath = showExpression . unIsExpression

showPath :: Path -> T.Text
showPath = showIsPath

nonPathError :: a
nonPathError = P.error "HaXPath internal error: unexpected non-Path expression"

instance IsPath Path where
  append axis (Expression (Path context p es)) u = Expression $ Path context (append axis p u) es
  append _ _ _ = nonPathError

  Expression (Path context p es) # e = Expression $ Path context p $ unIsExpression e : es
  _ # _ = nonPathError

-- | Fix a relative path to start from anywhere in the document. This is equivalent to starting an XPath with @//@.
fromAnywhere :: RelativePath -> Path
fromAnywhere p = Expression $ Path Anywhere p []

-- | Fix a relative path to start from the document root. This is equivalent to starting an XPath with @/@.
fromRoot :: RelativePath -> Path
fromRoot p = Expression $ Path Root p []
