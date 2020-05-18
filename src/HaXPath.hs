{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HaXPath(
  at,
  Bool,
  child,
  contains,
  count,
  descendantOrSelf,
  doubleSlash,
  Expression,
  Eq,
  Filterable(..),
  fromRoot,
  IsPath(..),
  Lit(..),
  namedNode,
  node,
  Node,
  NodeSet,
  not,
  Number,
  Ord,
  Path,
  position,
  RelativePath,
  show,
  text,
  Text,
  (&&.),
  (/.),
  (//.),
  (/=.),
  (<.),
  (<=.),
  (=.),
  (>.),
  (>=.),
  (||.)
) where

import Data.Maybe (isJust)
import qualified Data.String as S
import qualified Data.Text as T
import Prelude ((+), (-), (*), (.), ($), (<>), (<$>))
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
                   Path PathType RelativePath [Expression']

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
showExpression (Path t p es) =
  let prefix = case t of
        Relative -> ""
        Absolute -> "/"
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

class Lit h x | h -> x where
  -- | Create an XPath literal value from a Haskell value.
  lit :: h -> Expression x

instance Lit P.Bool Bool where
  lit b = Expression $ Function (if b then "true" else "false") []

instance Lit P.Integer Number where
  lit = Expression . IntegerLiteral

instance Lit T.Text Text where
  lit = Expression . TextLiteral

instance S.IsString (Expression Text) where
  fromString = lit . T.pack

-- | The type of XPaths.
type Path = Expression NodeSet

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
(=.) :: Eq a => Expression a -> Expression a -> Expression Bool
x =. y = Expression $ Operator "=" (unExpression x) (unExpression y)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: Eq a => Expression a -> Expression a -> Expression Bool
x /=. y = Expression $ Operator "!=" (unExpression x) (unExpression y)
infix 4 /=.

-- | Type class of XPath types that can be ordered.
class Eq t => Ord t

instance Ord Text
instance Ord Number
instance Ord Bool

-- | The XPath @<@ operator.
(<.) :: Ord a => Expression a -> Expression a -> Expression Bool
x <. y = Expression $ Operator "<" (unExpression x) (unExpression y)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: Ord a => Expression a -> Expression a -> Expression Bool
x <=. y = Expression $ Operator "<=" (unExpression x) (unExpression y)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: Ord a => Expression a -> Expression a -> Expression Bool
x >. y = Expression $ Operator ">" (unExpression x) (unExpression y)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: Ord a => Expression a -> Expression a -> Expression Bool
x >=. y = Expression $ Operator ">=" (unExpression x) (unExpression y)
infix 4 >=.

instance P.Num (Expression Number) where
  Expression x + Expression y = Expression $ Operator "+" x y

  Expression x - Expression y = Expression $ Operator "-" x y

  Expression x * Expression y = Expression $ Operator "*" x y

  abs x = x * P.signum x

  signum x = boolToInt (x >. 0) - boolToInt (x <. 0)

  fromInteger = lit

-- | The XPath @position()@ function.
position :: Expression Number
position = Expression $ Function "position" []

-- | The XPath @text()@ function.
text :: Expression Text
text = Expression $ Function "text" []

-- | The XPath @contains()@ function.
contains :: Expression Text -> Expression Text -> Expression Bool
contains x y = Expression . Function "contains" $ [unExpression x, unExpression y]

-- | The XPath @count()@ function.
count :: IsPath p => p -> Expression Number
count p = Expression $ Function "count" [unExpression $ toPath p]

-- | The XPath @and@ operator.
(&&.) :: Expression Bool -> Expression Bool -> Expression Bool
x &&. y = Expression $ Operator "and" (unExpression x) (unExpression y)
infixr 3 &&.

-- | The XPath @or@ operator.
(||.) :: Expression Bool -> Expression Bool -> Expression Bool
x ||. y = Expression $ Operator "or" (unExpression x) (unExpression y)
infixr 2 ||.

-- | The XPath @not(.)@ function.
not :: Expression Bool -> Expression Bool
not x = Expression $ Function "not" [unExpression x]

data Axis = Ancestor |
            Child |
            Descendant |
            DescendantOrSelf |
            Parent

showAxis :: Axis -> T.Text
showAxis axis = case axis of
  Ancestor -> "ancestor"
  Child -> "child"
  Descendant -> "descendant"
  DescendantOrSelf -> "descendant-or-self"
  Parent -> "parent"

-- | Opaque representation of an XPath node.
data Node = Node {
  nName :: !T.Text,
  nPredicate :: ![Expression']
}

-- | The XPath @node()@ function.
node :: Node
node = namedNode "node()"

-- | Create a node within the given name.
namedNode :: T.Text -> Node
namedNode n = Node n []

nodeToRelativePath :: Axis -> Node -> RelativePath
nodeToRelativePath axis n = RelativeNode P.Nothing axis n

-- | The XPath @child::@ axis.
child :: Node -> RelativePath
child = nodeToRelativePath Child

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node -> RelativePath
descendantOrSelf = nodeToRelativePath DescendantOrSelf

-- | The XPath @//@ operator.
doubleSlash :: Node -> Path
doubleSlash n = fromRoot $ descendantOrSelf node /. n

-- | A relative XPath, i.e. an XPath that is relative to the current node.
data RelativePath = RelativeNode (P.Maybe RelativePath) Axis Node |
                    Bracketed (P.Maybe RelativePath) RelativePath [Expression']

showPrev :: P.Maybe RelativePath -> T.Text
showPrev = P.maybe "" $ \rp -> showRelativePath rp <> "/"

showRelativePath :: RelativePath -> T.Text
showRelativePath (RelativeNode prev axis n) = showPrev prev <>
  showAxis axis <>
  "::" <>
  nName n <>
  showExpressions (nPredicate n)
showRelativePath (Bracketed prev rp pred)
  | P.null pred = showPrev prev <> showRelativePath rp
  | P.otherwise = showPrev prev <> "(" <> showRelativePath rp <> ")" <> showExpressions pred

data PathType = Relative | Absolute

-- | Type class for allowing XPath-like operations. Do not create instances of this class.
class IsPath t where
  -- | The XPath (non-abbreviated) @/@ operator.
  (./.) :: t -> RelativePath -> t
  infixl 2 ./.

  -- | Convert to a Path.
  toPath :: t -> Path

instance IsPath RelativePath where
  rp ./. RelativeNode P.Nothing axis n = RelativeNode (P.Just rp) axis n
  rp ./. RelativeNode (P.Just prev) axis n = RelativeNode (P.Just $ rp ./. prev) axis n
  rp ./. Bracketed P.Nothing rp' pred = Bracketed (P.Just rp) rp' pred
  rp ./. b@(Bracketed (P.Just _) _ _) = Bracketed (P.Just rp) b []

  toPath rp = Expression $ Path Relative rp []

class Filterable t where
  -- | Filter a set of nodes by the given predicate.
  (#) :: t -> Expression b -> t
  infixl 3 #

instance Filterable Node where
  n # e = n { nPredicate = unExpression e : nPredicate n }

instance Filterable RelativePath where
  b@(Bracketed prev rp pred) # e
    | isJust prev = Bracketed P.Nothing b [unExpression e]
    | P.otherwise = Bracketed prev rp (unExpression e : pred)
  rn@(RelativeNode prev axis n) # e
   | isJust prev = Bracketed P.Nothing rn [unExpression e]
   | P.otherwise = RelativeNode prev axis n { nPredicate = unExpression e : nPredicate n }

-- | The XPath abbreviated @/@ operator.
(/.) :: IsPath p => p -> Node -> p
p /. n = p ./. child n
infixl 2 /.

-- | The XPath @//@ operator.
(//.) :: IsPath p => p -> Node -> p
p //. n = p ./. descendantOrSelf node ./. child n
infixl 2 //.

-- | Display an XPath expression. This is useful to sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show :: IsPath p => p -> T.Text
show = showExpression . unExpression . toPath

nonPathError :: a
nonPathError = P.error "HaXPath internal error: unexpected non-Path expression"

instance IsPath Path where
  Expression (Path t rp es) ./. rp' = Expression $ Path t (rp ./. rp') es
  _ ./. _ = nonPathError

  toPath = P.id

instance Filterable Path where
  Expression (Path context rp es) # e = Expression . Path context rp $ unExpression e : es
  _ # _ = nonPathError

-- | Fix a relative path to begin from the document root (i.e. create an absolute path).
fromRoot :: RelativePath -> Path
fromRoot rp = Expression $ Path Absolute rp []
