{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module HaXPath(
  (&&.),
  (/=.),
  (<.),
  (<=.),
  (=.),
  (>.),
  (>=.),
  (||.),
  (|.),
  AbsolutePath,
  ancestor,
  at,
  Bool,
  child,
  contains,
  Context,
  count,
  CurrentContext,
  descendant,
  descendantOrSelf,
  DocumentRoot,
  doesNotContain,
  DoubleSlashOperator(..),
  Eq,
  false,
  Filterable(..),
  following,
  followingSibling,
  IsContext,
  IsExpression,
  Literal(..),
  namedNode,
  node,
  Node,
  not,
  Number,
  Ord,
  parent,
  Path,
  PathLike,
  position,
  RelativePath,
  root,
  RootContext,
  self,
  show,
  SlashOperator(..),
  text,
  Text,
  true
) where

import           Data.Proxy  (Proxy (Proxy))
import qualified Data.String as S
import qualified Data.Text   as T
import           Prelude     (($), (*), (+), (-), (.), (<$>), (<>), (==))
import qualified Prelude     as P

-- | XPath textual (string) data type.
newtype Text = Text { unText :: Expression }

-- | XPath numeric data type.
newtype Number = Number { unNumber :: Expression }

-- | XPath boolean data type.
newtype Bool = Bool { unBool :: Expression }

-- | XPath @true()@ value.
true :: Bool
true = Bool $ Function "true" []

-- | XPath @false()@ value.
false :: Bool
false = Bool $ Function "false" []

data PathBegin = FromRootContext | FromCurrentContext deriving (P.Eq)

-- Internal data type to represent an XPath expression.
data Expression = Function T.Text [Expression] |
                  -- Apply the named function to zero or more arguments.
                  Operator T.Text Expression Expression |
                  -- Apply a binary operator to the two operands.
                  Attribute T.Text |
                  -- Access the given attribute of the node (@).
                  TextLiteral T.Text |
                  -- Text value in quotes.
                  IntegerLiteral P.Integer |
                  -- Literal integer (XPath number).
                  NamedNode T.Text |
                  -- Select node with the provided name.
                  FilteredNode Expression [Expression] |
                  LocationStep Axis Expression |
                  -- From current context move along the given axis and select nodes matching the expression.
                  PathFrom PathBegin Expression (P.Maybe Expression) [Expression]
                  -- From the starting point, take the first path (expression), then follow the next path (expression)
                  -- (if present) and finally filter by zero or more boolean (expressions).

-- | Class of types which can be used to form a valid XPath expression. Library users should not create instances of
-- this class.
class IsExpression a where
  toExpression :: a -> Expression

instance IsExpression Text where
  toExpression = unText

instance IsExpression Number where
  toExpression = unNumber

instance IsExpression Bool where
  toExpression = unBool

showExpression :: Expression -> T.Text
showExpression (Function f es) = f <> "(" <> args <> ")"
  where
    args = T.intercalate ", " $ showExpression <$> es
showExpression (Operator o a b) =
  showOperand a <> " " <> o <> " " <> showOperand b
  where
    showOperand e@(TextLiteral _)    = showExpression e
    showOperand e@(IntegerLiteral _) = showExpression e
    showOperand e@(Function _ _)     = showExpression e
    showOperand e@(Attribute _)      = showExpression e
    showOperand e                    = "(" <> showExpression e <> ")"

showExpression (Attribute a) = "@" <> a
showExpression (TextLiteral t) = T.pack (P.show t)
showExpression (IntegerLiteral i) = T.pack $ P.show i
showExpression (PathFrom begin p pNextMay preds) =
  let prefix = case begin of
        FromRootContext    -> "/"
        FromCurrentContext -> ""
  in
  let showPath x = case x of
        LocationStep _ _ -> showExpression x
        _                -> "(" <> showExpression x <> ")"
  in
  let fullPShowed = prefix <> showPath p <> case pNextMay of
        P.Nothing    -> ""
        P.Just pNext -> "/" <> showPath pNext
  in
  showWithPredicates fullPShowed preds
showExpression (LocationStep axis n) = showAxis axis <> "::" <> showExpression n
showExpression (NamedNode n) = n
showExpression (FilteredNode n preds) = showExpression n <> showPredicates preds

showPredicates :: [Expression] -> T.Text
showPredicates preds =  "[" <> T.intercalate "][" (showExpression <$> preds) <> "]"

showWithPredicates :: T.Text -> [Expression] -> T.Text
showWithPredicates s es
  | P.not (P.null es) = "(" <> s <> ")" <> showPredicates es
  | P.otherwise = s

-- | Display an XPath expression. This is useful to sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show :: (PathLike p, IsExpression p) => p -> T.Text
show = showExpression . toExpression

-- | Type class for Haskell values which can be converted to XPath (literal) values.
class IsExpression (AsExpression h) => Literal h where
  type AsExpression h
  -- | Create an XPath value from a Haskell value.
  lit :: h -> AsExpression h

instance Literal P.Bool where
  type AsExpression P.Bool = Bool

  lit P.True  = true
  lit P.False = false

instance Literal P.Integer where
  type AsExpression P.Integer = Number

  lit = Number . IntegerLiteral

instance Literal T.Text where
  type AsExpression T.Text = Text

  lit = Text . TextLiteral

instance Literal P.String where
  type AsExpression P.String = Text

  lit = Text . TextLiteral . T.pack

instance S.IsString Text where
  fromString = lit

boolToInt :: Bool -> Number
boolToInt (Bool b) = Number b

-- | Access the value of a node's attribute in text form (equivalent to XPath's @\@@).
at :: T.Text -> Text
at = Text . Attribute

-- | Type class of XPath types that can be compared for equality. Library users should not create instances of this
-- class.
class IsExpression t => Eq t

instance Eq Text
instance Eq Number
instance Eq Bool

-- | The XPath @=@ operator.
(=.) :: Eq a => a -> a -> Bool
x =. y = Bool $ Operator "=" (toExpression x) (toExpression y)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: Eq a => a -> a -> Bool
x /=. y = Bool $ Operator "!=" (toExpression x) (toExpression y)
infix 4 /=.

-- | Type class of XPath types that can be ordered. Library users should not create instances of this class.
class Eq t => Ord t

instance Ord Text
instance Ord Number
instance Ord Bool

-- | The XPath @<@ operator.
(<.) :: Ord a => a -> a -> Bool
x <. y = Bool $ Operator "<" (toExpression x) (toExpression y)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: Ord a => a -> a -> Bool
x <=. y = Bool $ Operator "<=" (toExpression x) (toExpression y)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: Ord a => a -> a -> Bool
x >. y = Bool $ Operator ">" (toExpression x) (toExpression y)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: Ord a => a -> a -> Bool
x >=. y = Bool $ Operator ">=" (toExpression x) (toExpression y)
infix 4 >=.

instance P.Num Number where
  Number x + Number y = Number $ Operator "+" x y

  Number x - Number y = Number $ Operator "-" x y

  Number x * Number y = Number $ Operator "*" x y

  abs x = x * P.signum x

  signum x = boolToInt (x >. 0) - boolToInt (x <. 0)

  fromInteger = lit

-- | The XPath @position()@ function.
position :: Number
position = Number $ Function "position" []

-- | The XPath @text()@ function.
text :: Text
text = Text $ Function "text" []

-- | The XPath @contains()@ function.
contains :: Text -> Text -> Bool
contains x y = Bool . Function "contains" $ [toExpression x, toExpression y]

-- | The opposite of 'contains'.
doesNotContain :: Text -> Text -> Bool
doesNotContain x y = not $ contains x y

-- | The XPath @count()@ function.
count :: IsContext c => Path c -> Number
count p = Number $ Function "count" [toExpression p]

-- | The XPath @and@ operator.
(&&.) :: Bool -> Bool -> Bool
x &&. y = Bool $ Operator "and" (toExpression x) (toExpression y)
infixr 3 &&.

-- | The XPath @or@ operator.
(||.) :: Bool -> Bool -> Bool
x ||. y = Bool $ Operator "or" (toExpression x) (toExpression y)
infixr 2 ||.

-- | The XPath @not(.)@ function.
not :: Bool -> Bool
not x = Bool $ Function "not" [toExpression x]

data Axis = Ancestor |
            Child |
            Descendant |
            DescendantOrSelf |
            Following |
            FollowingSibling |
            Parent |
            Self

showAxis :: Axis -> T.Text
showAxis axis = case axis of
  Ancestor         -> "ancestor"
  Child            -> "child"
  Descendant       -> "descendant"
  DescendantOrSelf -> "descendant-or-self"
  Following        -> "following"
  FollowingSibling -> "following-sibling"
  Parent           -> "parent"
  Self             -> "self"

-- | An XPath node.
newtype Node = Node { unNode :: Expression }

instance IsExpression Node where
  toExpression = unNode

-- | An XPath beginning from some context `c` (either the root context or the current context).
newtype Path c = Path { unPath :: Expression }

-- | An XPath relative to the current context.
type RelativePath = Path CurrentContext

-- | An XPath beginning from the document root.
type AbsolutePath = Path RootContext

-- | Type to indicate the XPath begins from the current context.
data CurrentContext

-- | Type to indicate the XPath begins from the document root.
data RootContext

-- | Class of valid types for the type parameter `c` in 'Path'. Library users should not create instances of this class.
class IsContext c where
  toPathBegin :: proxy c -> PathBegin

instance IsContext RootContext where
  toPathBegin _ = FromRootContext

instance IsContext CurrentContext where
  toPathBegin _ = FromCurrentContext

instance IsContext c => IsExpression (Path c) where
  toExpression = unPath

-- | The XPath @node()@ function.
node :: Node
node = Node $ Function "node" []

-- | Create a node with the given name.
namedNode :: T.Text -> Node
namedNode = Node . NamedNode

-- | Type to represent the root of the document. Useful in forming an XPaths which must begin from the root.
data DocumentRoot = DocumentRoot

-- | The root of the document. There is no corresponding XPath expression for 'root' but it can be used to indicate that
-- an XPath must be begin from the root by using this as the first step in the path.
root :: DocumentRoot
root = DocumentRoot

-- | Type family which allows a context to be inferred. This allows for support of abbreviated syntax.
type family Context p where
  Context (Path c) = c
  Context Node = CurrentContext
  Context DocumentRoot = RootContext

-- | Constraint for path-like types - i.e. they either a 'Path' or otherwise can be converted to one using abbreviated
-- syntax rules.
type PathLike p = IsContext (Context p)

-- | Type class for the XPath @/@ operator. It can operate on multiple types as the axes can be inferred based on
-- XPath's abbreviated syntax. Library users should not create instances of this class.
class (PathLike p, PathLike q) => SlashOperator p q where
  (/.) :: p -> q -> Path (Context p)
  infixl 8 /.

instance IsContext c => SlashOperator (Path c) (Path CurrentContext) where
  pa /. nextPa = Path $ case toExpression pa of
    PathFrom begin fstPath P.Nothing preds -> PathFrom begin fstPath (P.Just $ toExpression nextPa) preds
    _ -> PathFrom
      (toPathBegin (Proxy :: Proxy c))
      (toExpression $ fromCurrentContext pa)
      (P.Just $ toExpression nextPa)
      []

instance IsContext c => SlashOperator (Path c) Node where
  pa /. n = pa /. child n

instance SlashOperator Node (Path CurrentContext) where
  n /. pa = child n /. pa

instance SlashOperator Node Node where
  n /. nextNode = child n /. child nextNode

instance SlashOperator DocumentRoot (Path CurrentContext) where
  DocumentRoot /. p = fromRootContext p

instance SlashOperator DocumentRoot Node where
  DocumentRoot /. n = fromRootContext (child n)

-- | Type class for the XPath @//@ operator. It can operate on multiple types as the axes can be inferred based on
-- XPath's abbreviated syntax. Library users should not create instances of this class.
class (PathLike p, PathLike q) => DoubleSlashOperator p q where
  (//.) :: p -> q -> Path (Context p)
  infixl 8 //.

instance IsContext c => DoubleSlashOperator (Path c) (Path CurrentContext) where
  pa //. nextPa = Path $ case toExpression pa of
    PathFrom begin fstPath P.Nothing preds -> PathFrom begin fstPath nextPa' preds
    _ -> PathFrom (toPathBegin (Proxy :: Proxy c)) (toExpression $ fromCurrentContext pa) nextPa' []

    where
      nextPa' = P.Just . toExpression $ descendantOrSelf node /. nextPa

instance IsContext c => DoubleSlashOperator (Path c) Node where
  pa //. n = pa /. descendantOrSelf node /. n

instance DoubleSlashOperator Node (Path CurrentContext) where
  n //. pa = child n //. pa

instance DoubleSlashOperator Node Node where
  n //. nextNode = child n //. child nextNode

instance DoubleSlashOperator DocumentRoot (Path CurrentContext) where
  DocumentRoot //. p = fromRootContext (descendantOrSelf node) /. p

instance DoubleSlashOperator DocumentRoot Node where
  DocumentRoot //. n = fromRootContext (descendantOrSelf node /. n)

locationStep :: Axis -> Node -> Path c
locationStep axis n = Path $ LocationStep axis (toExpression n)

-- | The XPath @ancestor::@ axis.
ancestor :: Node -> Path CurrentContext
ancestor = locationStep Ancestor

-- | The XPath @child::@ axis.
child :: Node -> Path CurrentContext
child = locationStep Child

-- | The XPath @descendant::@ axis.
descendant :: Node -> Path CurrentContext
descendant = locationStep Descendant

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node -> Path CurrentContext
descendantOrSelf = locationStep DescendantOrSelf

-- | The XPath @following::@ axis.
following :: Node -> Path CurrentContext
following = locationStep Following

-- | The XPath @following-sibling::@ axis.
followingSibling :: Node -> Path CurrentContext
followingSibling = locationStep FollowingSibling

-- | The XPath @parent::@ axis.
parent :: Node -> Path CurrentContext
parent = locationStep Parent

-- | The XPath @self::@ axis.
self :: Node -> Path CurrentContext
self = locationStep Self

changeContext :: PathBegin -> Path c -> Path c'
changeContext begin (Path p) = Path $ case p of
  PathFrom _ fstPath sndPath preds -> PathFrom begin fstPath sndPath preds
  LocationStep _ _                 -> if begin == FromRootContext then PathFrom begin p P.Nothing [] else p
  other                            -> PathFrom begin other P.Nothing []

fromCurrentContext :: Path c -> Path CurrentContext
fromCurrentContext = changeContext FromCurrentContext

fromRootContext :: Path CurrentContext -> Path RootContext
fromRootContext = changeContext FromRootContext

-- | The union of two node-sets.
(|.) :: (PathLike p, PathLike q, IsExpression p, IsExpression q, Context p ~ Context q) => p -> q -> Path (Context p)
x |. y = Path $ Operator "|" (toExpression x) (toExpression y)
infix 7 |.

-- | Type class to allow filtering of node sets. Library users should not create instances of this class.
class (IsExpression p, PathLike p) => Filterable p where
  (#) :: p -> [Bool] -> p
  infixl 9 #

instance IsContext c => Filterable (Path c) where
  xp # preds =
    let predExps = toExpression <$> preds in
    Path $ case toExpression xp of
      LocationStep axis (FilteredNode n ps)  -> LocationStep axis (FilteredNode n (ps <> predExps))
      LocationStep axis e                    -> LocationStep axis (FilteredNode e predExps)
      PathFrom begin firstSteps nextSteps ps -> PathFrom begin firstSteps nextSteps (ps <> predExps)
      otherExp                               -> PathFrom (toPathBegin (Proxy :: Proxy c)) otherExp P.Nothing predExps

instance Filterable Node where
  n # preds =
    let predExps = toExpression <$> preds in
    Node $ case toExpression n of
      FilteredNode nExp ps -> FilteredNode nExp (ps <> predExps)
      otherExp             -> FilteredNode otherExp predExps
