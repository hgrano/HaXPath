{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | The core module of the XPath-generating DSL. This module should be used as a qualified import.
module HaXPath(
  -- * Basic data types
  IsExpression,
  Showed,
  Bool',
  Bool,
  false,
  true,
  Number',
  Number,
  Text',
  Text,
  text,
  -- * Nodes
  Node',
  Node,
  node,
  namedNode,
  DocumentRoot',
  root',
  DocumentRoot,
  root,
  at,
  -- * Basic combinators
  not,
  (&&.),
  (||.),
  contains,
  doesNotContain,
  Eq,
  (=.),
  (/=.),
  Ord,
  (<.),
  (<=.),
  (>.),
  (>=.),
  position,
  -- * Paths
  CurrentContext,
  RootContext,
  IsContext,
  Context,
  Path',
  Path,
  AbsolutePath',
  AbsolutePath,
  RelativePath',
  RelativePath,
  PathLike,
  show',
  show,
  -- * Axes
  ancestor,
  child,
  descendant,
  descendantOrSelf,
  following,
  followingSibling,
  parent,
  self,
  -- * Path combinators
  SlashOperator(..),
  DoubleSlashOperator(..),
  Filterable(..),
  count,
  (|.)
) where

import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Proxy  (Proxy (Proxy))
import Data.Semigroup (sconcat)
import qualified Data.String as S
import           Prelude     (($), (*), (+), (-), (.), (<$>), (<>), (==))
import qualified Prelude     as P

-- | XPath textual (string) data type, which can be showed as the string type @s@.
newtype Text' s = Text { unText :: Expression s }

-- | 'Text'' specialised so it can be shown as 'P.String'.
type Text = Text' P.String

-- | XPath numeric data type, which can be showed as the string type @s@.
newtype Number' s = Number { unNumber :: Expression s }

-- | 'Number'' specialised so it can be shown as 'P.String'.
type Number = Number' P.String

-- | XPath boolean data type, which can be showed as the string type @s@.
newtype Bool' s = Bool { unBool :: Expression s }

-- | 'Bool'' specialised so it can be shown as 'P.String'.
type Bool = Bool' P.String

-- | XPath @true()@ value.
true :: S.IsString s => Bool' s
true = Bool $ Function "true" []

-- | XPath @false()@ value.
false :: S.IsString s => Bool' s
false = Bool $ Function "false" []

data PathBegin = FromRootContext | FromCurrentContext deriving (P.Eq)

-- Internal data type to represent an XPath expression using the string-like type s.
data Expression s = Function s [Expression s] |
                    -- Apply the named function to zero or more arguments.
                    Operator s (Expression s) (Expression s) |
                    -- Apply a binary operator to the two operands.
                    Attribute s |
                    -- Access the given attribute of the node (@).
                    TextLiteral s |
                    -- Text value in quotes.
                    IntegerLiteral P.Integer |
                    -- Literal integer (XPath number).
                    NamedNode s |
                    -- Select node with the provided name.
                    FilteredNode (Expression s) [Expression s] |
                    LocationStep Axis (Expression s) |
                    -- From current context move along the given axis and select nodes matching the expression.
                    PathFrom PathBegin (Expression s) (P.Maybe (Expression s)) [Expression s]
                    -- From the starting point, take the first path (expression), then follow the next path (expression)
                    -- (if present) and finally filter by zero or more boolean (expressions).

-- | Class of types which can be used to form a valid XPath expression. Library users should not create instances of
-- this class.
class IsExpression a where
  toExpression :: a -> Expression (Showed a)

instance IsExpression (Text' s) where
  toExpression = unText

instance IsExpression (Number' s) where
  toExpression = unNumber

instance IsExpression (Bool' s) where
  toExpression = unBool

showExpression :: (S.IsString s, P.Show s) => Expression s -> [s]
showExpression (Function f es) = [f, "("] <> args <> [")"]
  where
    args = intercalate [", "] $ showExpression <$> es
showExpression (Operator o a b) =
  showOperand a <> [" ", o, " "] <> showOperand b
  where
    showOperand e@(TextLiteral _)    = showExpression e
    showOperand e@(IntegerLiteral _) = showExpression e
    showOperand e@(Function _ _)     = showExpression e
    showOperand e@(Attribute _)      = showExpression e
    showOperand e                    = "(" : showExpression e <> [")"]

showExpression (Attribute a) = ["@", a]
showExpression (TextLiteral t) = [S.fromString $ P.show t]
showExpression (IntegerLiteral i) = [S.fromString $ P.show i]
showExpression (PathFrom begin p pNextMay preds) =
  let prefix = case begin of
        FromRootContext    -> "/"
        FromCurrentContext -> ""
  in
  let showPath x = case x of
        LocationStep _ _ -> showExpression x
        _                -> "(" : showExpression x <> [")"]
  in
  let fullPShowed = prefix : showPath p <> case pNextMay of
        P.Nothing    -> []
        P.Just pNext -> "/" : showPath pNext
  in
  showWithPredicates fullPShowed preds
showExpression (LocationStep axis n) = showAxis axis : ["::"] <> showExpression n
showExpression (NamedNode n) = [n]
showExpression (FilteredNode n preds) = showExpression n <> showPredicates preds

showPredicates :: (S.IsString s, P.Show s) => [Expression s] -> [s]
showPredicates preds =  "[" : intercalate ["]["] (showExpression <$> preds) <> ["]"]

showWithPredicates :: (S.IsString s, P.Show s) => [s] -> [Expression s] -> [s]
showWithPredicates s es
  | P.not (P.null es) = "(" : s <> [")"] <> showPredicates es
  | P.otherwise = s

-- | Display an XPath expression. This is useful to sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show' :: (PathLike p,
          IsExpression p,
          P.Monoid (Showed p),
          S.IsString (Showed p),
          P.Show (Showed p)) =>
          p -> Showed p
show' = sconcat . (P.mempty :|) . showExpression . toExpression

-- | Specialisation of 'show'' to only generate 'P.String's.
show :: (PathLike p, IsExpression p, Showed p ~ P.String) => p -> P.String
show = show'

instance S.IsString s => S.IsString (Text' s) where
  fromString = Text . TextLiteral . S.fromString

boolToInt :: Bool' s -> Number' s
boolToInt (Bool b) = Number b

-- | Access the value of a node's attribute in text form (equivalent to XPath's @\@@).
at :: s -> Text' s
at = Text . Attribute

-- | Type class of XPath types that can be compared for equality. Library users should not create instances of this
-- class.
class IsExpression t => Eq t

instance Eq (Text' s)
instance Eq (Number' s)
instance Eq (Bool' s)

-- | The XPath @=@ operator.
(=.) :: (Eq a, S.IsString (Showed a)) => a -> a -> Bool' (Showed a)
x =. y = Bool $ Operator "=" (toExpression x) (toExpression y)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (Eq a, S.IsString (Showed a)) => a -> a -> Bool' (Showed a)
x /=. y = Bool $ Operator "!=" (toExpression x) (toExpression y)
infix 4 /=.

-- | Type class of XPath types that can be ordered. Library users should not create instances of this class.
class Eq t => Ord t

instance Ord (Text' s)
instance Ord (Number' s)
instance Ord (Bool' s)

-- | The XPath @<@ operator.
(<.) :: (Ord a, S.IsString (Showed a)) => a -> a -> Bool' (Showed a)
x <. y = Bool $ Operator "<" (toExpression x) (toExpression y)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: (Ord a, S.IsString (Showed a)) => a -> a -> Bool' (Showed a)
x <=. y = Bool $ Operator "<=" (toExpression x) (toExpression y)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: (Ord a, S.IsString (Showed a)) => a -> a -> Bool' (Showed a)
x >. y = Bool $ Operator ">" (toExpression x) (toExpression y)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: (Ord a, S.IsString (Showed a)) => a -> a -> Bool' (Showed a)
x >=. y = Bool $ Operator ">=" (toExpression x) (toExpression y)
infix 4 >=.

instance  S.IsString s => P.Num (Number' s) where
  Number x + Number y = Number $ Operator "+" x y

  Number x - Number y = Number $ Operator "-" x y

  Number x * Number y = Number $ Operator "*" x y

  abs x = x * P.signum x

  signum x = boolToInt (x >. 0) - boolToInt (x <. 0)

  fromInteger = Number . IntegerLiteral

-- | The XPath @position()@ function.
position :: S.IsString s => Number' s
position = Number $ Function "position" []

-- | The XPath @text()@ function.
text :: S.IsString s => Text' s
text = Text $ Function "text" []

-- | The XPath @contains()@ function.
contains :: S.IsString s => Text' s -> Text' s -> Bool' s
contains x y = Bool . Function "contains" $ [toExpression x, toExpression y]

-- | The opposite of 'contains'.
doesNotContain :: S.IsString s => Text' s -> Text' s -> Bool' s
doesNotContain x y = not $ contains x y

-- | The XPath @count()@ function.
count :: (IsContext c, S.IsString s) => Path' c s -> Number' s
count p = Number $ Function "count" [toExpression p]

-- | The XPath @and@ operator.
(&&.) :: S.IsString s => Bool' s -> Bool' s -> Bool' s
x &&. y = Bool $ Operator "and" (toExpression x) (toExpression y)
infixr 3 &&.

-- | The XPath @or@ operator.
(||.) :: S.IsString s => Bool' s -> Bool' s -> Bool' s
x ||. y = Bool $ Operator "or" (toExpression x) (toExpression y)
infixr 2 ||.

-- | The XPath @not(.)@ function.
not :: S.IsString s => Bool' s -> Bool' s
not x = Bool $ Function "not" [toExpression x]

data Axis = Ancestor |
            Child |
            Descendant |
            DescendantOrSelf |
            Following |
            FollowingSibling |
            Parent |
            Self

showAxis :: S.IsString s => Axis -> s
showAxis axis = case axis of
  Ancestor         -> "ancestor"
  Child            -> "child"
  Descendant       -> "descendant"
  DescendantOrSelf -> "descendant-or-self"
  Following        -> "following"
  FollowingSibling -> "following-sibling"
  Parent           -> "parent"
  Self             -> "self"

-- | An XPath node which can be showed as the string type @s@.
newtype Node' s = Node { unNode :: Expression s }

-- | 'Node'' specialised so it can be shown as 'P.String'.
type Node = Node' P.String

instance IsExpression (Node' s) where
  toExpression = unNode

-- | An XPath beginning from some context `c` (either the root context or the current context).
newtype Path' c s = Path { unPath :: Expression s }

-- | 'Path'' specialised so it can be shown as 'P.String'.
type Path c = Path' c P.String

-- | An XPath relative to the current context.
type RelativePath' = Path' CurrentContext

-- | 'RelativePath'' specialised so it can be shown as 'P.String'.
type RelativePath = RelativePath' P.String

-- | An XPath beginning from the document root.
type AbsolutePath' = Path' RootContext

-- | 'AbsolutePath'' specialised so it can be shown as 'P.String'.
type AbsolutePath = AbsolutePath' P.String

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

instance IsContext c => IsExpression (Path' c s) where
  toExpression = unPath

-- | The XPath @node()@ function.
node :: S.IsString s => Node' s
node = Node $ Function "node" []

-- | Create a node with the given name.
namedNode :: S.IsString s => s -> Node' s
namedNode = Node . NamedNode

-- | Type to represent the root of the document. Useful in forming an XPaths which must begin from the root.
data DocumentRoot' s = DocumentRoot

-- | 'DocumentRoot'' specialised so it can be used in paths to be shown as 'P.String'.
type DocumentRoot = DocumentRoot' P.String

-- | The root of the document. There is no corresponding XPath expression for 'root' but it can be used to indicate that
-- an XPath must be begin from the root by using this as the first step in the path.
root' :: DocumentRoot' s
root' = DocumentRoot

-- | Specialisation of 'root'' so it can be used in paths to be shown as 'P.String'.
root :: DocumentRoot
root = root'

-- | Type family which allows a context to be inferred. This allows for support of abbreviated syntax.
type family Context p where
  Context (Path' c s) = c
  Context (Node' s) = CurrentContext
  Context (DocumentRoot' s) = RootContext

-- | Type family which associates an expression type with the type that will be returned by 'show'' when it is dislayed
-- in XPath syntax. This allows flexiblity to use different string-like types, such as 'P.String', @Text@, @ByteString@
-- or even builders for these types.
type family (Showed p) where
  Showed (Number' s) = s
  Showed (Text' s) = s
  Showed (Bool' s) = s
  Showed (Path' c s) = s
  Showed (Node' s) = s
  Showed (DocumentRoot' s) = s

-- | Constraint for path-like types - i.e. they either a 'Path' or otherwise can be converted to one using abbreviated
-- syntax rules.
type PathLike p = IsContext (Context p)

-- | Type class for the XPath @/@ operator. It can operate on multiple types as the axes can be inferred based on
-- XPath's abbreviated syntax. Library users should not create instances of this class.
class (PathLike p, PathLike q, Showed p ~ Showed q) => SlashOperator p q where
  -- | The XPath @/@ operator.
  (/.) :: p -> q -> Path' (Context p) (Showed q)
  infixl 8 /.

instance IsContext c => SlashOperator (Path' c s) (Path' CurrentContext s) where
  pa /. nextPa = Path $ case toExpression pa of
    PathFrom begin fstPath P.Nothing preds -> PathFrom begin fstPath (P.Just $ toExpression nextPa) preds
    _ -> PathFrom
      (toPathBegin (Proxy :: Proxy c))
      (toExpression $ fromCurrentContext pa)
      (P.Just $ toExpression nextPa)
      []

instance IsContext c => SlashOperator (Path' c s) (Node' s) where
  pa /. n = pa /. child n

instance SlashOperator (Node' s) (Path' CurrentContext s) where
  n /. pa = child n /. pa

instance SlashOperator (Node' s) (Node' s) where
  n /. nextNode = child n /. child nextNode

instance SlashOperator (DocumentRoot' s) (Path' CurrentContext s) where
  DocumentRoot /. p = fromRootContext p

instance SlashOperator (DocumentRoot' s) (Node' s) where
  DocumentRoot /. n = fromRootContext (child n)

-- | Type class for the XPath @//@ operator. It can operate on multiple types as the axes can be inferred based on
-- XPath's abbreviated syntax. Library users should not create instances of this class.
class (PathLike p, PathLike q, Showed p ~ Showed q) => DoubleSlashOperator p q where
  -- | The XPath @//@ operator.
  (//.) :: p -> q -> Path' (Context p) (Showed q)
  infixl 8 //.

instance (IsContext c, S.IsString s) => DoubleSlashOperator (Path' c s) (Path' CurrentContext s) where
  pa //. nextPa = Path $ case toExpression pa of
    PathFrom begin fstPath P.Nothing preds -> PathFrom begin fstPath nextPa' preds
    _ -> PathFrom (toPathBegin (Proxy :: Proxy c)) (toExpression $ fromCurrentContext pa) nextPa' []

    where
      nextPa' = P.Just . toExpression $ descendantOrSelf node /. nextPa

instance (IsContext c, S.IsString s) => DoubleSlashOperator (Path' c s) (Node' s) where
  pa //. n = pa /. descendantOrSelf node /. n

instance S.IsString s => DoubleSlashOperator (Node' s) (Path' CurrentContext s) where
  n //. pa = child n //. pa

instance S.IsString s => DoubleSlashOperator (Node' s) (Node' s) where
  n //. nextNode = child n //. child nextNode

instance S.IsString s => DoubleSlashOperator (DocumentRoot' s) (Path' CurrentContext s) where
  DocumentRoot //. p = fromRootContext (descendantOrSelf node) /. p

instance S.IsString s => DoubleSlashOperator (DocumentRoot' s) (Node' s) where
  DocumentRoot //. n = fromRootContext (descendantOrSelf node /. n)

locationStep :: Axis -> Node' s -> Path' c s
locationStep axis n = Path $ LocationStep axis (toExpression n)

-- | The XPath @ancestor::@ axis.
ancestor :: Node' s -> Path' CurrentContext s
ancestor = locationStep Ancestor

-- | The XPath @child::@ axis.
child :: Node' s -> Path' CurrentContext s
child = locationStep Child

-- | The XPath @descendant::@ axis.
descendant :: Node' s -> Path' CurrentContext s
descendant = locationStep Descendant

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node' s -> Path' CurrentContext s
descendantOrSelf = locationStep DescendantOrSelf

-- | The XPath @following::@ axis.
following :: Node' s -> Path' CurrentContext s
following = locationStep Following

-- | The XPath @following-sibling::@ axis.
followingSibling :: Node' s -> Path' CurrentContext s
followingSibling = locationStep FollowingSibling

-- | The XPath @parent::@ axis.
parent :: Node' s -> Path' CurrentContext s
parent = locationStep Parent

-- | The XPath @self::@ axis.
self :: Node' s -> Path' CurrentContext s
self = locationStep Self

changeContext :: PathBegin -> Path' c s -> Path' c' s
changeContext begin (Path p) = Path $ case p of
  PathFrom _ fstPath sndPath preds -> PathFrom begin fstPath sndPath preds
  LocationStep _ _                 -> if begin == FromRootContext then PathFrom begin p P.Nothing [] else p
  other                            -> PathFrom begin other P.Nothing []

fromCurrentContext :: Path' c s -> Path' CurrentContext s
fromCurrentContext = changeContext FromCurrentContext

fromRootContext :: Path' CurrentContext s -> Path' RootContext s
fromRootContext = changeContext FromRootContext

-- | The union of two node-sets.
(|.) :: (PathLike p,
         PathLike q,
         IsExpression p,
         IsExpression q,
         Context p ~ Context q,
         Showed p ~ Showed q,
         S.IsString (Showed q)) =>
         p -> q-> Path' (Context p) (Showed q)
x |. y = Path $ Operator "|" (toExpression x) (toExpression y)
infix 7 |.

-- | Type class to allow filtering of node sets. Library users should not create instances of this class.
class (IsExpression p, PathLike p) => Filterable p where
  -- | Filter the nodes returned by @p@ such that they match the list of predicates.
  (#) :: Showed p ~ s => p -> [Bool' s] -> p
  infixl 9 #

instance IsContext c => Filterable (Path' c s) where
  xp # preds =
    let predExps = toExpression <$> preds in
    Path $ case toExpression xp of
      LocationStep axis (FilteredNode n ps)  -> LocationStep axis (FilteredNode n (ps <> predExps))
      LocationStep axis e                    -> LocationStep axis (FilteredNode e predExps)
      PathFrom begin firstSteps nextSteps ps -> PathFrom begin firstSteps nextSteps (ps <> predExps)
      otherExp                               -> PathFrom (toPathBegin (Proxy :: Proxy c)) otherExp P.Nothing predExps

instance Filterable (Node' s) where
  n # preds =
    let predExps = toExpression <$> preds in
    Node $ case toExpression n of
      FilteredNode nExp ps -> FilteredNode nExp (ps <> predExps)
      otherExp             -> FilteredNode otherExp predExps
