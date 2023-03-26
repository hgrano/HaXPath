{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Wrapper over the "HaXPath" module which supports stronger type gurantuees such that XPaths must be valid with
-- respect to the document schema. This module should be used as a qualified import.
module HaXPath.Schematic (
  -- * Basic data types
  ToNonSchematic(..),
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
  IsNode(..),
  namedNode,
  DocumentRoot',
  root',
  DocumentRoot,
  root,
  Attributes,
  AttributesUsed,
  IsAttribute(..),
  at,
  -- * Basic combinators
  not,
  (&&.),
  (||.),
  (=.),
  (/=.),
  (<.),
  (<=.),
  (>.),
  (>=.),
  contains,
  doesNotContain,
  position,
  -- * Paths
  Path',
  Path,
  AbsolutePath',
  AbsolutePath,
  RelativePath',
  RelativePath,
  PathLike,
  SelectNode,
  ReturnNode,
  Relatives,
  show',
  show,
  -- * Axes
  Axis,
  Ancestor,
  ancestor,
  Child,
  child,
  Descendant,
  descendant,
  DescendantOrSelf,
  descendantOrSelf,
  Following,
  following,
  FollowingSibling,
  followingSibling,
  Parent,
  parent,
  -- * Path combinators
  (/.),
  (//.),
  (#),
  count,
  -- * Utilities
  Member
) where

import           Data.HList.CommonMain (HMember)
import           Data.Proxy            (Proxy (Proxy))
import qualified Data.String           as S
import           Data.Kind             (Type)
import qualified HaXPath               as X
import           Prelude               (($), (*), (+), (.), (<$>))
import qualified Prelude               as P

-- | Type level membership constraint indicating that the type @x@ is a member of the type-level list @xs@.
type Member x xs = HMember x xs 'P.True

-- | The type of boolean expressions which depend on the value of the attribute(s) @as@ and can be showed as the string
-- type @s@.
newtype Bool' (as :: [Type]) s = Bool { unBool :: X.Bool' s }

-- | 'Bool'' specialised so it can be shown as 'P.String'.
type Bool as = Bool' as P.String

-- | XPath @true()@ value.
true :: S.IsString s => Bool' as s
true = Bool X.true

-- | XPath @false()@ value.
false :: S.IsString s => Bool' as s
false = Bool X.false

-- | The type of simple numeric expressions which depend on the value of the attribute(s) @as@ and can be showed as the
-- string type @s@.
newtype Number' (as :: [Type]) s = Number { unNumber :: X.Number' s }

-- | 'Number'' specialised so it can be shown as 'P.String'
type Number as = Number' as P.String

-- | The type of simple text expressions which depend on the value of the attribute(s) @as@ and can be showed as the
-- string type @s@.
newtype Text' (as :: [Type]) s = Text { unText :: X.Text' s }

-- | 'Text'' specialised so it can be shown as 'P.String'
type Text as = Text' as P.String

-- | The type of path expressions which can be showed as the string type @s@ and are formed by these steps:
--
-- 1. Starting from the context @c@ and moving through the given @axis@.
-- 1. Selecting node(s) of type @n@.
-- 1. Performing zero or more location steps.
-- 1. Finally returning the node(s) of type @rn@.
newtype Path' c axis n rn s = Path { unPath :: X.Path' c s }

-- | 'Path'' specialised so it can be shown as 'P.String'.
type Path c axis n rn = Path' c axis n rn P.String

-- | An XPath beginning from the document root for the schema @sc@, returning a node of type @rn@.
type AbsolutePath' sc rn = Path' X.RootContext Self (DocumentRoot sc) rn

-- | 'AbsolutePath'' specialised so it can be shown as 'P.String'
type AbsolutePath sc rn = (AbsolutePath' sc rn) P.String

-- | An XPath beginning from the current context.
type RelativePath' = Path' X.CurrentContext

-- | 'RelativePath'' specialised so it can be shown as 'P.String'
type RelativePath axis n rn = RelativePath' axis n rn P.String

instance S.IsString s => S.IsString (Text' as s) where
  fromString = Text . S.fromString

-- | Type class for conversion from a schematic value to its underlying, non-schematic version.
class ToNonSchematic t where
  -- | Corresponding non-schematic type.
  type NonSchematic t

  -- | Convert from the schematic to the non-schematic version.
  toNonSchematic :: t -> NonSchematic t

instance ToNonSchematic (Bool' as s) where
  type NonSchematic (Bool' as s) = X.Bool' s

  toNonSchematic = unBool

instance ToNonSchematic (Number' as s) where
  type NonSchematic (Number' as s) = X.Number' s

  toNonSchematic = unNumber

instance ToNonSchematic (Text' as s) where
  type NonSchematic (Text' as s) = X.Text' s

  toNonSchematic = unText

instance ToNonSchematic (Path' c axis n rn s) where
  type NonSchematic (Path' c axis n rn s) = X.Path' c s

  toNonSchematic = unPath

instance ToNonSchematic (Node' n s) where
  type NonSchematic (Node' n s) = X.Node' s

  toNonSchematic = unNode

instance ToNonSchematic (DocumentRoot' sc s) where
  type NonSchematic (DocumentRoot' sc s) = X.DocumentRoot' s

  toNonSchematic = unDocumentRoot

-- This type class is not exposed as this would allow for arbitrary, non-schematic expression to be converted to
-- a schematic version when the underlying expression does not actually conform to the schema.
class FromNonSchematic x t where
  fromNonSchematic :: x -> t

instance FromNonSchematic (X.Bool' s) (Bool' as s) where
  fromNonSchematic = Bool

instance FromNonSchematic (X.Number' s) (Number' as s) where
  fromNonSchematic = Number

instance FromNonSchematic (X.Text' s) (Text' as s) where
  fromNonSchematic = Text

instance FromNonSchematic (X.Path' c s) (Path' c axis n rn s) where
  fromNonSchematic = Path

instance FromNonSchematic (X.Node' s) (Node' n s) where
  fromNonSchematic = Node

instance FromNonSchematic (X.DocumentRoot' s) (DocumentRoot' sc s) where
  fromNonSchematic = DocumentRoot

-- | The XPath @text()@ function.
text :: forall (as :: [Type]) s. S.IsString s => Text' as s
text = Text X.text

-- | The XPath @contains()@ function.
contains :: S.IsString s => Text' as s -> Text' as s -> Bool' as s
contains = binary X.contains

-- | The opposite of 'contains'.
doesNotContain :: S.IsString s => Text' as s -> Text' as s -> Bool' as s
doesNotContain = binary X.doesNotContain

-- | The XPath @count()@ function.
count :: (X.IsContext c, S.IsString s) => Path' c axis n rn s -> Number' as s
count = Number . X.count . unPath

-- | The XPath @position()@ function.
position :: S.IsString s => Number' as s
position = Number X.position

unary :: (ToNonSchematic t, ToNonSchematic u, FromNonSchematic (NonSchematic u) u) =>
         (NonSchematic t -> NonSchematic u) ->
          t ->
          u
unary op x = fromNonSchematic (op $ toNonSchematic x)

binary :: (ToNonSchematic t, ToNonSchematic u, ToNonSchematic v, FromNonSchematic (NonSchematic v) v) =>
          (NonSchematic t -> NonSchematic u -> NonSchematic v) ->
          t ->
          u ->
          v
binary op x y = fromNonSchematic (toNonSchematic x `op` toNonSchematic y)

-- | The XPath @or@ operator.
(||.) :: S.IsString s => Bool' as s -> Bool' as s -> Bool' as s
(||.) = binary (X.||.)
infixr 2 ||.

-- | The XPath @and@ operator.
(&&.) :: S.IsString s => Bool' as s -> Bool' as s -> Bool' as s
(&&.) = binary (X.&&.)
infixr 3 &&.

-- | The XPath @not()@ function.
not :: S.IsString s => Bool' as s -> Bool' as s
not = Bool . X.not . unBool

-- | Access the value of the attribute @a@ of a node (equivalent to XPath's @\@@).
at :: (IsAttribute a, Member a as, S.IsString s) => proxy a -> Text' as s
at proxy = Text (X.at $ attributeName proxy)

-- | Type class for node attributes.
class IsAttribute a where
  -- | Return the name of the attribute.
  attributeName :: S.IsString s => proxy a -> s

-- | Type family which returns the node attribute(s) used within a given expression.
type family AttributesUsed t where
  AttributesUsed (Bool' as s) = as
  AttributesUsed (Text' as s) = as
  AttributesUsed (Number' as s) = as

-- | The XPath @=@ operator.
(=.) :: (ToNonSchematic t,
         X.Eq (NonSchematic t),
         S.IsString (X.Showed (NonSchematic t))) =>
         t -> t -> Bool' (AttributesUsed t) (X.Showed (NonSchematic t))
(=.) = binary (X.=.)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (ToNonSchematic t,
          X.Eq (NonSchematic t),
          S.IsString (X.Showed (NonSchematic t))) =>
          t -> t -> Bool' (AttributesUsed t) (X.Showed (NonSchematic t))
(/=.) = binary (X./=.)
infix 4 /=.

-- | The XPath @<@ operator.
(<.) :: (ToNonSchematic t,
         X.Ord (NonSchematic t),
         S.IsString (X.Showed (NonSchematic t))) =>
         t -> t -> Bool' (AttributesUsed t) (X.Showed (NonSchematic t))
(<.) = binary (X.<.)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: (ToNonSchematic t,
         X.Ord (NonSchematic t),
         S.IsString (X.Showed (NonSchematic t))) =>
         t -> t -> Bool' (AttributesUsed t) (X.Showed (NonSchematic t))
(<=.) = binary (X.<=.)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: (ToNonSchematic t,
         X.Ord (NonSchematic t),
         S.IsString (X.Showed (NonSchematic t))) =>
         t -> t -> Bool' (AttributesUsed t) (X.Showed (NonSchematic t))
(>.) = binary (X.>.)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: (ToNonSchematic t,
         X.Ord (NonSchematic t),
         S.IsString (X.Showed (NonSchematic t))) =>
         t -> t -> Bool' (AttributesUsed t) (X.Showed (NonSchematic t))
(>=.) = binary (X.>=.)
infix 4 >=.

instance S.IsString s => P.Num (Number' a s) where
  (+) = binary (+)
  (*) = binary (*)
  abs = unary P.abs
  signum = unary P.signum
  fromInteger = Number . P.fromInteger
  negate = unary P.negate

-- | Type of an XPath node of type @n@ which can be showed as the string type @s@.
newtype Node' (n :: Type) s = Node { unNode :: X.Node' s }

-- | 'Node'' specialised so it can be shown as 'P.String'.
type Node n = Node' n P.String

-- | Type class of node types.
class IsNode n where
  -- | Return the name of the node.
  nodeName :: S.IsString s => proxy n -> s

-- | Create a node expression of the given type.
namedNode :: forall n s. (IsNode n, S.IsString s) => Node' n s
namedNode = Node . X.namedNode $ nodeName (Proxy :: Proxy n)

-- | Type family to constrain the possible relatives of nodes of type @n@ through the given axis.
type family Relatives n axis :: [Type]

-- | Type of the XPath @ancestor::@ axis.
data Ancestor

-- | Type of the XPath @child::@ axis.
data Child

-- | Type of the XPath @descendant::@ axis.
data Descendant

-- | Type of the XPath @descendant-or-self::@ axis.
data DescendantOrSelf

-- | Type of the XPath @following::@ axis.
data Following

-- | Type of the XPath @following-sibling::@ axis.
data FollowingSibling

-- | Type of the XPath @parent::@ axis.
data Parent

-- | Type of the XPath @self::@ axis.
data Self

type instance Relatives n Self = '[n]

-- | Type of the document root for the schema @sc@ which can be showed as the string type @s@. Useful in forming an
-- XPaths which must begin from the root.
newtype DocumentRoot' sc s = DocumentRoot { unDocumentRoot :: X.DocumentRoot' s }

-- | 'DocumentRoot'' specialised so it can be shown as 'P.String'.
type DocumentRoot sc = DocumentRoot' sc P.String

type instance Relatives (DocumentRoot' sc s) Ancestor = '[]
type instance Relatives (DocumentRoot' sc s) Following = '[]
type instance Relatives (DocumentRoot' sc s) FollowingSibling = '[]
type instance Relatives (DocumentRoot' sc s) Parent = '[]

-- | The root of the document for the schema @s@.
root' :: DocumentRoot' sc s
root' = DocumentRoot X.root'

-- | 'root'' specialised so it can be shown as 'P.String'.
root :: DocumentRoot sc
root = root'

-- | Type family to infer of the axis of a location step based on the type of the step.
type family Axis p where
  Axis (Path' c axis n rn s) = axis
  Axis (Node' n s) = Child
  Axis (DocumentRoot' sc s) = Self

-- | Type family to infer the type of the node selected by the first location step in a path.
type family SelectNode p where
  SelectNode (Path' c axis n rn s) = n
  SelectNode (Node' n s) = n
  SelectNode (DocumentRoot' sc s) = DocumentRoot' sc s

-- | Type family to infer the node selected by the last location step in a path.
type family ReturnNode p where
  ReturnNode (Path' c axis n rn s) = rn
  ReturnNode (Node' n s) = n
  ReturnNode (DocumentRoot' sc s) = DocumentRoot' sc s

-- | Constraint for types from which a path can be inferred.
type PathLike p = (ToNonSchematic p, X.PathLike (NonSchematic p))

-- | The XPath @/@ operator.
(/.) :: (Member (SelectNode q) (Relatives (ReturnNode p) (Axis q)),
          PathLike p,
          PathLike q,
          X.SlashOperator (NonSchematic p) (NonSchematic q)) =>
          p ->
          q ->
          Path' (X.Context (NonSchematic p)) (Axis p) (SelectNode p) (ReturnNode q) (X.Showed (NonSchematic q))
(/.) = binary (X./.)
infixl 8 /.

-- | The XPath @//@ operator.
(//.) :: (Member (SelectNode q) (Relatives (ReturnNode p) Descendant),
          PathLike p,
          PathLike q,
          X.DoubleSlashOperator (NonSchematic p) (NonSchematic q)) =>
          p ->
          q ->
          Path' (X.Context (NonSchematic p)) (Axis p) (SelectNode p) (ReturnNode q) (X.Showed (NonSchematic q))
(//.) = binary (X.//.)
infixl 8 //.

-- | Type family which contrains the possible attributes a node of type @n@ may have.
type family Attributes n :: [Type]

-- | Filter the path-like expression using the given predicate(s). The predicates must only make use of the attributes
-- of the type of node selected by the path, otherwise it will not type check.
(#) :: (PathLike p,
        ToNonSchematic p,
        FromNonSchematic (NonSchematic p) p,
        X.Filterable (NonSchematic p)) =>
        p -> [Bool' (Attributes (ReturnNode p)) (X.Showed (NonSchematic p))] -> p
p # preds = fromNonSchematic $ toNonSchematic p X.# (toNonSchematic <$> preds)
infixl 9 #

-- | The XPath @ancestor::@ axis.
ancestor :: Node' n s -> Path' X.CurrentContext Ancestor n n s
ancestor (Node n) = Path $ X.ancestor n

-- | The XPath @child::@ axis.
child :: Node' n s -> Path' X.CurrentContext Child n n s
child (Node n) = Path $ X.child n

-- | The XPath @descendant::@ axis.
descendant :: Node' n s -> Path' X.CurrentContext Descendant n n s
descendant (Node n) = Path $ X.descendant n

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node' n s -> Path' X.CurrentContext DescendantOrSelf n n s
descendantOrSelf (Node n) = Path $ X.descendantOrSelf n

-- | The XPath @following::@ axis.
following :: Node' n s -> Path' X.CurrentContext Following n n s
following (Node n) = Path $ X.following n

-- | The XPath @following-sibling::@ axis.
followingSibling :: Node' n s -> Path' X.CurrentContext FollowingSibling n n s
followingSibling (Node n) = Path $ X.followingSibling n

-- | The XPath @parent::@ axis.
parent :: Node' n s -> Path' X.CurrentContext Parent n n s
parent (Node n) = Path $ X.parent n

-- | Display an XPath expression. This is useful for sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show' :: (PathLike p,
          X.IsExpression (NonSchematic p),
          P.Monoid (X.Showed (NonSchematic p)),
          S.IsString (X.Showed (NonSchematic p)),
          P.Show (X.Showed (NonSchematic p))) =>
          p -> X.Showed (NonSchematic p)
show' = X.show' . toNonSchematic

-- | 'show'' specialised to generate 'P.String's.
show :: (PathLike p,
        X.Showed (NonSchematic p) ~ P.String,
        X.IsExpression (NonSchematic p)) =>
        p -> P.String
show = show'
