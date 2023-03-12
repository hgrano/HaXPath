{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaXPath.Schematic (
  (&&.),
  (/.),
  (//.),
  (/=.),
  (<.),
  (<=.),
  (=.),
  (>.),
  (>=.),
  (||.),
  (#),
  AbsolutePath,
  ancestor,
  Ancestor,
  at,
  Attributes,
  Axis,
  Bool,
  child,
  Child,
  contains,
  count,
  descendant,
  Descendant,
  descendantOrSelf,
  DescendantOrSelf,
  DocumentRoot,
  doesNotContain,
  false,
  following,
  Following,
  followingSibling,
  FollowingSibling,
  IsAttribute(..),
  IsNode(..),
  lit,
  Member,
  namedNode,
  Node,
  not,
  Number,
  parent,
  Parent,
  Path,
  PathLike,
  position,
  RelativePath,
  Relatives,
  ReturnNode,
  root,
  SelectNode,
  show,
  text,
  Text,
  ToNonSchematic(..),
  true
) where

import           Data.HList.CommonMain (HMember)
import           Data.Proxy            (Proxy (Proxy))
import qualified Data.String           as S
import qualified Data.Text             as T
import qualified HaXPath               as X
import           Prelude               (($), (*), (+), (.), (<$>))
import qualified Prelude               as P

-- | Type level membership constraint indicating that the type @x@ is a member of the type-level list @xs@.
type Member x xs = HMember x xs 'P.True

-- | The type of boolean expressions which depend on the value of the attribute(s) @as@.
newtype Bool (as :: [*]) = Bool { unBool :: X.Bool }

-- | XPath @true()@ value.
true :: Bool as
true = Bool X.true

-- | XPath @false()@ value.
false :: Bool as
false = Bool X.false

-- | The type of simple numeric expressions which depend on the value of the attribute(s) @as@.
newtype Number (as :: [*]) = Number { unNumber :: X.Number }

-- | The type of simple text expressions which depend on the value of the attribute(s) @as@.
newtype Text (as :: [*]) = Text { unText :: X.Text }

-- | The type of path expressions formed by these steps:
--
-- 1. Starting from the context @c@ and moving through the given @axis@.
-- 1. Selecting node(s) of type @n@.
-- 1. Performing zero or more location steps.
-- 1. Finally returning the node(s) of type @rn@.
newtype Path c axis n rn  = Path { unPath :: X.Path c }

type AbsolutePath s = Path X.RootContext Self (DocumentRoot s)

type RelativePath = Path X.CurrentContext

-- | Create a literal XPath value.
lit :: (FromNonSchematic (X.AsExpression h) t, X.Literal h) => h -> t
lit = fromNonSchematic . X.lit

instance S.IsString (Text as) where
  fromString = Text . S.fromString

-- | Type class for conversion from a schematic value to its underlying, non-schematic version.
class ToNonSchematic t where
  -- | Corresponding non-schematic type.
  type NonSchematic t

  -- | Convert from the schematic to the non-schematic version.
  toNonSchematic :: t -> NonSchematic t

instance ToNonSchematic (Bool as) where
  type NonSchematic (Bool as) = X.Bool

  toNonSchematic = unBool

instance ToNonSchematic (Number as) where
  type NonSchematic (Number as) = X.Number

  toNonSchematic = unNumber

instance ToNonSchematic (Text as) where
  type NonSchematic (Text as) = X.Text

  toNonSchematic = unText

instance ToNonSchematic (Path c axis n rn) where
  type NonSchematic (Path c axis n rn) = X.Path c

  toNonSchematic = unPath

instance ToNonSchematic (Node n) where
  type NonSchematic (Node n) = X.Node

  toNonSchematic = unNode

instance ToNonSchematic (DocumentRoot s) where
  type NonSchematic (DocumentRoot s) = X.DocumentRoot

  toNonSchematic = unDocumentRoot

-- This type class is not exposed as this would allow for arbitrary, non-schematic expression to be converted to
-- a schematic version when the underlying expression does not actually conform to the schema.
class FromNonSchematic x t where
  fromNonSchematic :: x -> t

instance FromNonSchematic X.Bool (Bool as) where
  fromNonSchematic = Bool

instance FromNonSchematic X.Number (Number as) where
  fromNonSchematic = Number

instance FromNonSchematic X.Text (Text as) where
  fromNonSchematic = Text

instance FromNonSchematic (X.Path c) (Path c axis n rn) where
  fromNonSchematic = Path

instance FromNonSchematic X.Node (Node n) where
  fromNonSchematic = Node

instance FromNonSchematic X.DocumentRoot (DocumentRoot n) where
  fromNonSchematic = DocumentRoot

-- | The XPath @text()@ function.
text :: forall (as :: [*]). Text as
text = Text X.text

-- | The XPath @contains()@ function.
contains :: Text as -> Text as -> Bool as
contains = binary X.contains

-- | The opposite of 'contains'.
doesNotContain :: Text as -> Text as -> Bool as
doesNotContain = binary X.doesNotContain

-- | The XPath @count()@ function.
count :: X.IsContext c => Path c axis n rn -> Number as
count = Number . X.count . unPath

-- | The XPath @position()@ function.
position :: Number as
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
(||.) :: Bool as -> Bool as -> Bool as
(||.) = binary (X.||.)
infixr 2 ||.

-- | The XPath @and@ operator.
(&&.) :: Bool as -> Bool as -> Bool as
(&&.) = binary (X.&&.)
infixr 3 &&.

-- | The XPath @not()@ function.
not :: Bool a -> Bool a
not = Bool . X.not . unBool

-- | Access the value of the attribute @a@ of a node (equivalent to XPath's @\@@).
at :: (IsAttribute a, Member a as) => proxy a -> Text as
at proxy = Text (X.at $ attributeName proxy)

-- | Type class for node attributes.
class IsAttribute a where
  -- | Return the name of the attribute.
  attributeName :: proxy a -> T.Text

-- | The XPath @=@ operator.
(=.) :: (ToNonSchematic (t as), X.Eq (NonSchematic (t as))) => t as -> t as -> Bool as
(=.) = binary (X.=.)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (ToNonSchematic (t as), X.Eq (NonSchematic (t as))) => t as -> t as -> Bool as
(/=.) = binary (X./=.)
infix 4 /=.

-- | The XPath @<@ operator.
(<.) :: (ToNonSchematic (t as), X.Ord (NonSchematic (t as))) => t as -> t as -> Bool as
(<.) = binary (X.<.)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: (ToNonSchematic (t as), X.Ord (NonSchematic (t as))) => t as -> t as -> Bool as
(<=.) = binary (X.<=.)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: (ToNonSchematic (t as), X.Ord (NonSchematic (t as))) => t as -> t as -> Bool as
(>.) = binary (X.>.)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: (ToNonSchematic (t as), X.Ord (NonSchematic (t as))) => t as -> t as -> Bool as
(>=.) = binary (X.>=.)
infix 4 >=.

instance P.Num (Number a) where
  (+) = binary (+)
  (*) = binary (*)
  abs = unary P.abs
  signum = unary P.signum
  fromInteger = Number . P.fromInteger
  negate = unary P.negate

-- | Type of an XPath node of type @n@.
newtype Node (n :: *) = Node { unNode :: X.Node }

-- | Type class of node types.
class IsNode n where
  -- | Return the name of the node.
  nodeName :: proxy n -> T.Text

-- | Create a node expression of the given type.
namedNode :: forall n. IsNode n => Node n
namedNode = Node . X.namedNode $ nodeName (Proxy :: Proxy n)

-- | Type family to constrain the possible relatives of nodes of type @n@ through the given axis.
type family Relatives n axis :: [*]

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

-- | Type of the document root for the schema @s@. Useful in forming an XPaths which must begin from the root.
newtype DocumentRoot s = DocumentRoot { unDocumentRoot :: X.DocumentRoot }

type instance Relatives (DocumentRoot s) Ancestor = '[]
type instance Relatives (DocumentRoot s) Following = '[]
type instance Relatives (DocumentRoot s) FollowingSibling = '[]
type instance Relatives (DocumentRoot s) Parent = '[]

-- | The root of the document for the schema @s@.
root :: DocumentRoot s
root = DocumentRoot X.root

-- | Type family to infer of the axis of a location step based on the type of the step.
type family Axis p where
  Axis (Path c axis n rn) = axis
  Axis (Node n) = Child
  Axis (DocumentRoot s) = Self

-- | Type family to infer the type of the node selected by the first location step in a path.
type family SelectNode p where
  SelectNode (Path c axis n rn) = n
  SelectNode (Node n) = n
  SelectNode (DocumentRoot s) = DocumentRoot s

-- | Type family to infer the node selected by the last location step in a path.
type family ReturnNode p where
  ReturnNode (Path c axis n rn) = rn
  ReturnNode (Node n) = n
  ReturnNode (DocumentRoot s) = DocumentRoot s

-- | Constraint for types from which a path can be inferred.
type PathLike p = (ToNonSchematic p, X.PathLike (NonSchematic p))

-- | The XPath @/@ operator.
(/.) :: (Member (SelectNode q) (Relatives (ReturnNode p) (Axis q)),
          PathLike p,
          PathLike q,
          X.SlashOperator (NonSchematic p) (NonSchematic q)) =>
          p ->
          q ->
          Path (X.Context (NonSchematic p)) (Axis p) (SelectNode p) (ReturnNode q)
(/.) = binary (X./.)
infixl 8 /.

-- | The XPath @//@ operator.
(//.) :: (Member (SelectNode q) (Relatives (ReturnNode p) Descendant),
          PathLike p,
          PathLike q,
          X.DoubleSlashOperator (NonSchematic p) (NonSchematic q)) =>
          p ->
          q ->
          Path (X.Context (NonSchematic p)) (Axis p) (SelectNode p) (ReturnNode q)
(//.) = binary (X.//.)
infixl 8 //.

-- | Type family which contrains the possible attributes a node of type @n@ may have.
type family Attributes n :: [*]

-- | Filter the path-like expression using the given predicate(s). The predicates must only make use of the attributes
-- of the type of node selected by the path, otherwise it will not type check.
(#) :: (PathLike p,
        ToNonSchematic p,
        FromNonSchematic (NonSchematic p) p,
        X.Filterable (NonSchematic p)) =>
        p -> [Bool (Attributes (ReturnNode p))] -> p
p # preds = fromNonSchematic $ toNonSchematic p X.# (toNonSchematic <$> preds)
infixl 9 #

-- | The XPath @ancestor::@ axis.
ancestor :: Node n -> Path X.CurrentContext Ancestor n n
ancestor (Node n) = Path $ X.ancestor n

-- | The XPath @child::@ axis.
child :: Node n -> Path X.CurrentContext Child n n
child (Node n) = Path $ X.child n

-- | The XPath @descendant::@ axis.
descendant :: Node n -> Path X.CurrentContext Descendant n n
descendant (Node n) = Path $ X.descendant n

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node n -> Path X.CurrentContext DescendantOrSelf n n
descendantOrSelf (Node n) = Path $ X.descendantOrSelf n

-- | The XPath @following::@ axis.
following :: Node n -> Path X.CurrentContext Following n n
following (Node n) = Path $ X.following n

-- | The XPath @following-sibling::@ axis.
followingSibling :: Node n -> Path X.CurrentContext FollowingSibling n n
followingSibling (Node n) = Path $ X.followingSibling n

-- | The XPath @parent::@ axis.
parent :: Node n -> Path X.CurrentContext Parent n n
parent (Node n) = Path $ X.parent n

-- | Display an XPath expression. This is useful for sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show :: (PathLike p, X.IsExpression (NonSchematic p)) => p -> T.Text
show = X.show . toNonSchematic
