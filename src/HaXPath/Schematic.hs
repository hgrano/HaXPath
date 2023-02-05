{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

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
  ancestor,
  Ancestor,
  at,
  Attributes,
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
  Path,
  PathLike,
  position,
  Relatives,
  root,
  show,
  text,
  Text,
  ToNonSchematic(..)
) where

import           Data.HList.CommonMain (HMember)
import qualified Data.String as S
import qualified Data.Text   as T
import qualified HaXPath     as X
import           Prelude     (($), (*), (+), (.), (<$>))
import qualified Prelude     as P

type Member x xs = HMember x xs 'P.True

-- | The type of simple boolean expressions.
newtype Bool (as :: [*]) = Bool { unBool :: X.Bool }

-- | The type of simple numeric expressions.
newtype Number (as :: [*]) = Number { unNumber :: X.Number }

-- | The type of simple text expressions.
newtype Text (as :: [*]) = Text { unText :: X.Text }

newtype Path c axis n rn  = Path { unPath :: X.Path c }

-- | Create a literal XPath value.
lit :: (FromNonSchematic (X.AsExpression h) t, X.Literal h) => h -> t
lit = fromNonSchematic . X.lit

instance S.IsString (Text as) where
  fromString = Text . S.fromString

class ToNonSchematic t where
  type NonSchematic t

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

-- make sure to hide this function!
instance FromNonSchematic X.Node (Node n) where
  fromNonSchematic = Node

instance FromNonSchematic X.DocumentRoot (DocumentRoot n) where
  fromNonSchematic = DocumentRoot

-- | The XPath @text()@ function.
text :: Text as
text = Text X.text

-- | The XPath @contains()@ function.
contains :: Text as -> Text as -> Bool as
contains = binary X.contains

-- | The opposite of 'contains'.
doesNotContain :: Text as -> Text as -> Bool as
doesNotContain = binary X.doesNotContain

-- | The XPath @count()@ function.
count :: X.IsCtx c => Path c axis n rn -> Number as
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
(||.) :: Bool a -> Bool a -> Bool a
(||.) = binary (X.||.)
infixr 2 ||.

-- | The XPath @and@ operator.
(&&.) :: Bool a -> Bool a -> Bool a
(&&.) = binary (X.&&.)
infixr 3 &&.

-- | The XPath @not()@ function.
not :: Bool a -> Bool a
not = Bool . X.not . unBool

at :: (IsAttribute a, Member a as) => proxy a -> Text as
at proxy = Text (X.at $ attributeName proxy)

class IsAttribute a where
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

-- | Type of an XPath node.
newtype Node (n :: *) = Node { unNode :: X.Node }

class IsNode n where
  nodeName :: proxy n -> T.Text

-- | Create a node with the given name.
namedNode :: IsNode n => proxy n -> Node n
namedNode = Node . X.namedNode . nodeName

type family Relatives n axis :: [*]

data Ancestor
data Child
data Descendant
data DescendantOrSelf
data Following
data FollowingSibling
data Parent
data Self

type instance Relatives n Self = '[n]

newtype DocumentRoot s = DocumentRoot { unDocumentRoot :: X.DocumentRoot } 

type instance Relatives (DocumentRoot s) Ancestor = '[]
type instance Relatives (DocumentRoot s) Parent = '[]

root :: DocumentRoot s
root = DocumentRoot X.root

type family Axis p where
  Axis (Path c axis n rn) = axis
  Axis (Node n) = Child

type family SelectNode p where
  SelectNode (Path c axis n rn) = n
  SelectNode (Node n) = n

type family ReturnNode p where
  ReturnNode (Path c axis n rn) = rn 
  ReturnNode (Node n) = n
  ReturnNode (DocumentRoot s) = DocumentRoot s

type PathLike p = (ToNonSchematic p, X.PathLike (NonSchematic p))

(/.) :: (Member (SelectNode q) (Relatives (ReturnNode p) (Axis q)),
          PathLike p,
          PathLike q,
          X.SlashOperator (NonSchematic p) (NonSchematic q)) =>
          p ->
          q ->
          Path (X.Context (NonSchematic p)) (Axis p) (SelectNode p) (ReturnNode q)
(/.) = binary (X./.)

(//.) :: (Member (SelectNode q) (Relatives (ReturnNode p) Descendant),
          PathLike p,
          PathLike q,
          X.DoubleSlashOperator (NonSchematic p) (NonSchematic q)) =>
          p ->
          q ->
          Path (X.Context (NonSchematic p)) (Axis p) (SelectNode p) (ReturnNode q)
(//.) = binary (X.//.)

type family Attributes n :: [*]

(#) :: (PathLike p,
        ToNonSchematic p,
        FromNonSchematic (NonSchematic p) p,
        X.Filterable (NonSchematic p)) =>
        p -> [Bool (Attributes (ReturnNode p))] -> p
p # preds = fromNonSchematic $ toNonSchematic p X.# (toNonSchematic <$> preds)

-- | The XPath @ancestor::@ axis.
ancestor :: Node n -> Path X.CurrentCtx Ancestor n n
ancestor (Node n) = Path $ X.ancestor n

-- | The XPath @child::@ axis.
child :: Node n -> Path X.CurrentCtx Child n n
child (Node n) = Path $ X.child n

-- | The XPath @descendant::@ axis.
descendant :: Node n -> Path X.CurrentCtx Descendant n n
descendant (Node n) = Path $ X.descendant n

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node n -> Path X.CurrentCtx DescendantOrSelf n n
descendantOrSelf (Node n) = Path $ X.descendantOrSelf n

-- | The XPath @following::@ axis.
following :: Node n -> Path X.CurrentCtx Following n n
following (Node n) = Path $ X.following n

-- | The XPath @following-sibling::@ axis.
followingSibling :: Node n -> Path X.CurrentCtx FollowingSibling n n
followingSibling (Node n) = Path $ X.followingSibling n

-- | The XPath @parent::@ axis.
parent :: Node n -> Path X.CurrentCtx Parent n n
parent (Node n) = Path $ X.parent n

-- | Display an XPath expression. This is useful for sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show :: (PathLike p, X.IsExpression (NonSchematic p)) => p -> T.Text
show = X.show . toNonSchematic
