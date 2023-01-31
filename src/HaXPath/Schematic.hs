{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module HaXPath.Schematic where --(
--   (&&.),
--   (*.),
--   (+.),
--   (-.),
--   (./.),
--   (/.),
--   (//.),
--   (/=.),
--   (<.),
--   (<=.),
--   (=.),
--   (>.),
--   (>=.),
--   (||.),
--   abs,
--   ancestor,
--   at,
--   Attribute,
--   Bool,
--   child,
--   contains,
--   count,
--   descendant,
--   descendantOrSelf,
--   doesNotContain,
--   doubleSlash,
--   Expression,
--   Filterable(..),
--   following,
--   followingSibling,
--   fromInteger,
--   fromRoot,
--   IsPath(..),
--   lit,
--   MultiNode,
--   namedNode,
--   negate,
--   Node,
--   node,
--   NodeAttribute,
--   NodeAttributes,
--   NodesAttributes,
--   not,
--   Number,
--   parent,
--   Path,
--   position,
--   RelativePath,
--   SchemaNodes,
--   show,
--   signum,
--   text,
--   Text,
--   Union
-- ) where

import           Data.HList.CommonMain (HMember)
--import           Data.HList.HList (HMember)
--import           Data.Proxy  (Proxy(Proxy))
import qualified Data.String as S
import qualified Data.Text   as T
-- import           Data.Type.Equality
import qualified HaXPath     as X
import           Prelude     (($), (*), (+), (.), (<$>))
import qualified Prelude     as P

-- | The union of two sets of types.
-- class Union (l :: [*]) (m :: [*]) (lm :: [*]) | l m -> lm

-- instance Union '[] m m

-- instance Union l' m lm => Union (l ': l') m (l ': lm)
-- type family TypeEq a b where
--   TypeEq a a = 'P.True
--   TypeEq a b = 'P.False

-- type a /= b = TypeEq a b ~ 'P.False

-- class DeleteAll (x :: *) (xs :: [*]) (ys :: [*]) | x xs -> ys

-- instance DeleteAll x '[] '[]

-- instance DeleteAll x xs ys => DeleteAll x (x ': xs) ys

-- instance ((x == x') ~ 'P.False, DeleteAll x xs ys) => DeleteAll x (x' ': xs) (x' ': ys)

type Member x xs = HMember x xs 'P.True
-- | An XPath expression returning a value of type 'x', involving zero or more attributes 'a'.
-- newtype Expression x = Expression { unExpression :: x }

-- | The type of simple boolean expressions.
newtype Bool (as :: [*]) = Bool { unBool :: X.Bool }

-- | The type of simple numeric expressions.
newtype Number (as :: [*]) = Number { unNumber :: X.Number }

-- | The type of simple text expressions.
newtype Text (as :: [*]) = Text { unText :: X.Text }

newtype Path c axis n rn  = Path { unPath :: X.Path c }

-- | Create a literal XPath value.
-- lit :: X.Literal h x => h -> Expression x
-- lit = Expression . X.lit

instance S.IsString (Text as) where
  fromString = Text . S.fromString

class ToExpression t where
  type Expression t

  toExpression :: t -> Expression t

instance ToExpression (Bool as) where
  type Expression (Bool as) = X.Bool

  toExpression = unBool

instance ToExpression (Number as) where
  type Expression (Number as) = X.Number

  toExpression = unNumber

instance ToExpression (Text as) where
  type Expression (Text as) = X.Text

  toExpression = unText

instance ToExpression (Path c axis n rn) where
  type Expression (Path c axis n rn) = X.Path c

  toExpression = unPath

instance ToExpression (Node n) where
  type Expression (Node n) = X.Node

  toExpression = unNode

instance ToExpression (DocumentRoot s) where
  type Expression (DocumentRoot s) = X.DocumentRoot

  toExpression = unDocumentRoot

class FromExpression x t where
  fromExpression :: x -> t

instance FromExpression X.Bool (Bool as) where
  fromExpression = Bool

instance FromExpression X.Number (Number as) where
  fromExpression = Number

instance FromExpression X.Text (Text as) where
  fromExpression = Text

instance FromExpression (X.Path c) (Path c axis n rn) where
  fromExpression = Path

-- make sure to hide this function!
instance FromExpression X.Node (Node n) where
  fromExpression = Node

instance FromExpression X.DocumentRoot (DocumentRoot n) where
  fromExpression = DocumentRoot

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

unary :: (ToExpression t, ToExpression u, FromExpression (Expression u) u) => (Expression t -> Expression u) -> t -> u
unary op x = fromExpression (op $ toExpression x)

binary :: (ToExpression t, ToExpression u, ToExpression v, FromExpression (Expression v) v) =>
          (Expression t -> Expression u -> Expression v) ->
          t ->
          u ->
          v
binary op x y = fromExpression (toExpression x `op` toExpression y)

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

-- | The type of an attribute.
--newtype Attribute (a :: *) = Attribute { unAttribute :: X.Text }

-- | Access the value of a node's attribute in text form (equivalent to XPath's @\@@). It is recommend to call this
-- function once only for each attribute in your schema. The resulting value can be re-used.
at :: (IsAttribute a, Member a as) => proxy a -> Text as
at proxy = Text (X.at $ attributeName proxy)

class IsAttribute a where
  attributeName :: proxy a -> T.Text

-- data TestAttribute

-- testAttribute :: Member TestAttribute as => Text as
-- testAttribute = at (Proxy :: Proxy TestAttribute) "testAttribute"

-- class Eq t where
--   (=.) :: Union a b c => t s a -> t s b -> Bool s c

-- instance Eq Number where
--   Number a =. Number b = Bool (a X.=. b)

-- | The XPath @=@ operator.
(=.) :: (ToExpression (t as), X.Eq (Expression (t as))) => t as -> t as -> Bool as
(=.) = binary (X.=.)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (ToExpression (t as), X.Eq (Expression (t as))) => t as -> t as -> Bool as
(/=.) = binary (X./=.)
infix 4 /=.

-- | The XPath @<@ operator.
(<.) :: (ToExpression (t as), X.Ord (Expression (t as))) => t as -> t as -> Bool as
(<.) = binary (X.<.)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: (ToExpression (t as), X.Ord (Expression (t as))) => t as -> t as -> Bool as
(<=.) = binary (X.<=.)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: (ToExpression (t as), X.Ord (Expression (t as))) => t as -> t as -> Bool as
(>.) = binary (X.>.)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: (ToExpression (t as), X.Ord (Expression (t as))) => t as -> t as -> Bool as
(>=.) = binary (X.>=.)
infix 4 >=.

-- instance P.Num (Number a) where
--   (+) = binary (+)
--   (-) = binary (-)
--   (*) = binary (*)


-- | The XPath @+@ operator.
-- (+.) :: (ToExpression t x, P.Num x) => t a -> t a -> t a
-- (+.) = binary (+)
-- infixl 6 +.

-- | The XPath @-@ operator.
-- (-.) :: Union a b c => Expression X.Number a -> Expression X.Number b -> Expression X.Number c
-- (-.) = binary (-)
-- infixl 6 -.

-- -- | The XPath @*@ operator.
-- (*.) :: Union a b c => Expression X.Number a -> Expression X.Number b -> Expression X.Number c
-- (*.) = binary (*)
-- infixl 6 *.

-- | Schematic XPath equivalent of 'P.negate'
-- negate :: Expression X.Number a -> Expression X.Number a
-- negate = Expression . P.negate . unExpression

-- -- | Schematic XPath equivalent of 'P.abs'
-- abs :: Expression X.Number a -> Expression X.Number a
-- abs = Expression . P.abs . unExpression

-- -- | Schematic XPath equivalent of 'P.signum'
-- signum :: Expression X.Number a -> Expression X.Number a
-- signum = Expression . P.signum . unExpression

-- -- | Schematic XPath equivalent of 'P.fromInteger'
-- fromInteger :: P.Integer -> Number
-- fromInteger = Expression . P.fromInteger

-- | 'P.Num' instance provided only for use of numeric literals. Use '+.', '-.', '*.' operators instead.
instance P.Num (Number a) where
  (+) = binary (+)
  (*) = binary (*)
  abs = unary P.abs
  signum = unary P.signum
  fromInteger = Number . P.fromInteger
  negate = unary P.negate

-- | The type of an XPath expression for returning nodes of a variety of types.
newtype MultiNode (n :: [*]) = MultiNode { unMultiNode :: X.Node }

-- | Type of a single XPath node.
newtype Node (n :: *) = Node { unNode :: X.Node }

class IsNode n where
  nodeName :: proxy n -> T.Text

-- | Create a node with the given name.
namedNode :: IsNode n => proxy n -> Node n
namedNode = Node . X.namedNode . nodeName

-- | A given schema 's' may be comprised of a universe of nodes 'n'.
-- class SchemaNodes (n :: [*]) | s -> n

-- -- | The XPath @node()@ function.
-- node :: SchemaNodes s n => MultiNode s n
-- node = Node $ X.namedNode "node()"

class HasRelatives n axis where
  type Relatives n axis :: [*]

data Ancestor
data Child
data Descendant
data DescendantOrSelf
data Following
data FollowingSibling
data Parent
data Self

instance HasRelatives (Node n) Self where
  type Relatives (Node n) Self = '[n]

-- instance HasRelatives (Node n) DescendantOrSelf where
--   type Relatives (Node n)
--instance HasRelation (Node n) Descendant d => HasRelation (Node n) DescendantOrSelf d

newtype DocumentRoot s = DocumentRoot { unDocumentRoot :: X.DocumentRoot } 

root :: DocumentRoot s
root = DocumentRoot X.root

class (ToExpression p, X.PathLike (Expression p)) => PathLike p where
  type Axis p
  type SelectNode p
  type ReturnNode p

instance X.IsCtx c => PathLike (Path c axis n rn) where
  --type Context (Path c axis n rn) = c
  type Axis (Path c axis n rn) = axis
  type SelectNode (Path c axis n rn) = n
  type ReturnNode (Path c axis n rn) = rn

instance PathLike (Node n) where
  --type Context (Node n) = X.CurrentCtx
  type Axis (Node n) = Child
  type SelectNode (Node n) = n
  type ReturnNode (Node n) = n

instance PathLike (DocumentRoot s) where
  type Axis (DocumentRoot s) = Self
  type SelectNode (DocumentRoot s) = DocumentRoot s
  type ReturnNode (DocumentRoot s) = DocumentRoot s

(/.) :: (Member (SelectNode q) (Relatives (ReturnNode p) (Axis q)),
          PathLike p,
          PathLike q,
          X.SlashOperator (Expression p) (Expression q)) =>
          p ->
          q ->
          Path (X.Context (Expression p)) (Axis p) (SelectNode p) (ReturnNode q)
(/.) = binary (X./.)

-- class (PathLike p, PathLike q) => SlashOperator p q where
--   -- type SlashOutput p q

--   (/.) :: (Member (SelectNode q) (Relatives (ReturnNode p) (Axis q))) =>
--           p ->
--           q ->
--           Path (Context p) (Axis p) (SelectNode p) (ReturnNode q)
--   infixl 8 /.

-- instance X.IsCtx c => SlashOperator (Path c axis n rn) (Path X.CurrentCtx axis' n' rn') where
--   -- type SlashOutput (Path c axis n rn) (Path X.CurrentCtx axis' n' rn') = Path c axis n rn'

--   Path pa /. Path nextPa = Path (pa X./. nextPa)

-- instance X.IsCtx c => SlashOperator (Path c axis n rn) (Node n') where
--   -- type SlashOutput (Path c axis n rn) (Node n') = Path c axis n n'

--   Path pa /. Node n = Path (pa X./. n)

-- instance (HasRelation n axis n') =>
--           SlashOperator (Node n) (Path X.CurrentCtx axis n' rn) (Path X.CurrentCtx Child n rn) where
--   Node n /. Path pa = Path (n X./. pa)

-- instance (HasRelation n Child n') => SlashOperator (Node n) (Node n') (Path X.CurrentCtx Child n n') where
--   Node n /. Node n' = Path (n X./. n')

-- instance (HasRelation (DocumentRoot s) axis n) =>
--           SlashOperator (DocumentRoot s) (Path X.CurrentCtx axis n rn) (Path X.RootCtx axis n rn) where
--   DocumentRoot r /. Path p = Path (r X./. p)

-- instance (HasRelation (DocumentRoot s) Child n) =>
--           SlashOperator (DocumentRoot s) (Node n) (Path X.RootCtx Child n n) where
--   DocumentRoot r /. Node n = Path (r X./. n)

-- -- | A relative XPath for a schema 's' returning a set of nodes which may be any of the type-list 'n'.
-- newtype RelativePath (s :: *) (n :: [*]) = RelativePath  { unRelativePath :: X.RelativePath }

-- -- | The type of XPaths for a schema 's' returning a set of nodes which may be any of the type-list 'n'.
-- newtype Path (s :: *) (n :: [*]) = Path { unPath :: X.Path }

-- | Type class for allowing XPath-like operations. Do not create instances of this class.
-- class X.IsPath u => IsPath (t :: * -> [*] -> *) (u :: *) | t -> u where
--   -- | Convert a schematic XPath to its non-schematic equivalent.
--   toNonSchematicPath :: t s n -> u

--   -- | Unsafely (without type checking) convert a non-schematic XPath to its schematic equivalent.
--   unsafeFromNonSchematicPath :: u -> t s n

-- instance IsPath RelativePath X.RelativePath where
--   toNonSchematicPath = unRelativePath

--   unsafeFromNonSchematicPath = RelativePath

-- instance IsPath Path X.Path where
--   toNonSchematicPath = unPath

--   unsafeFromNonSchematicPath = Path

-- | Witnesses that a node of type 'n' may have an attribute of type 'a'.
-- class NodeAttribute n a

-- | Witnesses that a node of type 'n' may have zero or more of a set of attributes 'a'.
--class NodeAttributes (n :: *) (a :: [*]) | n -> a

class HasAttributes (n :: *) where
  type As n :: [*]

-- instance (NodeAttribute n h, NodeAttributes n t) => NodeAttributes n (h ': t)

-- instance NodeAttributes n '[]

-- | Witnesses that a set of nodes 'n' may have zero or more of a set of attributes 'a'.
-- class NodesAttributes (n :: [*]) (a :: [*])

-- instance (NodeAttributes n a, NodesAttributes n' a) => NodesAttributes (n ': n') a

-- instance NodesAttributes '[] a

class Filterable t where
  type B t
  -- | Filter a set of nodes by the given predicate.
  (#) :: t -> [B t] -> t
  infixl 3 #

-- (#.) :: Node n -> [Bool n] -> Node n
-- Node n #. preds = Node $ n X.# (unBool <$> preds)

instance HasAttributes n => Filterable (Node n) where
  type B (Node n) = Bool (As n)
  Node n # preds = Node $ n X.# (unBool <$> preds)

instance (HasAttributes rn, X.IsCtx c) => Filterable (Path c axis n rn) where
  type B (Path c axis n rn) = Bool (As rn)
  Path p # preds = Path $ p X.# (unBool <$> preds)

-- | The XPath (non-abbreviated) @/@ operator.
-- (./.) :: IsPath p u => p s m -> RelativePath s n -> p s n
-- p1 ./. p2 = unsafeFromNonSchematicPath $ toNonSchematicPath p1 X../. toNonSchematicPath p2
-- infixl 2 ./.

-- | The XPath abbreviated @/@ operator.
-- (/.) :: IsPath p u => p s m -> MultiNode s n -> p s n
-- p /. (Node n) = unsafeFromNonSchematicPath $ toNonSchematicPath p X./. n
-- infixl 2 /.

-- -- | The XPath @//@ operator.
-- (//.) :: IsPath p u => p s m -> MultiNode s n -> p s n
-- p //. (Node n) = unsafeFromNonSchematicPath $ toNonSchematicPath p X.//. n
-- infixl 2 //.

-- | Fix a relative path to begin from the document root (i.e. create an absolute path).
-- fromRoot :: RelativePath s n -> Path s n
-- fromRoot = unsafeFromNonSchematicPath . X.fromRoot . toNonSchematicPath

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

-- -- | The XPath @//@ operator.
-- doubleSlash :: MultiNode s n -> Path s n
-- doubleSlash (Node n) = unsafeFromNonSchematicPath $ X.doubleSlash n

-- | Display an XPath expression. This is useful for sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show :: X.IsCtx c => Path c axis n rn -> T.Text
show = X.show . unPath
