{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module HaXPath.Schematic(
  abs,
  Attribute,
  Bool,
  child,
  contains,
  count,
  descendantOrSelf,
  doubleSlash,
  Expression,
  Filterable(..),
  fromInteger,
  fromRoot,
  IsPath(..),
  lit,
  namedNode,
  negate,
  Node,
  NodeAttribute,
  NodeAttributes,
  NodesAttributes,
  node,
  not,
  Number,
  Path,
  position,
  RelativePath,
  SchemaNodes,
  show,
  signum,
  text,
  Text,
  Union,
  unsafeAt,
  (+.),
  (-.),
  (*.),
  (&&.),
  (./.),
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

import qualified Data.String as S
import qualified Data.Text as T
import qualified HaXPath as X
import Prelude ((+), (-), (*), (.), ($))
import qualified Prelude as P
--
-- | The union of two sets of types.
class Union (l :: [*]) (m :: [*]) (lm :: [*]) | l m -> lm

instance Union '[] m m

instance Union l' m lm => Union (l ': l') m (l ': lm)

-- | An XPath expression returning a value of type 'x', involving zero or more attributes 'a'.
newtype Expression (x :: *) (a :: [*]) = Expression { unExpression :: X.Expression x }

type Bool = Expression X.Bool '[]
type Number = Expression X.Number '[]
type Text = Expression X.Text '[]

lit :: X.Lit h x => h -> Expression x '[]
lit = Expression . X.lit

instance S.IsString Text where
  fromString = Expression . S.fromString

-- | The XPath @text()@ function.
text :: Expression X.Text '[]
text = Expression X.text

contains :: Union a b c => Expression X.Text a -> Expression X.Text b -> Expression X.Bool c
x `contains` y = Expression $ unExpression x `X.contains` unExpression y

count :: IsPath p u => p s n -> Expression X.Number '[]
count = Expression . X.count . toNonSchematicPath

position :: Expression X.Number '[]
position = Expression $ X.position

binary :: Union a b c =>
          (X.Expression x -> X.Expression y -> X.Expression z) ->
          Expression x a ->
          Expression y b ->
          Expression z c
binary op x y = Expression $ unExpression x `op` unExpression y

-- | The XPath @or()@ function.
(||.) :: Union a b c => Expression X.Bool a -> Expression X.Bool b -> Expression X.Bool c
(||.) = binary (X.||.)
infixr 2 ||.

-- | The XPath @and@ operator.
(&&.) :: Union a b c => Expression X.Bool a -> Expression X.Bool b -> Expression X.Bool c
(&&.) = binary (X.&&.)
infixr 3 &&.

-- | The XPath @not()@ function.
not :: Expression X.Bool a -> Expression X.Bool a
not = Expression . X.not . unExpression

type Attribute a = Expression X.Text '[a]

-- | Access the value of a node's attribute in text form (equivalent to XPath's @\@@). Unsafe because it has no way to
-- check if the attribute name provided matches the schema. It is recommend to call this function once only for each
-- attribute in your schema. The resulting value can be re-used.
unsafeAt :: T.Text -> Attribute a
unsafeAt = Expression . X.at

-- | The XPath @=@ operator.
(=.) :: (X.Eq x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(=.) = binary (X.=.)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (X.Eq x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(/=.) = binary (X./=.)
infix 4 /=.

-- | The XPath @<@ operator.
(<.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(<.) = binary (X.<.)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(<=.) = binary (X.<=.)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(>.) = binary (X.>.)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(>=.) = binary (X.>=.)
infix 4 >=.

-- | The XPath @+@ operator.
(+.) :: Union a b c => Expression X.Number a -> Expression X.Number b -> Expression X.Number c
(+.) = binary (+)
infixl 6 +.

-- | The XPath @-@ operator.
(-.) :: Union a b c => Expression X.Number a -> Expression X.Number b -> Expression X.Number c
(-.) = binary (-)
infixl 6 -.

-- | The XPath @*@ operator.
(*.) :: Union a b c => Expression X.Number a -> Expression X.Number b -> Expression X.Number c
(*.) = binary (*)
infixl 6 *.

-- | Schematic XPath equivalent of 'P.negate'
negate :: Expression X.Number a -> Expression X.Number a
negate = Expression . P.negate . unExpression

-- | Schematic XPath equivalent of 'P.abs'
abs :: Expression X.Number a -> Expression X.Number a
abs = Expression . P.abs . unExpression

-- | Schematic XPath equivalent of 'P.signum
signum :: Expression X.Number a -> Expression X.Number a
signum = Expression . P.signum . unExpression

-- | Schematic XPath equivalent of 'P.fromInteger'
fromInteger :: P.Integer -> Expression X.Number '[]
fromInteger = Expression . P.fromInteger

-- | 'P.Num' instance provided only for use of numeric literals. Use '+.', '-.', '*.' operators instead.
instance P.Num (Expression X.Number '[]) where
  (+) = (+.)
  (*) = (*.)
  abs = abs
  signum = signum
  fromInteger = fromInteger
  negate = negate

newtype MultiNode (s :: *) (n :: [*]) = Node X.Node

type Node (s :: *) (n :: *) = MultiNode s '[n]

namedNode :: T.Text -> Node s n
namedNode = Node . X.namedNode

-- | A given schema 's' may have use a universe of nodes 'n'.
class SchemaNodes (s :: *) (n :: [*]) | s -> n

-- | The XPath @node()@ function.
node :: SchemaNodes s n => MultiNode s n
node = Node $ X.namedNode "node()"

-- | A relative XPath for a schema 's' returning a set of nodes which may any of the type-list 'n'.
newtype RelativePath (s :: *) (n :: [*]) = RelativePath  { unRelativePath :: X.RelativePath }

-- | The type of XPaths for a schema 's' returning a set of nodes which may any of the type-list 'n'.
newtype Path (s :: *) (n :: [*]) = Path { unPath :: X.Path }

-- | Type class for allowing XPath-like operations. Do not create instances of this class.
class X.IsPath u => IsPath (t :: * -> [*] -> *) (u :: *) | t -> u where
  -- | Convert a schematic XPath to its non-schematic equivalent.
  toNonSchematicPath :: t s n -> u

  -- | Unsafely (without type checking) convert a non-schematic XPath to its schematic equivalent.
  unsafeFromNonSchematicPath :: u -> t s n

instance IsPath RelativePath X.RelativePath where
  toNonSchematicPath = unRelativePath

  unsafeFromNonSchematicPath = RelativePath

instance IsPath Path X.Path where
  toNonSchematicPath = unPath

  unsafeFromNonSchematicPath = Path

-- | Witnesses that a node of type 'n' may have an attribute of type 'a'.
class NodeAttribute n a

-- | Witnesses that a node of type 'n' may have zero or more of a set of attributes 'a'.
class NodeAttributes (n :: *) (a :: [*])

instance (NodeAttribute n h, NodeAttributes n t) => NodeAttributes n (h ': t)

instance NodeAttributes n '[]

-- | Witnesses that a set of nodes 'n' may have zero or more of a set of attributes 'a'.
class NodesAttributes (n :: [*]) (a :: [*])

instance (NodeAttributes n a, NodesAttributes n' a) => NodesAttributes (n ': n') a

instance NodesAttributes '[] a

class Filterable (t :: * -> [*] -> *) where
  (#) :: NodesAttributes n a => t s n -> Expression X.Bool a -> t s n
  infixl 3 #

instance Filterable MultiNode where
  Node n # Expression pred = Node $ n X.# pred

instance Filterable RelativePath where
  RelativePath rp # Expression pred = RelativePath $ rp X.# pred

instance Filterable Path where
  Path rp # Expression pred = Path $ rp X.# pred

(./.) :: IsPath p u => p s m -> RelativePath s n -> p s n
p1 ./. p2 = unsafeFromNonSchematicPath $ toNonSchematicPath p1 X../. toNonSchematicPath p2
infixl 2 ./.

(/.) :: IsPath p u => p s m -> MultiNode s n -> p s n
p /. (Node n) = unsafeFromNonSchematicPath $ toNonSchematicPath p X./. n
infixl 2 /.

(//.) :: IsPath p u => p s m -> MultiNode s n -> p s n
p //. (Node n) = unsafeFromNonSchematicPath $ toNonSchematicPath p X.//. n
infixl 2 //.

fromRoot :: RelativePath s n -> Path s n
fromRoot = unsafeFromNonSchematicPath . X.fromRoot . toNonSchematicPath

child :: MultiNode s n -> RelativePath s n
child (Node n) = unsafeFromNonSchematicPath $ X.child n

descendantOrSelf :: MultiNode s n -> RelativePath s n
descendantOrSelf (Node n) = unsafeFromNonSchematicPath $ X.descendantOrSelf n

doubleSlash :: MultiNode s n -> Path s n
doubleSlash (Node n) = unsafeFromNonSchematicPath $ X.doubleSlash n

show :: IsPath p u => p s n -> T.Text
show = X.show . toNonSchematicPath
