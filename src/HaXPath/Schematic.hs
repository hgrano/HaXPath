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
  Attribute,
  child,
  descendantOrSelf,
  doubleSlash,
  Expression,
  fromRoot,
  IsPath(..),
  namedNode,
  Node,
  NodeAttribute,
  NodeAttributes,
  NodesAttributes,
  node,
  not,
  Path,
  RelativePath,
  SchemaNodes,
  show,
  text,
  Text,
  Union,
  unsafeAt,
  (#),
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
import Prelude ((.), ($))
--import qualified Prelude as P
--
-- | The union of two sets of types.
class Union (l :: [*]) (m :: [*]) (lm :: [*]) | l m -> lm

instance Union '[] m m

instance Union l' m lm => Union (l ': l') m (l ': lm)

-- | An XPath expression returning a value of type 'x', involving zero or more attributes 'a'.
newtype Expression (x :: *) (a :: [*]) = Expression { unExpression :: X.Expression x }

type Text = Expression X.Text '[]

instance S.IsString Text where
  fromString = Expression . S.fromString

-- | The XPath @text()@ function.
text :: Expression X.Text '[]
text = Expression X.text

binary :: Union a b c =>
          (X.Expression x -> X.Expression y -> X.Expression z) ->
          Expression x a ->
          Expression y b ->
          Expression z c
binary op x y = Expression $ unExpression x `op` unExpression y

-- | The XPath @or()@ function.
(||.) :: Union a b c => Expression X.Bool a -> Expression X.Bool b -> Expression X.Bool c
(||.) = binary (X.||.)
infixr 4 ||.

-- | The XPath @and@ operator.
(&&.) :: Union a b c => Expression X.Bool a -> Expression X.Bool b -> Expression X.Bool c
(&&.) = binary (X.&&.)
infixr 5 &&.

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
infixr 4 =.

-- | The XPath @!=@ operator.
(/=.) :: (X.Eq x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(/=.) = binary (X./=.)
infixr 4 /=.

-- | The XPath @<@ operator.
(<.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(<.) = binary (X.<.)

-- | The XPath @<=@ operator.
(<=.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(<=.) = binary (X.<=.)

-- | The XPath @>@ operator.
(>.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(>.) = binary (X.>.)

-- | The XPath @>=@ operator.
(>=.) :: (X.Ord x, Union a b c) => Expression x a -> Expression x b -> Expression X.Bool c
(>=.) = binary (X.>=.)

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

(./.) :: IsPath p u => p s m -> RelativePath s n -> p s n
p1 ./. p2 = unsafeFromNonSchematicPath $ toNonSchematicPath p1 X../. toNonSchematicPath p2
infixl 2 ./.

(#) :: (IsPath p u, X.Filterable u, NodesAttributes n a) => p s n -> Expression X.Bool a -> p s n
p # expr = unsafeFromNonSchematicPath $ toNonSchematicPath p X.# unExpression expr
infixl 3 #

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
