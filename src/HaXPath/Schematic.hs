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
  and,
  Expression,
  IsExpression,
  IsPath(..),
  not,
  or,
  Path,
  RelativePath,
  text,
  Union,
  unsafeAt,
  (=.),
  (/=.),
  (<.),
  (<=.),
  (>.),
  (>=.),
  (/.),
  (#)
) where

import qualified Data.String as S
import qualified Data.Text as T
import qualified HaXPath as X
import Prelude ((.), ($))
import qualified Prelude as P

class Union (l :: [*]) (m :: [*]) (lm :: [*]) | l m -> lm

instance Union '[] m m

instance Union l' m lm => Union (l ': l') m (l ': lm)

newtype Expression (x :: *) (a :: [*]) = Expression { unExpression :: X.Expression x } deriving (P.Eq, P.Ord)

class IsExpression (h :: *) (x :: *) (a :: [*]) | h -> x a where
  fromHaskell :: h -> Expression x a

instance IsExpression P.String X.Text '[] where
  fromHaskell = Expression . S.fromString

instance IsExpression P.Integer X.Int '[] where
  fromHaskell = Expression . P.fromInteger

instance IsExpression P.Bool X.Bool '[] where
  fromHaskell = Expression . X.literalBool

instance IsExpression (Expression x a) x a where
  fromHaskell = P.id

text :: Expression X.Text '[]
text = Expression X.text

or :: (IsExpression h1 X.Bool a, IsExpression h2 X.Bool b, Union a b c) =>
      h1 -> h2 -> Expression X.Bool c
x `or` y = Expression $ unExpression (fromHaskell x) `X.or` unExpression (fromHaskell y)
infixr 4 `or`

and :: (IsExpression h1 X.Bool a, IsExpression h2 X.Bool b, Union a b c) =>
      h1 -> h2 -> Expression X.Bool c
x `and` y = Expression $ unExpression (fromHaskell x) `X.and` unExpression (fromHaskell y)
infixr 5 `and`

not :: IsExpression h X.Bool a => h -> Expression X.Bool a
not = Expression . X.not . unExpression . fromHaskell

unsafeAt :: T.Text -> Expression X.Text a
unsafeAt = Expression . X.at

binary :: (IsExpression h1 x1 a1, IsExpression h2 x2 a2, Union a1 a2 a3) =>
          (X.Expression x1 -> X.Expression x2 -> X.Expression x3) -> h1 -> h2 -> Expression x3 a3
binary op x y = Expression $ unExpression (fromHaskell x) `op` unExpression (fromHaskell y)

(=.) :: (X.Eq x, IsExpression h1 x a, IsExpression h2 x b, Union a b c) =>
        h1 -> h2 -> Expression X.Bool c
(=.) = binary (X.=.)

(/=.) :: (X.Eq x, IsExpression h1 x a, IsExpression h2 x b, Union a b c) =>
        h1 -> h2 -> Expression X.Bool c
(/=.) = binary (X./=.)

(<.) :: (X.Ord x, IsExpression h1 x a, IsExpression h2 x b, Union a b c) =>
        h1 -> h2 -> Expression X.Bool c
(<.) = binary (X.<.)

(<=.) :: (X.Ord x, IsExpression h1 x a, IsExpression h2 x b, Union a b c) =>
        h1 -> h2 -> Expression X.Bool c
(<=.) = binary (X.<=.)

(>.) :: (X.Ord x, IsExpression h1 x a, IsExpression h2 x b, Union a b c) =>
        h1 -> h2 -> Expression X.Bool c
(>.) = binary (X.>.)

(>=.) :: (X.Ord x, IsExpression h1 x a, IsExpression h2 x b, Union a b c) =>
        h1 -> h2 -> Expression X.Bool c
(>=.) = binary (X.>=.)

newtype RelativePath s n = RelativePath  { unRelativePath :: X.RelativePath }  deriving (P.Eq, P.Ord)

newtype Path s n = Path { unPath :: X.Path } deriving (P.Eq, P.Ord)

instance IsExpression (Path s n) X.NodeSet '[] where
  fromHaskell = Expression . unPath

class X.IsPath u => IsPath t u | t -> u where
  toUntypedPath :: t s n -> u

  unsafeFromUntypedPath :: u -> t s n

instance IsPath RelativePath X.RelativePath where
  toUntypedPath = unRelativePath

  unsafeFromUntypedPath = RelativePath

instance IsPath Path X.Path where
  toUntypedPath = unPath

  unsafeFromUntypedPath = Path

-- | Witnesses that a node of type 'n' may have an attribute of type 'a'.
class NodeAttribute n a

-- | Witnesses that a node of type 'n' may have zero or more of a set of attributes 'a'.
class NodeAttributes (n :: *) (a :: [*])

instance (NodeAttribute n h, NodeAttributes n t) => NodeAttributes n (h ': t)

instance NodeAttributes n '[]

(/.) :: IsPath p u => p s m -> RelativePath s n -> p s n
p1 /. p2 = unsafeFromUntypedPath $ toUntypedPath p1 X./. toUntypedPath p2
infixr 2 /.

(#) :: (IsPath p u, NodeAttributes n a) => p s n -> Expression X.Bool a -> p s n
p # expr = unsafeFromUntypedPath $ toUntypedPath p X.# unExpression expr
