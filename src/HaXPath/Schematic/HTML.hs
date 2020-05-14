{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HaXPath.Schematic.HTML where

import qualified HaXPath as X
import qualified HaXPath.Schematic as S

-- | The HTML schema.
data HTML

-- | Type of XPaths conforming to the 'HTML' schema.
type Path n = S.Path HTML n

-- | Type of relative XPaths conforming to the 'HTML' schema.
type RelativePath n = S.RelativePath HTML n

-- * Node types.

-- | Type of the HTML @a@ node.
data A

-- | The HTML @a@ node.
a :: RelativePath '[A]
a = S.unsafeFromNonSchematicPath "a"

-- | Type of the HTML @div@ node.
data Div

instance S.NodeAttribute Div Clazz
instance S.NodeAttribute Div Id

-- | The HTML @div@ node.
div :: RelativePath '[Div]
div = S.unsafeFromNonSchematicPath "div"

-- | Type of the HTML @span@ node.
data Span

instance S.NodeAttribute Span Clazz
instance S.NodeAttribute Span Id

-- | The HTML @span@ node.
span :: RelativePath '[Div]
span = S.unsafeFromNonSchematicPath "span"

-- | Type of the HTML @td@ node.
data Td

instance S.NodeAttribute Td Clazz
instance S.NodeAttribute Td Id

-- | The HTML @td@ node.
td :: RelativePath '[Td]
td = S.unsafeFromNonSchematicPath "td"

-- | Type of the HTML @tr@ node.
data Tr

-- | The HTML @tr@ node.
tr :: RelativePath '[Tr]
tr = S.unsafeFromNonSchematicPath "tr"

-- * Attribute types.

-- | Type of the HTML @\@class@ attribute.
data Clazz

-- | HTML '@class' attribute.
clazz :: S.Expression X.Text '[Clazz]
clazz = S.unsafeAt "class"

-- | Type of the HTML @\@id@ attribute.
data Id

-- | HTML @\@id@ attribute.
id :: S.Expression X.Text '[Id]
id = S.unsafeAt "id"
