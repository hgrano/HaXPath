{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaXPath.Schematic.HTML where

import qualified Data.Text         as T
import qualified HaXPath.Schematic as S

-- * Basic types

-- | The type of HTML documents.
data HTML

-- | The type of a HTML XPath expression for returning nodes of a variety of types.
type MultiNode = S.MultiNode HTML

-- | Type of a single HTML XPath node.
type Node n = S.Node HTML n

-- | The type of HTML XPaths.
type Path = S.Path HTML

-- | The type of relative HTML XPaths.
type RelativePath = S.RelativePath HTML

-- * Attribute types

-- | Type of the HTML @\@class@ attribute.
data Classname

-- | @\@class@.
classname :: S.Attribute Classname
classname = S.at "class"

-- | Type of the HTML @\@data-*@ attribute.
data Pagedata

-- | Access the @\@data-<custom\>@ attribute.
pagedata :: T.Text -> S.Attribute Pagedata
pagedata custom = S.at $ "data-" <> custom

-- | Type of the HTML @\@id@ attribute.
data Colspan

-- | @\@colspan@.
colspan :: S.Attribute Colspan
colspan = S.at "colspan"

-- | Type of the HTML @\@id@ attribute.
data Id

-- | @\@id@.
id :: S.Attribute Id
id = S.at "id"

-- | Type of the HTML @\@href@ attribute.
data Href

-- | @\@href@.
href :: S.Attribute Href
href = S.at "href"

-- | Type of the HTML @\@onclick@ attribute.
data Onclick

-- | @\@onclick@.
onclick :: S.Attribute Onclick
onclick = S.at "onClick"

-- | Type of the HTML @\@style@ attribute.
data Style

-- | @\@style@.
style :: S.Attribute Style
style = S.at "style"

-- | Type of the HTML @\@title@ attribute.
data Title

-- | @\@title@.
title :: S.Attribute Title
title = S.at "title"

-- * Node types

-- | Type of the HTML @\<a\>@ node.
data A

-- | Select an @\<a\>@.
a :: Node A
a = S.namedNode "a"

-- Attributes specific to <a>
instance S.NodeAttribute A Href

-- | Type of the HTML @\<body\>@ node.
data Body

-- | Select a @\<body\>@.
body :: Node Body
body = S.namedNode "body"

-- | Type of the HTML @\<div\>@ node.
data Div

-- | Select a @\<div\>@.
div :: Node Div
div = S.namedNode "div"

-- | Type of the HTML @\<h1\>@ node.
data H1

-- | Select a @\<h1\>@.
h1 :: Node H1
h1 = S.namedNode "h1"

-- | Type of the HTML @\<h2\>@ node.
data H2

-- | Select a @\<h2\>@.
h2 :: Node H2
h2 = S.namedNode "h2"

-- | Type of the HTML @\<h3\>@ node.
data H3

-- | Select a @\<h3\>@.
h3 :: Node H3
h3 = S.namedNode "h3"

-- | Type of the HTML @\<input\>@ node.
data Input

-- | Select an @\<input\>@.
input :: Node Input
input = S.namedNode "input"

-- | Type of the HTML @\<option\>@ node.
data Option

-- | Select an @\<option\>@.
option :: Node Option
option = S.namedNode "option"

-- | Type of the HTML @\<select\>@ node.
data Select

-- | Select a @\<select\>@.
select :: Node Select
select = S.namedNode "select"

-- | Type of the HTML @\<span\>@ node.
data Span

-- | Select a @\<span>.
span :: Node Span
span = S.namedNode "span"

-- | Type of the HTML @\<strong\>@ node.
data Strong

-- | Select a @\<strong>.
strong :: Node Strong
strong = S.namedNode "strong"

-- | Type of the HTML @\<sup\>@ node.
data Sup

-- | Select a @\<sup>.
sup :: Node Sup
sup = S.namedNode "sup"

-- | Type of the HTML @\<table\>@ node.
data Table

-- | Select a @\<table>.
table :: Node Table
table = S.namedNode "table"

-- | Type of the HTML @\<tbody\>@ node.
data Tbody

-- | Select a @\<tbody\>@.
tbody :: Node Tbody
tbody = S.namedNode "tbody"

-- | Type of the HTML @\<td\>@ node.
data Td

-- | Select a @\<td\>@.
td :: Node Td
td = S.namedNode "td"

-- Attributes specific to <td>
instance S.NodeAttribute Td Colspan

-- | Type of the HTML @\<tr\>@ node.
data Tr

-- | Select a @\<tr\>@.
tr :: Node Tr
tr = S.namedNode "tr"

instance S.SchemaNodes HTML '[A, Body, Div, H1, H2, H3, Input, Option, Select, Span, Strong, Sup, Table, Tbody, Td, Tr]

-- Attributes common to all node types
instance S.NodeAttribute A Classname
instance S.NodeAttribute A Id
instance S.NodeAttribute A Onclick
instance S.NodeAttribute A Pagedata
instance S.NodeAttribute A Style
instance S.NodeAttribute A Title

instance S.NodeAttribute Body Classname
instance S.NodeAttribute Body Id
instance S.NodeAttribute Body Onclick
instance S.NodeAttribute Body Pagedata
instance S.NodeAttribute Body Style
instance S.NodeAttribute Body Title

instance S.NodeAttribute Div Classname
instance S.NodeAttribute Div Id
instance S.NodeAttribute Div Onclick
instance S.NodeAttribute Div Pagedata
instance S.NodeAttribute Div Style
instance S.NodeAttribute Div Title

instance S.NodeAttribute H1 Classname
instance S.NodeAttribute H1 Pagedata
instance S.NodeAttribute H1 Id
instance S.NodeAttribute H1 Onclick
instance S.NodeAttribute H1 Style
instance S.NodeAttribute H1 Title

instance S.NodeAttribute H2 Classname
instance S.NodeAttribute H2 Id
instance S.NodeAttribute H2 Onclick
instance S.NodeAttribute H2 Pagedata
instance S.NodeAttribute H2 Style
instance S.NodeAttribute H2 Title

instance S.NodeAttribute H3 Classname
instance S.NodeAttribute H3 Pagedata
instance S.NodeAttribute H3 Id
instance S.NodeAttribute H3 Onclick
instance S.NodeAttribute H3 Style
instance S.NodeAttribute H3 Title

instance S.NodeAttribute Input Classname
instance S.NodeAttribute Input Id
instance S.NodeAttribute Input Onclick
instance S.NodeAttribute Input Pagedata
instance S.NodeAttribute Input Style
instance S.NodeAttribute Input Title

instance S.NodeAttribute Option Classname
instance S.NodeAttribute Option Id
instance S.NodeAttribute Option Onclick
instance S.NodeAttribute Option Pagedata
instance S.NodeAttribute Option Style
instance S.NodeAttribute Option Title

instance S.NodeAttribute Select Classname
instance S.NodeAttribute Select Id
instance S.NodeAttribute Select Onclick
instance S.NodeAttribute Select Pagedata
instance S.NodeAttribute Select Style
instance S.NodeAttribute Select Title

instance S.NodeAttribute Span Classname
instance S.NodeAttribute Span Id
instance S.NodeAttribute Span Onclick
instance S.NodeAttribute Span Pagedata
instance S.NodeAttribute Span Style
instance S.NodeAttribute Span Title

instance S.NodeAttribute Strong Classname
instance S.NodeAttribute Strong Id
instance S.NodeAttribute Strong Onclick
instance S.NodeAttribute Strong Pagedata
instance S.NodeAttribute Strong Style
instance S.NodeAttribute Strong Title

instance S.NodeAttribute Sup Classname
instance S.NodeAttribute Sup Id
instance S.NodeAttribute Sup Onclick
instance S.NodeAttribute Sup Pagedata
instance S.NodeAttribute Sup Style
instance S.NodeAttribute Sup Title

instance S.NodeAttribute Table Classname
instance S.NodeAttribute Table Id
instance S.NodeAttribute Table Onclick
instance S.NodeAttribute Table Pagedata
instance S.NodeAttribute Table Style
instance S.NodeAttribute Table Title

instance S.NodeAttribute Tbody Classname
instance S.NodeAttribute Tbody Id
instance S.NodeAttribute Tbody Onclick
instance S.NodeAttribute Tbody Pagedata
instance S.NodeAttribute Tbody Style
instance S.NodeAttribute Tbody Title

instance S.NodeAttribute Td Classname
instance S.NodeAttribute Td Id
instance S.NodeAttribute Td Onclick
instance S.NodeAttribute Td Pagedata
instance S.NodeAttribute Td Style
instance S.NodeAttribute Td Title

instance S.NodeAttribute Tr Classname
instance S.NodeAttribute Tr Id
instance S.NodeAttribute Tr Onclick
instance S.NodeAttribute Tr Pagedata
instance S.NodeAttribute Tr Style
instance S.NodeAttribute Tr Title
