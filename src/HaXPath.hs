{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module HaXPath(
  (#),
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
  (|.),
  ancestor,
  at,
  Bool,
  child,
  contains,
  count,
  descendant,
  descendantOrSelf,
  DocumentRoot,
  doesNotContain,
  Eq,
  false,
  following,
  followingSibling,
  IsCtx,
  IsExpression,
  Literal(..),
  namedNode,
  node,
  Node,
  not,
  Number,
  Ord,
  parent,
  Path,
  position,
  root,
  self,
  show,
  text,
  Text,
  true
) where

import           Data.Proxy  (Proxy(Proxy))
import qualified Data.String as S
import qualified Data.Text   as T
import           Prelude     (($), (*), (+), (-), (.), (<$>), (<>), (==))
import qualified Prelude     as P

-- | XPath textual (string) data type.
newtype Text = Text { unText :: Expression }

-- | XPath numeric data type.
newtype Number = Number { unNumber :: Expression }

-- | XPath boolean data type.
newtype Bool = Bool { unBool :: Expression }

-- | XPath true value.
true :: Bool
true = Bool $ Function "true" []

-- | XPath false value.
false :: Bool
false = Bool $ Function "false" []

data PathBegin = FromRootCtx | FromCurrentCtx deriving (P.Eq)

-- Internal data type to represent an XPath expression.
data Expression = Function T.Text [Expression] |
                  -- Apply the named function to zero or more arguments.
                  Operator T.Text Expression Expression |
                  -- Apply a binary operator to the two operands.
                  Attribute T.Text |
                  -- Access the given attribute of the node (@).
                  TextLiteral T.Text |
                  -- Text value in quotes.
                  IntegerLiteral P.Integer |
                  -- Literal integer (XPath number).
                  NamedNode T.Text |
                  -- Select node with the provided name.
                  FilteredNode Expression [Expression] |
                  LocationStep Axis Expression |
                  -- From current context move along the given axis and select nodes matching the expression.
                  PathFrom PathBegin Expression (P.Maybe Expression) [Expression]
                  -- From the starting point, take the first path (expression), then follow the next path (expression)
                  -- (if present) and finally filter by zero or more boolean (expressions).

-- | Class of for all types which can be used to form a valid XPath expression. Library users should not create
-- instances of this class.
class IsExpression a where
  toExpression :: a -> Expression

instance IsExpression Text where
  toExpression = unText

instance IsExpression Number where
  toExpression = unNumber

instance IsExpression Bool where
  toExpression = unBool

showExpression :: Expression -> T.Text
showExpression (Function f es) = f <> "(" <> args <> ")"
  where
    args = T.intercalate ", " $ showExpression <$> es
showExpression (Operator o a b) =
  showOperand a <> " " <> o <> " " <> showOperand b
  where
    showOperand e@(TextLiteral _) = showExpression e
    showOperand e@(IntegerLiteral _) = showExpression e
    showOperand e@(Function _ _) = showExpression e
    showOperand e@(Attribute _) = showExpression e
    showOperand e = "(" <> showExpression e <> ")"

showExpression (Attribute a) = "@" <> a
showExpression (TextLiteral t) = "'" <> t <> "'" -- TODO escaping
showExpression (IntegerLiteral i) = T.pack $ P.show i
showExpression (PathFrom begin p pNextMay preds) =
  let prefix = case begin of
        FromRootCtx -> "/"
        FromCurrentCtx -> ""
  in
  let showPath x = case x of
        LocationStep _ _ -> showExpression x
        _ -> "(" <> showExpression x <> ")"
  in
  let fullPShowed = prefix <> showPath p <> case pNextMay of
        P.Nothing -> ""
        P.Just pNext -> "/" <> showPath pNext
  in
  showWithPredicates fullPShowed preds
showExpression (LocationStep axis n) = showAxis axis <> "::" <> showExpression n
showExpression (NamedNode n) = n
showExpression (FilteredNode n preds) = showExpression n <> showPredicates preds

showPredicates :: [Expression] -> T.Text
showPredicates preds =  "[" <> T.intercalate "][" (showExpression <$> preds) <> "]"

showWithPredicates :: T.Text -> [Expression] -> T.Text
showWithPredicates s es
  | P.not (P.null es) = "(" <> s <> ")" <> showPredicates es
  | P.otherwise = s

-- | Display an XPath expression. This is useful to sending the XPath expression to a separate XPath evaluator e.g.
-- a web browser.
show :: IsExpression a => a -> T.Text
show = showExpression . toExpression

-- | Type class for Haskell values which can be converted to a XPath (literal) values.
class IsExpression x => Literal h x | h -> x where
  -- | Create an XPath value from a Haskell value.
  lit :: h -> x

instance Literal P.Bool Bool where
  lit P.True = true
  lit P.False = false

instance Literal P.Integer Number where
  lit = Number . IntegerLiteral

instance Literal T.Text Text where
  lit = Text . TextLiteral

instance S.IsString Text where
  fromString = lit . T.pack

boolToInt :: Bool -> Number
boolToInt (Bool b) = Number b

-- | Access the value of a node's attribute in text form (equivalent to XPath's @\@@).
at :: T.Text -> Text
at = Text . Attribute

-- | Type class of XPath types that can be compared for equality. Library users should not create instances of this
-- class.
class IsExpression t => Eq t

instance Eq Text
instance Eq Number
instance Eq Bool

-- | The XPath @=@ operator.
(=.) :: Eq a => a -> a -> Bool
x =. y = Bool $ Operator "=" (toExpression x) (toExpression y)
infix 4 =.

-- | The XPath @!=@ operator.
(/=.) :: Eq a => a -> a -> Bool
x /=. y = Bool $ Operator "!=" (toExpression x) (toExpression y)
infix 4 /=.

-- | Type class of XPath types that can be ordered. Library users should not create instances of this class.
class Eq t => Ord t

instance Ord Text
instance Ord Number
instance Ord Bool

-- | The XPath @<@ operator.
(<.) :: Ord a => a -> a -> Bool
x <. y = Bool $ Operator "<" (toExpression x) (toExpression y)
infix 4 <.

-- | The XPath @<=@ operator.
(<=.) :: Ord a => a -> a -> Bool
x <=. y = Bool $ Operator "<=" (toExpression x) (toExpression y)
infix 4 <=.

-- | The XPath @>@ operator.
(>.) :: Ord a => a -> a -> Bool
x >. y = Bool $ Operator ">" (toExpression x) (toExpression y)
infix 4 >.

-- | The XPath @>=@ operator.
(>=.) :: Ord a => a -> a -> Bool
x >=. y = Bool $ Operator ">=" (toExpression x) (toExpression y)
infix 4 >=.

instance P.Num Number where
  Number x + Number y = Number $ Operator "+" x y

  Number x - Number y = Number $ Operator "-" x y

  Number x * Number y = Number $ Operator "*" x y

  abs x = x * P.signum x

  signum x = boolToInt (x >. 0) - boolToInt (x <. 0)

  fromInteger = lit

-- | The XPath @position()@ function.
position :: Number
position = Number $ Function "position" []

-- | The XPath @text()@ function.
text :: Text
text = Text $ Function "text" []

-- | The XPath @contains()@ function.
contains :: Text -> Text -> Bool
contains x y = Bool . Function "contains" $ [toExpression x, toExpression y]

-- | The opposite of 'contains'.
doesNotContain :: Text -> Text -> Bool
doesNotContain x y = not $ contains x y

-- | The XPath @count()@ function.
count :: IsCtx c => Path c -> Number
count p = Number $ Function "count" [toExpression p]

-- | The XPath @and@ operator.
(&&.) :: Bool -> Bool -> Bool
x &&. y = Bool $ Operator "and" (toExpression x) (toExpression y)
infixr 3 &&.

-- | The XPath @or@ operator.
(||.) :: Bool -> Bool -> Bool
x ||. y = Bool $ Operator "or" (toExpression x) (toExpression y)
infixr 2 ||.

-- | The XPath @not(.)@ function.
not :: Bool -> Bool
not x = Bool $ Function "not" [toExpression x]

data Axis = Ancestor |
            Child |
            Descendant |
            DescendantOrSelf |
            Following |
            FollowingSibling |
            Parent |
            Self

showAxis :: Axis -> T.Text
showAxis axis = case axis of
  Ancestor         -> "ancestor"
  Child            -> "child"
  Descendant       -> "descendant"
  DescendantOrSelf -> "descendant-or-self"
  Following        -> "following"
  FollowingSibling -> "following-sibling"
  Parent           -> "parent"
  Self             -> "self"

-- | An XPath node.
newtype Node = Node { unNode :: Expression }

instance IsExpression Node where
  toExpression = unNode

-- | An XPath beginning from some context `c` (either the root context or the current context).
newtype Path c = Path { unPath :: Expression }

-- | Type to indicate the XPath begins from the current context.
data CurrentCtx

-- | Type to indicate the XPath begins from the document root.
data RootCtx

-- | Class of valid types for the type parameter `c` in 'Path'. Library users should not create instances of this class.
class IsCtx c where
  toPathBegin :: proxy c -> PathBegin

instance IsCtx RootCtx where
  toPathBegin _ = FromRootCtx

instance IsCtx CurrentCtx where
  toPathBegin _ = FromCurrentCtx

instance IsCtx c => IsExpression (Path c) where
  toExpression = unPath

-- | The XPath @node()@ function.
node :: Node
node = Node $ Function "node" []

-- | Create a node with the given name.
namedNode :: T.Text -> Node
namedNode = Node . NamedNode

-- | Type to represent the root of the document. Useful in forming an XPaths which must begin from the root.
data DocumentRoot = DocumentRoot

-- | The root of the document.
root :: DocumentRoot
root = DocumentRoot

-- | Type class for the XPath @/@ operator. It can operate on multiple types as the axes can be inferred based on
-- XPath's abbreviated syntax. Library users should not create instances of this class.
class SlashOperator p q r | p q -> r where
  (/.) :: p -> q -> r
  infixl 8 /.

instance IsCtx c => SlashOperator (Path c) (Path CurrentCtx) (Path c) where
  pa /. nextPa = Path $ case toExpression pa of
    PathFrom begin fstPath P.Nothing preds -> PathFrom begin fstPath (P.Just $ toExpression nextPa) preds
    _ -> PathFrom
      (toPathBegin (Proxy :: Proxy c))
      (toExpression $ fromCurrentCtx pa)
      (P.Just $ toExpression nextPa)
      []

instance IsCtx c => SlashOperator (Path c) Node (Path c) where
  pa /. n = pa /. child n

instance SlashOperator Node (Path CurrentCtx) (Path CurrentCtx) where
  n /. pa = child n /. pa

instance SlashOperator Node Node (Path CurrentCtx) where
  n /. nextNode = child n /. child nextNode

instance SlashOperator DocumentRoot (Path CurrentCtx) (Path RootCtx) where
  DocumentRoot /. p = fromRootCtx p

instance SlashOperator DocumentRoot Node (Path RootCtx) where
  DocumentRoot /. n = fromRootCtx (child n)

-- | Type class for the XPath @//@ operator. It can operate on multiple types as the axes can be inferred based on
-- XPath's abbreviated syntax. Library users should not create instances of this class.
class DoubleSlashOperator p q r | p q -> r where
  (//.) :: p -> q -> r
  infixl 8 //.

instance IsCtx c => DoubleSlashOperator (Path c) (Path CurrentCtx) (Path c) where
  pa //. nextPa = Path $ case toExpression pa of
    PathFrom begin fstPath P.Nothing preds -> PathFrom begin fstPath nextPa' preds
    _ -> PathFrom (toPathBegin (Proxy :: Proxy c)) (toExpression $ fromCurrentCtx pa) nextPa' []

    where
      nextPa' = P.Just . toExpression $ descendantOrSelf node /. nextPa

instance IsCtx c => DoubleSlashOperator (Path c) Node (Path c) where
  pa //. n = pa /. descendantOrSelf node /. child n

instance DoubleSlashOperator Node (Path CurrentCtx) (Path CurrentCtx) where
  n //. pa = child n //. pa

instance DoubleSlashOperator Node Node (Path CurrentCtx) where
  n //. nextNode = child n //. child nextNode

instance DoubleSlashOperator DocumentRoot (Path CurrentCtx) (Path RootCtx) where
  DocumentRoot //. p = fromRootCtx (descendantOrSelf node) /. p

instance DoubleSlashOperator DocumentRoot Node (Path RootCtx) where
  DocumentRoot //. n = fromRootCtx (descendantOrSelf node /. n)

locationStep :: Axis -> Node -> Path c
locationStep axis n = Path $ LocationStep axis (toExpression n)

-- | The XPath @ancestor::@ axis.
ancestor :: Node -> Path CurrentCtx
ancestor = locationStep Ancestor

-- | The XPath @child::@ axis.
child :: Node -> Path CurrentCtx
child = locationStep Child

-- | The XPath @descendant::@ axis.
descendant :: Node -> Path CurrentCtx
descendant = locationStep Descendant

-- | The XPath @descendant-or-self::@ axis.
descendantOrSelf :: Node -> Path CurrentCtx
descendantOrSelf = locationStep DescendantOrSelf

-- | The XPath @following::@ axis.
following :: Node -> Path CurrentCtx
following = locationStep Following

-- | The XPath @following-sibling::@ axis.
followingSibling :: Node -> Path CurrentCtx
followingSibling = locationStep FollowingSibling

-- | The XPath @parent::@ axis.
parent :: Node -> Path CurrentCtx
parent = locationStep Parent

-- | The XPath @self::@ axis.
self :: Node -> Path CurrentCtx
self = locationStep Self

changeCtx :: PathBegin -> Path c -> Path c'
changeCtx begin (Path p) = Path $ case p of
  PathFrom _ fstPath sndPath preds -> PathFrom begin fstPath sndPath preds
  LocationStep _ _ -> if begin == FromRootCtx then PathFrom begin p P.Nothing [] else p
  _ -> P.error "HaXPath internal error: unexpected non-path expression"

fromCurrentCtx :: Path c -> Path CurrentCtx
fromCurrentCtx = changeCtx FromCurrentCtx

fromRootCtx :: Path CurrentCtx -> Path RootCtx
fromRootCtx = changeCtx FromRootCtx

-- | The union of two node-sets.
(|.) :: IsCtx c => Path c -> Path c -> Path c
x |. y = Path $ Operator "|" (toExpression x) (toExpression y)
infix 7 |.

-- | Type class to allow either 'Node's or 'Path's. This is useful when only 'Node's are present as the axes can be
-- omitted when using XPath's abbreviated syntax. Library users should not create instances of this class.
class Filterable a where
  (#) :: a -> [Bool] -> a
  infixl 9 #

instance IsCtx c => Filterable (Path c) where
  xp # preds =
    let predExps = toExpression <$> preds in
    Path $ case toExpression xp of
      LocationStep axis (FilteredNode n ps) -> LocationStep axis (FilteredNode n (ps <> predExps))
      LocationStep axis e -> LocationStep axis (FilteredNode e predExps)
      PathFrom begin firstSteps nextSteps ps -> PathFrom begin firstSteps nextSteps (ps <> predExps)
      otherExp -> PathFrom (toPathBegin (Proxy :: Proxy c)) otherExp P.Nothing predExps

instance Filterable Node where
  n # preds =
    let predExps = toExpression <$> preds in
    Node $ case toExpression n of
      FilteredNode nExp ps -> FilteredNode nExp (ps <> predExps)
      otherExp -> FilteredNode otherExp predExps
