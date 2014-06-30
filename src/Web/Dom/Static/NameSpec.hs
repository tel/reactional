{-# LANGUAGE OverloadedStrings #-}

{-

What is a name spec? It's emmet, it's CSS
-----------------------------------------

The most basic name spec is just a plain string containing only
letters, capital or lowercase, and underscores. These are the
allowable characters of an HTML tag name and that's exactly what such
a fragment translates as.

    div    ==>   <div></div>
    span   ==>   <span></span>

Perhaps unexpectedly, there is no limitation to the kinds of tags
used.

    spoon  ==>   <spoon></spoon>

The next most common name spec is a div with a specified id. This is
created using CSS selector format.

    ul#nav ==>   <ul id=\"nav\"></ul>

and if the element type is left out then it's assumed to be \"div\"

    #wrap  ==>   <div id=\"wrap\"></div>

As an edge case, the last ID specified \"wins\"

    #a#b   ==>   <div id=\"b\"></div>

The third special syntax is the class specifier, again using CSS
selector format.

    .foo   ==>   <div class=\"foo\"></div>
    .a.b   ==>   <div class=\"a b\"></div>

Multiple classes may be specified at once, each using their own
period-delimited class specification.

Finally, additional attributes may be specified inline.

    div#bar.a.b{name=sam, class=b z}

    ==>

    <div id=\"bar\" class=\"a b\" name=\"sam\"></div>

Notably, the inline specified attributes will not override the id and
class attribtues specified using the \"#\" and \".\" syntaxes.

-}

module Web.Dom.Static.NameSpec ( ElSpec (..), parseElSpec ) where

import           Control.Applicative
import           Control.Arrow        (first, (+++))
import qualified Data.Attoparsec.Text as At
import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T

data ElSpec =
  ElSpec { tagName :: Maybe Text
         , idName  :: Maybe Text
         , classes :: [Text]
         , attrs   :: HashMap Text Text
         }
  deriving Show

pName :: At.Parser Text
pName = At.takeWhile1 (\c -> isAlpha c || (c == '_'))

pId :: At.Parser Text
pId = At.char '#' *> pName

pCls :: At.Parser Text
pCls = At.char '.' *> pName

pBraces :: At.Parser a -> At.Parser a
pBraces p = At.char '{' *> p <* At.char '}'

pCommaSpace :: At.Parser ()
pCommaSpace = () <$ At.char ',' <* At.takeWhile isSpace

pAttr :: At.Parser (Text, Text)
pAttr = (,) <$> pName <* At.char '=' <*> At.takeWhile (\c -> c /= ',' && c /= '}')

pSpec :: At.Parser ElSpec
pSpec =
  mk <$> optional pName
     <*> (condense <$> many (Left <$> pId <|> Right <$> pCls))
     <*> (mkAttrs <$> At.option mempty (pBraces (At.sepBy pAttr pCommaSpace)))
  where
    mk :: Maybe Text -> (Maybe Text, [Text]) -> HashMap Text Text -> ElSpec
    mk tg (ids, cls) hm = ElSpec tg ids cls hm
    mkAttrs :: [(Text, Text)] -> HashMap Text Text
    mkAttrs = HM.fromList
    condense :: [Either Text Text] -> (Maybe Text, [Text])
    condense = first getLast . accumEither . map (Last . Just +++ pure)

accumEither :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
accumEither = foldr ae (mempty, mempty) where
  ae (Left a)  (l, r) = (a <> l, r)
  ae (Right a) (l, r) = (l, a <> r)

parseElSpec :: Text -> Either String ElSpec
parseElSpec = At.parseOnly pSpec
