{-# LANGUAGE RankNTypes #-}

-- | A Static DOM is just a tree of nodes.
module Web.Dom.Static (

    Element (Element), tag, attrs
  , Node (El, Tx), children, element, text
  , module Web.Dom.Types
  
  
  ) where

import           Control.Applicative
import           Data.HashMap.Strict  (HashMap)
import           Data.Text            (Text)
import           Web.Dom.Types

data Element =
  Element { _tag   :: {-# UNPACK #-} !Text
          , _attrs :: !(HashMap Attr Text)
          }
  deriving ( Eq, Show )

tag :: Functor f => (Text -> f Text) -> Element -> f Element
tag inj (Element t a) = (\t' -> Element t' a) <$> inj t
{-# INLINE tag #-}

attrs :: Functor f
         => (HashMap Attr Text -> f (HashMap Attr Text))
         -> Element -> f Element
attrs inj (Element t a) = (\a' -> Element t a') <$> inj a
{-# INLINE attrs #-}

data Node
  = El !Element [Node]
  | Tx {-# UNPACK #-} !Text
  deriving ( Eq, Show )

-- | Affine traversal
children :: Applicative f => ([Node] -> f [Node]) -> Node -> f Node
children inj x = case x of
  El e cs -> El e <$> inj cs
  _       -> pure x
{-# INLINE children #-}

-- | Affine traversal
element :: Applicative f => (Element -> f Element) -> Node -> f Node
element inj x = case x of
  El e cs -> flip El cs <$> inj e
  _       -> pure x
{-# INLINE element #-}

text :: Applicative f => (Text -> f Text) -> Node -> f Node
text inj x = case x of
  Tx t -> Tx <$> inj t
  _    -> pure x
{-# INLINE text #-}
