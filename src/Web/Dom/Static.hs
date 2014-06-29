{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}

-- | A Static DOM is just a tree of nodes.
module Web.Dom.Static (

    Element (Element), tag, attrs
  , NodeF (El, Tx), Node (..), children, element, text
  , module Ty


  ) where

import           Control.Applicative
import qualified Data.Foldable       as F
import           Data.HashMap.Strict (HashMap)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Data.Text           (Text)
import qualified Data.Traversable    as T
import qualified Web.Dom.Types       as Ty

data Element =
  Element { _tag   :: {-# UNPACK #-} !Text
          , _attrs :: !(HashMap Ty.Attr Text)
          }
  deriving ( Eq, Show )

tag :: Functor f => (Text -> f Text) -> Element -> f Element
tag inj (Element t a) = (\t' -> Element t' a) <$> inj t
{-# INLINE tag #-}

attrs :: Functor f
         => (HashMap Ty.Attr Text -> f (HashMap Ty.Attr Text))
         -> Element -> f Element
attrs inj (Element t a) = (\a' -> Element t a') <$> inj a
{-# INLINE attrs #-}

data NodeF x
  = El !Element (Seq x)
  | Tx {-# UNPACK #-} !Text
  deriving ( Eq, Show, Functor, F.Foldable, T.Traversable )

newtype Node =
  Node { unwrapNode :: NodeF Node }
  deriving ( Eq, Show)

-- | Affine traversal
children :: Applicative f => (Seq Node -> f (Seq Node)) -> Node -> f Node
children inj x = case unwrapNode x of
  El e cs -> Node . El e <$> inj cs
  _       -> pure x
{-# INLINE children #-}

-- | Affine traversal
element :: Applicative f => (Element -> f Element) -> Node -> f Node
element inj x = case unwrapNode x of
  El e cs -> Node . flip El cs <$> inj e
  _       -> pure x
{-# INLINE element #-}

text :: Applicative f => (Text -> f Text) -> Node -> f Node
text inj x = case unwrapNode x of
  Tx t -> Node . Tx <$> inj t
  _    -> pure x
{-# INLINE text #-}
