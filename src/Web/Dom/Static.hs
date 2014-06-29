{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | A Static DOM is just a tree of nodes.
module Web.Dom.Static (

    Element (Element), tag, attrs
  , NodeF (El, Tx), Node (..), children, element, text
  , module Ty


  ) where

import           Control.Applicative
import qualified Data.Foldable       as F
import           Data.HashMap.Strict (HashMap)
import           Data.Monoid
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

data NodeF ty x where
  El :: Element -> Seq x -> NodeF Ty.El x
  Tx :: Text    -> NodeF Ty.Tx x

evalNodeF :: (Element -> Seq x -> r)
          -> (Text             -> r)
          -> NodeF ty x
          -> r
evalNodeF f g x = case x of
  El e s -> f e s
  Tx t   -> g t
{-# INLINE evalNodeF #-}

deriving instance Eq x   => Eq   (NodeF ty x)
deriving instance Show x => Show (NodeF ty x)

instance Functor (NodeF ty) where
  fmap f x = case x of
    El e cs -> El e (fmap f cs)
    Tx t    -> Tx t

instance F.Foldable (NodeF ty) where
  foldMap f x = case x of
    El e cs -> F.foldMap f cs
    Tx t    -> mempty

instance T.Traversable (NodeF ty) where
  traverse inj x = case x of
    El e cs -> El e <$> T.traverse inj cs
    Tx t    -> pure (Tx t)

data Node where
  Node :: { unwrapNode :: NodeF ty Node } -> Node

instance Eq Node where
  Node nf1 == Node nf2 = case (nf1, nf2) of
    (El e1 s1, El e2 s2) -> e1 == e2 && s1 == s2
    (Tx t1, Tx t2)       -> t1 == t2
    _                    -> False
  
deriving instance Show Node

evalNode :: (Element -> Seq Node -> r)
         -> (Text                -> r)
         -> Node
         -> r
evalNode f g (Node x) = case x of
  El e s -> f e s
  Tx t   -> g t
{-# INLINE evalNode #-}

-- | Affine traversal
children :: Applicative f => (Seq Node -> f (Seq Node)) -> Node -> f Node
children inj x =
  evalNode (\e cs -> Node . El e <$> inj cs)
           (\_ -> pure x)
           x
{-# INLINE children #-}

-- | Affine traversal
element :: Applicative f => (Element -> f Element) -> Node -> f Node
element inj x =
  evalNode (\e cs -> Node . flip El cs <$> inj e)
           (\_    -> pure x)
           x
{-# INLINE element #-}

text :: Applicative f => (Text -> f Text) -> Node -> f Node
text inj x =
  evalNode (\_ _ -> pure x)
           (\t   -> Node . Tx <$> inj t)
           x
{-# INLINE text #-}
