{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | A Static DOM is just a tree of nodes.
module Web.Dom.Static (

    Element (Element), tag, attrs
  , NodeF (El, Tx), Node (..), children, element, text
  , module Ty


  ) where

import           Control.Applicative
import qualified Data.Foldable           as F
import           Data.Hashable
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import           Data.Sequence           (Seq)
import qualified Data.Sequence           as Seq
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Traversable        as T
import qualified Web.Dom.Static.NameSpec as NS
import qualified Web.Dom.Types           as Ty

data Element =
  Element { _tag   :: {-# UNPACK #-} !Text
          , _attrs :: !(HashMap Ty.Attr Text)
          }
  deriving ( Eq )

instance Show Element where
  show (Element tg attrs) =
    "<" ++ tagName ++ mkAttrs (HM.toList attrs) ++ ">" ++ "</" ++ tagName ++ ">"
    where
      tagName = Text.unpack tg
      mkAttrs :: [(Text, Text)] -> String
      mkAttrs [] = ""
      mkAttrs xs = " " ++ mkAttrs' xs
      mkAttrs' ats =
        unwords (map (\(k, v) -> Text.unpack k ++ "=\"" ++ Text.unpack v ++ "\"") ats)

specElement :: NS.ElSpec -> Element
specElement es =
  Element (fromMaybe "div" (NS.tagName es))
          (fixId . fixCls $ NS.attrs es)
  where
    fixId = case NS.idName es of
      Nothing -> id
      Just i  -> HM.insert "id" i
    fixCls = case NS.classes es of
      [] -> id
      cs -> HM.insert "class" (Text.unwords cs)

instance IsString Element where
  fromString s = case specElement <$> NS.parseElSpec (fromString s) of
    Left err ->
      error $ "Could not parse element spec, " ++ show s ++ ": " ++ err
    Right e  -> e

tag :: Functor f => (Text -> f Text) -> Element -> f Element
tag inj (Element t a) = (\t' -> Element t' a) <$> inj t
{-# INLINE tag #-}

attrs :: Functor f
         => (HashMap Ty.Attr Text -> f (HashMap Ty.Attr Text))
         -> Element -> f Element
attrs inj (Element t a) = (\a' -> Element t a') <$> inj a
{-# INLINE attrs #-}

attr :: Functor f
     => Ty.Attr
     -> (Maybe Text -> f (Maybe Text))
     -> Element -> f Element
attr a = attrs . at a where
  at :: (Functor f, Hashable k, Eq k)
     => k
     -> (Maybe v -> f (Maybe v))
     -> HashMap k v -> f (HashMap k v)
  at k inj m =
    let go Nothing  = HM.delete k m
        go (Just v) = HM.insert k v m
    in go <$> inj (HM.lookup k m)
  {-# INLINE at #-}
{-# INLINE attr #-}

infixr 4 #
(#) :: Element -> Text -> Element
e # i = e { _attrs = fx (_attrs e) } where
  fx m = HM.insert "id" i m
{-# INLINE (#) #-}

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
  Node :: NodeF ty Node -> Node

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
