{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A dissection is an ST thread based on a value of a recursive
-- type. Each layer of the ADT can be "cut out" producing a mutable
-- reference to that point in the original value. Given that reference
-- we can traverse up or down the type.

module Web.Dom.Dissect where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State.Strict
import qualified Data.Foldable              as F
import           Data.Functor.Foldable      as Rs
import           Data.Monoid
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.STRef
import qualified Data.Traversable           as T
import Data.Traversable (Traversable)

--------------------------------------------------------------------------------

type Dist f g = forall x . f (g x) -> g (f x)

-- | A very general monadic unfold.
mana :: (Unfoldable t, Monad m)
     => Dist (Base t) m
     -> (a -> m (Base t a))
     -> (a -> m t)
mana dist inj = rec where rec = inj >=> liftM embed . dist . fmap rec

manaT :: (Unfoldable t, Traversable (Base t), Monad m)
      => (a -> m (Base t a))
      -> (a -> m t)
manaT = mana T.sequence

--------------------------------------------------------------------------------

newtype S w m a = S { unS :: Seq w -> m (a, Seq w) } deriving Functor

instance (Functor m, Monad m) => Applicative (S w m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (S w m) where
  return a = S (\ws -> return (a, ws))
  S go >>= f = S $ \ws -> do
    (a, ws') <- go ws
    unS (f a) ws'

instance MonadTrans (S w) where
  lift m = S (\ws -> (liftM (flip (,) ws) m))

runS :: S w m a -> m (a, Seq w)
runS (S go) = go mempty

writel :: Monad m => w -> S w m ()
writel w = S (\ws -> return ((), w <| ws))

writer :: Monad m => w -> S w m ()
writer w = S (\ws -> return ((), ws |> w))

--------------------------------------------------------------------------------

data Pop s f a
  = Pass (f a)
  | Pop  (STRef s (f a))

fmapST :: Functor f => (a -> b) -> Pop s f a -> ST s (Pop s f b)
fmapST f (Pass fa) = return (Pass (fmap f fa))
fmapST f (Pop ref) = do
  x    <- readSTRef ref
  ref' <- newSTRef (fmap f x)
  return (Pop ref')

pass :: f (Fix (Pop s f)) -> Fix (Pop s f)
pass = Fix . Pass

stop :: Fix (Pop s f) -> ST s (f (Fix (Pop s f)))
stop (Fix x) = case x of
  Pass fa -> return fa
  Pop  r  -> readSTRef r

slap :: Foldable t => t -> Fix (Pop s (Base t))
slap = cata pass

build :: (Unfoldable t, Traversable (Base t))
      => Fix (Pop s (Base t)) -> ST s t
build = manaT stop

-- stop . pass = return
-- liftM pass . stop

stamp' :: STRef s (f a) -> ST s (STRef s (Pop s1 f a))
stamp' r = do
  x <- readSTRef r
  newSTRef (Pass x)

one'
  :: Fix (Pop s1 f)
  -> S (STRef s1 (Pop s1 f (Fix (Pop s1 f))))
       (ST s1)
       (Pop s1 f (Fix (Pop s1 f)))
one' (Fix d) = do
  r <- lift $ case d of
    Pop sr -> stamp' sr
    Pass b -> newSTRef d
  writel r
  return d

z :: (Unfoldable t, Traversable (Pop s1 f), Base t ~ Pop s1 f)
  => Fix (Pop s1 f)
  -> S (STRef s1 (Pop s1 f (Fix (Pop s1 f))))
       (ST s1)
       t
z = manaT one'

--------------------------------------------------------------------------------

-- The type 'Down' is very similar to 'Fix'. 

data Down s t
  = Down (Base t (Down s t))
  | Out  (STRef s (Base t (Down s t)))
makePrisms ''Down

up :: Down s t -> ST s (Base t (Down s t))
up x = case x of
  Down s -> return s
  Out  r -> readSTRef r

-- up . Down == return
-- liftM Down . up == return

slice :: Foldable t => t -> Down s t
slice = cata Down

reassemble :: (Traversable (Base t), Unfoldable t) => Down s t -> ST s t
reassemble = manaT up

-- liftM slice . reassemble == return
-- reassemble . slice       == return

--------------------------------------------------------------------------------

type Ref s t = STRef s (Down s t)

newtype Dissect s t a =
  Dissect { unDissect :: StateT (Ref s t) (ST s) a }
  deriving ( Functor, Applicative, Monad )

dissect :: Foldable t => (forall s . Dissect s t a) -> t -> a
dissect d t = runST $ do
  ref    <- newSTRef (slice t)
  (a, _) <- runStateT (unDissect d) ref
  return a

--------------------------------------------------------------------------------

type PredF f = forall a . f a -> Bool

-- cut :: PredF (Base t) -> Dissect s t (Seq (Ref s t))
-- cut p = Dissect (StateT (liftM swap . runS . comp)) where
--   swap :: (a, b) -> (b, a)
--   swap (a, b) = (b, a)
--   -- comp :: Ref s t -> S (Ref s t) (ST s) (Ref s t)
--   comp r = do
--     down <- lift (readSTRef r)
--     case down of
--       Down base
--         | p base    -> _
--         | otherwise -> _
--       Out ref -> _

stamp :: STRef s (Base t (Down s t)) -> ST s (Ref s t)
stamp r = do
  x <- readSTRef r
  newSTRef (Down x)

one :: Down s t -> S (Ref s t) (ST s) (Down s t)
one d = do
  r <- lift $ case d of
    Out sr -> stamp sr
    Down b -> newSTRef d
  writel r
  return d

-- iter :: (Down s (m a) -> m a) -> Down s t -> m a
-- iter phi d = case d of
  

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
