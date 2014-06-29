{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Web.Dom.V2 where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Foldable              (Foldable)
import qualified Data.Sequence              as Seq
import           Data.STRef
import           Data.Traversable           (Traversable, traverse)
import qualified Data.Traversable           as T
import           Web.Dom
import qualified Web.Dom.Static             as St

newtype Layer s f =
  Layer (f (Obj s f))


deriving instance Eq (f (Obj s f)) => Eq (Layer s f)

data Obj s f =
  Obj { _up   :: STRef s (Layer s f)
      , _here :: STRef s (Layer s f)
      } deriving ( Eq )

makeIso    ''Layer
makeLenses ''Obj

-- > compress = anaM resolve
compress :: Obj s St.NodeF -> ST s St.Node
compress = a where
  a = liftM St.Node . traverse a <=< resolve

  resolve :: Obj s f -> ST s (f (Obj s f))
  resolve x = review layer <$> readSTRef (view here x)

-- This is kind of like a catamorphism, but it has a weird recursive
-- trick which builds the "up" references
--
-- > expand = cata Fix
expand :: St.Node -> ST s (Obj s St.NodeF)
expand = c Nothing where
  c m t = mdo
    let par = case m of
          Nothing -> ref
          Just r  -> r
    x   <- traverse (c (Just ref)) (St.unwrapNode t)
    ref <- newSTRef (Layer x)
    return (Obj par ref)

downUp :: Obj s St.NodeF -> ST s Bool
downUp o = do
  l@(Layer (St.El e cs)) <- readSTRef (view here o)
  let c Seq.:< _ = Seq.viewl cs
  l' <- readSTRef (view up c)
  return (l == l')

-- > runST $ do
--     obj <- expand $
--       St.Node (St.El (St.Element "foo" mempty)
--                      (Seq.fromList [St.Node (St.El (St.Element "foo" mempty)
--                                                    mempty)]))
--     downUp obj
-- True

-- compress . expand = return
-- liftM expand . compress = return

--------------------------------------------------------------------------------

data Vst s =
  Vst { _root   :: Obj s St.NodeF
      , _garden :: [Obj s St.NodeF]
      }
makeLenses ''Vst

newtype Virtual s a =
  Virtual { unVirtual :: StateT (Vst s) (ST s) a }
  deriving ( Functor, Applicative, Monad )

runVirtual :: (forall s . Virtual s a) -> St.Node -> a
runVirtual vs n = runST $ do
  rt <- expand n
  let vst = Vst rt []
  evalStateT (unVirtual vs) vst

snapshot :: Virtual s (St.Node, [St.Node])
snapshot = do
  Vst { _root = rt, _garden = gd } <- liftSt get
  rt_ <- liftST (compress rt)
  gd_ <- liftST (mapM compress gd)
  return (rt_, gd_)

liftST :: ST s a -> Virtual s a
liftST = Virtual . lift

liftSt :: StateT (Vst s) (ST s) a -> Virtual s a
liftSt = Virtual

--------------------------------------------------------------------------------

instance DomNode (Virtual s) where
  newtype Node (Virtual s) ty = VN (Obj s St.NodeF)

  -- childNodes vn = do
  --   St.El e cs <- liftST $ vn ^. vNode . here ^!. stref . from layer
  --   return (fmap _ cs)

  appendChild          = undefined

  cloneNode            = undefined
  createElement        = undefined
  createTextNode       = undefined
  deepCloneNode        = undefined
  getElementsByTagName = undefined
  insertBefore         = undefined
  isEqualNode          = undefined
  nodeValue            = undefined
  parentElement        = undefined
  removeChild          = undefined
  replaceChild         = undefined

vNode :: Iso' (Node (Virtual s) ty) (Obj s St.NodeF)
vNode = iso (\(VN i) -> i) VN

--------------------------------------------------------------------------------

type MLens m s a = forall f . (Traversable f) => (a -> f a) -> (s -> m (f ()))

mlens :: Monad m => (s -> a -> m ()) -> (s -> m a) -> MLens m s a
mlens set get inj s = get s >>= T.mapM (set s) . inj
{-# INLINE mlens #-}

infixl 8 ^!.
(^!.) :: Monad m => s -> MLens m s a -> m a
(^!.) = flip mview
{-# INLINE (^!.) #-}

infixr 4 %!~
(%!~) :: Monad m => MLens m s a -> (a -> a)  -> s ->  m ()
(%!~) = mover
{-# INLINE (%!~) #-}

infixr 4 .!~
(.!~) :: Monad m => MLens m s a -> a -> s -> m ()
(.!~) = mset
{-# INLINE (.!~) #-}

mview :: Monad m => MLens m s a -> s -> m a
mview l = liftM getConst . l Const
{-# INLINE mview #-}

mover :: Monad m => MLens m s a -> (a -> a)  -> s ->  m ()
mover l f = liftM runIdentity . l (Identity . f)
{-# INLINE mover #-}

mset :: Monad m => MLens m s a -> a -> s -> m ()
mset l = mover l . const
{-# INLINE mset #-}

stref :: MLens (ST s) (STRef s a) a
stref = mlens writeSTRef readSTRef
{-# INLINE stref #-}

--------------------------------------------------------------------------------
