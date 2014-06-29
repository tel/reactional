{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.Dom.V3 where

import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Monoid
import           Data.STRef
import qualified Data.Traversable           as T
import           Web.Dom
import qualified Web.Dom.Static             as St
import           Web.Dom.Types

newtype Virt s a =
  Virt { unVirt :: StateT [ANode (Virt s)] (ST s) a }
  deriving ( Functor, Applicative, Monad )

readHere :: Node (Virt s) ty -> Virt s (St.NodeF ty (ANode (Virt s)))
readHere = liftST . readSTRef . here

liftST :: ST s a -> Virt s a
liftST = Virt . lift

newNode :: St.NodeF ty (ANode (Virt s)) -> ST s (Node (Virt s) ty)
newNode e = N <$> newSTRef e <*> newSTRef Nothing

instance DomNode (Virt s) where
  data Node (Virt s) ty =
    N { here   :: STRef s (St.NodeF ty (ANode (Virt s)))
      , parent :: STRef s (Maybe (Node (Virt s) El))
      }

  childNodes n = do
    St.El _ cs <- readHere n
    return cs

  nodeValue n = do
    St.Tx t <- readHere n
    return t

  parentElement n   = liftST (readSTRef (parent n))
  createTextNode t  = liftST (newNode (St.Tx t))
  createElement tag = liftST (newNode (St.El (St.Element tag mempty) mempty))

  cloneNode n = do
    l <- readHere n
    liftST . newNode $ case l of
      St.El e cs -> St.El e mempty
      St.Tx t    -> St.Tx t

  deepCloneNode = liftST . go where
    go :: Node (Virt s) a -> ST s (Node (Virt s) a)
    go n = do
      l <- readSTRef (here n)
      case l of
        St.Tx t    ->                   newNode (St.Tx t)
        St.El e cs -> T.mapM goA cs >>= newNode . St.El e
    goA :: ANode (Virt s) -> ST s (ANode (Virt s))
    goA x = case x of
      Left  n -> Left  <$> go n
      Right n -> Right <$> go n

  isEqualNode n1 n2 = return (n1 == n2)

deriving instance Eq (Node (Virt s) ty)
