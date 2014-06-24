{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Virtual implementations of the 'Dom' classes. Faster, more pure.
module Web.Dom.Virtual where

import           Control.Applicative
import           Control.Monad.State
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import           Data.Void
import           Web.Dom
import qualified Web.Dom.Static      as Static
import           Web.Dom.Types

data St
  = St { seed  :: Int
       , nodes :: IntMap (Static.NodeF (ANode Virt))
       }
    deriving ( Eq, Show )

newtype Virt a = Virt { unVirt :: State St a }
  deriving ( Functor, Applicative, Monad, MonadState St )

fresh :: Static.NodeF Void -> Virt (ANode Virt)
fresh n = do
  i <- gets seed
  modify $ \st ->
    st { seed = i + 1
       , nodes = IM.insert i (fmap absurd n) (nodes st)
       }
  return $ case n of
    Static.El e _ -> Left  (VN i)
    Static.Tx t   -> Right (VN i)

freshEl :: Static.Element -> Virt (Node Virt El)
freshEl e = fromLeft <$> fresh (Static.El e []) where
  fromLeft (Left a) = a

freshTx :: Text -> Virt (Node Virt Tx)
freshTx t = fromRight <$> fresh (Static.Tx t) where
  fromRight (Right a) = a

findNode :: Node Virt ty -> Virt (Static.NodeF (ANode Virt))
findNode (VN i) = fix . IM.lookup i <$> gets nodes where
  fix Nothing  = error $ "Accessed non-existent virtual node: " ++ show i
  fix (Just a) = a

findEl :: Node Virt El -> Virt (Static.Element, [ANode Virt])
findEl vn = do
  n <- findNode vn
  case n of
    Static.Tx{}    -> error "Virtual text node stored at element address"
    Static.El e cs -> return (e, cs)

findTx :: Node Virt Tx -> Virt Text
findTx vn = do
  n <- findNode vn
  case n of
    Static.Tx t -> return t
    Static.El{} -> error "Virtual element stored at text node address"

forceId :: ANode Virt -> Int
forceId (Left  (VN i)) = i
forceId (Right (VN i)) = i

instance DomNode Virt where
  newtype Node Virt ty = VN { virtId :: Int }
    deriving ( Eq, Show )

  createElement  tg = freshEl (Static.Element tg mempty)
  createTextNode    = freshTx
  childNodes        = fmap snd . findEl
  nodeValue         = findTx

  -- breadth-first search
  contains parent node = do
    (_, cs) <- findEl parent
    if elem (virtId node) (map forceId cs)
      then return True
      else anyM (flip contains node) (mapMaybe goLeft cs)

    where
      goLeft (Left a) = Just a
      goLeft _        = Nothing

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f []     = return False
anyM f (x:xs) = do
  r <- f x
  if r then return True else anyM f xs
