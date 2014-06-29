{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Virtual implementations of the 'Dom' classes. Faster, more pure.
module Web.Dom.Virtual where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.ST
import           Data.Either          (isRight)
import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as IM
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence        (Seq, (|>), (<|), ViewL (..))
import qualified Data.Sequence        as Seq
import           Data.STRef.Strict
import           Data.Text            (Text)
import           Data.Void
import           Web.Dom
import qualified Web.Dom.Static       as Static
import           Web.Dom.Types

-- | The \"virtual\" DOM. Since the Dom interface provides access to,
-- essentially, mutable references in the form of elements it's
-- necessary for this virtual, pure representation to use an
-- 'ST'-style thread-local phantom variable.
--
-- Notably, this threaded notion is pretty much inescapable just given
-- the nature of the 'DomNode', 'DomEl', and 'DomView' APIs. They're
-- fundamentally imperative.
newtype Virt s a =
  Virt {
    unVirt :: ReaderT (VNode s El) (ST s) a
  } deriving ( Functor, Applicative, Monad, MonadFix )

mkTop :: ST s (VNode s El)
mkTop = mfix go where
  go top = do
    VN <$> newSTRef (vnode top)
  vnode top =
    VirtNode {
      here = Static.El (Static.Element "" mempty) mempty,
      up   = top
      }

runVirt :: (forall s . Virt s a) -> a
runVirt vs = runST $ do
  top <- mkTop
  runReaderT (unVirt vs) top

data VirtNode s =
  VirtNode { here :: Static.NodeF (AVNode s)
           , up   :: VNode s El
           } deriving ( Eq )

type  VNode s t =  Node (Virt s) t
type AVNode s   = ANode (Virt s)

liftST :: ST s a -> Virt s a
liftST = Virt . lift

vask :: Virt s (VNode s 'El)
vask = Virt ask

instance DomNode (Virt s) where
  newtype Node (Virt s) ty = VN { theNode :: STRef s (VirtNode s) }
    deriving ( Eq )

  createElement  tg = freshEl (Static.Element tg mempty)
  createTextNode    = freshTx
  childNodes        = fmap snd . findEl
  nodeValue         = findTx
  isEqualNode a b   = return (a == b)

  parentElement n = do
    top <- vask
    VirtNode { up = it }  <- liftST (readSTRef (theNode n))
    return $ if it == top then Nothing else Just it

  cloneNode n = do
    vn  <- getsVN id n
    top <- vask
    let new = VirtNode { up   = top
                       , here = case here vn of
                           it@Static.Tx {} -> it
                           Static.El e _   -> Static.El e mempty
                       }
    VN <$> liftST (newSTRef new)

  appendChild parent child = do
    x <- cycles child parent
    when (isRight x) (link (|>) parent (forgetNode child))
    return x

  insertBefore parent ref child = do
    x <- cycles child parent
    when (isRight x) $
      link (addBefore (forgetNode ref))
           parent
           (forgetNode child)
    return x
    where
      addBefore :: AVNode s -> Seq (AVNode s) -> AVNode s -> Seq (AVNode s)
      addBefore ref all new =
        let (l, c, r) = smashl (== ref) all
        in case c of
          Nothing -> l |> new
          Just x  -> l <> (new <| x <| r)

  insertAfter parent ref child = do
    x <- cycles child parent
    when (isRight x) $
      link (addAfter (forgetNode ref))
           parent
           (forgetNode child)
    return x
    where
      addAfter :: AVNode s -> Seq (AVNode s) -> AVNode s -> Seq (AVNode s)
      addAfter ref all new =
        let (l, c, r) = smashl (== ref) all
        in case c of
          Nothing -> l |> new
          Just x  -> l <> (x <| new <| r)

  getElementsByTagName = undefined

  deepCloneNode n = do
    vn  <- getsVN id n
    vn' <- case vn of
      Static.Tx {}   -> return vn
      Static.El e cs -> do
        Static.El e <$> mapM (anyNode $ liftM forgetNode . deepCloneNode) cs
    ref <- liftST (newSTRef vn')
    return (VN ref)
   
  removeChild          = undefined
  replaceChild         = undefined

smashl :: (a -> Bool) -> Seq a -> (Seq a, Maybe a, Seq a)
smashl at seq =
  let (l, r) = Seq.breakl at seq
  in case Seq.viewl r of
    EmptyL -> (l, Nothing, r)
    a :< s -> (l, Just a, s)

cycles :: (INodeType a, DomNode m) => Node m a -> Node m 'El -> m (Either String ())
cycles parent child = do
  case forgetNode child of
    Right tx -> return (Right ())
    Left  el -> do
      x <- contains child parent
      if x then return (Left "Cycle detected") else return (Right ())


modifyVN :: (VirtNode s -> VirtNode s) -> VNode s ty -> Virt s ()
modifyVN f n = liftST (modifySTRef (theNode n) f)

getsVN :: (VirtNode s -> r) -> VNode s ty -> Virt s r
getsVN f = liftM f . liftST . readSTRef . theNode

link :: (Seq (AVNode s) -> ANode (Virt s) -> Seq (AVNode s)) ->
        VNode s El -> AVNode s -> Virt s ()
link f newParent = anyNode $ \child -> do

  -- We need to:
  --
  -- 1. Set the child's parent-link to the new parent
  -- 2. Remove the child from the old parent's child-list
  -- 3. Merge the child into the new parent's childlist

  -- (1)
  modifyVN (\vn -> vn { up = newParent }) child

  -- (2)
  oldParent <- getsVN up child
  modifyVN (\vn -> vn { here = elim child (here vn) }) oldParent

  -- (3)
  modifyVN (\vn ->
             vn { here = let Static.El e cs = here vn
                         in Static.El e (f cs (forgetNode child)) })
           newParent

  where
    elim :: INodeType a =>
            VNode s a ->
            Static.NodeF (AVNode s) ->
            Static.NodeF (AVNode s)
    elim c (Static.El e cs) = Static.El e (Seq.filter (/= forgetNode c) cs)


fresh :: Static.NodeF Void -> Virt s (AVNode s)
fresh n = do
  top <- vask
  z <- liftST . newSTRef $
    VirtNode { here = fmap absurd n
             , up   = top
             }
  return $ case n of
    Static.El {} -> Left  (VN z)
    Static.Tx {} -> Right (VN z)

freshEl :: Static.Element -> Virt s (VNode s El)
freshEl e = fromLeft <$> fresh (Static.El e mempty) where
  fromLeft (Left a) = a

freshTx :: Text -> Virt s (VNode s Tx)
freshTx t = fromRight <$> fresh (Static.Tx t) where
  fromRight (Right a) = a

findNode :: VNode s ty -> Virt s (Static.NodeF (AVNode s))
findNode n = do
  vn <- (liftST . readSTRef . theNode) n
  return (here vn)

findEl :: VNode s El -> Virt s (Static.Element, Seq (AVNode s))
findEl n = do
  vn <- findNode n
  case vn of
    Static.Tx {}   -> error "Virtual text node claiming to be an element"
    Static.El e cs -> return (e, cs)

findTx :: VNode s Tx -> Virt s Text
findTx n = do
  vn <- findNode n
  case vn of
    Static.El {}  -> error "Virtual element node claiming to be a text node"
    Static.Tx t   -> return t
