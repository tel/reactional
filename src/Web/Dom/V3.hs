{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Web.Dom.V3 (
  Virt
  ) where

import           Control.Applicative
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Monoid
import           Data.Sequence              (Seq, ViewL (..), ViewR (..), (<|),
                                             (|>))
import qualified Data.Sequence              as Seq
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

liftSt :: StateT [ANode (Virt s)] (ST s) a -> Virt s a
liftSt = Virt

newNode :: St.NodeF ty (ANode (Virt s)) -> ST s (Node (Virt s) ty)
newNode e = N <$> newSTRef e <*> newSTRef Nothing

modE :: (St.Element -> Seq (ANode (Virt s)) -> (St.Element, Seq (ANode (Virt s))))
     -> Node (Virt s) El -> Virt s ()
modE f n = do
  x  <- readHere n
  let x' = case x of
        St.El e cs -> uncurry St.El (f e cs)
  liftST (writeSTRef (here n) x')

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

  -- always removes child first
  appendChild p c = do
    ok <- case forgetNode c of
      Left ce -> contains ce p
      Right t -> return True
    case ok of
      False -> return (Left hierarchyErrorMessage)
      True  -> do
        x <- removeChild p c
        case x of
          Left err -> return x
          Right ()  -> do
            liftST (writeSTRef (parent c) (Just p))
            modE (\e cs -> (e, cs |> forgetNode c)) p
            return (Right ())
    where
     hierarchyErrorMessage = "HierarchyRequestError: Failed to execute 'appendChild' on 'Node': The new child element contains the parent."

  insertBefore p r c = do
    ok <- case forgetNode c of
      Left ce -> contains ce p
      Right t -> return True
    case ok of
      False -> return (Left hierarchyErrorMessage)
      True  -> do
        x <- removeChild p c
        case x of
          Left err -> return x
          Right ()  -> do
            liftST (writeSTRef (parent c) (Just p))
            modE (\e cs -> (e, insertBeforeS cs (forgetNode r) (forgetNode c))) p
            return (Right ())
    where
     hierarchyErrorMessage = "HierarchyRequestError: Failed to execute 'appendChild' on 'Node': The new child element contains the parent."
     insertBeforeS :: Seq (ANode (Virt s))
                   -> ANode (Virt s)
                   -> ANode (Virt s)
                   -> Seq (ANode (Virt s))
     insertBeforeS s r i = case smashl (== r) s of
       (l, Nothing, _) -> l |> i
       (l, Just h,  r) -> l <> (i <| h <| r)

  removeChild p c = do
    x <- contains p c
    case x of
      False -> return (Left notFoundErrorMessage)
      True -> do
        modE (\e cs -> (e, Seq.filter (anyNode (not . nodeEq c)) cs)) p
        liftST (writeSTRef (parent c) Nothing)
        return (Right ())
    where
      notFoundErrorMessage =
        "NotFoundError: Failed to execute 'removeChild' on 'Node': The node to be removed is not a child of this node."

  getElementsByTagName (Just top) tg = lookupTag top where
    lookupTag :: Node (Virt s) 'El -> Virt s (Seq (Node (Virt s) 'El))
    lookupTag n = do
      St.El (St.Element theTag attr) cs <- readHere n
      found <- T.mapM lookupTag' cs
      let found' = join found
      if tg == theTag
        then return (n <| found')
        else return found'

    lookupTag' :: ANode (Virt s) -> Virt s (Seq (Node (Virt s) 'El))
    lookupTag' x = case x of
      Left e  -> lookupTag e
      Right t -> return mempty

  getElementsByTagName Nothing tg = do
    els <- liftSt get
    all <- mapM getRecursive els
    return (join (Seq.fromList all))
    where
      getRecursive :: ANode (Virt s) -> Virt s (Seq (Node (Virt s) 'El))
      getRecursive x = case x of
        Left e  -> getElementsByTagName (Just e) tg
        Right x -> return mempty


nodeEq :: (INodeType ty, INodeType ty')
        => Node (Virt s) ty -> Node (Virt s) ty' -> Bool
nodeEq n1 n2 = forgetNode n1 == forgetNode n2

smashl :: (a -> Bool) -> Seq a -> (Seq a, Maybe a, Seq a)
smashl at seq =
  let (l, r) = Seq.breakl at seq
  in case Seq.viewl r of
    EmptyL -> (l, Nothing, r)
    a :< s -> (l, Just a, s)

deriving instance Eq (Node (Virt s) ty)


--------------------------------------------------------------------------------
-- Running and manipulating Virtual DOMs

-- | Converts a static dom node into a reference to a virtual one.
explode :: St.Node -> ST s (ANode (Virt s))
explode = explodeP Nothing where
  explodeP :: Maybe (Node (Virt s) 'El) -> St.Node -> ST s (ANode (Virt s))
  explodeP m (St.Node x) = do
    case x of
      St.El e cs -> mdo
        cs'  <- T.mapM (explodeP (Just me)) cs
        me   <- N <$> newSTRef (St.El e cs')
                  <*> newSTRef m
        return (Left me)
      St.Tx t -> do
        me <- N <$> newSTRef (St.Tx t)
                <*> newSTRef m
        return (Right me)

-- | Assume the existence of a particular 'St.Node' in the context of
-- a virtual dom.
assume :: St.Node -> Virt s ()
assume n = do
  s <- liftST (explode n)
  liftSt (modify (s :))

-- | Run a virtual dom operation assuming the existence of some nodes.
runVirt' :: (forall s . Virt s a) -> [St.Node] -> a
runVirt' v xs = runST $ do
  refs <- mapM explode xs
  evalStateT (unVirt v) refs

-- | Run a virtual dom operation in an empty environment.
runVirt :: (forall s . Virt s a) -> a
runVirt v = runVirt' v []

-- | Compute the static form of a virtual reference
reify :: Node (Virt s) ty -> Virt s (St.NodeF ty St.Node)
reify = T.traverse reifyAny <=< readHere

-- | Compute the static form of a virtual reference without knowing
-- what kind will result.
reifyAny :: ANode (Virt s) -> Virt s St.Node
reifyAny x = case x of
  Left  e -> St.Node <$> reify e
  Right t -> St.Node <$> reify t

reifyAll :: Virt s [St.Node]
reifyAll = liftSt get >>= mapM reifyAny
