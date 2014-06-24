{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           FRP.Sodium

-- import           GHCJS.Foreign
-- import           GHCJS.Prim
-- import           GHCJS.Types

data Nde
data Txt

data AnElement r where AnElement :: Element r a -> AnElement r

class DomEl r where
  data Element r :: * -> *

  -- | Tests whether two nodes are equal by reference.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.isEqualNode>
  isEqualNode :: Element r a -> Element r a -> r Bool

  -- | Returns a duplicate of the node on which this method was
  -- called. Note that the result of this call will not 'isEqualNode'
  -- the argument, but if @Eq (Element r)@ then they should be '(==)'.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.cloneNode>
  cloneNode :: Element r a -> r (Element r a)
  
  -- | Indicates whether a node is a descendant of a given node.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.contains>
  contains :: Element r Nde -> Element r a -> r Bool

  -- | Removes a child node from the DOM. 'Left' on exception.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild>
  removeChild :: Element r Nde -> Element r a -> r (Either String ())

  -- Replaces one child node of the specified element with another.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.replaceChild>
  --
  -- Argument order is @replaceChild parent new old@.
  replaceChild :: Element r Nde -> Element r a -> Element r b -> r ()

class DomEl r => Dom r where

  -- | Attempts to locate an element in the DOM
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.getElementById>
  getElementById
    :: Text ->
      -- ^ Identifier
      r (Maybe (Element r Nde))
      -- ^ Element if such an element with a matching identifier
      -- exists in the document

  -- | Builds a new element with a given tag name
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.createElement>
  createElement
    :: Text ->
      -- ^ Tag name
      r (Element r Nde)
      -- ^ Fresh element

  -- | Builds a "text node", one which cannot accept children, has no
  -- form,
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.createTextNode>
  createTextNode
    :: Text ->
      -- ^
      r (Element r Txt)

  -- | The action @appendChild parent child@ attempts to put the
  -- @child@ as the next child under @parent@. This will fail if it
  -- forms a loop---the failure message is implementation dependent.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild>
  appendChild
    :: Element r Nde ->
      -- ^ Parent node
      Element r a ->
      -- ^ Child node
      r (Either String ())
      -- ^ Error if 'Left'.

  -- | Inserts the specified node before a reference element as a
  -- child of the current node.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.insertBefore>
  insertBefore
    :: Element r Nde ->
      -- ^ Parent element
      Element r a ->
      -- ^ Element to-be-inserted
      Element r b ->
      -- ^ Reference element, the one before which the new element is
      -- inserted
      r (Either String ())
      -- ^ Error if 'Left'.

  -- insertAfter <- (insertBefore, nextSibling)
  -- nextSibling
  -- setId
  -- setClass
  -- setAttribute

--------------------------------------------------------------------------------

data El = ElNode Text (Maybe Text) [AnElement Sim] | ElText Text

newtype Sim a =
  Sim { unSim :: State (Map Int El, Int) a }
  deriving (Functor, Applicative, Monad, MonadState (Map Int El, Int))

fresh :: El -> Sim (Element Sim a)
fresh x = do
  v <- use _2
  _2 .= v + 1
  _1 . at v .= Just x
  return (SimEl v)

instance DomEl Sim where
  newtype Element Sim a = SimEl { getSimEl :: Int }

instance Dom Sim where

  getElementById i = do
    els <- use (_1 . to Map.toList)
    return $ case filter (isId i) els of
      []       -> Nothing
      [(n, _)] -> Just (SimEl n)
      _        -> error "Malformed DOM: multiple els with same ID"
    where
      isId _ (_, ElText _)            = False
      isId i (_, ElNode _ Nothing _)  = False
      isId i (_, ElNode _ (Just j) _) = i == j
      
  createElement  tag = fresh (ElNode tag Nothing [])
  createTextNode txt = fresh (ElText txt)
    
  appendChild = undefined
  
  insertBefore = undefined

--------------------------------------------------------------------------------

-- foreign import javascript unsafe
--   "document.getElementById($1)"
--   _getById :: JSString -> IO (JSRef DomElement_)

-- foreign import javascript unsafe
--   "document.createElement($1)"
--   _createElement :: JSString -> IO (JSRef DomElement_)

-- foreign import javascript unsafe
--   "document.createTextNode($1)"
--   _createTextNode :: JSString -> IO (JSRef DomElement_)

-- foreign import javascript safe
--   "$1.appendChild($2)"
--   _appendChild :: JSRef DomElement_ -> JSRef DomElement_ -> IO (JSRef DomElement_)

-- foreign import javascript safe
--   "$1.insertBefore($2, $3)"
--   _insertBefore :: JSRef DomElement_ ->
--                   JSRef DomElement_ ->
--                   JSRef DomElement_ ->
--                   IO (JSRef DomElement_)

-- data DomElement_ = DomElement_

-- instance Dom IO where
--   newtype Element IO a = DomElement { unDomElement :: JSRef DomElement_ }

--   getElementById i = do
--     e <- _getById (toJSString i)
--     return $ if isNull e then Nothing else Just (DomElement e)
--   createElement = fmap DomElement . _createElement . toJSString
--   createTextNode = fmap DomElement . _createTextNode . toJSString

--   appendChild parent child =
--     fmap fx . try $ _appendChild (unDomElement parent) (unDomElement child)
--     where
--       fx (Left e) = Left (show (e :: JSException))
--       fx (Right a) = Right ()

--   insertBefore parent child reference =
--     fmap fx . try $ _insertBefore (unDomElement parent)
--                                   (unDomElement child)
--                                   (unDomElement reference)
--     where
--       fx (Left e) = Left (show (e :: JSException))
--       fx (Right a) = Right ()

--------------------------------------------------------------------------------

{-

Features

  * Varying properties
  * Event delegation to the top of a component tree
  * Synthetic events carried out using FRP
  * requestAnimationFrame driven render loop
  * Component/Vdom distinction

-}

--------------------------------------------------------------------------------

-- data Vdom
--   = VNode Text (Map String String) [Vdom]
--   | VText Text
--   deriving ( Eq, Ord, Show, Read )

-- instance IsString Vdom where
--   fromString s = VText (fromString s)

-- gen :: Vdom -> IO DomElement
-- gen (VText t) = createTextNode t
-- gen (VNode nm attrs cs) = do
--   cs' <- mapM gen cs
--   me  <- createElement nm
--   mapM_ (appendChild me) cs'
--   return me

-- data C p s
-- newC :: (p -> s -> C p s) -> p -> [C p s] -> C p s
-- newC f p cs = f p undefined


--------------------------------------------------------------------------------

{-

What do you do when you specify a component? You define the render
function which builds a component tree in context of the *properties*
and the *state*. We'll ignore state for now.

-}

-- class (Applicative m, Monad m) => Render m
-- instance Render Identity

-- data Component p = Component { runComponent :: forall m . Render m => p -> m Vdom }

-- type ToComponent p = [Vdom] -> Component p

-- instance IsString (Component p) where
--   fromString s = makeComponent (const . return . fromString $ s)

-- makeComponent :: (forall m . Render m => p -> m Vdom) -> Component p
-- makeComponent = Component

-- generic :: Text -> ToComponent p
-- generic nm cs = makeComponent $ \p -> return (VNode nm Map.empty cs)

-- divC, butC, strC :: ToComponent p
-- divC  = generic "div"
-- spanC = generic "span"
-- butC  = generic "button"
-- strC  = generic "strong"

-- genC :: Component p -> p -> Vdom
-- genC = (runIdentity .) . runComponent

-- -- box :: Component Int
-- -- box = makeComponent $ \p ->
-- --   runComponent (divC [ spanC [ fromString (show p) ] ]) p

-- -- box = mk $ \p s ->
-- --   div [] [ span [] [ p ^. num . to show ] ]

-- --------------------------------------------------------------------------------

-- it :: Component p
-- it =
--   divC [ butC [ strC [ "Hi" ] ]
--        , butC [ "Hi" ]
--        , butC [ "Hi" ]
--        , strC [ "foobar" ]
--        ]

-- main :: IO ()
-- main = do
--   me <- getById "outer"
--   case me of
--     Nothing -> putStrLn "oops!"
--     Just e  -> do
--       c <- gen (genC it ())
--       appendChild e c
--       return ()


main = putStrLn "'elo"
