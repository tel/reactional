{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Web.Dom (

    NodeType     (..)
  , INodeType    (..)
  , ANode, anyNode
  , Position     (..)
  , BoundingRect (..)

  , DomNode      (..)
  , childElements, childTextNodes
  , DomEl        (..)
  , DomView      (..)

  ) where

import           Control.Applicative
import           Control.Monad       hiding (mapM)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (mapMaybe, isJust, fromJust)
import           Data.Sequence       (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence       as Seq
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Traversable    (mapM)
import           Prelude             hiding (mapM)
import qualified Web.Dom.Static      as Static
import           Web.Dom.Types

{-

    We work with a simplied model of the DOM. The DOM exists within a
    stateful context and persists objects called Nodes. A Node is either
    an Element or Text.

-}

-- data NodeType = El | Tx

class INodeType ty where
  forgetNode :: Node r ty -> ANode r

instance INodeType El where forgetNode = Left
instance INodeType Tx where forgetNode = Right

-- | A node of some, unknown type.
type ANode r = Either (Node r El) (Node r Tx)

-- | Simple eliminator for 'ANode's
anyNode :: (forall a . INodeType a => Node r a -> z) -> ANode r -> z
anyNode f x = case x of
  Left  n -> f n
  Right n -> f n

-- | Instances of 'DomNode' provide an interface to a tree of "nodes"
-- which are either branching (of type 'El') or terminal (of type
-- 'Tx'). Little information is available beyond the ability to tag
-- nodes, the text stored at terminal nodes,
class Monad r => DomNode r where

  -- | The type of nodes inside the DOM. The final type index attempts
  -- to determine whether a given type is an element or a text node.
  -- If we don't know then we just return 'ANode'.
  data Node r :: NodeType -> *

  -- | In an HTML document creates the specified HTML element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.createElement>
  createElement :: Tag -> r (Node r El)

  -- | Creates a new Text node.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.createTextNode>
  createTextNode :: Text -> r (Node r Tx)

  -- | Returns all elements of a given tag under a node. If no top
  -- node is passed then it returns all tag-matched elements in the
  -- DOM.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.getElementsByClassName>
  getElementsByTagName :: Maybe (Node r El) -> Tag -> r [Node r El]


  -- /Node/ The node interface observes a tree of nodes and provides
  -- mechanisms for traversing the tree while adding, removing,
  -- replacing, and reordering nodes.

  -- | Returns the collection of child nodes of the given element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.childNodes>
  childNodes :: Node r El -> r (Seq (ANode r))

  -- | Returns the node's first child in the tree, or 'Nothing' if the
  -- node is childless.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.firstChild>
  firstChild :: Node r El -> r (Maybe (ANode r))
  firstChild el = liftM (safeHead . Seq.viewl) (childNodes el) where
    safeHead EmptyL   = Nothing
    safeHead (x :< _) = Just x

  -- | lastChild returns the last child of a node, or 'Nothing' if the
  -- node is childless.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.lastChild>
  lastChild :: Node r El -> r (Maybe (ANode r))
  lastChild el = liftM (safeLast . Seq.viewr) (childNodes el) where
    safeLast EmptyR   = Nothing
    safeLast (_ :> x) = Just x

  -- | Returns the node immediately preceding the specified one in its
  -- parent's childNodes list, or 'Nothing' if the specified node is
  -- the first node in that list or has no parent.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.previousSibling>
  previousSibling :: INodeType a => Node r a -> r (Maybe (ANode r))
  previousSibling n = do
    mp   <- parentElement n
    case mp of
      Nothing -> return Nothing
      Just p -> do
        cs  <- childNodes p
        cs' <- mapM (\c -> isEqualNode' (forgetNode n) c >>= \x -> return (c, not x)) cs
        let t = Seq.takeWhileL snd cs'
        return $ case Seq.viewr t of
          EmptyR     -> Nothing
          _ :> (x,_) -> Just x

  -- | Returns the node immediately following the specified one in its
  -- parent's childNodes list, or 'Nothing' if the specified node is
  -- the last node in that list.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.nextSibling>
  nextSibling :: INodeType a => Node r a -> r (Maybe (ANode r))
  nextSibling n = do
    mp   <- parentElement n
    case mp of
      Nothing -> return Nothing
      Just p -> do
        cs  <- childNodes p
        cs' <- mapM (\c -> isEqualNode' (forgetNode n) c >>= \x -> return (c, not x)) cs
        let t = Seq.takeWhileR snd cs'
        return $ case Seq.viewl t of
          EmptyL     -> Nothing
          (x,_) :< _ -> Just x

  -- | Returns or sets the value of the current node. Compare this
  -- with 'children'.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.nodeValue>
  nodeValue :: Node r Tx -> r Text

  -- | Returns the DOM node's parent Element, or 'Nothing' if the node
  -- either has no parent, or its parent isn't a DOM Element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.parentElement>
  --
  -- Note that implementations in Javascript may need to consider
  -- @parentNode@ as well.
  parentElement :: INodeType a => Node r a -> r (Maybe (Node r El))

  -- | Adds a node to the end of the list of children of a specified
  -- parent node. If the node already exists it is removed from
  -- current parent node, then added to new parent node. If a cycle
  -- would be generated then this raises an error.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.appendChild>
  appendChild :: INodeType a => Node r El -> Node r a -> r (Either String ())

  -- | Inserts the specified node before a reference element as a
  -- child of the current node. If a cycle would be generated then
  -- this raises an error.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.insertBefore>
  --
  -- > insertBefore parent reference target
  insertBefore :: (INodeType a, INodeType b) =>
                  Node r El -> Node r a -> Node r b -> r (Either String ())

  -- | Inserts the specified node after a reference element as a child
  -- of the current node. If a cycle would be generated then this
  -- raises an error.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.insertBefore>
  --
  -- > insertAfter parent reference target
  insertAfter :: (INodeType a, INodeType b) =>
                 Node r El -> Node r a -> Node r b -> r (Either String ())
  insertAfter parent reference target = do
    next <- nextSibling reference
    case next of
      Nothing        -> appendChild  parent   target
      Just (Left x)  -> insertBefore parent x target
      Just (Right x) -> insertBefore parent x target

  -- | Returns a duplicate of the node on which this method was
  -- called. Children are not copied—in fact the new node will have no
  -- children. The duplicate node has no parent (parentNode is
  -- 'Nothing') until it's added to the document, for example using
  -- 'appendChild'.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.cloneNode>
  cloneNode :: INodeType a => Node r a -> r (Node r a)

  -- | Returns a duplicate of the node on which this method was
  -- called. Children are copied as well. The duplicate node has no
  -- parent (parentNode is 'Nothing') until it's added to the
  -- document, for example using 'appendChild'.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.cloneNode>
  deepCloneNode :: INodeType a => Node r a -> r (Node r a)

  -- | Indicates whether a node is a descendant of a given node.
  -- Trivially, @contains e e@ holds. By default the algorithm is
  -- @O(d)@ linear in the length of path from the parent to the child.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.contains>
  contains :: INodeType a => Node r El -> Node r a -> r Bool
  contains parent child = case forgetNode child of
    Right _ -> do
      mp <- parentElement child
      case mp of
        Nothing -> return False
        Just p  -> go p
    Left  x -> go x

    where
      go :: Node r El -> r Bool
      go c = do
        done <- isEqualNode parent c
        case done of
          True  -> return True
          False -> do
            up <- parentElement c
            case up of
              Nothing -> return False -- We're at the top
              Just p  -> contains parent p


  -- | Returns whether a node has child nodes or not.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.hasChildNodes>
  hasChildNodes :: Node r El -> r Bool
  hasChildNodes = liftM Seq.null . childNodes

  -- | Tests whether two nodes are (reference) equal.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.isEqualNode>
  isEqualNode :: INodeType a => Node r a -> Node r a -> r Bool

  -- | Removes a child node from the DOM. Returns 'Left' if child not
  -- actually a child of the parent.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild>
  removeChild :: Node r El -> Node r a -> r (Either String ())

  -- | Replaces one child node of the specified element with another.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Node.removeChild>
  replaceChild :: Node r El -> Node r a -> Node r b -> r (Either String ())


-- /Element/ The element interface is specific to nodes of type 'El'.
-- It facilitates access to attributes and dressings related to the
-- properties of a DOM element (as compared to a DOM text node, for
-- instance).
class DomNode r => DomEl r where
  -- | Returns a reference to the element by its ID.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.getElementById>
  getElementById :: Id -> r (Maybe (Node r El))

  -- | Returns all elements of a given class under a node. If no top
  -- node is passed then it returns all class-matched elements in the
  -- DOM.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/document.getElementsByClassName>
  getElementsByClassName :: Maybe (Node r El) -> Class -> r [Node r El]

  -- | Gets the element's identifier.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.id>
  getElementId :: Node r El -> r (Maybe Id)

  -- | Sets the element's identifier.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.id>
  setElementId :: Node r El -> Maybe Id -> r ()

  -- | Returns the name of the element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.tagName>
  tagName :: Node r El -> r Tag

  -- | Returns a token list of the class attribute of the element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/element.classList>
  getClassList :: Node r El -> r [Class]

  -- | Sets the class list.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/element.classList>
  setClassList :: Node r El -> [Class] -> r ()

  -- | Gets the name propery of a DOM object; it only applies to the
  -- following elements: @a@, @applet@, @button@, @form@, @frame@,
  -- @iframe@, @img@, @input@, @map@, @meta@, @object@, @param@,
  -- @select@, and @textarea@.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/element.name>
  getName :: Node r El -> r (Maybe Text)

  -- | Sets the name propery of a DOM object; it only applies to the
  -- following elements: @a@, @applet@, @button@, @form@, @frame@,
  -- @iframe@, @img@, @input@, @map@, @meta@, @object@, @param@,
  -- @select@, and @textarea@.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/element.name>
  setName :: Node r El -> Text -> r (Either String ())

  -- | Read only access to the attributes of an element
  attributes :: Node r El -> r (HashMap Attr Text)

  -- | Returns a boolean value indicating whether the specified
  -- element has the specified attribute or not.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.hasAttribute>
  hasAttribute :: Node r El -> Attr -> r Bool
  hasAttribute n a = liftM (HM.member a) (attributes n)

  -- | Removes an attribute from the specified element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.removeAttribute>
  removeAttribute :: Node r El -> Attr -> r ()

  -- | Adds a new attribute or changes the value of an existing
  -- attribute on the specified element.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.setAttribute>
  setAttribute :: Node r El -> Attr -> Text -> r ()

  -- | Returns the value of a specified attribute on the element. If
  -- the given attribute does not exist or is @""@ then returns
  -- 'Nothing'.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/element.getAttribute>
  getAttribute :: Node r El -> Attr -> r (Maybe Text)
  getAttribute n a = liftM (HM.lookup a) (attributes n)

  -- NOTE: Eliminate this from the class? It ought to always be more
  -- efficient to work from the static structure. No need to overload.

  -- | Get a static representation of the \"inner HTML\". Note that
  -- this is very different from the standard implementation of
  -- @innerHTML@ which is string-based. Implementations are encouraged
  -- to use the DOM structure to generate the inner HTML efficiently.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.innerHTML>
  getInnerHTML :: Node r El -> r (Static.Node)

  -- NOTE: Eliminate this from the class? It ought to always be more
  -- efficient to work from the static structure. No need to overload.

  -- | Write a static representation to the \"inner HTML\" of a node.
  -- Note that this is very different from the standard implementation
  -- of @innerHTML@ which is string-based. Implementations are
  -- encouraged to use the static structure to generate the inner HTML
  -- efficiently.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.innerHTML>
  setInnerHTML :: Node r El -> Static.Node -> r ()


-- | When using 'scrollIntoView' one can choose whether to scroll to
-- the top or the bottom of the screen.
data Position = Top | Bottom

-- | A 'BoundingRect' gives the bounds of an element.
data BoundingRect =
  BoundingRect { bottom :: {-# UNPACK #-} !Double
               , height :: {-# UNPACK #-} !Double
               , left   :: {-# UNPACK #-} !Double
               , right  :: {-# UNPACK #-} !Double
               , top    :: {-# UNPACK #-} !Double
               , width  :: {-# UNPACK #-} !Double
               }
  deriving ( Eq, Ord, Show, Read )

-- | Instances of 'DomView' model not only the tree structure of 'Dom'
-- but also the physical state of the elements in a rendered view.
class DomEl r => DomView r where
  -- | The width of the left border of an element in pixels. It
  -- includes the width of the vertical scrollbar if the text
  -- direction of the element is right–to–left and if there is an
  -- overflow causing a left vertical scrollbar to be rendered.
  -- 'clientLeft' does not include the left margin or the left
  -- padding.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.clientLeft>
  clientLeft :: Node r El -> r Int

  -- | The width of the top border of an element in pixels. It does
  -- not include the top margin or padding.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.clientTop>
  clientTop :: Node r El -> r Int

  -- | Gets the number of pixels that an element's content is scrolled
  -- to the top.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.scrollTop>
  getScrollTop :: Node r El -> r Int

  -- | Gets the number of pixels that an element's content is scrolled
  -- to the top.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.scrollTop>
  setScrollTop :: Node r El -> Int -> r ()

  -- | Gets the number of pixels that an element's content is scrolled
  -- to the left.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.scrollLeft>
  getScrollLeft :: Node r El -> r Int

  -- | Gets the number of pixels that an element's content is scrolled
  -- to the left.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.scrollLeft>
  setScrollLeft :: Node r El -> Int -> r ()

  -- | Returns a text rectangle object that encloses a group of text
  -- rectangles.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.getBoundingClientRect>
  getBoundingClientRect :: Node r El -> r BoundingRect

  -- | Scrolls the element into view.
  -- <https://developer.mozilla.org/en-US/docs/Web/API/Element.scrollIntoView>
  scrollIntoView :: Node r El -> Position -> r ()

childElements :: DomNode r => Node r El -> r (Seq (Node r El))
childElements = liftM (mapMaybeS goLeft) . childNodes where
  goLeft (Left a) = Just a
  goLeft _        = Nothing

childTextNodes :: DomNode r => Node r El -> r (Seq (Node r Tx))
childTextNodes = liftM (mapMaybeS goRight) . childNodes where
  goRight (Right a) = Just a
  goRight _         = Nothing

isEqualNode' :: (DomNode r, Monad r) => ANode r -> ANode r -> r Bool
isEqualNode' (Left n1)  (Left n2)  = isEqualNode n1 n2
isEqualNode' (Right n1) (Right n2) = isEqualNode n1 n2
isEqualNode' _          _          = return False

mapMaybeS :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybeS f = fmap fromJust . Seq.filter isJust . fmap f
