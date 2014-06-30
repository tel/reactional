{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Web.Components where

import qualified Data.Foldable  as F
import           Data.Sequence  (Seq)
import qualified Data.Sequence  as Seq
import           Data.Text      (Text)
import           GHC.Exts       (IsList (..))
import qualified Web.Dom.Static as St

data Ast
  = Literal St.Element (Seq Ast)
  | Text Text
  deriving Show

lit :: St.Element -> [Ast] -> Ast
lit e = Literal e . Seq.fromList

txt :: Text -> Ast
txt = Text

foo :: Ast
foo = lit "div" []

class Show1 a where
  show1 :: Show b => a b -> String

instance (Show b, Show1 a) => Show (a b) where
  show = show1

data X a = forall b . Show b => X (a b)

deriving instance Show1 a => Show (X a) 
