{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Sized where

import Data.Monoid

newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

-- This part I updated to make it compliant with modern haskell
-- Make Size an instance of Semigroup (required for Monoid)
instance Semigroup Size where
  (<>) (Size x) (Size y) = Size (x + y)

-- Make Size an instance of Monoid
instance Monoid Size where
  mempty = Size 0
  mappend = (<>)
