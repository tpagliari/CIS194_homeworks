{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
import Prelude hiding (fmap, Functor)

-- The functor class is defined in Prelude
-- The first argument of Functor has kind * -> *
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Some instances of functor
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap h Nothing  = Nothing
    fmap h (Just a) = Just (h a)

instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap h [] = []
    fmap h (x:xs) = h x : fmap h xs

instance Functor IO where
    fmap :: (a -> b) -> IO a -> IO b
    fmap h io = io >>= (\n -> return . h $ n)

-- something a bit more mind-twisting:
instance Functor ((->) e) where
    -- fmap :: (a -> b) -> ((->) e) a -> ((->) e) b
    fmap :: (a -> b) -> (e -> a) -> (e -> b)
    fmap h f = h . f -- it's the function composition operator
