{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}
import Prelude hiding (mempty, Monoid, mappend)

-- The monoid class describes instances that have
-- a special element called "mempty",
-- a binary operation "mappend" (<>) that is associative
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

(<>) :: Monoid m => m -> m -> m
(<>) = mappend


-- let us write some instances
instance Monoid [a] where
    mempty  = [] -- neutral wrt ++
    mappend = (++) -- binary, associative