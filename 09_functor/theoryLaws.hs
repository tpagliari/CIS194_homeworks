import Prelude hiding (fmap, Functor)
-- Any Functor instance will also satisfy the functor laws,
-- which are part of the definition of a mathematical functor
-- These are
-- fmap id = id
-- fmap (g . h) = (fmap g) . (fmap h)

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Any Haskeller worth their salt would reject this code as a gruesome abomination:
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap _ [] = []
    fmap g (x:xs) = g x : g x : fmap g xs