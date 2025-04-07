module Lists where
    
-- As a demonstration, let's define our own classes
import Prelude hiding ((<*>), Functor, Applicative)
class Functor f where
    fmap :: (a -> b) -> f a -> f b
class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> (f a -> f b)


-- We can view a List as a ordered collections of elements
-- Let's wrap it into a newtype 
newtype ZipList a = ZipList { getZipList :: [a] } deriving Show

instance Functor ZipList where
    fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure :: a -> ZipList a
    pure x = ZipList [x]

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)


-- Lists based on the nondeterministic computation point of view
instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b] 
    fmap = map
instance Applicative [] where
    pure :: a -> [a]
    pure x = [x]

    (<*>) :: [a -> b] -> [a] -> [b]
    (<*>) gs xs = [g x | g <- gs, x <- xs]

