-- class of things a that can be converted to a list of Ints
class Listable a where
    toList :: a -> [Int]

-- create some instances of Listable
-- type definition of the constructors are not mandatory
instance Listable Int where
    toList :: Int -> [Int]
    toList n = [n]

instance Listable Bool where
    toList :: Bool -> [Int]
    toList True = [1]
    toList False = [0]

instance  Listable [Int] where
    toList :: [Int] -> [Int]
    toList = id

-- Binary tree type
-- data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)
data Tree a where
  Leaf :: Tree a
  Node :: (Tree a) -> a -> (Tree a) -> Tree a

-- data List t = Nil | Cons t (List t) deriving Show

-- We ask a to be Listable.
-- Note that the function toList applied to x is not the same of the LHS,
-- but it's the one defined above. Only toList applyed to trees is recoursive here.
instance (Listable a) => Listable (Tree a) where
    toList Leaf = []
    toList (Node left x right) = toList left ++ toList x ++ toList right