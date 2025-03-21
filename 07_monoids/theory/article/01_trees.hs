import Prelude hiding ((!!))

{- Binary trees as faster list-like data structure
that reduces random access from O(n) to O(log n).
We use a binary tree that stores the elements a at the leaves.
Furthermore, every node is annotated with a value of type v -}

data Tree v a = Leaf v a | Node v (Tree v a) (Tree v a)

-- The leaves should store the elements of our list from left to right
toList :: Tree v a -> [a]
toList (Leaf _ a) = [a]
toList (Node _ left right) = toList left ++ toList right

-- Annotations
tag :: Tree v a -> v
tag (Leaf v _) = v
tag (Node v _ _) = v

-- Head: retrieve the leftmost element of the list
headTree :: Tree v a -> a
headTree (Leaf _ a) = a
headTree (Node _ left right) = headTree left

-- What if we need to retrieve the n-th element?
-- Let us annotate each subtree with its size
type Size = Integer

-- We can make sure that they are always correct by using smart constructors
-- which automatically annotate the right sizes
leaf :: v -> a -> Tree Size a
leaf _ = Leaf 1

node :: Tree Size a -> Tree Size a -> Tree Size a
node left right = Node (tag left + tag right) left right

-- we can now find the n-th leaf
-- this is efficient O(log n) if the tree is balanced
(!!) :: Tree Size a -> Integer -> a
(Leaf v a) !! 0 = a
(Node v left right) !! n
    | n < tag left = left  !! n
    | otherwise    = right !! (n - tag left)



