{- Recoursive data type (Tree) with polymorphism
Each node stores an extra Integer representing the height at that node -}
data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
    deriving (Show, Eq)

{- Function that turns a list in a balanced binary tree -}
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

{- Helper function to get the height of a node -}
height :: Tree a -> Int
height Leaf = -1
height (Node h _ _ _) = h

{- Smart node constructor with update height-}
node :: Tree a -> a -> Tree a -> Tree a
node left a right = Node (1 + max (height left) (height right)) left a right

{- Balanced insert algorithm -}
insert :: a -> Tree a -> Tree a
insert x Leaf = node Leaf x Leaf
insert x (Node h left y right)
    | height left < height right = node (insert x left) y right
    | otherwise                  = node left y (insert x right)

