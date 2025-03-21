{- A priority queue stores items that have different “priorities”
and always returns the most urgent one first. 
We represent priorities as integers and imagine them as points in time
so the smallest ones are more urgent. -}

data Queue v a = Leaf v a | Node v (Queue v a) (Queue v a)

type Priority = Int

-- Annotations
tag :: Queue v a -> v
tag (Leaf v _) = v
tag (Node v _ _) = v

-- Smart constructors to correctly store priorities
leaf :: Priority -> a -> Queue Priority a
leaf = Leaf 

node :: Queue Priority a -> Queue Priority a -> Queue Priority a
node left right = Node (min (tag left) (tag right)) left right

-- Reconstruct the element that has the smallest priority in O(log n) time
winner :: Queue Priority a -> a
winner (Leaf _ a) = a
winner (Node v left right)
    | tag left == v = winner left
    | otherwise     = winner right
