{- The same tree structure can be used for two quite different purposes,
just by using different annotations -}

data Tree v a = Leaf v a | Branch v (Tree v a) (Tree v a)

newtype Size = Size Int deriving (Eq, Ord, Show)
newtype Priority = Priority Int deriving (Eq, Ord, Show)

-- Monoid instance for Size with Semigroup instance as superclass
instance Semigroup Size where
    (<>) :: Size -> Size -> Size
    Size x <> Size y = Size (x + y)
instance Monoid Size where
    mempty :: Size
    mempty = Size 0

-- Monoid instance for Priority with Semigroup instance as superclass
instance Semigroup Priority where
    (<>) :: Priority -> Priority -> Priority
    Priority x <> Priority y = Priority (min x y)
instance Monoid Priority where
    mempty :: Priority
    mempty = Priority maxBound


-- Annotations
tag :: Tree v a -> v
tag (Leaf v _) = v
tag (Branch v _ _) = v

-- Smart constructors for both implementation of the tree
leaf :: (Monoid v) => a -> Tree v a
leaf a = Leaf mempty a

branch :: (Monoid v) => Tree v a -> Tree v a -> Tree v a
branch left right = Branch (tag left <> tag right) left right


