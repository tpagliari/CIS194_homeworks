module JoinList where
import Sized ( Sized(..), getSize, Size (Size) )
import Scrabble ( Score (Score), scoreString, score )

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Append function for JoinLists
-- Note that for different monoid instances, <> means different things
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jla +++ jlb = Append (tag jla <> tag jlb) jla jlb

-- Helper function which gets the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


-- Finds the JoinList element at the specified index
-- By an index in a JoinList we mean the index in the list that it represents
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
    | i == 0    = Just a
    | otherwise = Nothing
indexJ i (Append _ left right)
    | i < sizel = indexJ i left
    | otherwise = indexJ (i - sizel) right
        where sizel = getSize . size . tag $ left

-- The dropJ function drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append _ left right)
    | n >= sizel + sizer = Empty -- n out of bound
    | n >= sizel         = dropJ (n - sizel) right
    | otherwise          = dropJ n left +++ right
        where sizel = getSize . size . tag  $ left
              sizer = getSize . size . tag  $ right

-- takeJ function returns the first n elements of a JoinList,
-- dropping all other elements
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ jl@(Single m a) = jl
takeJ n jl@(Append _ left right)
    | n >= sizel + sizer = jl
    | n >= sizel         = left +++ takeJ (n - sizel) right
    | otherwise          = takeJ n left
        where sizel = getSize . size . tag  $ left
              sizer = getSize . size . tag  $ right


-- Function to test out JoinLists annotated with scores
scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine str = Single (scoreString str) str


-- Buffer type to track both the size and score of a buffer
-- if a and b are monoids, then (a, b) is also a monoid
type Buffer = JoinList (Size, Score) String

-- helper functions to work with buffers
bscoreChar :: Char -> Buffer
bscoreChar c = Single (Size 1, score c) [c]

bscoreString :: String -> Buffer
bscoreString = foldr ((+++) . bscoreChar) Empty