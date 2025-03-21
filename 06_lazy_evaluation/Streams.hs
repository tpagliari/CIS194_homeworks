module Streams where

-- Data type of polymorphic streams
-- A stream is simply defined as an element followed by a stream
data Stream t = Cons t (Stream t)

-- Convert a Stream to an infinite list
streamToList :: Stream t -> [t]
streamToList (Cons x xs) = x : streamToList xs

-- Show a stream
-- outputs only the first 20 elements as a string
instance (Show t) => Show (Stream t) where
    show :: Stream t -> String
    show = show . take 40 . streamToList -- RHS show is from class Show


{- Utility functions for working with streams -}

-- repeat for streams
streamRepeat :: t -> Stream t
streamRepeat x = Cons x (streamRepeat x)

-- map for streams
streamMap :: (t1 -> t2) -> Stream t1 -> Stream t2
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- iterate for streams
streamFromSeed :: (t -> t) -> t -> Stream t
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- alternate the elements of two streams
interleaveStreams :: Stream t -> Stream t -> Stream t
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))


{- Define some streams -}

-- stream of natural numbers
nats :: Stream Integer
nats = streamFromSeed (\n -> n+1) 1

-- Ruler function
-- the n-th element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n
-- > 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap zumkeller nats)
    where zumkeller n
                | odd n = 1
                | otherwise = 1 + zumkeller (n `div` 2)
