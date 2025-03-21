{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (foldl, foldr)

-- Definition of . operator which defines the composition of two functions
comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

-- All functions in Haskell take only one (1) argument. 
-- For example, the function
f :: Int -> Int -> Int
f x y = 2*x + y
-- is, in fact, left-associative:
-- f takes as input an integer and returns a function N -> N
f' :: Int -> (Int -> Int)
(f' x) y = 2*x + y

-- if the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest
foldr :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 

-- if the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
foldl :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs

-- Let us define map with fold right operator:
--  * map takes as arg a function f and returns a function :: [a1] -> [a2]
--  * [1,2,3,4] = 1:2:3:4:[], foldr f z [a] substitues f to : and z to [] in the list [a].
map :: (t1 -> b) -> [t1] -> [b]
map f = foldr ((:) . f) []
-- Substitute (+) to (:) and 0 to []
-- sum [1,2,3] == sum (1:2:3:[]) == (foldr (+) 0)(1:2:3:[]) == 1+2+3+0 == 6
sum :: [Integer] -> Integer
sum = foldr (+) 0