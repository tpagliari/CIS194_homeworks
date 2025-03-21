{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use sum" #-}
import Prelude hiding (foldr)

{- We can generalize folds data types as well -}

-- List ----------------------------------------

-- define a List data type
data List a = Empty | Cons a (List a)

-- Here's a simple high order function
sumList :: (Num a) => List a -> a
sumList Empty = 0
sumList (Cons x xs) = x + sumList xs

-- Here's the fold definition
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Empty       = z 
foldr f z (Cons x xs) = f x (foldr f z xs)

-- sum using fold
sumList' :: (Num a) => List a -> a
sumList' = foldr (+) 0


-- Trees ---------------------------------------

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- let us see some high order functions
sumTree :: (Num a) => Tree a -> a
sumTree Leaf = 0
sumTree (Node left x right) = sumTree left + x + sumTree right

depthTree :: Tree a -> Integer
depthTree Leaf = 0
depthTree (Node left x right) = 1 + max (depthTree left) (depthTree right)

flattenTree :: Tree a -> [a]
flattenTree Leaf = []
flattenTree (Node left x right) = flattenTree left ++ [x] ++ flattenTree right

-- How can we generalize this with fold
foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree f z Leaf = z
foldTree f z (Node left x right) = f (foldTree f z right) x (foldTree f z left)

-- Sum the trees elements, if numbers
sumTree' :: (Num a) => Tree a -> a
sumTree' = foldTree (\bi a bj -> bi + a + bj) 0

depthTree' :: Tree a -> Integer
depthTree' = foldTree (\mL _ mR -> 1 + max mL mR) 0

flattenTree' :: Tree a -> [a]
flattenTree' = foldTree (\bL a bR -> bL ++ [a] ++ bR) []