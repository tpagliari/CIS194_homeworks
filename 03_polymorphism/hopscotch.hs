{-# OPTIONS_GHC -Wall #-}

{-
This function is used to define the cardinality of the new
index ensembles. For example, a index ensemble of cardinality
10, quotiented on 3, wil have cardinality 3.
> caridnality 3 8 == 2
> cardinality 0 n == 0
> cardinality 1 n == n
-}
cardinality :: Int -> Int -> Int
cardinality 0 _ = 0
cardinality k n = n `div` k

{-
This function is used to define new index ensembles. For example,
a index ensemble [1..10] quotieted 3 becomes [3,6,9]. 
Here, v should be the index ensemble we are using.
> ensemble 2 [1..13] == [2,4,6,8,10,12]
> ensemble 1 == identity
> ensemble 0 == []
-}
ensemble :: Int -> [Int] -> [Int]
ensemble k v = take (cardinality k (length v)) (map (k *) v)

{-
This function maps a vector into a list of vectors, each defined
on a varying index ensemble. The n-th list in the output contains
every n-th element from the input list. This is achieved by modifying the
underlying index ensemble on which the input vector is defined: for each
n-th list in the output vector, the definition of the index ensembles changes 
using the ensemble function, allowing to keep every n-th element from the
original input vector. For example:
> skip [1,2,3,4] == [[1,2,3,4],[2,4],[3],[4]]
> skip "abcd" == ["abcd","bd","c","d"]
> skip [] == []
> skip [1] == [[1]]
-}
skip :: [a] -> [[a]]
skip lst = skip' lst 1
  where
    skip' :: [a] -> Int -> [[a]]
    skip' v k
      | k <= length v = ([ x | (x, i) <- zip v [1..], i `elem` ensemble k [1..(length v)]]) : skip' v (k+1)
      | otherwise = []