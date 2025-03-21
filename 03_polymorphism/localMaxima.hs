{-# OPTIONS_GHC -Wall #-}
import Data.Maybe (fromJust)

{-
Given a list of 3 integers, this function returns
True iff the element in the middle is greater than the others.
-}
isPeak :: [Int] -> Maybe Bool
isPeak [a,b,c] = Just (b > a && b > c)
isPeak _ = Nothing

{-
Given a list of integers, this function returns 
a list with the local maxima of the input list, in the
same order they appear in the input list. Despite the use of !!,
this function is safe because - if there is insufficient number of 
elements - an empty list is returned.
> localMaxima [1,1,1,1] == []
> localMaxima [1,5,4,5,3] == [5,5]
-}
localMaxima :: [Int] -> [Int]
localMaxima lst
    | length lst <= 2 = []
    | fromJust (isPeak (take 3 lst)) = (lst !! 1) : localMaxima (drop 2 lst)
    | otherwise = localMaxima (drop 1 lst)