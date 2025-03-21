import Data.List (nub, sort, group)

{- Utility function that safely extract all the elements except
the last one from a list -}
safeInit :: [Int] -> [Int]
safeInit []  = []
safeInit [_] = []
safeInit (x:xs) = x : safeInit xs

{- Utility function that returns the list of only the duplicates -}
duplicates :: [Int] -> [Int]
duplicates xs = concat [safeInit ys | ys <- (group . sort) xs, length ys > 1]


{- Function that returns a list of lists that containes unique values
of the original lists. The number of sublist is equal to the maximum
number of dublicates that an element has -}
f :: [Int] -> [[Int]]
f [] = []
f xs = (sort . nub) xs : f (duplicates xs)


{- Function that creates a bijection between a list of Int and a 
sequence of * . We need to wrap it with a function that insert (-1)
at the beginning of the list of int 0..9 to make the algorithm work -}
intToStarsAlg :: [Int] -> String
intToStarsAlg [x] = (concat . replicate (10-x-1)) " "
intToStarsAlg (x:y:xs) = (concat . replicate (y-x-1)) " " ++ "*" ++ intToStarsAlg (y:xs)

putUpfront :: [Int] -> [Int]
putUpfront xs = (-1):xs

intToStars :: [Int] -> String
intToStars = intToStarsAlg . putUpfront


{- Function that returns the histogramof recurrences as a String -}
-- Utility function
getList :: [[a]] -> [a]
getList [] = []
getList (x:_) = x

histogram :: [Int] -> String
histogram [] = "==========\n0123456789\n"
histogram xs =  intToStars((getList.f) xs) ++ "\n" ++ histogram (duplicates xs)