{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}

{- Function that returns True if and only if there are an odd number of True
values contained in the input list -}
xor :: [Bool] -> Bool
xor = odd . sum . map fromEnum

xor' :: [Bool] -> Bool
xor' = odd . foldr ((+) . fromEnum) 0

{- Implement map as a fold -}
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x xs -> f x : xs) []

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr ((:) . f) []

{- foldl with foldr -}
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse