-- Helpers
-- Reverse a list (beginner way but not very efficient)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

-- Convert positive Integers to a list of digits
toDigitsRev :: Int -> [Int]
toDigitsRev n
  | (n * (-1)) >= 0 = []
  | otherwise       = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Int -> [Int]
toDigits n = reverseList (toDigitsRev n)

-- Double numbers in the list alternatively from the right
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList [a] = [a]
doubleList (x:y:xs) = x:(2*y):doubleList xs 

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:xs) = reverseList (doubleList (reverseList (x:xs)))

-- Calculate the sum of alldigits in a list
fullDigits :: [Int] -> [Int]
fullDigits [] = []
fullDigits (x:xs) = toDigits x ++ fullDigits xs

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = sum (fullDigits (x:xs)) 

-- Find whether an Integer could be a valid credit card number
validate :: Int -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
