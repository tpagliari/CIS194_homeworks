module Fibonacci where

-- Compute the n-th Fibonacci number F_n
-- Time complexity is O(F_n), hence exponential in n
fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

-- Infinite list of all Fibonacci numbers
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Compute Fibonacci "using memoization" idea
-- Complexity time is O(n)
fibs2 :: [Integer]
fibs2 = map fib' [0..]
    where fib' n
            | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibs2 !! (n-1) + fibs2 !! (n-2)
