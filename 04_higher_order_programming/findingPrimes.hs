import Data.List

{- Sieve of Sundaram Algorithm
Given an integer n, generate all the odd prime numbers up to 2n + 2
(0.37 secs, 282,092,352 bytes) with n = 1000 -}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = (map (\i -> 2*i + 1) . filter (`notElem` start n)) [1..n]

start :: Integer -> [Integer]
start n = [i + j + 2*i*j | i<-[1..n], j<-[1..n], i <= j, i + j + 2*i*j <= n]

----------------------------------------------------------------------------

{- Another solution -}
{- (0.59 secs, 501,867,136 bytes) with n = 1000 -}
sieveSundaram' :: Integer -> [Integer]
sieveSundaram' n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2*i*j)
                . filter (\(i, j) -> i + j + 2*i*j <= n)
                $ cartProd [1..n] [1..n]

-- Return all possible pairs
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
