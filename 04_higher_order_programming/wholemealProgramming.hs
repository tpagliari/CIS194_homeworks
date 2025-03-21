-- This function is not written in good Haskell style
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- We wish to reimplement fun1 with good Haskell style
fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x-2) * y) 1 . filter even

-- But probably better this way
fun1'' :: [Integer] -> Integer
fun1'' = product . map (subtract 2) . filter even

------------------------------------------------------

-- This function is not written in good Haskell style
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- We can rewrite in a more abstract way
fun2' :: Integer -> Integer
fun2' = sum
        . filter even
        . takeWhile (/= 1)
        . iterate (\n -> if odd n then 3*n + 1 else div n 2)
