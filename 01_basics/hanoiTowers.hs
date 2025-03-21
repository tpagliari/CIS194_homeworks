-- The Towers of Hanoi

-- To move n discs (stacked in increasing size) from peg a to peg b
-- using peg c as temporary storage,
-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n src goal tmp
  | n <= 0 = []
  | n == 1 = [(src, goal)]
  | otherwise = hanoi (n-1) src tmp goal ++ [(src, goal)] ++ hanoi (n-1) tmp goal src
