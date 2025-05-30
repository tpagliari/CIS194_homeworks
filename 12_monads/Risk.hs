module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random :: RandomGen g => g -> (DieValue, g)
  random  = first DV . randomR (1,6)

  randomR :: RandomGen g => (DieValue, DieValue) -> g -> (DieValue, g)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

-- Multiple dice roll
nDice :: Int -> Rand StdGen [DieValue]
nDice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Number of dice rolls for attack army
atkDice :: Army -> Int
atkDice n
    | n >= 4 = 3
    | otherwise = n - 1

-- Number of dice rolls for def army
defDice :: Army -> Int
defDice n
    | n >= 2 = 2
    | otherwise = n

