module Scrabble where
import Data.Char (toLower)

newtype Score = Score Int
    deriving (Show, Eq, Num, Ord)

instance Semigroup Score where
    (<>) :: Score -> Score -> Score
    (<>) = (+)
instance Monoid Score where
    mempty :: Score
    mempty = Score 0

-- Function that implements the tile scoring values as shown at
-- http://www.thepixiepit.co.uk/scrabble/rules.html; any characters
-- not mentioned (punctuation, spaces, etc.) should be given zero points.
score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where c' = toLower c

scoreString :: String -> Score
scoreString = sum . map score -- sum works with any type that is an instance of Num