module Dice where

import Data.Char (chr)
-- import System.Random

data Dice =
  One | Two | Three | Four | Five | Six
  deriving (Eq, Ord, Enum, Bounded) -- Enum not necessary?
instance Show Dice where
  show d = [chr $ 9855 + diceValue d]
-- -- this instance is "inspired" from a "school of haskell" page on a random coin
-- instance Random Dice where
--   randomR (a, b) g =
--     case randomR (fromEnum a, fromEnum b) g of
--       (x, g') -> (toEnum x, g')
--   random g = randomR (minBound, maxBound) g

-- diceValue :: Dice -> Integral
diceValue :: Dice -> Int
diceValue One   = 1
diceValue Two   = 2
diceValue Three = 3
diceValue Four  = 4
diceValue Five  = 5
diceValue Six   = 6
