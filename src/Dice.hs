module Dice where

import Data.Char (chr)

data Dice =
  One | Two | Three | Four | Five | Six
  deriving (Eq, Ord) -- Enum not necessary?
instance Show Dice where
  show d = [chr $ 9855 + diceValue d]
  -- show One   = "\9856"
  -- show Two   = "\9857"
  -- show Three = "\9858"
  -- show Four  = "\9859"
  -- show Five  = "\9860"
  -- show Six   = "\9861"

-- diceValue :: Dice -> Integral
diceValue :: Dice -> Int
diceValue One   = 1
diceValue Two   = 2
diceValue Three = 3
diceValue Four  = 4
diceValue Five  = 5
diceValue Six   = 6
