-- Haskell file defining the Card datatype and its dependencies

module Card where

import Data.Char (chr)

data Suit =
  Club | Diamond | Heart | Spade
  deriving (Eq, Ord, Enum)
instance Show Suit where
  show Club = "\9827 "
  show Diamond = "\9830 "
  show Heart = "\9829 "
  show Spade = "\9824 "

data Rank =
  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded)
-- not sure if can define as "int in range 2..10 or J,Q,K,A". for now, brute force
instance Show Rank where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

-- a Card is a combination of Rank and Suit
-- the accessor functions rank and suit will be useful, so the syntax below will be used instead of just
--   data Card = Card Rank Suit
data Card = Card {rank :: Rank, suit :: Suit}
  deriving Eq

-- we can use Unicode symbols to display each card as its own character
instance Show Card where
  show (Card r s) =
    let
      rmod = case r of
        Two   ->  2
        Three ->  3
        Four  ->  4
        Five  ->  5
        Six   ->  6
        Seven ->  7
        Eight ->  8
        Nine  ->  9
        Ten   -> 10
        Jack  -> 11
        Queen -> 13  -- 12 is the Knave -- not used in the typical French deck
        King  -> 14
        Ace   ->  1
      smod = case s of
        Club    ->  3*16
        Diamond ->  2*16
        Heart   ->  1*16
        Spade   ->  0*16
    in ( chr $ 127136 + rmod + smod ) : " "

-- making cards well-ordered will let us sort in a well-defined manner,
-- which will likely be important for comparing hands.
-- We can use sortBy to provide our own sorting function, so there's no need to force Card to be Ord
orderCard :: Card -> Card -> Ordering
orderCard a b = if rank a == rank b then compare (suit a) (suit b) else compare (rank a) (rank b)
