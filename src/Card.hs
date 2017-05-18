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
instance Show Rank where
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"
  show r     = show $ fromEnum r + 2 -- Two = "2", Three = "3", etc
instance Read Rank where
  readsPrec _ (r:rs) = if r `elem` ['2'..'9'] ++ ['T','J','Q','K','A']
                       then [(rmap r,rs)]
                       else []
                         where rmap 'T' = Ten
                               rmap 'J' = Jack
                               rmap 'Q' = Queen
                               rmap 'K' = King
                               rmap 'A' = Ace
                               rmap x = toEnum ((read [x])-2)
  readsPrec _ [] = []
  -- ReadS [a] = String -> [(a,String)]
  -- readList :: ReadS [a]   == String -> [(a,String)]
  -- this needs a real implementation. needs to map "8T3" to [([Eight,Ten,Three],"")]
  readList (r:rs) = readIter $ read [r] :: [([Rank],String)]
    where
      readIter :: [([Rank],String)] -> [([Rank],String)]
      readIter [(rh,"")] = [(rh,"")]
      readIter [(rh,remain)] = [(rh',remain')]
        where
          rh' = rh ++ nr
          [(nr,remain')] = readList remain
      readIter [] = error "shouldn't get to here?"

  
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
        Queen -> 13  -- 12 is the Knave -- not used in the typical French deck
        King  -> 14
        Ace   ->  1
        _     -> fromEnum r + 2 -- assign Two = 2, Three = 3, etc
      smod = case s of
        Club    ->  3*16
        Diamond ->  2*16
        Heart   ->  1*16
        Spade   ->  0*16
    in ( chr $ 127136 + rmod + smod ) : " "
  showList (r:rs) x = show r ++ (showList rs x)
  showList [] x = x

-- making cards well-ordered will let us sort in a well-defined manner,
-- which will likely be important for comparing hands.
-- We can use sortBy to provide our own sorting function, so there's no need to force Card to be Ord
orderCard :: Card -> Card -> Ordering
orderCard a b = if rank a == rank b then compare (suit a) (suit b) else compare (rank a) (rank b)
