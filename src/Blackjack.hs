
module Blackjack where

import Card
import Deck

-- for shuffleM
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle

import Data.Char (chr)
-- import Data.List (intercalate) -- concat

-- alias for readability
type Hand = [Card]

cardCount :: Card -> Int
cardCount (Card Two   _) = 2
cardCount (Card Three _) = 3
cardCount (Card Four  _) = 4
cardCount (Card Five  _) = 5
cardCount (Card Six   _) = 6
cardCount (Card Seven _) = 7
cardCount (Card Eight _) = 8
cardCount (Card Nine  _) = 9
cardCount (Card Ace   _) = 11
cardCount       _        = 10 -- the rest are face cards

-- returns the hard count, i.e. the count without including soft aces
hardCount :: Hand -> Int
-- hardCount hand = sum $ map cardCount hand
hardCount = sum . map cardCount

numAces :: Hand -> Int
numAces h = length $ filter isAce h
  where
    isAce (Card Ace _) = True
    isAce _            = False

count :: Hand -> Int
count hand = soften (hardCount hand) (numAces hand)
  where soften hc na
          | hc <= 21 || na < 1  = hc
          | otherwise           = soften (hc - 10) (na - 1)

-- is this hand a blackjack?
-- a hand after splitting should not count as a blackjack, so this function should only be used to check the first deal
isBlackjack :: Hand -> Bool
isBlackjack hand = length hand == 2 && hardCount hand == 21

canSplit :: Hand -> Bool
-- canSplit [a,b] = rank a == rank b
-- we want to be able to allow splitting on a J and Q, for instance:
canSplit [a,b] = cardCount a == cardCount b
canSplit _     = False

isBust :: Hand -> Bool
isBust = (21 <) . count

-- returns the number of "unused" aces left
-- softness :: Hand -> Int

-- returns whether there are unused aces left
isSoft :: Hand -> Bool
isSoft hand = (count hand) + 10*(numAces hand) - (hardCount hand) > 0

--- define action type
data Action =
  Hit | Stand | Double | Split -- Insurance ? Surrender ?
  deriving (Eq, Show)

--- determine Dealer's strategy (which is deterministic in Blackjack)
dealerStrategy :: Hand -> Action
dealerStrategy hand
  | count hand > 17   = Stand
    -- will use the optional rule of dealer hitting a soft 17 (it makes the game harder)
  | count hand == 17  = if isSoft hand then Hit else Stand
  | otherwise         = Hit

deal :: Deck -> Hand -> (Deck, Hand)
deal (d:ds) h = (ds, d:h)

-- dealN :: MonadRandom mr => Int -> mr Deck -> mr Hand -> (Deck, Hand)
-- dealN 0 d h = (d, h)
-- dealN n (d:ds) h = dealN (n-1) ds (d:h)

--- at some point we could generalize this to include multiple players
data Table = Table {deck :: Deck, dealerHand :: Hand, playerHand :: Hand, state :: TableState}
-- instance Show Table where
--   show table = "Dealer: " ++ dcs ++ "\n"
--                ++ 

data TableState = PlayerTurn | DealerTurn | Done

printTable :: Table -> IO ()
printTable table = do
  putStrLn $ "Dealer: " ++ d
  putStrLn $ "Player: " ++ p
  putStrLn $ "Deck: " ++ bl ++ "  (" ++ show(length(theDeck)) ++ ")"
  where d = case st of
          PlayerTurn -> bl ++ " " ++ (concat $ show <$> tail(dealerHand table)) :: String
          DealerTurn -> concat $ show <$> dealerHand table :: String
        st = state table
        p =  concat $ show <$> playerHand table :: String
        dk = concat $ show <$> theDeck :: String
        bl = [chr 127136]
        theDeck = deck table

makeTable :: Deck -> Table
makeTable (p1:d1:p2:d2:dr) = Table dr [d2,d1] [p2,p1] PlayerTurn
makeTable _ = error "insufficient cards in deck to make table"

dealPlayer :: Table -> Table
dealPlayer table = Table ds dl (dh:ph) PlayerTurn where
  (dh:ds) = deck table
  dl = dealerHand table
  ph = playerHand table  

dealDealer :: Table -> Table
dealDealer table = Table ds (dh:dl) ph DealerTurn where
  (dh:ds) = deck table
  dl = dealerHand table
  ph = playerHand table

-- placeholder, not implemented yet
-- will likely need player input on player's turn
stepTable :: Table -> IO Table
stepTable (Table (nc:tailDeck) oldDH pH DealerTurn) = return (Table tailDeck newDH pH newState) where
  newDH = nc:oldDH
  newState = if dealerStrategy newDH == Hit then DealerTurn else Done
stepTable (Table oldDeck dH oldPH PlayerTurn) = return (Table newDeck dH newPH newState) where
  newDeck = oldDeck
  newPH = oldPH
  newState = DealerTurn -- right now just have the player stand
  
