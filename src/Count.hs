-- module for blackjack card-counting functions

module Count where

import Card
import Blackjack

deckCount :: (Card -> Int) -> [Card] -> Int
-- this should be implemented in terms of folds?
deckCount countFunc deck = sum $ fmap countFunc deck

-- the most basic counting system
countHiLo :: Card -> Int
countHiLo (Card rank _)
  | rank <= Six   =  1
  | rank >= Ten   = -1
  | otherwise     =  0

-- the only other system that is balanced and level 1
countHiOpt1 :: Card -> Int
countHiOpt1 card
  | rank card == Two  = 0
  | rank card == Ace  = 0
  | otherwise         = countHiLo card

-- level 2, balanced
countHiOpt2 :: Card -> Int
countHiOpt2 (Card cr _)
  | cr `elem` [Two, Three, Six, Seven]   =  1
  | cr `elem` [Four, Five]               =  2
  | cr `elem` [Eight, Nine]              =  0
  | otherwise                            = -2
  
-- unbalanced, part of an easier system (running count only?)
countKO :: Card -> Int
countKO (Card cr _)
  | cr <= Seven =  1
  | cr >= Ten   = -1
  | otherwise   =  0

-- this one is fun because it depends on the color of the seven
countRed7 :: Card -> Int
countRed7 (Card cr cs)
  | cr == Seven   = if elem cs [Heart, Diamond] then 1 else 0
  | otherwise     = countHiLo (Card cr cs)
