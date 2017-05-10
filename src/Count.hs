-- module for blackjack card-counting functions

module Count where

import Card
import Blackjack
import Data.List (nub)

-- Num: because the count needs to use (+)
deckCount :: Num a => (Card -> a) -> [Card] -> a
-- this should be implemented in terms of folds?
deckCount countFunc deck = sum $ fmap countFunc deck

-- the most basic counting system
countHiLo :: Num a => Card -> a
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
  | otherwise                            = -2   -- 10s and As
  
-- Knockout system: unbalanced, easier since it uses running count only
countKO :: Num a => Card -> a
countKO (Card cr _)
  | cr <= Seven =  1
  | cr >= Ten   = -1
  | otherwise   =  0

-- this one is fun because it depends on the color of the seven
countRed7 :: Num a => Card -> a
countRed7 (Card cr cs)
  | cr == Seven   = if elem cs [Heart, Diamond] then 1 else 0
  | otherwise     = countHiLo (Card cr cs)


-- with Int, has severe underflow for e.g. 400 `choose` 200
choose :: Integer -> Integer -> Integer
choose n k
  | k < 0      = 0
  | k > n      = 0
  | k == 0     = 1
  | k == n     = 1
  | k > (n `div` 2)  = choose n (n-k)
  | otherwise  = (n - k + 1) * (choose n (k-1)) `div` k

-- -- takes first N cards that satisfy a function in a (shuffled) deck
-- -- and return them with the part of the deck leftover
-- takeFirstNThat :: Int -> (Card -> Bool) -> [Deck] -> [Deck]
-- takeFirstNThat n f deck =
-- -- partition p xs == (filter p xs, filter (not . p) xs)

-- lists all possible counts the particular count type can result in for this deck
-- TODO: make version that saves possible counts, along with number of cards providing that count
possibleCounts :: (Eq a, Num a) => (Card -> a) -> [Card] -> [a]
possibleCounts countFunc deck = possCounts where
  possCounts = nub $ countFunc <$> deck
