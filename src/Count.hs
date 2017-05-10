-- module for blackjack card-counting functions

module Count where

import Card
import Blackjack
import Data.List (sort, nub)

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
  | otherwise  = fromIntegral(n - k + 1) * (choose n (k-1)) `div` (fromIntegral k)

-- -- takes first N cards that satisfy a function in a (shuffled) deck
-- -- and return them with the part of the deck leftover
-- takeFirstNThat :: Int -> (Card -> Bool) -> [Deck] -> [Deck]
-- takeFirstNThat n f deck =
-- -- partition p xs == (filter p xs, filter (not . p) xs)

-- lists all possible counts the particular count type can result in for this deck
possibleCounts :: (Ord a, Num a) => (Card -> a) -> [Card] -> [a]
possibleCounts countFunc deck = possCounts where
  possCounts = sort . nub $ countFunc <$> deck

-- this version saves possible counts, along with number of cards providing that count
possibleCountsWithMax :: (Ord a, Num a) => (Card -> a) -> [Card] -> [(a,Int)]
possibleCountsWithMax countFunc deck = zip possCounts countsList where
  deckCounts = countFunc <$> deck
  possCounts = sort $ nub deckCounts
  fList = numCountsInDeck <$> possCounts --  :: (Ord a, Num a) => [a -> Int]
  numCountsInDeck ct dk = length $ filter (== ct) dk 
  countsList = sequence fList deckCounts

-- return possible sets of numbers of each class of card counts, given a total number of cards and a deck count
possibleCountSetsWithWeights :: (Eq a, Num a, Fractional b) => Int -> a -> [(a,Int)] -> [(b,[Int])] -- return list of possible quantities of each card class
-- so if the possible counts are [-1,0,1] it will return e.g. ([[3,4,2],...[Nm,Nz,Np]...] where [Nm,Nz,Np] satisty Nm + Nz + Np = nCards and Np - Nm - nCount
-- will need to keep track of counts and max separately, so they are not returned here
possibleCountSetsWithWeights nCards nCount countWithMaxList = result where
  countList    = fst <$> countWithMaxList      -- :: [a]
  countMaxList = snd <$> countWithMaxList                                          :: [Int]
  -- list of length (length possCounts) where each element is a list from 0 to the max allowed for the corresponding count
  countSpread = enumFromTo 0 <$> countMaxList                                       :: [[Int]]
  -- list of lists of length (length possCounts) where [0,3,4] refers to 0 cards of 1st count, 3 cards of 2nd count, etc.
  possResNoConstraint = sequence countSpread                                        :: [[Int]]
  countNumSets = filter countCheckFunc $ filter numCheckFunc possResNoConstraint    :: [[Int]]
  numCheckFunc   = (nCards ==) . sum                                                :: [Int] -> Bool
  countCheckFunc = (nCount ==) . sum . zipWith (*) countList . map fromIntegral     :: [Int] -> Bool
  result = weightFunc countMaxList <$> countNumSets
  weightFunc :: (Fractional b) => [Int] -> [Int] -> (b, [Int]) -- takes list of max counts, possible set of count numbers, and returns weight with the number of counts
  weightFunc ctMaxes numsOfCts = (product $ zipWith wfHelp ctMaxes numsOfCts, numsOfCts)
  wfHelp :: (Fractional b) => Int -> Int -> b
  -- we need to weight each choice of count numbers by the product of (nMax choose n) for each count possibility
  wfHelp maxCountNum countNum = recip . fromInteger $ choose (fromIntegral maxCountNum) (fromIntegral countNum)
