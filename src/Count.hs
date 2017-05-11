-- module for blackjack card-counting functions
-- {-# LANGUAGE ScopedTypeVariables #-} -- doesn't seem to have the desired effect?

module Count where

import Card
import Blackjack
import Data.List (sort, nub)
import Control.Monad.Random

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
  | 2*k > n    = choose n (n-k)
  | otherwise  = (n - k + 1) * (choose n (k-1)) `div` k

-- -- takes first N cards that satisfy a function in a (shuffled) deck
-- -- and return them with the part of the deck leftover
-- takeFirstNThat :: Int -> (Card -> Bool) -> [Deck] -> [Deck]
-- takeFirstNThat n f deck =
-- -- partition p xs == (filter p xs, filter (not . p) xs)

-- lists all possible counts the particular count type can result in for this deck
-- possibly not usefull 
possCounts :: (Ord a, Num a) => (Card -> a) -> [Card] -> [a]
possCounts countFunc deck = possCounts where
  possCounts = sort . nub $ map countFunc deck

-- returns the total count for a whole deck, which may not be 0 for unbalanced counting functions
totalDeckCount :: (Num a) => (Card -> a) -> [Card] -> a
totalDeckCount countFunc deck = sum $ map countFunc deck
-- totalDeckCount = sum . map 

-- this version saves possible counts, along with number of cards providing that count
possCountsWithMax :: (Ord a, Num a) => (Card -> a) -> [Card] -> [(a,Int)]
possCountsWithMax countFunc deck = zip possibleCounts countsList
  where
    deckCounts = map countFunc deck
    possibleCounts = sort $ nub deckCounts
    fList = map numCountsInDeck possibleCounts --  :: (Ord a, Num a) => [a -> Int]
    numCountsInDeck ct dk = length $ filter (== ct) dk 
    countsList = sequence fList deckCounts

-- return possible sets of numbers of each class of card counts, given a total number of cards and a deck count
possCountSetsFromCountsWithMax :: (Eq a, Num a) => Int -> a -> [(a,Int)] -> [[Int]] -- return list of possible quantities of each card class
-- so if the possible counts are [-1,0,1] it will return e.g. ([[3,4,2],...[Nm,Nz,Np]...] where [Nm,Nz,Np] satisty Nm + Nz + Np = nCards and Np - Nm - nCount
-- will need to keep track of counts and max separately, so they are not returned here
possCountSetsFromCountsWithMax nCards nCount countWithMaxList = countNumSets where
  (countList,countMaxList) = unzip countWithMaxList                              -- :: ([a],[Int])
  -- list of length (length possCounts) where each element is a list from 0 to the max allowed for the corresponding count
  countSpread = enumFromTo 0 <$> countMaxList                                       :: [[Int]]
  -- list of lists of length (length possCounts) where [0,3,4] refers to 0 cards of 1st count, 3 cards of 2nd count, etc.
  possResNoConstraint = sequence countSpread                                        :: [[Int]]
  countNumSets = filter countCheckFunc $ filter numCheckFunc possResNoConstraint    :: [[Int]]
    where
      numCheckFunc   = (nCards ==) . sum                                                :: [Int] -> Bool
      countCheckFunc = (nCount ==) . sum . zipWith (*) countList . map fromIntegral     :: [Int] -> Bool

-- given list of maximum numbers of each count, and a list of possible sets of counts, provide a list of possible sets of counts with their weights (determined 
possCountSetsWithWeightsFromMaxAndSets :: [Int] -> [[Int]] -> [(Double,[Int])]
possCountSetsWithWeightsFromMaxAndSets maxCounts setsOfCounts = weightFunc maxCounts <$> setsOfCounts
  where
    -- takes list of max counts, possible set of count numbers, and returns weight with the number of counts
    weightFunc :: [Int] -> [Int] -> (Double, [Int])
    weightFunc ctMaxes numsOfCts = (product $ zipWith wfHelp ctMaxes numsOfCts, numsOfCts)
      where
        wfHelp :: Int -> Int -> Double
        -- we need to weight each choice of count numbers by the product of (nMax choose n) for each count possibility
        wfHelp maxCountNum countNum = fromInteger $ choose (fromIntegral maxCountNum) (fromIntegral countNum)

-- this is the function intended for external use.
-- takes a counting function, number of cards, intended count, and deck,
-- returning both the set of possible card counts and a list of sets of numbers of cards w/ each count, along with the weights for each set of possibilities
possCountSetsWithWeights :: (Num a, Ord a) => ((Card) -> a) -> Int -> a -> [Card] -> ([a],[(Double,[Int])])
possCountSetsWithWeights countFunc nCards nCount deck = result where
  countsWithMax = possCountsWithMax countFunc deck
  (possCounts, maxCounts) = unzip countsWithMax
  setsOfCounts = possCountSetsFromCountsWithMax nCards nCount countsWithMax
  countSetsW = possCountSetsWithWeightsFromMaxAndSets maxCounts setsOfCounts
  result = (possCounts, countSetsW)

-- get a random element given the weights, using the global random number generator
-- could use Control.Monad.Random.Class.getRandomR or System.Random.randomRIO
-- seems to work for floating w, but not integral
weightedDraw :: (Random w, Num w, Ord w) => [(w,a)] -> IO a
weightedDraw source = do
  let randRange = (0,foldl (+) 0 $ map fst source) --  :: (w,w)
  -- let randRange = foldl (\x -> (+) (fst x)) 0 source
  wtThresh <- getRandomR randRange  --    :: IO w
  let result = itFunc wtThresh 0 source
  return result
    where
      -- itFunc :: w -> [(w,a)] -> a
      itFunc wtThresh cumulWeight ((wt,val):ls)
        | newWeight > wtThresh  = val
        | otherwise             = itFunc wtThresh newWeight ls
        where newWeight = cumulWeight + wt
      itFunc _ _ [] = error "Ran out of weight vals. Might need to just roll the whole function again. Possibly Integral vs. Floating issue"
