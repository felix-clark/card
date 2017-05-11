-- module for blackjack card-counting functions
-- {-# LANGUAGE ScopedTypeVariables #-} -- doesn't seem to have the desired effect?

module Count where

import Card
import Blackjack
import Deck
import Data.List (sort, nub)
import Control.Monad.Random
import System.Random.Shuffle

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
totalDeckCount :: (Num a, Ord a) => (Card -> a) -> [Card] -> a
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

possCountSetsFromMax :: [Int] -> [[Int]]
possCountSetsFromMax countMaxList = possResNoConstraint where
  -- list of length (length possCounts) where each element is a list from 0 to the max allowed for the corresponding count
  countSpread = enumFromTo 0 <$> countMaxList                                       :: [[Int]]
  -- list of lists of length (length possCounts) where [0,3,4] refers to 0 cards of 1st count, 3 cards of 2nd count, etc.
  possResNoConstraint = sequence countSpread                                        :: [[Int]]

-- get result of count from a set of numbers of each count
countResult :: (Num a, Eq a) => [a] -> [Int] -> a
countResult possCounts numCounts = sum . zipWith (*) possCounts $ map fromIntegral numCounts

-- return possible sets of numbers of each class of card counts, given a total number of cards and a deck count
possCountSetsFromCountsWithMax :: (Eq a, Num a) => Int -> a -> [(a,Int)] -> [[Int]] -- return list of possible quantities of each card class
-- so if the possible counts are [-1,0,1] it will return e.g. ([[3,4,2],...[Nm,Nz,Np]...] where [Nm,Nz,Np] satisty Nm + Nz + Np = nCards and Np - Nm - nCount
-- will need to keep track of counts and max separately, so they are not returned here
possCountSetsFromCountsWithMax nCards nCount countWithMaxList = countNumSets where
  (countList,countMaxList) = unzip countWithMaxList                              -- :: ([a],[Int])
  possResNoConstraint = possCountSetsFromMax countMaxList                           :: [[Int]]
  countNumSets = filter countCheckFunc $ filter numCheckFunc possResNoConstraint    :: [[Int]]
    where
      numCheckFunc   = (nCards ==) . sum                                            :: [Int] -> Bool
      countCheckFunc = (nCount ==) . (countResult countList)                        :: [Int] -> Bool

decoratePre :: (a -> b) -> [a] -> [(b,a)]
decoratePre func (x:xs) = (func x, x):(decoratePre func xs)
decoratePre _ [] = []

-- takes list of max counts, possible set of count numbers, and returns weight with the number of counts
weightFunc :: [Int] -> [Int] -> Integer
weightFunc ctMaxes numsOfCts = product $ zipWith wfHelp ctMaxes numsOfCts
  where
    wfHelp :: Int -> Int -> Integer
      -- we need to weight each choice of count numbers by the product of (nMax choose n) for each count possibility
    wfHelp maxCountNum countNum = choose (fromIntegral maxCountNum) (fromIntegral countNum)


-- given list of maximum numbers of each count, and a list of possible sets of counts, provide a list of possible sets of counts with their weights (determined 
possCountSetsWithWeightsFromMaxAndSets :: [Int] -> [[Int]] -> [(Integer,[Int])]
possCountSetsWithWeightsFromMaxAndSets maxCounts setsOfCounts = decoratePre (weightFunc maxCounts) setsOfCounts

-- takes a counting function, number of cards, intended count, and deck,
-- returning both the set of possible card counts and a list of sets of numbers of cards w/ each count, along with the weights for each set of possibilities
possCountSetsWithWeights :: (Num a, Ord a) => ((Card) -> a) -> Int -> a -> [Card] -> ([a],[(Integer,[Int])])
possCountSetsWithWeights countFunc nCards nCount deck = result where
  countsWithMax = possCountsWithMax countFunc deck
  (possCounts, maxCounts) = unzip countsWithMax
  setsOfCounts = possCountSetsFromCountsWithMax nCards nCount countsWithMax
  countSetsW = possCountSetsWithWeightsFromMaxAndSets maxCounts setsOfCounts
  result = (possCounts, countSetsW)

-- get a random element given the weights, using the global random number generator
-- could use Control.Monad.Random.Class.getRandomR or System.Random.randomRIO
weightedIntDraw :: (Random w, Integral w) => [(w,a)] -> IO a
weightedIntDraw source = do
  let randRange = (0, pred . foldl (+) 0 $ map fst source) --  :: (w,w)
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
      itFunc _ _ [] = error "Ran out of weight vals. Should not happen in Integral implementation."

weightedFloatDraw :: (Random w, Ord w, Fractional w, Show w) => [(w,a)] -> IO a
weightedFloatDraw source = do
  let randRange = (0, foldl (+) 0 $ map fst source) --  :: (w,w)
  -- let randRange = foldl (\x -> (+) (fst x)) 0 source
  wtThresh <- getRandomR randRange  --    :: IO w
  -- check if treshhold is at upper bound?
  let result = itFunc wtThresh 0 source
  return result
    where
      -- itFunc :: w -> [(w,a)] -> a
      itFunc wtThresh cumulWeight ((wt,val):ls)
        | newWeight > wtThresh  = val
        | otherwise             = itFunc wtThresh newWeight ls
        where newWeight = cumulWeight + wt
      itFunc wtTh _ [] = error $ "Ran out of weight vals. Is threshold " ++ show wtTh ++ " above upper bound?"

-- uses evalRandIO, which uses the IO monad specifically
drawNRand :: Int -> [a] -> IO [a]
drawNRand n deck = do
  shuffled <- evalRandIO $ shuffleM deck
  return $ take n shuffled

-- given total number of cards, counting function, desired count, return shuffled deck
-- getDeckWithNCount :: (Num a, Ord a, MonadRandom m) => Int -> Int -> (Card -> a) -> a -> m [Card]
getDeckWithCardsAndCount :: (Num a, Ord a) => Int -> Int -> (Card -> a) -> a -> IO [Card]
getDeckWithCardsAndCount nCards nDecks countFunc nCount = return result where
  unshuffled = concat $ replicate nDecks standardDeck
  result = []

-- returns a deck with total number of cards randomized uniformly, and the count set
-- not actually be appropriate for generating tables based on a count, because P(N|C) is not typically uniform
getDeckWithCount :: (Num a, Ord a) => Int -> (Card -> a) -> a -> IO [Card]
getDeckWithCount nDecks cf nc = do
  let dkSz = length standardDeck
  nCards <- getRandomR (nDecks*dkSz `div` 4, nDecks*dkSz)
  getDeckWithCardsAndCount nCards nDecks cf nc


probN :: (Fractional p) => [Card] -> Int -> p
probN deck n = (normFact *) . fromIntegral $ likeF n where
  normFact = recip . fromIntegral . sum $ map likeF [1..dkSz]
  likeF = likeN deck
  likeN deck n
    | n <= dkSz `div` 4  = 0
    | n >  dkSz          = 0
    | otherwise          = 1
  dkSz = length deck

-- P(N|C) = L(C|N) / sum_N L(C|N)  = num (C && N) / num (C)
-- should be careful with this def : it ignores P(N)
probNCondC :: (Num a, Ord a, Fractional p) => (Card -> a) -> [Card] -> a -> Int -> p
probNCondC countFunc deck c n = result where
  possCtsMax = possCountsWithMax countFunc deck   -- :: [(a,Int)]
  (possCts,ctMaxes) = unzip possCtsMax
  possCtSetsWithC = filter ((c ==) . (countResult possCts)) possCtSetsUnfiltered :: [[Int]]
    where
      possCtSetsUnfiltered = possCountSetsFromMax ctMaxes
  possCtSetsWithCN = filter ((n ==) . sum) possCtSetsWithC                       :: [[Int]]
  foldFunc = (+) . wFunc                  :: [Int] -> Integer -> Integer
    where
      wFunc = weightFunc ctMaxes          :: [Int] -> Integer
  likeNAndC = foldr foldFunc 0 possCtSetsWithCN                   :: Integer -- L(C|N)
  likeC     = foldr foldFunc 0 possCtSetsWithC                 :: Integer -- sum_N L(C|N)
  result = (fromInteger likeNAndC) / (fromInteger likeC)  :: (Fractional p) => p

-- P(C|N) = L(N|C) / sum_C L(N|C)
-- the form of this computation is robust in that is doesn't rely on P(N).
--  given N of a shuffled deck, the probability of C is proportional to number of decks w/ count C
-- just count all possible decks with count C of size N, divide that by possible decks of size N
probCCondN :: (Num a, Ord a, Fractional p) => (Card -> a) -> [Card] -> Int -> a -> p
probCCondN countFunc deck n c = result where
  possCtsMax = possCountsWithMax countFunc deck   -- :: [(a,Int)]
  (possCts,ctMaxes) = unzip possCtsMax
  possCtSetsWithN = filter ((n ==) . sum) possCtSetsUnfiltered :: [[Int]]
    where
      possCtSetsUnfiltered = possCountSetsFromMax ctMaxes
  possCtSetsWithNC = filter ((c ==) . (countResult possCts)) possCtSetsWithN                       :: [[Int]]
  foldFunc = (+) . wFunc                                  :: [Int] -> Integer -> Integer
    where
      wFunc = weightFunc ctMaxes                          :: [Int] -> Integer
  likeCAndN = foldr foldFunc 0 possCtSetsWithNC           :: Integer -- L(C|N)
  likeN  = foldr foldFunc 0 possCtSetsWithN               :: Integer -- sum_N L(C|N)
  result = (fromInteger likeCAndN) / (fromInteger likeN)  :: (Fractional p) => p

-- probability of C, given P(N)
-- P(C) = sum_N P(C|N)*P(N)
-- must use this formula as opposed to counting possible decks, since P(N) is effectively set by casino
probC :: (Num a, Ord a, Fractional p) => (Card -> a) -> [Card] -> a -> p
probC countFunc deck c = sum ls where
  possNs = filter goodN allNs     :: [Int]
    where
      allNs = enumFromTo 1 $ length deck      :: [Int]
    -- filter out possible Ns that aren't allowed so we don't call the expensive P(C|N) more than necessary
      goodN = (0<) . (probN deck)    :: Int -> Bool
  ls = [ (probCCondN countFunc deck n c) * (probN deck n) | n <- possNs]
