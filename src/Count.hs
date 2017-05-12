-- module for blackjack card-counting functions
-- {-# LANGUAGE ScopedTypeVariables #-} -- doesn't seem to have the desired effect?

module Count where

import Card
import Blackjack
import Deck
import Data.List (sort, nub, partition)
import Data.Ratio
import Control.Monad.Random
import System.Random.Shuffle

class (Num a, Ord a) => CtT a
instance CtT Int -- could also have fractional counts (some systems use 0.5), but Int should cover most cases

-- type CtResT = CtT a => [a]
-- this is [-1,0,1] for Level 1 counting systems

type CtSetT = [Int]
-- indicates a possible set of numbers for each count.
-- e g. [12,8,6], which for a count result of [-1,0,1] represents having 26 cards with a count of -6

-- Num: because the count needs to use (+)
deckCount :: CtT a => (Card -> a) -> [Card] -> a
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
  | otherwise                            = -2   -- 10s and As
  
-- Knockout system: unbalanced, easier since it uses running count only
countKO :: Card -> Int
countKO (Card cr _)
  | cr <= Seven =  1
  | cr >= Ten   = -1
  | otherwise   =  0

countOmega2 :: Card -> Int
countOmega2 card
  | cr == Six  = 2
  | cr == Nine = -1
  | otherwise  = countHiOpt2 card
  where cr = rank card

countZen :: Card -> Int
countZen card
  | cr == Six  = 2
  | cr == Ace  = -1
  | otherwise  = countHiOpt2 card
  where cr = rank card
  
-- this one is fun because it depends on the color of the seven
countRed7 :: Card -> Int
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


-- lists all possible counts the particular count type can result in for this deck
-- possibly not usefull 
possCounts :: CtT a => (Card -> a) -> [Card] -> [a]
possCounts countFunc deck = possCounts where
  possCounts = sort . nub $ map countFunc deck

-- returns the total count for a whole deck, which may not be 0 for unbalanced counting functions
totalDeckCount :: CtT a => (Card -> a) -> [Card] -> a
totalDeckCount countFunc deck = sum $ map countFunc deck
-- totalDeckCount = sum . map 

-- this version saves possible counts, along with number of cards providing that count
possCountsWithMax :: CtT a => (Card -> a) -> [Card] -> [(a,Int)]
possCountsWithMax countFunc deck = zip possibleCounts countsList
  where
    deckCounts = map countFunc deck
    possibleCounts = sort $ nub deckCounts
    fList = map numCountsInDeck possibleCounts --  :: (Ord a, Num a) => [a -> Int]
    numCountsInDeck ct dk = length $ filter (== ct) dk 
    countsList = sequence fList deckCounts

possCountSetsFromMax :: [Int] -> [CtSetT]
possCountSetsFromMax countMaxList = possResNoConstraint where
  -- list of length (length possCounts) where each element is a list from 0 to the max allowed for the corresponding count
  countSpread = enumFromTo 0 <$> countMaxList                                       :: [[Int]]
  -- list of lists of length (length possCounts) where [0,3,4] refers to 0 cards of 1st count, 3 cards of 2nd count, etc.
  possResNoConstraint = sequence countSpread                                        :: [CtSetT]

-- get result of count from a set of numbers of each count
countResult :: CtT a => [a] -> CtSetT -> a
countResult possCounts numCounts = sum . zipWith (*) possCounts $ map fromIntegral numCounts

-- return possible sets of numbers of each class of card counts, given a total number of cards and a deck count
possCountSetsFromCountsWithMax :: CtT a => Int -> a -> [(a,Int)] -> [CtSetT] -- return list of possible quantities of each card class
-- so if the possible counts are [-1,0,1] it will return e.g. ([[3,4,2],...[Nm,Nz,Np]...] where [Nm,Nz,Np] satisty Nm + Nz + Np = nCards and Np - Nm - nCount
-- will need to keep track of counts and max separately, so they are not returned here
possCountSetsFromCountsWithMax nCards nCount countWithMaxList = countNumSets where
  (countList,countMaxList) = unzip countWithMaxList                              -- :: ([a],[Int])
  possResNoConstraint = possCountSetsFromMax countMaxList                           :: [CtSetT]
  countNumSets = filter countCheckFunc $ filter numCheckFunc possResNoConstraint    :: [CtSetT]
    where
      numCheckFunc   = (nCards ==) . sum                                            :: [Int] -> Bool
      countCheckFunc = (nCount ==) . (countResult countList)                        :: [Int] -> Bool

decoratePre :: (a -> b) -> [a] -> [(b,a)]
decoratePre func (x:xs) = (func x, x):(decoratePre func xs)
decoratePre _ [] = []

decorate :: (a -> b) -> [a] -> [(a,b)]
decorate func (x:xs) = (x, func x):(decorate func xs)
decorate _ [] = []

-- takes list of max counts, possible set of count numbers, and returns weight with the number of counts
weightFunc :: [Int] -> CtSetT -> Integer
weightFunc ctMaxes numsOfCts = product $ zipWith wfHelp ctMaxes numsOfCts
  where
    wfHelp :: Int -> Int -> Integer
      -- we need to weight each choice of count numbers by the product of (nMax choose n) for each count possibility
    wfHelp maxCountNum countNum = choose (fromIntegral maxCountNum) (fromIntegral countNum)


-- given list of maximum numbers of each count, and a list of possible sets of counts, provide a list of possible sets of counts with their weights (determined 
possCountSetsWithWeightsFromMaxAndSets :: [Int] -> [CtSetT] -> [(CtSetT,Integer)]
possCountSetsWithWeightsFromMaxAndSets maxCounts setsOfCounts = decorate (weightFunc maxCounts) setsOfCounts

-- takes a counting function, number of cards, intended count, and deck,
-- returning both the set of possible card counts and a list of sets of numbers of cards w/ each count, along with the weights for each set of possibilities
possCountSetsWithWeights :: CtT a => (Card -> a) -> Int -> a -> [Card] -> ([a],[(CtSetT,Integer)])
possCountSetsWithWeights countFunc nCards nCount deck = result where
  countsWithMax = possCountsWithMax countFunc deck
  (possCounts, maxCounts) = unzip countsWithMax
  setsOfCounts = possCountSetsFromCountsWithMax nCards nCount countsWithMax
  countSetsW = possCountSetsWithWeightsFromMaxAndSets maxCounts setsOfCounts
  result = (possCounts, countSetsW)


-- the probability of having a deck w/ length N is effectively set by the casino.
-- most casinos kick the shoe when it's down to 25% of its maximum length
probN :: [Card] -> Int -> Rational
probN deck n = (normFact *) . fromIntegral $ likeF n where
  normFact = (%) 1 $ fromIntegral . sum $ map likeF [1..dkSz]
  likeF = likeN deck
  likeN deck n
    | n <= dkSz `div` 4  = 0
    | n >  dkSz          = 0
    | otherwise          = 1
  dkSz = length deck

-- P(C|N) = L(N|C) / sum_C L(N|C)
-- the form of this computation is robust in that is doesn't rely on P(N).
--  given N of a shuffled deck, the probability of C is proportional to number of decks w/ count C
-- just count all possible decks with count C of size N, divide that by possible decks of size N
probCCondN :: (CtT a) => (Card -> a) -> [Card] -> Int -> a -> Rational
probCCondN countFunc deck n c = result where
  possCtsMax = possCountsWithMax countFunc deck   -- :: [(a,Int)]
  (possCts,ctMaxes) = unzip possCtsMax
  possCtSetsWithN = filter ((n ==) . sum) possCtSetsUnfiltered :: [CtSetT]
    where
      possCtSetsUnfiltered = possCountSetsFromMax ctMaxes
  possCtSetsWithNC = filter ((c ==) . (countResult possCts)) possCtSetsWithN                       :: [[Int]]
  foldFunc = (+) . wFunc                                  :: CtSetT -> Integer -> Integer
    where
      wFunc = weightFunc ctMaxes                          :: CtSetT -> Integer
  likeCAndN = foldr foldFunc 0 possCtSetsWithNC           :: Integer -- L(C|N)
  likeN  = foldr foldFunc 0 possCtSetsWithN               :: Integer -- sum_N L(C|N)
  result = (fromInteger likeCAndN) / (fromInteger likeN)  :: (Fractional p) => p


-- probability of C, given P(N)
-- P(C) = sum_N P(C|N)*P(N)
-- must use this formula as opposed to counting possible decks, since P(N) is effectively set by casino
probC :: (CtT a) => (Card -> a) -> [Card] -> a -> Rational
probC countFunc deck c = sum ls where
  possNs = filter goodN allNs     :: [Int]
    where
      allNs = enumFromTo 1 $ length deck      :: [Int]
    -- filter out possible Ns that aren't allowed so we don't call the expensive P(C|N) more than necessary
      goodN = (0<) . (probN deck)    :: Int -> Bool
  ls = [ (probCCondN countFunc deck n c) * (probN deck n) | n <- possNs]


-- P(N|C) = P(C|N)*P(N)/P(C)      /= num (C && N) / num (C)
probNCondC :: CtT a => (Card -> a) -> [Card] -> a -> Int -> Rational
probNCondC countFunc deck c n = pC_N * p_N / p_C where
  p_N  = probN deck n
  pC_N = probCCondN countFunc deck n c
  p_C  = probC countFunc deck c

-- -- fromList basically has the functionality of these weightedDraw functions
-- -- get a random element given the weights, using the global random number generator
-- -- could use Control.Monad.Random.Class.getRandomR or System.Random.randomRIO
-- weightedIntDraw :: (Random w, Integral w) => [(w,a)] -> IO a
-- weightedIntDraw source = do
--   let randRange = (0, pred . foldl (+) 0 $ map fst source) --  :: (w,w)
--   -- let randRange = foldl (\x -> (+) (fst x)) 0 source
--   wtThresh <- evalRandIO $ getRandomR randRange  --    :: IO w
--   let result = itFunc wtThresh 0 source
--   return result
--     where
--       -- itFunc :: w -> [(w,a)] -> a
--       itFunc wtThresh cumulWeight ((wt,val):ls)
--         | newWeight > wtThresh  = val
--         | otherwise             = itFunc wtThresh newWeight ls
--         where newWeight = cumulWeight + wt
--       itFunc _ _ [] = error "Ran out of weight vals. Should not happen in Integral implementation."

-- weightedFloatDraw :: (Random w, Ord w, Fractional w, Show w) => [(w,a)] -> IO a
-- weightedFloatDraw source = do
-- --  let randRange = (0, foldl (+) 0 $ map fst source) --  :: (w,w) -- sum is implemented w/ foldl anyway
--   let randRange = (0, sum $ map fst source) --  :: (w,w)
--   -- let randRange = foldl (\x -> (+) (fst x)) 0 source
--   --wtThresh <- evalRandIO $ getRandomR randRange  --    :: IO w
--   wtThresh <- getRandomR randRange  --    :: IO w
--   -- putStrLn $ "rng gives " ++ show wtThresh
--   -- check if treshhold is at upper bound?
--   let result = itFunc wtThresh 0 source
--   return result
--     where
--       -- itFunc :: w -> [(w,a)] -> a
--       itFunc wtThresh cumulWeight ((wt,val):ls)
--         | newWeight > wtThresh  = val
--         | otherwise             = itFunc wtThresh newWeight ls
--         where newWeight = cumulWeight + wt
--       itFunc wtTh _ [] = error $ "Ran out of weight vals. Is threshold " ++ show wtTh ++ " above upper bound?"

-- uses evalRandIO, which uses the IO monad specifically for RNG
drawNFrom :: Int -> [a] -> IO [a]
drawNFrom n deck = do
  shuffled <- evalRandIO $ shuffleM deck
  return $ take n shuffled


getDeckWithCountSet :: CtT a => (Card -> a) -> [a] -> CtSetT -> [Card] -> IO [Card]
getDeckWithCountSet countFunc possCts ctSet deck = do
  -- possCts = possCounts countFunc deck  -- :: [a]  -- typically [-1,0,1]
  sortedDeck <- sequence [ drawNFrom numCts $ filter ((ct == ) . countFunc) deck | (ct,numCts) <- zip possCts ctSet]  :: IO [[Card]]
  let concatDeck = concat sortedDeck  :: [Card]
  shuffledDeck <- evalRandIO $ shuffleM concatDeck
  return shuffledDeck

-- given total number of cards, counting function, desired count, return shuffled deck
--  possibly pass a stack of cards instead of a number of decks
-- getDeckWithNCount :: (Num a, Ord a, MonadRandom m) => Int -> Int -> (Card -> a) -> a -> m [Card]
getDeckWithCardsAndCount :: CtT a => Int -> [Card] -> (Card -> a) -> a -> IO [Card]
getDeckWithCardsAndCount nCards deck countFunc nCount = do
  -- countSet <- weightedIntDraw weightedCountSets
  countSet <- fromList weightedCountSets :: IO CtSetT
  getDeckWithCountSet countFunc possCts countSet deck
    where
      (possCts,weightedCountSets) = possCountSetsWithWeights countFunc nCards nCount deck

-- returns a deck with total number of cards randomized uniformly, and the count set
-- not actually be appropriate for generating tables based on a count, because P(N|C) is not typically uniform
getDeckWithCount :: CtT a => Int -> (Card -> a) -> a -> IO [Card]
getDeckWithCount nDecks cf count = do
  putStrLn $ show nsWithWeights
--  nCards <- weightedFloatDraw nsWithWeights            :: IO Int
  nCards <- fromList nsWithWeights
  getDeckWithCardsAndCount nCards unshuffled cf count  :: IO [Card]
  -- weightedFloatDraw nsWithWeights >>= (\nCards -> getDeckWithCardsAndCount nCards unshuffled cf count)
    where
      unshuffled = concat $ replicate nDecks standardDeck
      dkSz = length unshuffled                                 :: Int
      possNs = filter ((0< ) . (probN unshuffled))  [1..dkSz]  :: [Int]
      -- P(N|C) is proportional to P(C|N)*P(N). Denominator P(C) is a constant (given count).
      -- real probability would be probNCondC but we can skip the additional computation
      nsWithWeights = decorate (\n -> probCCondN cf unshuffled n count) possNs  :: [(Int,Double)]

