-- for defining blackjack strategies

module Strategy where

import Card
import Blackjack
import Data.Foldable (traverse_)

-- given dealer's shown card and player's hand, return decision from basic strategy
--   for 4/6/8 decks, dealer hits soft 17 (H17), double after split allowed, no surrender
basicStrategy :: Rank -> Hand -> Action
basicStrategy dc ph
  | canSplit ph   = pairStrategy dc . rank $ head ph
  | isSoft ph     = softStrategy dc $ count ph 
  | otherwise     = hardStrategy dc $ count ph
-- basicStrategy dc ph = strat dc (count ph) where
--   strat
--     | canSplit ph   = pairStrategy
--     | isSoft ph     = softStrategy
--     | otherwise     = hardStrategy

-- return the strategy given a dealer's card and a player's count
hardStrategy :: Rank -> Int -> Action
hardStrategy drank pc
  | pc == 11                     = Double
  | pc == 10 && dc <= 9          = Double
  | pc == 9  && dc `elem` [3..6] = Double
  | otherwise                    = hardStrategyNoDD drank pc
  where dc = rankCount drank

-- hard strategy when no doubling is allowed
hardStrategyNoDD :: Rank -> Int -> Action
hardStrategyNoDD drank pc
  | pc >= 17    = Stand
  | pc <= 11     = Hit
  | pc == 12    = if dc `elem` [4..6] then Stand else Hit
  | otherwise   = if dc <= 6 then Stand else Hit     -- for pc in [13..16]
  where dc = rankCount drank


softStrategy :: Rank -> Int -> Action
softStrategy dr pc
  | dr == Six  &&  pc `elem` [13..19] = Double
  | dr == Five &&  pc `elem` [13..18] = Double
  | dr == Four &&  pc `elem` [15..18] = Double
  | dr == Three && pc `elem` [17,18]  = Double
  | dr == Two  &&  pc == 18           = Double
  | otherwise                         = softStrategyNoDD dr pc

softStrategyNoDD :: Rank -> Int -> Action
softStrategyNoDD drank pc
  | pc <= 11    = error $ "you can't have a soft " ++ show pc
  | pc <= 17    = Hit
  | pc == 18    = if drank <= Eight then Stand else Hit
  | otherwise   = Stand

  
pairStrategy :: Rank -> Rank -> Action
pairStrategy dr pr
  | pr == Five && dr <= Nine   = Double
  | otherwise                  = pairStrategyNoDD dr pr

pairStrategyNoDD :: Rank -> Rank -> Action
pairStrategyNoDD dr pr
  | pr == Ace          = Split
  | pc == 10           = Stand
  | pr == Nine         = if dc `elem` [7,10,11] then Stand else Split
  | pr == Eight        = Split
  | pc `elem` [2,3,7]  = if dc <= 7 then Split else Hit
  | pc == 6            = if dc <= 6 then Split else Hit
  | pc == 5            = Hit
  | pc == 4            = if dc `elem` [5,6] then Split else Hit
  | otherwise          = hardStrategy dr pc -- placeholder
  where pc = rankCount pr
        dc = rankCount dr


showRank :: Rank -> String
showRank r
  | r `elem` [(Ten)..(King)]   = "T"
  | otherwise                  = show r
  

printHardStrategy :: IO ()
printHardStrategy = do
  putStrLn . (++) "    |" . concat $ pad . showRank <$> dealerUpRanks
  traverse_ (printHardSoftLine hardStrategy) playerCounts
  where
    dealerUpRanks = [(Two)..(Ten)] ++ [Ace]
    playerCounts = [8..17]
    pad str = "| " ++ str ++ " "   -- pads the output character

printSoftStrategy :: IO ()
printSoftStrategy = do
  putStrLn . (++) "    |" . concat $ pad . showRank <$> dealerUpRanks
  traverse_ (printHardSoftLine softStrategy) playerCounts
  where
    dealerUpRanks = [(Two)..(Ten)] ++ [Ace]
    playerCounts = [13..20]
    pad str = "| " ++ str ++ " "   -- pads the output character


printHardSoftLine :: (Rank -> Int -> Action) -> Int -> IO ()
printHardSoftLine strat pc = do
  let dealerUpRanks = [(Two)..(Ten)] ++ [Ace]                        :: [Rank]
  let stratList = strat <$> dealerUpRanks <*> [pc]                   :: [Action]
  let baseStr = (if pc >= 10 then " " else "  ") ++ show pc ++ " |"  :: String
  let strLine = foldl printel baseStr stratList
  putStrLn strLine
  where printel pref act = pref ++ "| " ++ (show act) ++ " "  :: String


printPairStrategy :: IO ()
printPairStrategy = do
  putStrLn . (++) "    |" . concat $ pad . showRank <$> ranks
  traverse_ printPairLine ranks
  where
    ranks = [(Two)..(Ten)] ++ [Ace]
    pad str = "| " ++ str ++ " "   -- pads the output character

printPairLine :: Rank -> IO ()
printPairLine pc = do
  let dealerUpRanks = [(Two)..(Ten)] ++ [Ace]                        :: [Rank]
  let stratList = pairStrategy <$> dealerUpRanks <*> [pc]                   :: [Action]
  let baseStr =  " " ++ showRank pc ++ showRank pc ++ " |"  :: String
  let strLine = foldl printel baseStr stratList
  putStrLn strLine
  where printel pref act = pref ++ "| " ++ (show act) ++ " "  :: String
