-- for defining blackjack strategies

module Strategy where

import Card
import Blackjack
import Data.Foldable (traverse_)

-- given dealer's shown card and player's hand, return decision from basic strategy
--   for 4/6/8 decks, dealer hits soft 17 (H17), double after split allowed, no surrender
basicStrategy :: Rank -> Hand -> Action
basicStrategy dc ph = strat dc (count ph) where
  strat
    | canSplit ph   = pairStrategy
    | isSoft ph     = softStrategy
    | otherwise     = hardStrategy

-- return the strategy given a dealer's card and a player's count
hardStrategy :: Rank -> Int -> Action
hardStrategy drank pc
     -- always stand hard 17
  | pc >= 17    = Stand
    -- have to hit on an 8 or lower
  | pc <= 8     = Hit
  | pc == 9     = if dc `elem` [3..6] then Double else Hit
  | pc == 10    = if dc <= 9 then Double else Hit
     -- always double on 11
  | pc == 11    = Double
  | pc == 12    = if dc `elem` [4..6] then Stand else Hit
  | otherwise   = if dc <= 6 then Stand else Hit     -- for pc in [13..16]
  where dc = rankCount drank

softStrategy :: Rank -> Int -> Action
softStrategy dcard pc
   -- still gotta stand a 20 or greater
  | pc >= 20    = Stand
 
pairStrategy :: Rank -> Int -> Action
pairStrategy dcard pc
  | pc `mod` 2 /= 0    = error "pair not divisible by 2"
  | otherwise          = hardStrategy dcard pc


printHardStrategy :: IO ()
printHardStrategy = do
  putStrLn " PH || 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | T | A "
  let dealerUpRanks = [(Two)..(Ten)] ++ [Ace]
  let playerCounts = [8..17]
  -- print dealerUpRanks
  -- print playerCounts
  traverse_ printHardLine playerCounts


-- printel :: String -> Action -> String
-- printel pref act = pref ++ "| " ++ (show act) ++ " "

printHardLine :: Int -> IO ()
printHardLine pc = do
  let dealerUpRanks = [(Two)..(Ten)] ++ [Ace]                        :: [Rank]
  let stratList = hardStrategy <$> dealerUpRanks <*> [pc]            :: [Action]
  let baseStr = (if pc >= 10 then " " else "  ") ++ show pc ++ " |"  :: String
  let strLine = foldl printel baseStr stratList
  putStrLn strLine
  where printel pref act = pref ++ "| " ++ (show act) ++ " "  :: String

-- showRank :: Rank -> String
-- showRank r
--   | r `elem` [(Ten)..(King)]   = "T"
--   | otherwise                  = show r
  
