-- data structures for computing the dealer bust rate

module DealerBust where

import Data.List (intercalate) -- don't need "find" anymore -- replaced w/ lookup
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

-- histogram can be implemented with a dictionary
import qualified Data.Map as M

import Card
import Deck
import Blackjack


data Result = Seventeen | Eighteen | Nineteen | Twenty | TwentyOne | Bust
  deriving (Eq,Enum,Ord)
instance Show Result where
  show Seventeen = "17"
  show Eighteen  = "18"
  show Nineteen  = "19"
  show Twenty    = "20"
  show TwentyOne = "21"
  show Bust      = "Bust"

resultFromHand :: Hand -> Result
resultFromHand hand
  | c > 21    = Bust
  | c == 21   = TwentyOne
  | c == 20   = Twenty
  | c == 19   = Nineteen
  | c == 18   = Eighteen
  | c == 17   = Seventeen
  | otherwise = error "dealer must end with at least 17"
  where c = count hand

type TrialHist = M.Map Rank (M.Map Result Integer)

makeEmptyTrialHist :: TrialHist
makeEmptyTrialHist = M.fromList $ (,) <$> bjRanks <*> [results]
  where
    bjRanks = [(Two)..(Ten)] ++ [Ace]                :: [Rank]
    results = M.fromList $ (,) <$> [(Seventeen)..(Bust)] <*> [0]  :: M.Map Result Integer

printTrialHist :: TrialHist -> IO ()
printTrialHist hist = do
  let topLineList = (++) " " . intercalate " | " $ show <$> results
  print topLineList
  traverse_ printLine upCards -- equivalent to: sequence_ $ printLine <$> upCards
  -- putStrLn "\ndump: "
  -- print hist
  where
    upCards = [(Two)..(Ten)] ++ [Ace]
    results = [(Seventeen)..(Bust)]
    printLine upCard = do
      putStrLn . (" " ++) . intercalate " | " . (:) (printUp upCard) $ show . getHistVal hist upCard <$> results
    printUp upc
      | rankCount upc == 10   = "T"
      | otherwise             = show upc

getHistVal :: TrialHist -> Rank -> Result ->  Integer
getHistVal hist dup res = M.findWithDefault 0 res $ M.findWithDefault M.empty dup hist
  

-- fill a trial with a rank,result
--   this will likely be slower than saving a list of results,
--   but will not have a memory scaling problem
incTrialHist :: TrialHist -> Rank -> Result -> TrialHist
incTrialHist hist dup res = M.adjust resMapInc dup hist
  where resMapInc = M.adjust succ res


incTrialHistFromDeck :: TrialHist -> Deck -> (TrialHist, Deck)
incTrialHistFromDeck hist (upcard:deck) = (newHist, newDeck) where
  newHist = incTrialHist hist dr res
  dr = rank upcard
  res = resultFromHand finalHand
  (finalHand, newDeck) = iterateDeck [upcard] deck
  iterateDeck h dk = case dealerStrategy h of
    Hit   -> iterateDeck ((head dk):h) (tail dk)
    Stand -> (h, dk)
      
  
