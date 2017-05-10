-- data structures for computing the dealer bust rate

module DealerResult where

import Data.List (intercalate) -- don't need "find" anymore -- replaced w/ lookup
import Data.Foldable (traverse_)
-- import Data.Maybe (fromMaybe)
import Numeric (showFFloat)

import Control.Monad

-- histogram can be implemented with a dictionary
import qualified Data.Map as M

import Card
import Deck
import Blackjack


data Result = Seventeen | Eighteen | Nineteen | Twenty | TwentyOne | Natural | Bust
  deriving (Eq,Enum,Ord)
instance Show Result where
  show Seventeen = " 17 "
  show Eighteen  = " 18 "
  show Nineteen  = " 19 "
  show Twenty    = " 20 "
  show TwentyOne = " 21 "
  show Natural   = " BJ "
  show Bust      = "Bust"

resultFromHand :: Hand -> Result
resultFromHand hand
  | isBj      = Natural
  | c > 21    = Bust
  | c == 21   = TwentyOne
  | c == 20   = Twenty
  | c == 19   = Nineteen
  | c == 18   = Eighteen
  | c == 17   = Seventeen
  | otherwise = error "dealer must end with at least 17"
  where
    isBj = isBlackjack hand
    c = count hand

type TrialHist = M.Map Rank (M.Map Result Integer)

emptyTrialHist :: TrialHist
-- emptyTrialHist = M.fromList $ (,) <$> bjRanks <*> [results]
-- emptyTrialHist = M.fromList $ ( flip (,) results) <$> bjRanks
emptyTrialHist = M.fromList $ forM bjRanks (,) results
  where
    bjRanks = [(Two)..(Ten)] ++ [Ace]                :: [Rank]
    dres    = [(Seventeen)..(TwentyOne)] ++ [Bust]   :: [Result] -- don't include natural blackjack, since dealer should check
    results = M.fromList $ forM dres (,) 0           :: M.Map Result Integer

printTrialHist :: TrialHist -> IO ()
printTrialHist hist = do
  let topLineList = (" % | " ++) . intercalate " | " $ show <$> results
  putStrLn topLineList
  traverse_ printLine upCards -- equivalent to: sequence_ $ printLine <$> upCards
  where
    upCards = [(Two)..(Ten)] ++ [Ace]
    results = [(Seventeen)..(TwentyOne)] ++ [(Bust)] -- leaving out natural blackjack, assuming dealer checks
    printLine upCard = do
      putStrLn . (" " ++) . intercalate " | " . (:) (printUp upCard) $ prnum . getHistValNorm hist upCard <$> results
    printUp upc
      | rankCount upc == 10   = "T"
      | otherwise             = show upc
    prnum val = (++) (if val < 0.1 then " " else "") $ showFFloat (Just 1) (100*val) "" -- 1 decimal after percent (permil)

getHistVal :: TrialHist -> Rank -> Result ->  Integer
getHistVal hist dup res = M.findWithDefault 0 res $ M.findWithDefault M.empty dup hist

getTotalForUpCard :: TrialHist -> Rank -> Integer
getTotalForUpCard hist dup =
  M.fold (+) 0 $ M.findWithDefault M.empty dup hist

getHistValNorm :: Fractional a => TrialHist -> Rank -> Result -> a
getHistValNorm hist dup res = num / den where
  num = fromIntegral . M.findWithDefault 0 res $ dupList
  den = fromIntegral . M.fold (+) 0 $ dupList
  dupList = M.findWithDefault M.empty dup hist

-- fill a trial with a rank,result
--   this will likely be slower than saving a list of results,
--   but will not have a memory scaling problem
incTrialHist :: TrialHist -> Rank -> Result -> TrialHist
incTrialHist hist dup res = M.adjust resMapInc dup' hist
  where
    resMapInc = M.adjust succ res
    dup'
      | dup `elem` [Jack,Queen,King]  = Ten
      | otherwise                     = dup


incTrialHistFromDeck :: TrialHist -> Deck -> (TrialHist, Deck)
incTrialHistFromDeck hist (upcard:deck) = (newHist, newDeck) where
  newHist = incTrialHist hist dr res
  dr = rank upcard
  res = resultFromHand finalHand
  (finalHand, newDeck) = iterDeck [upcard] deck
  iterDeck hand dk = case dealerStrategy hand of
    Hit   -> iterDeck ((head dk):hand) (tail dk)
    Stand -> (hand, dk)
      
  
