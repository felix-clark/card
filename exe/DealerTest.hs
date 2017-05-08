
import Card
import Deck
import DealerResult
import Control.Monad.Random (evalRandIO)

main :: IO()
main = do
  let hist = makeEmptyTrialHist                 :: TrialHist
  evalDeck <- evalRandIO $ getShuffledDecks 8   :: IO [Card]
  let (lastHist,_) = iter 64 hist evalDeck
  printTrialHist lastHist
  where
    iter n hres dres
      | n <= 0    = (hres,dres)
      | otherwise = uncurry (iter (n-1)) $ incTrialHistFromDeck hres dres
