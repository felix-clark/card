
import Card
import Deck
import DealerResult
import Control.Monad.Random (evalRandIO)

main :: IO()
main = do
  let hist = makeEmptyTrialHist                 :: TrialHist
  let nDecks = 8
  evalDeck <- evalRandIO $ getShuffledDecks nDecks   :: IO [Card]
  let shoeResetPoint = nDecks * 13     -- casinos typically reset the shoe when there is 25% left
  let (lastHist,_) = iter shoeResetPoint hist evalDeck
  printTrialHist lastHist
  where
    iter cut hres dres
      | length dres < cut  = (hres,dres)
      | otherwise             = uncurry (iter cut) $ incTrialHistFromDeck hres dres
