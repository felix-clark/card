
import Card
import Deck
import DealerResult
import Control.Monad.Random (evalRandIO)

doNLoops :: Monad m => Int -> (a -> b -> m b) -> a -> b -> m b
doNLoops 0 _ _ hist = return hist
doNLoops n f nDecks hist = do
  newHist <- f nDecks hist
  doNLoops (n-1) f nDecks newHist

fillHistWithShoe :: Int -> TrialHist -> IO TrialHist
fillHistWithShoe nDecks hist = do
  evalDeck <- evalRandIO $ getShuffledDecks nDecks   :: IO [Card]
  let shoeResetPoint = nDecks * 13    -- casinos typically reset the shoe when there is about 25% left
  let (lastHist,_) = iter shoeResetPoint hist evalDeck
  return lastHist
  where
    iter cut hres dres
      | length dres < cut  = (hres,dres)
      | otherwise             = uncurry (iter cut) $ incTrialHistFromDeck hres dres

main :: IO ()
main = do
  hist <- doNLoops 4096 fillHistWithShoe 8 emptyTrialHist   :: IO TrialHist
  printTrialHist hist
