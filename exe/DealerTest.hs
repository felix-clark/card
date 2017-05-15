
import Card
import Deck
import DealerResult
import Control.Monad.Random (evalRandIO)

doNLoops :: Monad m => Int -> (a -> m a) -> a -> m a
doNLoops 0 _ hist = return hist
doNLoops n f hist = do
  newHist <- f hist
  doNLoops (n-1) f newHist

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
  hist <- doNLoops 2048 (fillHistWithShoe 8) emptyTrialHist   :: IO TrialHist
  printTrialHist hist
