
import Card
import Deck
import DealerResult
import Count
-- import Control.Monad.Random (evalRandIO)
import System.Environment (getArgs)

-- doNLoops :: Monad m => Int -> (a -> m a) -> a -> m a
-- doNLoops 0 _ hist = return hist
-- doNLoops n f hist = do
--   newHist <- f hist
--   doNLoops (n-1) f newHist

fillHistWithCount :: (CtT a) => Int -> Int -> (Card -> a) -> a -> TrialHist -> IO TrialHist
fillHistWithCount nShoes nDecks countFunc count hist = do
  let unshuffledDeck = concat $ replicate nDecks standardDeck
  evalDecks <- getDecksWithCount nShoes unshuffledDeck countFunc count  :: IO [[Card]]
  let hist = iterHist evalDecks hist
  return hist
    where
      iterHist :: [[Card]] -> TrialHist -> TrialHist
      iterHist [] hist = hist
      iterHist (d:ds) hist = fst $ incTrialHistFromDeck hist d

main :: IO ()
main = do
  args <- getArgs
  let counts = map read args  :: [Int]
  sequence_ $ processWithCount <$> counts
    where
      processWithCount :: Int -> IO ()
      processWithCount count = do
        putStrLn $ "  Making table for count of " ++ show count ++ ":"
        hist <- fillHistWithCount 1024 6 countKO count emptyTrialHist   :: IO TrialHist
        printTrialHist hist
