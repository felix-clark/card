
import Card
import Deck
import DealerResult
import Count
-- import Control.Monad.Random (evalRandIO)
import System.Environment (getArgs)

doNLoops :: Monad m => Int -> (a -> m a) -> a -> m a
doNLoops 0 _ hist = return hist
doNLoops n f hist = do
  newHist <- f hist
  doNLoops (n-1) f newHist

fillHistWithCount :: (CtT a) => Int -> (Card -> a) -> a -> TrialHist -> IO TrialHist
fillHistWithCount nDecks countFunc count hist = do
  let unshuffledDeck = concat $ replicate nDecks standardDeck
  let dkSz = length unshuffledDeck
  -- just use a deck of half the max size for now, since the P(N|C) computations are expensive
  evalDeck <- getDeckWithCardsAndCount (dkSz `div` 2) unshuffledDeck countFunc count  :: IO [Card]
  return . fst $ incTrialHistFromDeck hist evalDeck
  -- let shoeResetPoint = nDecks * 13    -- casinos typically reset the shoe when there is about 25% left
  -- let (lastHist,_) = iter shoeResetPoint hist evalDeck
  -- return lastHist
  -- where
  --   iter cut hres dres
  --     | length dres < cut  = (hres,dres)
  --     | otherwise             = uncurry (iter cut) $ incTrialHistFromDeck hres dres

main :: IO ()
main = do
  args <- getArgs
  let counts = map read args  :: [Int]
  sequence_ $ processWithCount <$> counts
    where
      processWithCount :: Int -> IO ()
      processWithCount count = do
        putStrLn $ "  Making table for count of " ++ show count ++ ":"
        hist <- doNLoops 1024 (fillHistWithCount 6 countKO count) emptyTrialHist   :: IO TrialHist
        printTrialHist hist
      -- processWithCount count = hist <- doNLoops 2048 (fillHistWithCount 8 countKO 0) emptyTrialHist   :: Int -> IO TrialHist
      -- printTrialHist hist
