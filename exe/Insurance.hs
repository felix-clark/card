
import Card
import Deck
import Blackjack
import DealerResult
import Count
import System.Environment (getArgs)
import Numeric (showFFloat)

-- TODO: modify this to ensure that an ace is drawn from the deck first (shouldn't be huge difference w/ 6 deck)
-- this function is also woefully inefficient because it shuffles an entire deck to check the top card
-- this function counts the number of 10s that come up in nReps of decks generated by count
tensWithCount :: (CtT a) => Int -> Int -> (Card -> a) -> a -> IO Int
tensWithCount nReps nDecks countFunc count = do
  let unshuffledDeck = concat $ replicate nDecks standardDeck            :: [Card]
  (_,deckRmAce) <- drawFrom [(Ace ==) . rank] unshuffledDeck
  -- drawing a card from the deck will lead to a slightly shifted P(N) distribution internally, but that's probably less of a deal compared to the Ace missing. eventually should move to a more generic function
  evalDecks <- getDecksWithCount nReps deckRmAce countFunc count   :: IO [[Card]]
  return . length $ filter ( (10 == ) . cardCount . head) evalDecks

main :: IO ()
main = do
  args <- getArgs
  sequence_ $ processWithCount <$> map read args
  where
    --- number of reps should be ~ (2/error)^2 where error is desired std error of mean
    -- so e.g. for precision to 0.1 (0.01), need 400 (160 000) reps
    nReps = 2048
    -- nReps = 8*2048
    processWithCount :: Int -> IO ()
    processWithCount count = do
      nBJs <- tensWithCount nReps 6 countKO count
      -- nBJs <- tensWithCount nReps 1 countKO count
      putStr $ "with count of " ++ show count ++ ", EV is "
      -- insurance pays 2 to 1, so ROI is 3 on BJ and 0 otherwise
      putStrLn $ showFFloat (Just 3) (ev nBJs) " \177 " ++ showFFloat (Just 3) (meanErr nBJs) ""
      -- " (with a width of " ++ showFFloat (Just 3) (sqrt $ var nBJs) ")"
        where
          ev nBJs = ( (fromIntegral $ 3*nBJs) / (fromIntegral nReps) ) :: Double
          var nBJs = 9*rat*(1 - rat)
            where rat = (fromIntegral nBJs) / (fromIntegral nReps) :: Double
          meanErr nBJs = sqrt $ (var nBJs) / (fromIntegral nReps)
