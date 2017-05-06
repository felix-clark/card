import Card
import Deck
import Blackjack
import Strategy
import Control.Monad.Random (evalRandIO)

main :: IO ()
main = do
  let deck = getShuffledDecks 2
  evalDeck <- evalRandIO deck
  -- print evalDeck
--  let theTable = Table evalDeck [] [] PlayerTurn
  let theTable = makeTable evalDeck
  printTable theTable
  putStrLn ""
  putStrLn $ (++) " BS: " . show . basicStrategy (rank . head $ dealerHand theTable) $ playerHand theTable
  putStrLn ""
  printHardStrategy
--  putStrLn deck
