module Deck where

-- source code for generating and shuffling decks
import Card
import Control.Monad
import Control.Monad.Random
import System.Random.Shuffle

type Deck = [Card]

-- standardDeck is a 52-card deck, in order (unshuffled)
standardDeck :: Deck
-- we could use a list comprehension:
--   standardDeck = [ (Card r s) | r <- [(Two)..(Ace)], s <- [(Club)..(Spade)]]
-- but applicative syntax is more stylish:
standardDeck = Card <$> [(Two)..(Ace)] <*> [(Club)..(Spade)]

-- either of these type declarations work
-- getShuffledDeck :: (RandomGen g) => Rand g Deck
getShuffledDeck :: MonadRandom mr => mr Deck
getShuffledDeck = shuffleM standardDeck

-- getShuffledDecks :: (RandomGen g) => Int -> Rand g Deck
getShuffledDecks :: MonadRandom mr => Int -> mr Deck
getShuffledDecks n = shuffleM . concat $ replicate n standardDeck
        
-- to grab the deck, use evalRand (or evalRandIO for printing)
