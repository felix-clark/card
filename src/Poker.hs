
import Card

data HandType =
  HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush | FiveOfAKind
  deriving (Eq, Ord)

-- like compare, but returns which hand wins
compareHands :: Hand -> Hand -> Ordering
