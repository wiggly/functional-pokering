-- -*- mode: haskell; -*-

module Cards(
  Rank(..),
  Suit(..),
  Card(..),
  Deck,
  standardDeck,
  shuffleDeck,

  readCards,
  readCard
  )
       where

import Data.Char (chr, isSpace)
import Data.Maybe (Maybe, isJust, fromJust, catMaybes)
import System.Random
import System.Random.Shuffle
import Data.Monoid
import Control.DeepSeq

-- rank
data Rank = Ace
          | King
          | Queen
          | Jack
          | Ten
          | Nine
          | Eight
          | Seven
          | Six
          | Five
          | Four
          | Three
          | Two
          | LowAce
          deriving (Eq, Ord, Bounded, Enum)

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show Ten = "T"
  show Nine = "9"
  show Eight = "8"
  show Seven = "7"
  show Six = "6"
  show Five = "5"
  show Four = "4"
  show Three = "3"
  show Two = "2"
  show LowAce = "1"

ranks :: [Rank]
ranks = [Ace .. Two]

rankUnicodeOffset :: Rank -> Int
rankUnicodeOffset Queen = (fromEnum Queen) + 2
rankUnicodeOffset King = (fromEnum King) + 2
rankUnicodeOffset s = (fromEnum s) + 1

-- suit
data Suit = Spades
            | Hearts
            | Diamonds
            | Clubs
            deriving (Eq, Ord, Bounded, Enum)

-- not sure why but outputting these codes always look like they are incorrectly aligned so we have added
-- a space after each of them to stop them overlapping the next output character. this was done in show card
-- but we really want it done here for now I think.
instance Show Suit where
  show Spades = "♠ "
  show Hearts = "♥ "
  show Diamonds = "♦ "
  show Clubs = "♣ "

-- suits
suits :: [Suit]
suits = [Spades .. Clubs]

suitUnicode :: Suit -> Int
suitUnicode Spades = 0x1F0A0
suitUnicode Hearts = 0x1F0B0
suitUnicode Diamonds = 0x1F0C0
suitUnicode Clubs = 0x1F0D0

-- card
data Card = Card {
  rank :: Rank,
  suit :: Suit
  } deriving (Eq)

instance Show Card where
  show (Card {rank = r, suit = s}) = (show r) ++ (show s)

instance Read Card where
  readsPrec _ value = do
    let stripped = dropWhile isSpace value
        frontTwo = take 2 stripped
        rest = drop 2 stripped
      in maybe [] (\x -> [(x,rest)]) $ readCard $ take 2 stripped

instance Ord Card where
  compare a b = (rank a `compare` rank b) `mappend` (suit a `compare` suit b)

instance NFData Card where
  rnf (Card {rank=r, suit=s}) = r `seq` s `seq` ()

type Deck = [Card]

-- deck
createDeck :: [Rank] -> [Suit] -> Deck
createDeck rs ss = [Card r s | s <- ss, r <- rs]

standardDeck :: Deck
standardDeck = createDeck ranks suits

-- shuffle deck
shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck gen deck = shuffle' deck (length deck) gen

-- space-separated cards to an array of possible cards
readCards :: String -> Maybe [Card]
readCards str
  | allValid = Just (catMaybes cards)
  | otherwise = Nothing
  where cards = map readCard $ words str
        allValid = all isJust cards

-- a string representing a single card
readCard :: String -> Maybe Card
readCard (r:s:[])
  | (isJust mr) && (isJust ms) = Just (Card (fromJust mr) (fromJust ms))
  | otherwise = Nothing
  where mr = readRank r
        ms = readSuit s
readCard _ = Nothing

readRank :: Char -> Maybe Rank
readRank '1' = Just LowAce
readRank '2' = Just Two
readRank '3' = Just Three
readRank '4' = Just Four
readRank '5' = Just Five
readRank '6' = Just Six
readRank '7' = Just Seven
readRank '8' = Just Eight
readRank '9' = Just Nine
readRank 'T' = Just Ten
readRank 'J' = Just Jack
readRank 'Q' = Just Queen
readRank 'K' = Just King
readRank 'A' = Just Ace
readRank _ = Nothing

readSuit :: Char -> Maybe Suit
readSuit 's' = Just Spades
readSuit 'h' = Just Hearts
readSuit 'd' = Just Diamonds
readSuit 'c' = Just Clubs
readSuit _ = Nothing
