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

import Data.Char (chr)
import Data.Maybe (Maybe, isJust, fromJust, catMaybes)
import System.Random
import System.Random.Shuffle

-- rank
data Rank = Ace
          | Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          deriving (Eq, Ord, Bounded, Enum)

instance Show Rank where
  show Ace = "A"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"

ranks :: [Rank]
ranks = [Ace .. King]

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

instance Show Suit where
  show Spades = "♠"
  show Hearts = "♥"
  show Diamonds = "♦"
  show Clubs = "♣"

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
  }

instance Show Card where
  show (Card {rank = r, suit = s}) = (show r) ++ (show s) ++ " "
--  show (Card {rank = r, suit = s}) = [chr ((rankUnicodeOffset r) + (suitUnicode s)), ' ']

type Deck = [Card]

-- deck
createDeck :: [Rank] -> [Suit] -> Deck
createDeck rs ss = [Card r s | s <- ss, r <- rs]

standardDeck :: Deck
standardDeck = createDeck ranks suits

-- shuffle deck
shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck gen deck = shuffle' deck (length deck) gen


readCards :: String -> Maybe [Card]
readCards str
  | allValid = Just (catMaybes cards)
  | otherwise = Nothing
  where cards = map readCard $ words str
        allValid = all isJust cards

readCard :: String -> Maybe Card
readCard (r:s:[])
  | (isJust mr) && (isJust ms) = Just (Card (fromJust mr) (fromJust ms))
  | otherwise = Nothing
  where mr = readRank r
        ms = readSuit s
readCard _ = Nothing

readRank :: Char -> Maybe Rank
readRank 'A' = Just Ace
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
readRank _ = Nothing

readSuit :: Char -> Maybe Suit
readSuit 's' = Just Spades
readSuit 'h' = Just Hearts
readSuit 'd' = Just Diamonds
readSuit 'c' = Just Clubs
readSuit _ = Nothing
