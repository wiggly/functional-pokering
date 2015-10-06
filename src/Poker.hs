-- -*- mode: haskell; -*-

module Poker
       (
         HoldEmHand,
         pocketPair,
         suited,
         connected,

         PokerRank (..),
         pokerRank,

         generateHands,

         groupRanks,
         countRanks,
         countSuits,

         isPair,
         isTwoPair,
         isThreeOfAKind,
         isStraight,
         isFlush,
         isFullHouse,
         isFourOfAKind,
         isStraightFlush,

         readHand
       )
       where

import Cards
import Wiggly
import Data.List
import Data.Maybe (fromJust)

type HoldEmHand = (Card,Card)
type OmahaHand = (Card,Card,Card,Card)


data PokerRank = HighCard
               | Pair
               | TwoPair
               | ThreeOfAKind
               | Straight
               | Flush
               | FullHouse
               | FourOfAKind
               | StraightFlush
               deriving (Eq, Bounded, Enum, Ord, Show)

-- identical rank
pocketPair :: HoldEmHand -> Bool
pocketPair (x,y) = rank x == rank y

-- identical suit
suited :: HoldEmHand -> Bool
suited (x,y) = suit x == suit y

-- strongly connected, adjacent
connected :: HoldEmHand -> Bool
connected (x,y) = 1 == abs ((fromEnum (rank x)) - (fromEnum (rank y)))

-- take a board to choose from, a hand and return the best rank made by that hand on the board
pokerRank :: [Card] -> HoldEmHand -> PokerRank
pokerRank board hand
  | or $ map isStraightFlush hands = StraightFlush
  | or $ map isFourOfAKind hands = FourOfAKind
  | or $ map isFullHouse hands = FullHouse
  | or $ map isFlush hands = Flush
  | or $ map isStraight hands = Straight
  | or $ map isThreeOfAKind hands = ThreeOfAKind
  | or $ map isTwoPair hands = TwoPair
  | or $ map isPair hands = Pair
  | otherwise = HighCard
  where cards = (fst hand) : (snd hand) : board
        hands = nChooseK cards 5

isPair :: [Card] -> Bool
isPair xs = 1 == countPairs xs

isTwoPair :: [Card] -> Bool
isTwoPair xs = 2 == countPairs xs

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind xs = 1 == countTrips xs

--
-- this is a bit interesting. due to the fact that Ace enum value is zero we can't check that 
-- the interval between the first and last rank is exactly 4 so we instead have a check to see that
-- an Ace is present and that the sum of the enum values is exactly what is required by the other 
-- broadway cards
--
isStraight :: [Card] -> Bool
isStraight xs
  | (5 > length uniqueOrderedRanks) = False
  | 0 == head uniqueOrderedRanks && 42 == sum uniqueOrderedRanks = True
  | otherwise = isLegalInterval uniqueOrderedRanks
  where uniqueOrderedRanks = nub . sort $ map (fromEnum . rank) xs
        isLegalInterval ranks = ((last ranks) - (head ranks)) == 4

isFlush :: [Card] -> Bool
isFlush xs = 1 == countSuits xs

isFullHouse :: [Card] -> Bool
isFullHouse xs = hasPair && hasTrips
  where hasPair = 1 == countPairs xs
        hasTrips = 1 == countTrips xs

isFourOfAKind :: [Card] -> Bool
isFourOfAKind xs = 1 == countQuads xs

isStraightFlush :: [Card] -> Bool
isStraightFlush xs = isStraight xs && isFlush xs

-- generate some hands, return them and the remainder of the deck
generateHands :: Deck -> Int -> ([HoldEmHand],Deck)
generateHands deck count = (hands, remainder)
  where hands = map createHand (take count $ chunkList 2 deck)
        remainder = drop (count * 2) deck
        createHand (x:y:[]) = (x, y)

countMultipleRanks :: Int -> [Card] -> Int
countMultipleRanks n hand = length multiples
  where groupedRanks = countRanks . groupRanks $ hand
        multiples = filter (== n) groupedRanks

countPairs :: [Card] -> Int
countPairs = countMultipleRanks 2

countTrips :: [Card] -> Int
countTrips = countMultipleRanks 3

countQuads :: [Card] -> Int
countQuads = countMultipleRanks 4

countRanks :: [[Rank]] -> [Int]
countRanks xs = map length xs

groupRanks :: [Card] -> [[Rank]]
groupRanks xs = group . sort $ (map rank xs)

countSuits :: [Card] -> Int
countSuits xs = length (groupSuits xs)

groupSuits :: [Card] -> [[Suit]]
groupSuits xs = group . sort $ (map suit xs)

readHand :: String -> Maybe HoldEmHand
readHand str
  | (numCards == 2) = Just (head $ fromJust cards, last $ fromJust cards)
  | otherwise = Nothing
    where cards = readCards str
          numCards = case cards of
            (Just xs) -> length xs
            Nothing -> 0
