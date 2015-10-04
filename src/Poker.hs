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
         countSuits
       )
       where

import Cards
import Wiggly
import Data.List

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
  | isStraightFlush cards = StraightFlush
  | isFourOfAKind cards = FourOfAKind
  | isFullHouse cards = FullHouse
  | isFlush cards = Flush
  | isStraight cards = Straight
  | isThreeOfAKind cards = ThreeOfAKind
  | isTwoPair cards = TwoPair
  | isPair cards = Pair
  | otherwise = HighCard
  where cards = (fst hand) : (snd hand) : board 
  
isPair :: [Card] -> Bool
isPair xs = elem 1 pairCount
  where ps = nChooseK xs 5
        pairCount = map countPairs ps

isTwoPair :: [Card] -> Bool
isTwoPair xs = elem 2 pairCount
  where ps = nChooseK xs 5
        pairCount = map countPairs ps

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind xs = elem 1 tripCount
  where ps = nChooseK xs 5
        tripCount = map countTrips ps

isStraight :: [Card] -> Bool
isStraight xs = (length legalLengthHands) > 0
  where ps = nChooseK orderedRanks 5
        orderedRanks = sort $ map (fromEnum . rank) xs
        groupedRanks = map group ps
        legalLengthHands = filter (\x -> (length x) == 5) groupedRanks

isFlush :: [Card] -> Bool
isFlush xs = elem 1 suitCount
  where ps = nChooseK xs 5
        suitCount = map countSuits ps

isFullHouse :: [Card] -> Bool
isFullHouse xs = elem (1,1) bothCount
  where ps = nChooseK xs 5
        tripCount = map countTrips ps
        pairCount = map countPairs ps
        bothCount = zip tripCount pairCount

isFourOfAKind :: [Card] -> Bool
isFourOfAKind xs = elem 4 quadCount
  where ps = nChooseK xs 5
        quadCount = map countQuads ps

isStraightFlush :: [Card] -> Bool
isStraightFlush _ = False




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
