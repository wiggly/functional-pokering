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

         readHoldEmHand,

         ShowdownTally (..),
         pokerEquity,
         blankTally,
         addTally,

         newWin,
         newTie,
         newLoss

       )
       where

import Cards
import Wiggly
import Data.List
import Data.Maybe (fromJust, isJust)
import Data.Ratio

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
isStraight xs = or $ map isLegalInterval possibleHands
  where uniqueOrderedRanks = nub . sort $ map (fromEnum . rank) xs
        possibleHands = nChooseK uniqueOrderedRanks 5
        isLegalInterval ranks
          | 0 == (head ranks) = (((last ranks) - (head ranks)) == 4) || (42 == (sum ranks))
          | otherwise = ((last ranks) - (head ranks)) == 4

isFlush :: [Card] -> Bool
isFlush xs = 5 `elem` suitCounts
  where suits = groupSuits xs
        suitCounts = map length suits

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

readHoldEmHand :: String -> Maybe HoldEmHand
readHoldEmHand str
  | length str /= 4 = Nothing
  | isJust c1 && isJust c2 = Just (fromJust c1, fromJust c2)
  | otherwise = Nothing
    where c1 = readCard $ take 2 str
          c2 = readCard $ drop 2 str


--calculateEquity :: [HoldEmHand] -> [(HoldEmHand,Equity)]
--calculateEquity hands = map (\x -> (x,(1 % 2))) hands


data ShowdownTally = ShowdownTally { win :: Integer, tie :: Integer, loss :: Integer }
                   deriving (Eq, Show)

instance Ord ShowdownTally where
  compare a b = compare ((win a) + (tie a) - (loss a)) ((win b) + (tie b) - (loss b))

addTally :: ShowdownTally -> ShowdownTally -> ShowdownTally
addTally x y = ShowdownTally { win = (win x) + (win y), loss = (loss x) + (loss y), tie = (tie x) + (tie y) }

blankTally :: ShowdownTally
blankTally = ShowdownTally { win = 0, tie = 0, loss = 0 }

newWin :: ShowdownTally
newWin = ShowdownTally { win = 1, tie = 0, loss = 0 }

newTie :: ShowdownTally
newTie = ShowdownTally { win = 0, tie = 1, loss = 0 }

newLoss :: ShowdownTally
newLoss = ShowdownTally { win = 0, tie = 0, loss = 1 }






-- hand/rank/
pokerEquity :: [Card] -> [HoldEmHand] -> [(HoldEmHand,ShowdownTally)]
pokerEquity deck hs = if outrightWinner
                      then map (tallyHand winnerTally) ranked
                      else map (tallyHand tierTally) ranked
  where ranked = map (\x -> (x,ranker x)) hs
        ranker = pokerRank deck
        winningRank = maximum $ map snd ranked
        outrightWinner = 1 == (length $ filter (\x -> x == winningRank) $ map snd ranked)
        winnerTally h = if h == winningRank
                        then newWin
                        else newLoss
        tierTally h = if h == winningRank
                      then newTie
                      else newLoss
        tallyHand tf (hand,rank) = (hand,(tf rank))
