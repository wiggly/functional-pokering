-- -*- mode: haskell; -*-

module Poker
       (
         HoleCards (..),
         pocketPair,
         suited,
         connected,

         PokerRank (..),
         pokerRank,

         PokerHand (..),
         pokerHandCards,

         generateHands,

         groupRanks,
         countRanks,
         countSuits,

         bestPokerHand,

         isPair,
         isTwoPair,
         isThreeOfAKind,
         isStraight,
         isFlush,
         isFullHouse,
         isFourOfAKind,
         isStraightFlush,

         readHoleCards,

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
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.Ratio
import Data.Monoid
import GHC.Exts (groupWith)

import Debug.Trace (trace)

-- hole cards dealt to a player in Hold Em
type HoleCards = (Card,Card)

-- identical rank
pocketPair :: HoleCards -> Bool
pocketPair (x,y) = rank x == rank y

-- identical suit
suited :: HoleCards -> Bool
suited (x,y) = suit x == suit y

-- strongly connected, adjacent
-- FIXME: Aces
connected :: HoleCards -> Bool
connected (x,y) = 1 == abs ((fromEnum (rank x)) - (fromEnum (rank y)))

mergeHoleCards :: [Card] -> HoleCards -> [Card]
mergeHoleCards cards hc = cards ++ holeArray
  where holeArray = [fst hc, snd hc]

data PokerRank = StraightFlush
               | FourOfAKind
               | FullHouse
               | Flush
               | Straight
               | ThreeOfAKind
               | TwoPair
               | Pair
               | HighCard
               deriving (Eq, Bounded, Enum, Ord, Show)

-- a complete Hold Em hand along with its rank. Hands may be compared and ordered
data PokerHand = PokerHand PokerRank Card Card Card Card Card
               deriving (Show)

instance Eq PokerHand where
  (==) a b = and $ (ra == rb) : (map (\x -> (fst x) == (snd x)) $ zip cra crb)
    where ra = pokerHandRank a
          rb = pokerHandRank b
          cra = map rank (pokerHandCards a)
          crb = map rank (pokerHandCards b)

instance Ord PokerHand where
  compare a b = (compare ra rb) `mappend` (mconcat $ map (\x -> compare (fst x) (snd x)) $ zip cra crb)
    where ra = pokerHandRank a
          rb = pokerHandRank b
          cra = map rank (pokerHandCards a)
          crb = map rank (pokerHandCards b)

pokerHandRank :: PokerHand -> PokerRank
pokerHandRank (PokerHand r _ _ _ _ _) = r

pokerHandCards :: PokerHand -> [Card]
pokerHandCards (PokerHand _ a b c d e) = [a,b,c,d,e]

bestPokerHand :: [Card] -> PokerHand
bestPokerHand cards = head $ hands ++ fallback
  where fallback = [findHighCardHand cards]
        hands = catMaybes [ f cards | f <- finders ]
        finders = [findStraightFlushHand, findFourOfAKindHand, findFullHouseHand, findFlushHand, findStraightHand, findThreeOfAKindHand, findTwoPairHand, findPairHand]

findHighCardHand :: [Card] -> PokerHand
findHighCardHand cards = PokerHand HighCard a b c d e
  where (a:b:c:d:e:_) = take 5 $ sort cards

findPairHand :: [Card] -> Maybe PokerHand
findPairHand cards = if isPair cards
                     then constructHand
                     else Nothing
  where constructHand = Just $ PokerHand Pair a b c d e
        pairCards = head $ filter (\x -> 2 == length x) $ groupCardsByRank cards
        nonPairCards = cards \\ pairCards
        (a:b:_) = pairCards
        (c:d:e:_) = take 3 $ sort nonPairCards

findTwoPairHand :: [Card] -> Maybe PokerHand
findTwoPairHand cards = if isTwoPair cards
                        then constructHand
                        else Nothing
  where constructHand = Just $ PokerHand TwoPair a b c d e
        pairCards = concat $ take 2 $ filter (\x -> 2 == length x) $ groupCardsByRank cards
        nonPairCards = cards \\ pairCards
        (a:b:c:d:_) = pairCards
        e = head $ sort nonPairCards

findThreeOfAKindHand :: [Card] -> Maybe PokerHand
findThreeOfAKindHand cards = if isThreeOfAKind cards
                             then constructHand
                             else Nothing
  where constructHand = Just $ PokerHand ThreeOfAKind a b c d e
        setCards = head $ filter (\x -> 3 == length x) $ groupCardsByRank cards
        nonSetCards = cards \\ setCards
        (a:b:c:_) = setCards
        (d:e:_) = take 2 $ sort nonSetCards

-- TODO: this needs to deal with Low straights correctly... i.e. 5, 4, 3, 2, A
findStraightHand :: [Card] -> Maybe PokerHand
findStraightHand cards = if isStraight cards
                         then constructHand
                         else Nothing
  where constructHand = Just $ PokerHand Straight a b c d e
        (a:b:c:d:e:_) = head $ allPokerStraights cards

findFlushHand :: [Card] -> Maybe PokerHand
findFlushHand cards = if isFlush cards
                      then constructHand
                      else Nothing
  where constructHand = Just $ PokerHand Flush a b c d e
        (a:b:c:d:e:_) = bestPokerFlush cards

findFullHouseHand :: [Card] -> Maybe PokerHand
findFullHouseHand cards = if isFullHouse cards
                          then constructHand
                          else Nothing
  where constructHand = Just $ PokerHand FullHouse a b c d e
        (a:b:c:d:e:_) = cards

findFourOfAKindHand :: [Card] -> Maybe PokerHand
findFourOfAKindHand cards = if isFourOfAKind cards
                            then constructHand
                            else Nothing
  where constructHand = Just $ PokerHand FourOfAKind a b c d e
        quadCards = sort $ head $ filter (\x -> 4 == length x) $ groupCardsByRank cards
        nonQuadCards = cards \\ quadCards
        (a:b:c:d:_) = quadCards
        e = head $ sort nonQuadCards

findStraightFlushHand :: [Card] -> Maybe PokerHand
findStraightFlushHand cards = if isStraightFlush cards
                              then constructHand
                              else Nothing
  where constructHand = Just $ PokerHand StraightFlush a b c d e
        (a:b:c:d:e:_) = head $ sort $ filter isFlush $ allPokerFlushes cards

-- take a board to choose from, a hand and return the best rank made by that hand on the board
pokerRank :: [Card] -> HoleCards -> PokerRank
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
isFlush xs = any (\x -> x > 4) suitCounts
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
generateHands :: Deck -> Int -> ([HoleCards],Deck)
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


groupCardsByRank :: [Card] -> [[Card]]
groupCardsByRank xs = groupWith rank $ sort xs


countSuits :: [Card] -> Int
countSuits xs = length (groupSuits xs)

groupSuits :: [Card] -> [[Suit]]
groupSuits xs = group . sort $ (map suit xs)

groupBySuits :: [Card] -> [[Card]]
groupBySuits xs = groupWith (\x -> suit x) xs

readHoleCards :: String -> Maybe HoleCards
readHoleCards str
  | length str /= 4 = Nothing
  | isJust c1 && isJust c2 = Just (fromJust c1, fromJust c2)
  | otherwise = Nothing
    where c1 = readCard $ take 2 str
          c2 = readCard $ drop 2 str

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

scnd :: (a,b,c) -> b
scnd (_,x,_) = x

thrd :: (a,b,c) -> c
thrd (_,_,x) = x

-- hand/rank/
pokerEquity :: [Card] -> [HoleCards] -> [(HoleCards,ShowdownTally)]
pokerEquity deck hs = tallies
  where cards = map (mergeHoleCards deck) hs
        hands = map bestPokerHand cards
        handsIdx = sortOn fst $ zip hands [0..((length hands)-1)]
        grouped = groupWith fst handsIdx
        winners = head grouped
        losers = concat $ tail grouped
        tallyWinners = if length winners > 1
                       then map (\x -> (fst x, snd x, newTie) ) winners
                       else map (\x -> (fst x, snd x, newWin) ) winners
        tallyLosers = map (\x -> (fst x, snd x, newLoss) ) losers
        handsIdxTallies = tallyWinners ++ tallyLosers
        sortedHandsIdxTallies = sortOn scnd handsIdxTallies
        tallies = zip hs $ map thrd sortedHandsIdxTallies


pokerHandsXXX :: [Card] -> [HoleCards] -> [(HoleCards,PokerHand)]
pokerHandsXXX deck hs = holesAndHands
  where cards = map (mergeHoleCards deck) hs
        hands = map bestPokerHand cards
        holesAndHands = zip hs hands
--        sortHAH hah = trace ("hah:" ++ show hah) sortOn snd hah
--        grouped = trace ("sorted: " ++ show (sortHAH holesAndHands)) groupWith snd $ sortHAH holesAndHands

-- so, this is a bit of a lie, it actually doesn't give you ALL poker straight combos.
-- It cuts them down by removing repeated ranks that are available.
-- Therefore if As and Ah are both in the list the Ah will be discarded. This means
-- this cannot be directly used to find straight flushes
allPokerStraights :: [Card] -> [[Card]]
allPokerStraights xs = straights
  where cs = nubBy (\a b -> rank a == rank b) $ sort xs
        combos = nChooseK cs 5
        straights = filter isStraight combos

allPokerFlushes :: [Card] -> [[Card]]
allPokerFlushes xs = minLengthSuits
  where gs = groupBySuits xs
        ss = fmap sort gs
        minLengthSuits = map (\x -> take 5 x ) $ filter (\x -> length x > 4) ss

bestPokerFlush :: [Card] -> [Card]
bestPokerFlush xs = head ordered
  where ordered = sortBy comp $ allPokerFlushes xs
        comp a b = (mconcat $ map (\x -> compare (snd x) (fst x)) $ zip a b)
