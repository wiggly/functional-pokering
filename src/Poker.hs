-- -*- mode: haskell; -*-
{-# LANGUAGE BangPatterns #-}

module Poker
       (
         HoleCards (..),
         pocketPair,
         suited,
         connected,
         handShape,

         PokerRank (..),
         pokerRank,

         PokerHand (..),
         pokerHandCards,
         pokerHandRank,

         generateBoards,
         generateBoard,
         generateHands,

         groupRanks,
         countRanks,
         countSuits,

         bestPokerHand,

         readHoleCards,

         ShowdownTally (..),
         pokerEquity,

         blankTally,
         addTally,
         percentTally,
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
import Data.Char (isSpace)
import GHC.Exts (groupWith)
import Control.DeepSeq
import Data.Choose as DC

import Debug.Trace (trace)

-- hole cards dealt to a player in Hold Em
data HoleCards = HoleCards Card Card

instance Read HoleCards where
  readsPrec _ value = do
    let stripped = dropWhile isSpace value
        rest = drop 4 stripped
      in maybe [] (\x -> [(x,rest)]) $ readHoleCards $ take 4 stripped

instance Show HoleCards where
  show (HoleCards x y ) = (show x) ++ (show y)



-- identical rank
pocketPair :: HoleCards -> Bool
pocketPair (HoleCards x y) = rank x == rank y

-- identical suit
suited :: HoleCards -> Bool
suited (HoleCards x y) = suit x == suit y

-- strongly connected, adjacent
-- FIXME: Aces
connected :: HoleCards -> Bool
connected (HoleCards x y) = 1 == abs ((fromEnum (rank x)) - (fromEnum (rank y)))

mergeHoleCards :: [Card] -> HoleCards -> [Card]
mergeHoleCards cards (HoleCards x y) = cards ++ holeArray
  where holeArray = [x, y]

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

instance NFData PokerHand where
  rnf (PokerHand r a b c d e) = r `seq` a `seq` b `seq` c `seq` d `seq` e `seq` ()

pokerHandRank :: PokerHand -> PokerRank
pokerHandRank (PokerHand r _ _ _ _ _) = r

pokerHandCards :: PokerHand -> [Card]
pokerHandCards (PokerHand _ a b c d e) = [a,b,c,d,e]

bestPokerHand :: [Card] -> PokerHand
bestPokerHand = fromJust . constructPokerHand

-- take a board to choose from, a hand and return the best rank made by that hand on the board
pokerRank :: [Card] -> HoleCards -> PokerRank
pokerRank board (HoleCards x y) = let cards = x : y : board
  in pokerHandRank $ bestPokerHand cards

-- TODO: possibly broken
-- this is a bit interesting. due to the fact that Ace enum value is zero we can't check that
-- the interval between the first and last rank is exactly 4 so we instead have a check to see that
-- an Ace is present and that the sum of the enum values is exactly what is required by the other
-- broadway cards - this could be broken as well...TODO!!!
--
isStraight :: [Card] -> Bool
isStraight xs = or $ map isLegalInterval possibleHands
  where uniqueOrderedRanks = nub . sort $ map (fromEnum . rank) xs
        possibleHands = nChooseK uniqueOrderedRanks 5
        isLegalInterval ranks
          | 0 == (head ranks) = (((last ranks) - (head ranks)) == 4) || (42 == (sum ranks))
          | otherwise = ((last ranks) - (head ranks)) == 4

-- generate some hands, return them and the remainder of the deck
generateHands :: Deck -> Int -> ([HoleCards],Deck)
generateHands deck count = (hands, remainder)
  where hands = map createHand (take count $ chunkList 2 deck)
        remainder = drop (count * 2) deck
        createHand (x:y:[]) = (HoleCards x y)

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


-- TODO: we could make a better version that requires the cards to be sorted
groupCardsByRank :: [Card] -> [[Card]]
groupCardsByRank xs = groupWith rank xs


countSuits :: [Card] -> Int
countSuits xs = length (groupSuits xs)

groupSuits :: [Card] -> [[Suit]]
groupSuits xs = group . sort $ (map suit xs)

groupBySuits :: [Card] -> [[Card]]
groupBySuits xs = groupWith (\x -> suit x) xs

readHoleCards :: String -> Maybe HoleCards
readHoleCards str
  | length str /= 4 = Nothing
  | isJust c1 && isJust c2 = Just (HoleCards (fromJust c1) (fromJust c2))
  | otherwise = Nothing
    where c1 = readCard $ take 2 str
          c2 = readCard $ drop 2 str

data ShowdownTally = ShowdownTally { win :: Int, tie :: Int, loss :: Int }
                   deriving (Eq, Show)

instance Ord ShowdownTally where
  compare a b = compare ((win a) + (tie a) - (loss a)) ((win b) + (tie b) - (loss b))

instance NFData ShowdownTally where
  rnf ShowdownTally {win=a, tie=b, loss=c} = a `seq` b `seq` c `seq` ()

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

totalTally :: ShowdownTally -> Int
totalTally t = (win t) + (tie t) + (loss t)

percentTally :: (Fractional a) => ShowdownTally -> (a,a)
percentTally t = (we,te)
  where pct x = 100.0 * fromRational (x % total)
        we = pct wint
        wint = toInteger $ win t
        te = pct tint
        tint = toInteger $ tie t
        total = toInteger $ totalTally t

pokerEquity :: [Card] -> [HoleCards] -> [ShowdownTally]
pokerEquity deck hs = let hands = pokerHands deck hs
                      in pokerHandsToTally hands

pokerHandsToTally :: [PokerHand] -> [ShowdownTally]
pokerHandsToTally hs = let best = head $ group $ sort hs
                           bob = head best
                           good = if length best > 1
                             then newTie
                             else newWin
                       in map (\x -> tallyForHand x bob good newLoss) hs
  where tallyForHand h c g b = if h == c
                               then g
                               else b

pokerHands :: [Card] -> [HoleCards] -> [PokerHand]
pokerHands deck hs = hands
  where cards = map (mergeHoleCards deck) hs
        hands = map bestPokerHand cards

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

constructPokerHand :: [Card] -> Maybe PokerHand
constructPokerHand unsortedCards = let result h = firstThat isJust h
                                       hands = [ (constructStraightFlushHand suitedCards), (constructFourOfAKindHand quads cards), (constructFullHouseHand trips pairs), (constructFlushHand suitedCards), (constructStraightHand runningCards), (constructThreeOfAKindHand trips cards), (constructTwoPairHand pairs cards), (constructPairHand pairs cards), (constructHighCardHand cards) ]
                                       cards = sort unsortedCards
                                       rankedCards = groupCardsByRank cards
                                       suitedCards = groupBySuits cards
                                       runningCards = groupByRuns $ cards ++ lowAces cards
                                       pairs = filter (\x -> 2 == length x) rankedCards
                                       trips = filter (\x -> 3 == length x) rankedCards
                                       quads = filter (\x -> 4 == length x) rankedCards
                                       lowAces xs = map (\x -> (Card LowAce (suit x)) ) $ filter (\x -> (rank x) == Ace) xs
                                   in result hands

groupByRuns :: [Card] -> [[Card]]
groupByRuns cards = groupByBreak 0 breaks uniqueRanks
  where uniqueRanks = nubBy (\a b -> rank a == rank b) cards
        breaks = runBreaks uniqueRanks

-- convert cards to ranks and find out where discontinuities exist
-- this shows us where to split the cards at
runBreaks :: [Card] -> [Int]
runBreaks [] = []
runBreaks cards = indices
  where xs = map (fromEnum . rank) cards
        diffs = map (\(x,y) -> y-x ) $ zip xs (drop 1 xs)
        diffIndices = zip diffs [1..]
        breakPoints = filter (\x -> fst x /= 1) diffIndices
        indices = map snd breakPoints

-- group cards by break points
groupByBreak :: Int -> [Int] -> [Card] -> [[Card]]
groupByBreak start breaks [] = []
groupByBreak start [] xs = [xs]
groupByBreak start breaks xs = (fst taken):(groupByBreak break (tail breaks) (snd taken))
  where break = head breaks
        takeCount = break - start
        taken = splitAt takeCount xs


-- Find the best 5-card poker hand based on high card.
--
-- PRE: cards is sorted
constructHighCardHand :: [Card] -> Maybe PokerHand
constructHighCardHand cards = do
  fiveCards <- takeMay 5 cards
  let (a:b:c:d:e:_) = fiveCards
  return (PokerHand HighCard a b c d e)

-- Find the best 5-card poker hand based on a pair
--
-- PRE: pairs is an array of pairs found in the full set of cards
-- PRE: cards is sorted
constructPairHand :: [[Card]] -> [Card] -> Maybe PokerHand
constructPairHand pairs cards = do
  pair <- headMay pairs
  nonPairCards <- return $ cards \\ pair
  let (a:b:_) = pair
      (c:d:e:_) = nonPairCards
  return (PokerHand Pair a b c d e)

-- Find the best 5-card poker hand based on two pair
--
-- PRE: pairs is an array of pairs found in the full set of cards
-- PRE: cards is sorted
constructTwoPairHand :: [[Card]] -> [Card] -> Maybe PokerHand
constructTwoPairHand pairs cards = do
  twoPair <- fmap concat $ takeMay 2 pairs
  nonPairCards <- return $ cards \\ twoPair
  let (a:b:c:d:_) = twoPair
      (e:_) = nonPairCards
  return (PokerHand TwoPair a b c d e)

-- Find the best 5-card poker hand based on three of a kind
--
-- PRE: trips is an array of trips found in the full set of cards
-- PRE: cards is sorted
constructThreeOfAKindHand :: [[Card]] -> [Card] -> Maybe PokerHand
constructThreeOfAKindHand trips cards = do
  set <- headMay trips
  nonSetCards <- return $ cards \\ set
  let (a:b:c:_) = set
      (d:e:_) = nonSetCards
  return (PokerHand ThreeOfAKind a b c d e)

-- Find the best 5-card poker hand based on straight
--
-- PRE: runs is an array of arrays of cards that form contiguous sequences
constructStraightHand :: [[Card]] -> Maybe PokerHand
constructStraightHand runs = do
  highestRun <- headMay $ filter (\x -> length x > 4 ) runs
  let (a:b:c:d:e:_) = highestRun
  return (PokerHand Straight a b c d e)

-- Find the best 5-card poker hand based on flush
--
-- PRE: suited is an array of arrays of cards that are grouped by suit
-- TODO: this currently will only work for hands where only one flush is possible
constructFlushHand :: [[Card]] -> Maybe PokerHand
constructFlushHand suited = do
  flush <- headMay $ filter (\x -> length x > 4 ) suited
  let (a:b:c:d:e:_) = flush
  return (PokerHand Flush a b c d e)

-- Find the best 5-card poker hand based on full house
--
-- PRE: trips is an array of trips found in the full set of cards
-- PRE: pairs is an array of pairs found in the full set of cards
constructFullHouseHand :: [[Card]] -> [[Card]] -> Maybe PokerHand
constructFullHouseHand trips pairs = do
  set <- headMay trips
  pair <- headMay pairs
  let (a:b:c:_) = set
      (d:e:_) = pair
  return (PokerHand FullHouse a b c d e)

-- Find the best 5-card poker hand based on four of a kind
--
-- PRE: quads is an array of trips found in the full set of cards
-- PRE: cards is sorted
constructFourOfAKindHand :: [[Card]] -> [Card] -> Maybe PokerHand
constructFourOfAKindHand quads cards = do
  quad <- headMay quads
  nonQuadCards <- return $ cards \\ quad
  let (a:b:c:d:_) = quad
      e = head nonQuadCards
  return (PokerHand FourOfAKind a b c d e)

-- Find the best 5-card poker hand based on straight flush
--
-- PRE: suited is an array of arrays of cards that are grouped by suit
-- TODO: this currently will only work for hands where only one flush is possible
constructStraightFlushHand :: [[Card]] -> Maybe PokerHand
constructStraightFlushHand suited = do
  flush <- headMay $ filter (\x -> length x > 4 ) suited
  let cards = flush ++ lowAces flush
      lowAces xs = map (\x -> (Card LowAce (suit x)) ) $ filter (\x -> (rank x) == Ace) xs
      runningCards = groupByRuns cards
  highestRun <- headMay $ filter (\x -> length x > 4 ) runningCards
  let (a:b:c:d:e:_) = highestRun
  return (PokerHand StraightFlush a b c d e)

-- Generate all possible unique HoldEm community boards from a deck provided
generateBoards :: Int -> [Card] -> [[Card]]
generateBoards boardLength deck = let initial = Just $ DC.choose (length deck) boardLength
                             in unfoldr go initial
  where go nck
          | isJust nck = Just (board deck (DC.elems (fromJust nck)), DC.next (fromJust nck))
          | otherwise = Nothing
        board c i = map (\x -> c !! x) i


-- TODO: create a data type for this
handShape :: HoleCards -> String
handShape hand = concat [s, " ", c]
  where s = case (suited hand) of
          True -> "suited"
          False -> "off-suit"
        c = case (connected hand) of
          True -> "connected"
          False -> ""

-- generate board number N out of the deck available
-- N is zero-indexed and should not be larger that the
-- number of possible combinations
generateBoard :: [Card] -> Int -> [Card]
generateBoard deck n = map (\i -> deck !! i) $ nckValues n 5
