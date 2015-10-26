-- -*- mode: haskell; -*-

module Main (main) where

import Cards
import Cards.Output
import Data.List
import Poker
import System.Environment
import System.Random
import Rainbow
import qualified Data.ByteString as BS
import Control.Monad
import Data.Monoid
import Data.Maybe
import Wiggly (chunkList)
import Control.Applicative
import Data.Ratio

-- chunk functions
chunkCard :: Card -> Chunk String
chunkCard = fourColourDeckCardChunk

chunkCards :: String -> [Card] -> [Chunk String]
chunkCards sep cards = intersperse (chunk sep) $ map chunkCard cards

chunkBoard :: [Card] -> [Chunk String]
chunkBoard board = (chunk "Board: ") : cards
  where cards = chunkCards " " board

chunkHand :: HoldEmHand -> [Chunk String]
chunkHand h@(c1,c2) = [ (chunk "Hand:"),
                sep,
                (chunkCard c1),
                sep,
                (chunkCard c2),
                sep,
                (chunk (handShape h))
              ]
  where sep = chunk " "

chunkHandRank :: PokerRank -> [Chunk String]
chunkHandRank r = [chunk (show r)]

chunkRankedHand :: (HoldEmHand,PokerRank) -> [Chunk String]
chunkRankedHand (h,r) = chunkHand h ++ [sep] ++ chunkHandRank r
  where sep = chunk " "


-- chunk output functions
putChunks :: [Chunk String] -> IO ()
putChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256

putTable :: [Card] -> [(HoldEmHand,PokerRank)] -> IO ()
putTable board hands = putChunks $ title ++ boardChunks ++ handChunks
  where boardChunks = chunkBoard board ++ newline
        handChunks = concat $ map (\x -> (chunkRankedHand x) ++ newline ) hands
        newline = [chunk "\n"]
        title = [chunk "Poker Table\n"]

putCard :: Card -> IO ()
putCard x = putChunks $ [chunkCard x]

putCardLn :: Card -> IO ()
putCardLn x = do putCard x
                 putStrLn ""

putCards :: [Card] -> IO ()
putCards xs = putChunks $ map chunkCard xs

putHand :: HoldEmHand -> IO ()
putHand hand = putChunks $ chunkHand hand

putHandLn :: HoldEmHand -> IO ()
putHandLn hand = do putHand hand
                    putStrLn ""

putEquity :: (HoldEmHand,ShowdownTally) -> IO ()
putEquity (h,t) = do putHand h
                     putStrLn $ "equity: " ++ show e ++ "%"
  where e = 100 * (fromRational (tallyEquityPercentage t))

putTally :: (HoldEmHand,ShowdownTally) -> IO ()
putTally (h,t) = do putHand h
                    putStrLn $ "tally: " ++ (show t)

-- original unadorned output functions
showBoard :: [Card] -> String
showBoard xs = "Board: " ++ (intercalate " " $ map show xs)

showHand :: HoldEmHand -> String
showHand x = "Hand: " ++ (show (fst x)) ++ " " ++ (show (snd x)) ++ (handShape x)

showRankedHand :: (HoldEmHand,PokerRank) -> String
showRankedHand (hand,rank) = (showHand hand) ++ " - rank: " ++ (show rank)

displayHand :: HoldEmHand -> IO ()
displayHand x = do putStrLn $ showHand x

displayHands :: [HoldEmHand] -> IO ()
displayHands [] = putStrLn ""
displayHands (x:xs) = do displayHand x
                         displayHands xs

-- hand reading functions

-- TODO: create hand shape data type and show function for it
handShape :: HoldEmHand -> String
handShape hand = concat [s, " ", c]
  where s = case (suited hand) of
          True -> "suited"
          False -> "off-suit"
        c = case (connected hand) of
          True -> "connected"
          False -> ""

-- rank hands against flop from deck, returns pair of board and array of hands with their ranks
rankHands :: ([Card],[HoldEmHand]) -> ([Card],[(HoldEmHand,PokerRank)])
rankHands (board,hands) = (board,ranked)
  where ranked = map (\x -> (x, (pokerRank board x)) ) hands





evaluateOpposingHands :: StdGen -> IO ()
evaluateOpposingHands rnd = do putStrLn "Evaluating hands....."
                               handA <- putStr "First Hand: " >> getLine
                               putStrLn handA
                               handB <- putStr "Second Hand: " >> getLine
                               putStrLn handB
                               let defaultHand = ((Card Two Spades), (Card Three Hearts))
                                   hands = map readHoldEmHand [handA, handB]
                                   okay = all isJust hands
                                   ma = fromMaybe defaultHand (readHoldEmHand handA)
                               if okay
                                 then do putStrLn "read in hands"
                                         mapM_ putHandLn $ catMaybes hands
                                         --putHandLn $ fromJust $ head hands
                                         --putHandLn $ fromJust $ last hands
                                 else putStrLn "couldn't read hands"

tallyEquityPercentage :: ShowdownTally -> Rational
tallyEquityPercentage t = good % total
  where good = fromInteger $ win t + tie t
        bad = loss t
        total = fromInteger $ good + bad

calcEquity :: StdGen -> Integer -> [HoldEmHand] -> [(HoldEmHand,ShowdownTally)]
calcEquity rnd samples hands = mergeHandTallies tallies
  where tallies = foldr go [] boards
        go board acc = (pokerEquity board hands):acc
        boards = generateBoards rnd samples deck
        usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
        deck = filter (\x -> not $ elem x usedCards) standardDeck

mergeHandTallies :: [[(HoldEmHand,ShowdownTally)]] -> [(HoldEmHand,ShowdownTally)]
mergeHandTallies tallies = zipWith (,) hands ts
  where ts = mergeTallies $ map (map snd) tallies
        hands = map fst $ head tallies


mergeTallies ::  [[ShowdownTally]] -> [ShowdownTally]
mergeTallies tallies = foldr go (replicate (length $ head tallies) blankTally) tallies
  where go t acc = zipWith addTally t acc


generateBoards :: StdGen -> Integer -> [Card] -> [[Card]]
generateBoards rnd count deck = unfoldr go (count,rnd,deck)
  where go (n,r,d) = if n == 0
                     then Nothing
                     else Just ( (board r), (n-1,(nextRnd r),deck))
        board x = take 5 $ shuffleDeck (thisRnd x) deck
        thisRnd x = fst $ split x
        nextRnd x = snd $ split x

evaluateHands :: Integer -> [HoldEmHand] -> IO ()
evaluateHands samples hands = do
  rnd <- getStdGen
  mapM_ putHandLn $ hands
  let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
      deck = filter (\x -> not $ elem x usedCards) standardDeck
      shuffled = shuffleDeck rnd deck
      equity = calcEquity rnd samples hands
  putStrLn $ "Samples: " ++ (show samples)
  putStrLn "Used Cards:"
  mapM_ putCard $ usedCards
  putStrLn ""
  putStrLn "Deck:"
  mapM_ putCard deck
  putStrLn ""
  if length usedCards /= (length . nub) usedCards
    then error "ERR: Cards shared between hands"
    else do mapM_ putEquity equity
            mapM_ putTally equity

cmdLineEvalMain :: IO ()
cmdLineEvalMain = do
  (seed:samplesStr:rest) <- getArgs
  let strCards = rest
      hands = map readHoldEmHand strCards
      samples = read samplesStr :: Integer
  if all isJust hands
    then evaluateHands samples $ catMaybes hands
    else putStrLn $ "cannot parse hands: " ++ (unwords strCards)

testMain :: IO ()
testMain = do
  args <- getArgs
  let seed = read (head args) :: Int
      rnd = mkStdGen seed
      shuffled = shuffleDeck rnd standardDeck
      (hands, remainder) = (generateHands shuffled 8)
      (board, rankedHands) = rankHands ( (take 5 remainder), hands)
  putTable board rankedHands
  putStrLn "End of board\n\n"
  putHandLn (fst (head (drop 3 rankedHands)))
  evaluateOpposingHands rnd

main = cmdLineEvalMain
