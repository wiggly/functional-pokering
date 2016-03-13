-- -*- mode: haskell; -*-
module Main (main) where

import Cards
import Cards.Output
import Data.List
import Poker
import System.Environment
import System.Random
import Control.Monad
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Data.Ratio
import Control.DeepSeq
import Debug.Trace (trace)
import Control.Applicative (optional)
import Options.Applicative

-- rank hands against flop from deck, returns pair of board and array of hands with their ranks
rankHands :: ([Card],[HoleCards]) -> ([Card],[(HoleCards,PokerRank)])
rankHands (board,hands) = (board,ranked)
  where ranked = map (\x -> (x, (pokerRank board x)) ) hands

-- added stub and board cards conpared to old equity calc.
-- also moved shuffling out
newCalcEquity :: Int -> [Card] -> [Card] -> [HoleCards] -> [ShowdownTally]
newCalcEquity samples stub board hands = let boards = take samples $ generateBoards requiredBoardLength stub
                                             requiredBoardLength = 5 - (length board)
                                             blankTallies = replicate (length hands) blankTally
                                         in foldl' go blankTallies boards
  where go total completeBoard = zipWith addTally (pokerEquity (concat [board, completeBoard]) hands) $!! total


-- this version actually works and memory stays below 60kB on the heap graph
calcEquity :: StdGen -> Int -> [HoleCards] -> [ShowdownTally]
calcEquity rnd samples hands = let usedCards = foldr (\(HoleCards x y) acc -> x:y:acc ) [] hands
                                   unUsedCards = standardDeck \\ usedCards
                                   deck = shuffleDeck rnd unUsedCards
                                   boards = take samples $ generateBoards 5 deck
                                   blankTallies = replicate (length hands) blankTally
                               in foldl' go blankTallies boards
  where go total board = zipWith addTally (pokerEquity board hands) $!! total

-- this version, for some reason, evaluates the entire set of boards before beginning processing
-- xalcEquity :: StdGen -> Int -> [HoleCards] -> [ShowdownTally]
-- xalcEquity rnd samples hands = let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
--                                    unUsedCards = standardDeck \\ usedCards
--                                    deck = take samples $ shuffleDeck rnd unUsedCards
--                                    boards = generateBoards 5 deck
--                                    blankTallies = replicate (length hands) blankTally
--                                in foldr go blankTallies boards
--   where go board total = zipWith addTally (pokerEquity board hands) $!! total

-- calcEquity' :: StdGen -> Int -> [HoleCards] -> [ShowdownTally]
-- calcEquity' rnd samples hands = let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
--                                     unUsedCards = standardDeck \\ usedCards
--                                     deck = take samples $ shuffleDeck rnd unUsedCards
--                                     boards = generateBoards 5 deck
--                                     blankTallies = replicate (length hands) blankTally
--                                 in foldr go blankTallies boards
--   where go board total = zipWith addTally (pokerEquity board hands) total

-- TODO: check that dead/board/hole do not intersect
evaluateHands :: StdGen -> Int -> [Card] -> [Card] -> [HoleCards] -> IO ()
evaluateHands rnd trials dead board hands = do
  mapM_ putHandLn $ hands
  let usedCards = concat [dead, board, usedHandCards]
      usedHandCards = foldr (\(HoleCards x y) acc -> x:y:acc ) [] hands
      stub = standardDeck \\ usedCards
      shuffledStub = shuffleDeck rnd stub
      equity = newCalcEquity trials shuffledStub board hands
  putStrLn $ "Trials: " ++ (show trials)
  putStrLn "Used Cards:"
  mapM_ putCard $ usedCards
  putStrLn ""
  putStrLn "Stub:"
  mapM_ putCard stub
  putStrLn ""
  if length usedCards /= (length . nub) usedCards
    then error "ERR: Cards shared between hands"
    else do putStrLn "CALCULATING EQUITY NOW"
            let result = zip hands equity
            mapM_ putEquity result
            mapM_ putTally result

data PokerOpts = PokerOpts {
  seed :: Maybe Int,
  trials :: Maybe Int,
  deadCards :: [Card],
  boardCards :: [Card],
  hands :: [HoleCards]
  }

seedOption :: Parser (Maybe Int)
seedOption = optional $ option auto
  (long "seed"
   <> short 's'
   <> metavar "SEED"
   <> help "PRNG seed to use")

trialsOption :: Parser (Maybe Int)
trialsOption = optional $ option auto
  (long "trials"
   <> short 't'
   <> metavar "TRIALS"
   <> help "Number of sample boards to run"
   <> value (100000 :: Int))

deadCardsOption :: Parser [Card]
deadCardsOption = option auto
  (long "dead"
   <> short 'd'
   <> metavar "DEADCARDS"
   <> help "Cards known to be dead, no longer in the deck"
   <> value [])

boardCardsOption :: Parser [Card]
boardCardsOption = option auto
  (long "board"
   <> short 'b'
   <> metavar "BOARDCARDS"
   <> help "Cards already on the board"
   <> value [])

pokerOpts :: Parser PokerOpts
pokerOpts = PokerOpts <$> seedOption <*> trialsOption <*> deadCardsOption <*> boardCardsOption
  <*> many (argument auto (metavar "HANDS..."))

runEquity :: PokerOpts -> IO ()
runEquity (PokerOpts s t d b hs) = do
  rnd <- maybe getStdGen (return . mkStdGen) s
  trials <- return $ fromJust t
  putStrLn $ "rnd: " ++ (show rnd)
  putStrLn $ "trials: " ++ (show trials)
  putStrLn $ "dead: " ++ (show d)
  putStrLn $ "board: " ++ (show b)
  putStrLn $ "hands: " ++ (show hs)
  evaluateHands rnd trials d b hs

main :: IO ()
main = execParser opts >>= runEquity
  where
    opts = info (helper <*> pokerOpts)
      (fullDesc
       <> progDesc "Calculate equity for multiple hands"
       <> header "fp - poker equity calculator")
