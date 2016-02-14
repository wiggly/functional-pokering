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

-- rank hands against flop from deck, returns pair of board and array of hands with their ranks
rankHands :: ([Card],[HoleCards]) -> ([Card],[(HoleCards,PokerRank)])
rankHands (board,hands) = (board,ranked)
  where ranked = map (\x -> (x, (pokerRank board x)) ) hands

-- this version actually works and memory stays below 60kB on the heap graph
calcEquity :: StdGen -> Int -> [HoleCards] -> [ShowdownTally]
calcEquity rnd samples hands = let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
                                   unUsedCards = standardDeck \\ usedCards
                                   deck = shuffleDeck rnd unUsedCards
                                   boards = take samples $ generateBoards deck
                                   blankTallies = replicate (length hands) blankTally
                               in foldl' go blankTallies boards
  where go total board = zipWith addTally (pokerEquity board hands) $!! total

-- this version, for some reason, evaluates the entire set of boards before beginning processing
xalcEquity :: StdGen -> Int -> [HoleCards] -> [ShowdownTally]
xalcEquity rnd samples hands = let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
                                   unUsedCards = standardDeck \\ usedCards
                                   deck = take samples $ shuffleDeck rnd unUsedCards
                                   boards = generateBoards deck
                                   blankTallies = replicate (length hands) blankTally
                               in foldr go blankTallies boards
  where go board total = zipWith addTally (pokerEquity board hands) $!! total

calcEquity' :: StdGen -> Int -> [HoleCards] -> [ShowdownTally]
calcEquity' rnd samples hands = let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
                                    unUsedCards = standardDeck \\ usedCards
                                    deck = take samples $ shuffleDeck rnd unUsedCards
                                    boards = generateBoards deck
                                    blankTallies = replicate (length hands) blankTally
                                in foldr go blankTallies boards
  where go board total = zipWith addTally (pokerEquity board hands) total

evaluateHands :: StdGen -> Int -> [HoleCards] -> IO ()
evaluateHands rnd samples hands = do
  mapM_ putHandLn $ hands
  let usedCards = foldr (\x acc -> (fst x):(snd x):acc ) [] hands
      deck = standardDeck \\ usedCards
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
    else do putStrLn "CALCULATING EQUITY NOW"
            let result = zip hands equity
            mapM_ putEquity result
            mapM_ putTally result

cmdLineEvalMain :: IO ()
cmdLineEvalMain = do
  (seed:samplesStr:rest) <- getArgs
  let rnd = mkStdGen $ (read seed :: Int)
      strCards = rest
      hands = map readHoleCards strCards
      samples = read samplesStr :: Int
  if all isJust hands
    then evaluateHands rnd samples $ catMaybes hands
    else putStrLn $ "cannot parse hands: " ++ (unwords strCards)

main = cmdLineEvalMain
