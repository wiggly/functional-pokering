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

import Control.Parallel.Strategies

-- rank hands against flop from deck, returns pair of board and array of hands with their ranks
rankHands :: ([Card],[HoleCards]) -> ([Card],[(HoleCards,PokerRank)])
rankHands (board,hands) = (board,ranked)
  where ranked = map (\x -> (x, (pokerRank board x)) ) hands

-- instead of enumerating all hands this function calculates equity for a subset of
-- the combinations of possible hands given by the numbers min and max.
-- These are zero-indexed indices of the combination to be tested
calcEquity :: Int -> Int -> [Card] -> [HoleCards] -> [ShowdownTally]
calcEquity min max deck hands = let usedCards = foldr (\(HoleCards x y) acc -> x:y:acc ) [] hands
                                    deckx = standardDeck \\ usedCards
                                    generator = generateBoard deck
                                    boards = [ generator i | i <- [min..max] ]
                                    blankTallies = replicate (length hands) blankTally
                                in foldl' go blankTallies boards
  where go total board = zipWith addTally (pokerEquity board hands) $!! total

parCalcEquity :: Int -> [Card] -> [HoleCards] -> [ShowdownTally]
parCalcEquity samples deck hands = let cores = 4
                                       samplesPerCore = samples `div` cores
                                       lastSampleExtra = samples `rem` cores
                                       sections = [ (i * samplesPerCore, ((i+1) * samplesPerCore)-1) | i <- [0..(cores-1)] ]
                                       results = map (\x -> calcEquity (fst x) (snd x) deck hands) sections `using` parList rdeepseq
                                   in foldr (\x acc -> zipWith addTally x acc) (head results) (tail results)

evaluateHands :: StdGen -> Int -> [HoleCards] -> IO ()
evaluateHands rnd samples hands = do
  mapM_ putHandLn $ hands
  let usedCards = foldr (\(HoleCards x y) acc -> x:y:acc ) [] hands
      deck = standardDeck \\ usedCards
      shuffled = shuffleDeck rnd deck
      equity = parCalcEquity samples shuffled hands
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

-- TODO: remove this file altogether, make parallelism an option flag
main = cmdLineEvalMain
