-- -*- mode: haskell; -*-
module Main (main) where

import Cards
import Cards.Output
import Poker (generateBoards)
import System.Environment (getArgs)

-- this version displays the length of the boards array, which
-- implies it has to generate/unfold the full set of boards
-- before it can tell the length
showEachBoardLengthPlus :: [[Card]] -> IO ()
showEachBoardLengthPlus boards = do
  putStrLn $ "number of boards: " ++ ((show . length) boards)
  mapM_ (putStrLn . (show . length)) boards
  putStrLn ""

showEachBoardLength :: [[Card]] -> IO ()
showEachBoardLength boards = do
  mapM_ (putStrLn . (show . length)) boards


main = do
  (sizeStr:version:rest) <- getArgs
  let size = read sizeStr :: Int
      deck = take size standardDeck
  if version == "slow"
  then showEachBoardLengthPlus $ generateBoards 5 deck
  else showEachBoardLength $ generateBoards 5 deck
