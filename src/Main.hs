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

main = do
  args <- getArgs  
  let seed = read (head args) :: Int
      rnd = mkStdGen seed
      shuffled = shuffleDeck rnd standardDeck
      (hands, remainder) = (generateHands shuffled 8)
      (board, rankedHands) = rankHands ( (take 5 remainder), hands)
  putTable board rankedHands
  putStrLn "End of board\n\n"
  putHandLn (fst (head (drop 3 rankedHands)))
