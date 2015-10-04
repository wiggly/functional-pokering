module Main (main) where

import Cards
import Cards.Output
import Data.List
import Poker
import System.Environment
import System.Random
import Numeric (readHex)
import Data.Char (chr)
import Rainbow
import qualified Data.ByteString as BS

-- TODO: put all IO into a Cards.Output module
colouredCard :: Card -> Chunk String
colouredCard = fourColourDeckCardChunk

putChunks :: [Chunk String] -> IO ()
putChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256

putCard :: Card -> IO ()
putCard card = putChunks $ [colouredCard card]

putCards :: [Card] -> IO ()
putCards cards = putChunks $ cardChunks cards " "

putHand :: HoldEmHand -> IO ()
putHand hand = putChunks $ handChunks hand

putBoard :: [Card] -> IO ()
putBoard board = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256 $ (boardChunks board)

boardChunks :: [Card] -> [Chunk String]
boardChunks board = [(chunk "Board: ")] ++ cards ++ [(chunk "\n")]
  where cards = cardChunks board " "

cardChunks :: [Card] -> String -> [Chunk String]
cardChunks cards sep = intersperse (chunk sep) $ map colouredCard cards

handChunks :: HoldEmHand -> [Chunk String]
handChunks x = [ (chunk "Hand: "),
                 (colouredCard (fst x)),
                 (chunk " "),
                 (colouredCard (snd x)),
                 (chunk " "),
                 (chunk (handShape x)),
                 (chunk "\n")
               ]

-- TODO: create hand shape data type and show function for it
handShape :: HoldEmHand -> String
handShape hand = concat [s, " ", c]
  where s = case (suited hand) of
          True -> "suited"
          False -> "off-suit"
        c = case (connected hand) of
          True -> "connected"
          False -> ""

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
--  putStrLn $ show standardDeck
  putStrLn $ "Shuffled Deck: " ++ (show shuffled)
  putStrLn $ showBoard board
  putStrLn $ unlines (map showRankedHand rankedHands)
  putBoard board
  mapM_ putHand hands 
  putCards shuffled
  putStrLn ""
