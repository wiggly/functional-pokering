module Cards.Output
       (
         putCard,
         putCards,
         putHandLn,
         putEquity,
         putTally
       ) where

import Cards
import Data.List
import Numeric
import Poker
import Rainbow
import qualified Data.ByteString as BS

formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

-- chunk functions
chunkCard :: Card -> Chunk String
chunkCard = fourColourDeckCardChunk

chunkCards :: String -> [Card] -> [Chunk String]
chunkCards sep cards = intersperse (chunk sep) $ map chunkCard cards

chunkBoard :: [Card] -> [Chunk String]
chunkBoard board = (chunk "Board: ") : cards
  where cards = chunkCards " " board

chunkHoleCards :: HoleCards -> [Chunk String]
chunkHoleCards h@(HoleCards c1 c2) = [ (chunk "Hole Cards: "),
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

chunkRankedHand :: (HoleCards,PokerRank) -> [Chunk String]
chunkRankedHand (h,r) = chunkHoleCards h ++ [sep] ++ chunkHandRank r
  where sep = chunk " "


-- chunk output functions
putChunks :: [Chunk String] -> IO ()
putChunks = mapM_ BS.putStr . chunksToByteStrings toByteStringsColors256

putTable :: [Card] -> [(HoleCards,PokerRank)] -> IO ()
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

putHand :: HoleCards -> IO ()
putHand hand = putChunks $ chunkHoleCards hand

putHandLn :: HoleCards -> IO ()
putHandLn hand = do putHand hand
                    putStrLn ""

putEquity :: (HoleCards,ShowdownTally) -> IO ()
putEquity (h,t) = do putHand h
                     putStrLn $ " equity - win: " ++ winPct ++ "% - tie: " ++ tiePct ++ "%"
  where winPct = formatFloatN we 2
        tiePct = formatFloatN te 2
        (we,te) = percentTally t

putTally :: (HoleCards,ShowdownTally) -> IO ()
putTally (h,t) = do putHand h
                    putStrLn $ " tally: " ++ (show t)

-- original unadorned output functions
showBoard :: [Card] -> String
showBoard xs = "Board: " ++ (intercalate " " $ map show xs)

showHand :: HoleCards -> String
showHand h@(HoleCards x y) = "Hand: " ++ (show x) ++ " " ++ (show y) ++ (handShape h)

showRankedHand :: (HoleCards,PokerRank) -> String
showRankedHand (hand,rank) = (showHand hand) ++ " - rank: " ++ (show rank)

displayHand :: HoleCards -> IO ()
displayHand x = do putStrLn $ showHand x

displayHands :: [HoleCards] -> IO ()
displayHands [] = putStrLn ""
displayHands (x:xs) = do displayHand x
                         displayHands xs





-- red and white colouring
traditionalColourDeckCardChunk :: Card -> Chunk String
traditionalColourDeckCardChunk = ansiCard white red red white

-- four-colour deck colouring
fourColourDeckCardChunk :: Card -> Chunk String
fourColourDeckCardChunk = ansiCard white red cyan green

-- take a card and return a chunk coloured correctly for output to a terminal
-- the four colours are accepted in order for ;
-- Spades
-- Hearts
-- Diamonds
-- Clubs
--
ansiCard :: Radiant -> Radiant -> Radiant -> Radiant -> Card -> Chunk String
ansiCard r _ _ _ c@(Card _ Spades) = chunk (show c) & fore r
ansiCard _ r _ _ c@(Card _ Hearts) = chunk (show c) & fore r
ansiCard _ _ r _ c@(Card _ Diamonds) = chunk (show c) & fore r
ansiCard _ _ _ r c@(Card _ Clubs) = chunk (show c) & fore r
