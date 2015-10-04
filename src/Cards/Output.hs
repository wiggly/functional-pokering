module Cards.Output 
       (
         traditionalColourDeckCardChunk,
         fourColourDeckCardChunk
       ) where

import Cards
import Rainbow

















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
