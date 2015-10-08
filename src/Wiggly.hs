-- -*- mode: haskell; -*-

module Wiggly
       (
         nChooseK,
         chunkList,
         comboCount
       )
       where

comboCount :: Int -> Int -> Int
comboCount n k
  | n < k = error "Cannot choose more than the number of elements"
  | n == k = 1
  | k == 1 = n
  | otherwise = (comboCount (n-1) k) + (comboCount (n-1) (k-1))

-- n choose k
nChooseK :: [a] -> Int -> [[a]]
nChooseK xs 1 = map (\x -> [x]) xs
nChooseK xs n
  | (length xs) > n = foo ++ bar
  | (length xs) == n = [xs]
  | otherwise = []
  where foo = map (\x -> (head xs):x ) (nChooseK (tail xs) (n-1))
        bar = (nChooseK (tail xs) n)

chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = thisChunk : (chunkList n thisRemainder)
  where thisChunk = take n xs
        thisRemainder = drop n xs
