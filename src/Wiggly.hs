-- -*- mode: haskell; -*-

module Wiggly
       (
         nChooseK,
         chunkList
       )
       where

-- n choose k
nChooseK :: [a] -> Int -> [[a]]
nChooseK xs 1 = map (\x -> [x]) xs
nChooseK xs n
  | (length xs) > n = foo ++ bar 
  | (length xs) == n = [xs] 
  | otherwise = []
  where foo = map (\x -> (head xs):x ) (nChooseK (tail xs) (n-1))
        bar = (nChooseK (tail xs) n)

combinations :: Int -> Int -> Int
combinations n k
  | n == k = 1
  | otherwise = combinations (n-1) (k-1)  

chunkList :: Int -> [a] -> [[a]]  
chunkList _ [] = []
chunkList n xs = thisChunk : (chunkList n thisRemainder)
  where thisChunk = take n xs
        thisRemainder = drop n xs

