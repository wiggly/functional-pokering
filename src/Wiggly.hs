-- -*- mode: haskell; -*-

module Wiggly
       (
         sortOn,

         nChooseK,
         chunkList,
         comboCount
       )
       where

import Data.Ord (comparing)
import Data.List (sort, sortBy)

-- stolen from new base
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
    map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

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


-- groupRuns :: [a] -> [[a]]
-- groupRuns (x:xs) = foldr go ([x],[]) xs
--   where go x (current,runs) = if x == (p+1)
--                               then (init runs) ++ (tail runs) ++
--                               else runs ++ [[x]]


-- diffX :: Enum a => [a] -> [b]
-- diffX [] = []
-- diffX (x:xs) = snd $ foldr go (x,[]) xs
--   where go ce (last,lst) = ((fromEnum ce),((fromEnum ce)-last):lst)

diffY :: Num a => [a] -> [a]
diffY [] = []
diffY (x:xs) = reverse $ snd $ foldl go (x,[]) xs
  where go (last,lst) ce = (ce,(ce-last):lst)

discontinuityIndices :: (Num a, Ord a, Integral b) => [a] -> [b]
discontinuityIndices xs = map snd $ filter (\x -> fst x > 1) $ zip (diffY $ sort xs) [1..]


-- this kind of works but leaves out the final set of numbers that come after the final discontinuity
groupByDiscontinuity :: (Num a, Ord a) => [a] -> [[a]]
groupByDiscontinuity xs = res
  where d = discontinuityIndices xs
        (_,_,res) = foldl go (0,xs,[]) d
        go (offset,xs,ys) idx = (idx+offset, (snd (splitAt (idx-offset) xs)), (fst (splitAt (idx-offset) xs)):ys )
