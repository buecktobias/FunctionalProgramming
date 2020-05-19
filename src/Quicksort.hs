module Quicksort where
import Data.List

lstLength:: [Int] -> Int
lstLength [] = 0
lstLength (x:xs) = 1 + lstLength xs

lstLength2:: [Int] -> Int
lstLength2 ls = foldl (\x _ -> x + 1) 0 ls

abs2:: Int -> Int
abs2 x
    | x < 0 = -x
    | otherwise= x


evenNums:: Int -> Int -> [Int]
evenNums a b = filter even [a..b]

qSort:: (Ord a) => [a] -> [a]
qSort [] = []
qSort [n] = [n]
qSort (x:xs) = (qSort (filter (<x) xs)) ++ [x] ++ (qSort (filter (>=x) xs))


merge:: (Ord a) => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge l1@(x:xs) l2@(y:ys)
    | x < y = [x] ++ merge xs l2
    | otherwise = [y] ++ merge l1 ys

mergeSort:: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let
               m = round ((fromIntegral (length xs)) / 2)
               splitted = splitAt m xs
               firstList = fst splitted
               lastList = snd splitted
               in merge (mergeSort firstList) (mergeSort lastList)

mInsert:: (Ord x) => x -> [x] -> [x]
mInsert e [] = [e]
mInsert e ls
             | last ls < e = ls ++ [e]
             | otherwise= mInsert e (init ls) ++ [last ls]


insertionSort:: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort ls = insertionSort' ls 1

insertionSort':: (Ord a) => [a] -> Int -> [a]
insertionSort' ls n
                    | n < (length ls) =
                    let
                    splitted = splitAt (n - 1) ls
                    firstPart = fst splitted
                    lastPart = tail (snd splitted)
                    in
                    insertionSort' ((mInsert  (ls !! n)  firstPart) ++  lastPart) (n + 1)
                    | otherwise = init ls