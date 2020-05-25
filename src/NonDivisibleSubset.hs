module NonDivisibleSubset where

allInsertAtStart:: [[Int]] -> Int -> [[Int]]
allInsertAtStart all x = map (\ls -> x : ls) all

getPossibleCombis:: [Int] -> [[Int]]
getPossibleCombis [] = [[]]
getPossibleCombis all@(x:xs) = (allInsertAtStart (getPossibleCombis xs) x) ++ (getPossibleCombis xs)


allNotDivisible:: [Int] -> Int -> Bool
allNotDivisible nums divisor= length (filter (\x -> not (isDivisible x divisor)) nums) == (length nums)

nonDivisibleSubSet:: [Int] -> Int -> Int
nonDivisibleSubSet set div= maximum (map length (filter (\x -> allSumsDivisible x div) (getPossibleCombis set)))


allSumsDivisible:: [Int] -> Int -> Bool
allSumsDivisible nums div = allNotDivisible (getAllSums nums) div

isDivisible:: Int -> Int -> Bool
isDivisible num divisor = num `mod` divisor == 0


getAllSums:: [Int] -> [Int]
getAllSums  ls = map sum (filter (\x -> length x == 2) (getPossibleCombis ls))



