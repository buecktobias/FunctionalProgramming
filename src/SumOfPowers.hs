module SumOfPowers where

getCombisPowerSum:: Int -> Int -> [[Int]]
getCombisPowerSum result pow = filterCombis (getPossibleCombis [1 .. (ceiling (sqrt (fromIntegral result)))]) pow result

getAmountCombisPowerSum:: Int -> Int -> Int
getAmountCombisPowerSum result pow = amountCombis (getPossibleCombis [1 .. (ceiling (sqrt (fromIntegral result)))]) pow result


amountCombis:: [[Int]] -> Int -> Int-> Int 
amountCombis combis pow result = length (filterCombis combis pow result)


filterCombis:: [[Int]] -> Int -> Int -> [[Int]]
filterCombis combis pow result = filter (\ls -> bPowerSum ls pow result) combis

allInsertAtStart:: [[Int]] -> Int -> [[Int]]
allInsertAtStart all x = map (\ls -> x : ls) all

getPossibleCombis:: [Int] -> [[Int]]
getPossibleCombis [] = [[]]
getPossibleCombis all@(x:xs) = (allInsertAtStart (getPossibleCombis xs) x) ++ (getPossibleCombis xs)

bPowerSum:: [Int] -> Int -> Int -> Bool
bPowerSum nums pow s = (powerSum nums pow) == s

powerSum:: [Int] -> Int -> Int
powerSum [] pow = 0
powerSum nums pow = foldl1 (+) (map (\x -> power x pow) nums)

power:: Int -> Int -> Int
power n 0 = 1
power n x = n * (power n (x - 1))
