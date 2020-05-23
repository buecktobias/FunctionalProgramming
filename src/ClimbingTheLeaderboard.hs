module ClimbingTheLeaderboard where

getUniques:: (Eq a) => [a] -> [a]
getUniques ls = getUniques' ls []


getUniques':: (Eq a) => [a] -> [a] -> [a]
getUniques' [] uniques = uniques
getUniques' (x:xs) uniques = if not (x `elem` uniques) then getUniques' xs (uniques ++ [x]) else getUniques' xs uniques

insertRank:: [Int] -> Int -> [Int]
insertRank [] rank = [rank]
insertRank (x:xs) rank
  | rank <= x = [x] ++ insertRank xs rank
  | otherwise = [rank] ++ [x] ++ xs

getLeaderBoardRank:: [Int] -> Int -> Int
getLeaderBoardRank ranks r =  1    + (indexOf (getUniques (insertRank ranks r)) r)

indexOf:: [Int] -> Int -> Int
indexOf [] e = 0
indexOf (x:xs) e
  | x == e = 0
  | otherwise= 1 + indexOf xs e

getLeaderBoardRanks:: [Int] -> [Int] -> [Int]
getLeaderBoardRanks ranks ls = map (\x -> getLeaderBoardRank ranks x) ls
