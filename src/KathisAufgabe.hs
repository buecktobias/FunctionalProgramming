module KathisAufgabe where


getAmountNumsRepetitionsUpTo:: Int -> Int -> [Int]
getAmountNumsRepetitionsUpTo reps upTo = concat [replicate reps i  | i <- [0 .. upTo]]


getNumsCycle:: Int -> Int -> Int -> [Int]
getNumsCycle reps upTo cycleScore = take ((upTo + 1) * cycleScore * reps) (cycle (getAmountNumsRepetitionsUpTo reps upTo))


addColumnToMatrix:: [[Int]] -> [Int] -> [[Int]]
addColumnToMatrix matrix column = map (\(row, c) -> row ++ [c]) (zip matrix column)


makeListList:: [Int] -> [[Int]]
makeListList ls = map (\x -> [x]) ls


getAllCombis:: Int -> [[Int]]
getAllCombis amount = addColumnToMatrix (addColumnToMatrix (makeListList (getNumsCycle (amount*2) amount 1 )) (getNumsCycle 2 amount 4)) (getNumsCycle 1 amount 8)
