module PascalsTriangle where

fac :: Int -> Int 
fac 0 = 1
fac n = n * fac (n - 1)

calculateNum:: Int -> Int -> Int
calculateNum row column = round ((fromIntegral (fac row)) / (fromIntegral ((fac column) * (fac (row - column))))) :: Int

createPascalsTriangle:: Int -> [[Int]]
createPascalsTriangle size = map (\r -> map (\(row,column) -> calculateNum row column) r) rowsColumns
                            where
                            rowsColumns = [[(row, column) | column <- [0 .. row]] | row <- [0 .. size]]
