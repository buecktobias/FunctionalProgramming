module GridSearch where


getListFromTo:: [Int] -> Int -> Int -> [Int]
getListFromTo ls from to = result
      where
      toLs = fst (splitAt (to) ls)
      result = drop from toLs

getCoords:: Int -> Int -> [(Int, Int)]
getCoords maxY maxX = coordinates
          where 
          ys  = concat [replicate maxX  i| i <- [ 0 .. maxY]]
          xs = take (maxX * maxY) (cycle [0 .. maxX])
          coordinates = zip ys xs


gridSearch:: [[Int]] -> [[Int]] -> (Int, Int)
gridSearch grid pattern = (0, 0)   -- snd (head (filter (\grids -> (fst grids) == pattern) gridsAtCoordinates))
          where
          numRows = length pattern
          numColumns = (length  (pattern !! 0))
          maxY = length grid - numRows
          maxX = (length (grid !! 0)) - numColumns
          coordinates = getCoords maxY maxX
          gridsAtCoordinates = map (\(row, column) ->  (getGrid grid row column numRows numColumns, (row,column))) coordinates

getGrid:: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
getGrid grid row column numRows numColumns = [ let r = (grid !! rowIndex) in getListFromTo r (column) (column + numColumns)  | rowIndex <- [row .. (row + numRows - 1)]]



