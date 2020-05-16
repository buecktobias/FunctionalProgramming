module Main where

import PascalsTriangle

amountGoodStudents:: [Int] -> Int
amountGoodStudents ls = length (filter (>10) ls)


average:: [Int] -> Double
average ls = (fromIntegral (sum ls)) / (fromIntegral (length ls))

isGoodStudent:: (Int, String) -> Bool
isGoodStudent (grade,name) = grade > 10 

getNamesOfGoodStudents:: [(Int, String)] -> [(Int, String)]
getNamesOfGoodStudents ls = filter isGoodStudent ls



main :: IO ()
main = do
      let l = "Hallo was geht?"
      print (createPascalsTriangle 10)

