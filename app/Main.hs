module Main where

import PrefixCompression

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
      let l = "leonlol"
      let l2 = "leonlacht"
      let pre = findPrefix l l2
      print (removePrefix pre l )
      print (removePrefix pre l2 )
      print ( pre )

