module Main where
import Quicksort

main :: IO ()
main = do
      let t = [1,4,8,2,3,2,1,2]
      print (insertionSort t)


