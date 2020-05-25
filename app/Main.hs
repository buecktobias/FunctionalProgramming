module Main where
import NonDivisibleSubset

main :: IO ()
main = do
      let ranks = [19,10,12,10,24,25,22]
      print(nonDivisibleSubSet ranks 3)
