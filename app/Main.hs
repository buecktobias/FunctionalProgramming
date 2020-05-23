module Main where
import ClimbingTheLeaderboard

main :: IO ()
main = do
      let ranks = [100, 100, 50, 40, 40, 20, 10]
      let ls = [5, 25, 50, 120]
      print(indexOf ls 50)
