module Main where

import Lib

main :: IO ()
main = do
  let n = 10
  let result = fact n
  print result
