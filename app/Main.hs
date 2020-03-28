module Main where

import Lib
import Fibonacci

main :: IO ()
main = do
  let n = 20
  let result = fib n
  print result
