module Main where
import KathisAufgabe

main :: IO ()
main = do
        let matrix = [[1,2], [2, 4]]
        let newColumn = [2, 5]
        print(getAllCombis 3)
