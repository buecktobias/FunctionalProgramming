module StringCompression where


numRepetitions:: String -> Int -> Int
numRepetitions txt n = length (takeWhile (\x -> x == c) afterN)
                      where
                      c = txt !! n
                      afterN = drop n txt 


getNumRepetitions:: Int -> String
getNumRepetitions n 
                   | n == 1  = ""
                   |otherwise = show n

compressAt:: String -> Int -> String
compressAt msg n 
                  | n < length msg = [msg !! n] ++ (getNumRepetitions (numRepetitions msg n)) ++ compressAt msg (n + (numRepetitions msg n))
                  | otherwise = ""


compress:: String -> String
compress msg = compressAt msg 0
