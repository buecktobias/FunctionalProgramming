module SubStringSearching where

isSubString:: String -> String -> Bool
isSubString pat str = isSubString' pat str

isSubString':: String -> String -> Bool
isSubString' pat all@(s:str)
  | (pat_length) < (length all) = if fst (splitAt pat_length all) == pat then True else isSubString' pat str
  |otherwise = False
  where
  pat_length = length pat
