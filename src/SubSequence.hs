module SubSequence where

isSubSequence:: String -> String -> Bool
isSubSequence s1 [] = False
isSubSequence [] s2 = True
isSubSequence allSubSeq@(s1:subseq) (s2:seq) 
  | s2 == s1 = isSubSequence subseq seq
  |otherwise = isSubSequence allSubSeq seq
