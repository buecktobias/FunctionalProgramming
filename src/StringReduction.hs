module StringReduction where



reduction:: [Char] -> [Char]
reduction xs  = reduction' xs []


reduction'::  [Char] -> [Char] -> [Char]
reduction' [] before = []
reduction' (x:xs) before = if x `elem` before then reduction' xs ([x] ++ before) else [x] ++ reduction' xs ([x] ++ before)