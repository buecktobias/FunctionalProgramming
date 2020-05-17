module PrefixCompression where


findPrefix:: (Eq a) => [a] -> [a] -> [a]
findPrefix ls1 ls2 = map fst (takeWhile (uncurry (==)) (zip ls1 ls2))

removePrefix:: [a] -> [a] -> [a]
removePrefix prefix ls1 = drop (length prefix) ls1
