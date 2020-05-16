module Some where


m_max:: (Ord a) => [a] -> a
m_max =  foldl1 (\x y -> if x > y then x else y)




