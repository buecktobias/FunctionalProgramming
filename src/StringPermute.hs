module StringPermute where

updateElement:: Int -> a -> [a] -> [a]
updateElement i n ls = (fst splitted) ++ [n] ++ (drop 1 (snd splitted))
                      where
                      splitted = splitAt i ls

swapElems:: Int -> Int -> [a] -> [a]
swapElems i n ls = updateElement n iElem (updateElement i nElem ls)
                  where
                  iElem = ls !! i 
                  nElem = ls !! n

applyNTimes:: (Int -> Int -> [a] -> [a]) -> [(Int, Int)] -> [a] -> Int -> [a]
applyNTimes fun params changingLs 0 = changingLs
applyNTimes fun [] changingLs n = changingLs
applyNTimes fun (p:params) changingLs n = applyNTimes fun params (fun (fst p) (snd p) changingLs) (n-1)

                  
permute:: [a] -> [a]
permute ls = resultLs
            where
            evenNums = [0, 2 .. (length ls) - 2]  
            pairs =  zip evenNums (map (+1) evenNums)
            resultLs = applyNTimes swapElems pairs ls (length ls)
            
      
                 

