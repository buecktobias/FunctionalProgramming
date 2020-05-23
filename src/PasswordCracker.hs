module PasswordCracker where

isPasswordPrefix:: String -> String -> Bool
isPasswordPrefix prefix text = 
                              if (length prefix) <= (length text) then
                              fst (splitAt (length prefix) text) == prefix
                              else 
                              False

isCorrectPassword:: String -> [String] -> ([String], Bool)
isCorrectPassword [] passwords = ([], True)
isCorrectPassword text passwords = if (length prefixPasswords) == 0 then ([], False)
                                  else 
                                  let 
                                  in
                                  head (filter (\x -> snd x) (map (\x -> let correctPassword=isCorrectPassword (drop (length x) text) passwords in ([x] ++ fst correctPassword, snd correctPassword) ) prefixPasswords))
                                  where  
                                  prefixPasswords = filter (\x -> isPasswordPrefix x text) passwords

