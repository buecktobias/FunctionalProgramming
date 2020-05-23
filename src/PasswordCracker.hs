module PasswordCracker where

isPasswordPrefix:: String -> String -> Bool
isPasswordPrefix prefix text = 
                              if (length prefix) <= (length text) then
                              fst (splitAt (length prefix) text) == prefix
                              else 
                              False

isCorrectPassword:: String -> [String] -> [String]
isCorrectPassword [] passwords = []
isCorrectPassword text passwords = if (length prefixPasswords) == 0 then [] 
                                  else 
                                  let 
                                  pass = head prefixPasswords
                                  newText = drop (length pass) text
                                  in
                                  [pass] ++ (isCorrectPassword newText passwords)
                                  where  
                                  prefixPasswords = filter (\x -> isPasswordPrefix x text) passwords

