module Main where
import PasswordCracker

main :: IO ()
main = do
      let passwords = ["hat" ,"because", "can", "do", "must", "we", "what"]
      print(isCorrectPassword "wedowhatwemustbecausewecan" passwords)