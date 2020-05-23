module Main where
import PasswordCracker

main :: IO ()
main = do
      let passwords = ["because","wedow", "can", "do", "must", "we", "what"]
      print(isCorrectPassword "wedowhatwemustbecausewecan" passwords)


