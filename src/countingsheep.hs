import System.IO
import qualified Data.List as List
import Data.Char

main = do
  content <- getContents
  let str = map find . tail . lines $ content
  putStrLn $ unlines str

find :: String -> String
find str =
  if (read str :: Int) == 0
    then "INSOMNIA"
    else
      findLastNumber (read str :: Int) "0123456789" 1

findLastNumber :: Int -> String -> Int -> String
findLastNumber number numbers n =
  if null numbers
    then show number
    else findLastNumber newNumber newNumbers (n + 1)
  where
    newNumber = number * n
    newNumbers = clean numbers (show newNumber)

clean :: String -> String -> String
clean [] [] = []
clean [] ys = []
clean xs [] = xs
clean xs (y:ys) =
  if y `elem` xs
    then clean (List.delete y xs) ys
    else clean xs ys
