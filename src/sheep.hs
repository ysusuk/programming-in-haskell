import System.IO
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.Foldable
import Data.Char

main = do
  content <- getContents
  lns <- sequence . map start . tail . lines $ content
  putStr (unlines lns)

start :: String -> IO (String)
start str = do
  if str == "0"
    then return "INSOMNIA"
    else
      lastSeenDigit (read str :: Int) 1 notSeen
 where
    notSeen = [False, False, False, False, False, False, False, False, False, False]

lastSeenDigit :: Int -> Int -> [Bool] -> IO String
lastSeenDigit number n seen =
  if all (== True) seen
    then return (show (number * (n - 1)))
    else do
      putStrLn (show number)
      putStrLn (show n)
      putStrLn (show newSeen)
      lastSeenDigit number (n + 1) newSeen
  where
    nextNumber = number * n
    digits = str2Int nextNumber
    newSeen = seenDigitsInNumber digits seen

str2Int :: Int -> [Int]
str2Int number = map digitToInt (show number)

seenDigitsInNumber :: [Int] -> [Bool] -> [Bool]
seenDigitsInNumber [] ys = ys
seenDigitsInNumber (x:xs) seenDigits = seenDigitsInNumber xs $ checkSeenDigits x seenDigits

-- all
checkSeenDigits :: Int -> [Bool] -> [Bool]
checkSeenDigits digit seenDigits =
  Foldable.toList (Seq.update digit True (Seq.fromList seenDigits))
