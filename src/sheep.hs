import System.IO
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.Foldable
import Data.Char

main = do
  content <- getContents
  let lns =  tail . lines $ content
  let contents =  map start . zip [1..(length lns)] $ lns
  putStr (unlines contents)

start :: (Int, String) -> String
start (counter, str) =
  if str == "0"
    then "Case #" ++ (show counter) ++": INSOMNIA"
    else
      "Case #" ++ (show counter) ++": " ++ lastSeenDigit (read str :: Int) 1 notSeen
 where
    notSeen = [False, False, False, False, False, False, False, False, False, False]

lastSeenDigit :: Int -> Int -> [Bool] -> String
lastSeenDigit number n seen =
  if all (== True) seen
    then show (number * (n - 1))
    else
      lastSeenDigit number (n + 1) newSeen
  where
    nextNumber = number * n
    digits = digitsInNumber nextNumber
    newSeen = seenDigitsInNumber digits seen

digitsInNumber :: Int -> [Int]
digitsInNumber number = map digitToInt (show number)

seenDigitsInNumber :: [Int] -> [Bool] -> [Bool]
seenDigitsInNumber [] ys = ys
seenDigitsInNumber (x:xs) seenDigits = seenDigitsInNumber xs $ checkSeenDigits x seenDigits

-- all
checkSeenDigits :: Int -> [Bool] -> [Bool]
checkSeenDigits digit seenDigits =
  Foldable.toList (Seq.update digit True (Seq.fromList seenDigits))
