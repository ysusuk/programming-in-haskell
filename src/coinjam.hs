import Data.Char
import Data.List.Split
import Data.Digits(unDigits)
import Data.Numbers.Primes
import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main = do
  content <- getContents
  let lns =  tail . lines $ content
  let contents =  map start . zip [1..(length lns)] $ lns
  putStr (unlines contents)

start :: (Int, String) -> String
start (counter, str) =
  "Case #" ++ (show counter) ++ ":\n"
  where
    n = read ((splitOn " " str) !! 0) :: Int
    quantity = read ((splitOn " " str) !! 1) :: Int
    jams = generateJams n quantity (replicate n '0') []

nextNumber :: String -> String
nextNumber number =
  showIntAtBase 2 intToDigit ((unDigits 2 codeList) + 1) ""
  where
    codeList = map digitToInt number

generateJams :: Int -> Int -> String -> [(String, [Int])] -> [(String, [Int])]
generateJams n quantity number jams =
  case (checkJam n number) of
    Nothing -> generateJams n quantity newNumber jams
    Just value ->
      if quantity == 1
        then value : jams
        else
          generateJams n (quantity - 1) newNumber (value : jams)
    where
      newNumber = nextNumber number

checkJam :: Int -> String -> Maybe (String, [Int])
checkJam n code =
  if all (==True) [length code == n, head code == '1', last code == '1', isJam numberInterpretations]
    then Just (code, map nontrivialDivisor numberInterpretations)
    else Nothing
  where
    codeList = map digitToInt code
    numberInterpretations = interpretations codeList

isJam :: [Int] -> Bool
isJam numbers = all (== False) $ primes
  where
    primes = map isPrime numbers

interpretations :: [Int] -> [Int]
interpretations bin = [unDigits y bin | y <- [2..10]]

nontrivialDivisor :: Int -> Int
nontrivialDivisor k = [x | x <- [2..k-1], (k `mod` x) == 0 ] !! 0
