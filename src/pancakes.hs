import System.IO

main = do
  content <- getContents
  let lns =  tail . lines $ content
  let contents =  map start . zip [1..(length lns)] $ lns
  putStr (unlines contents)

start :: (Int, String) -> String
start (counter, str) =
  "Case #" ++ (show counter) ++ ": " ++ (show flippedToHappyFace)
  where
    pancakesStack = parsePancakesStack str
    flippedToHappyFace = flipPancakesToHappyFace 0 pancakesStack

flipPancakesToHappyFace :: Int -> [Bool] -> Int
flipPancakesToHappyFace n pancakesStack =
  if all (== True) pancakesStack
    then n
    else flipPancakesToHappyFace (n + 1) newPancakesStack
  where
    place = placeToFlip 0 pancakesStack
    newPancakesStack = flipPancakesStack place pancakesStack

placeToFlip :: Int -> [Bool] -> Int
placeToFlip n [] = n
placeToFlip n [x] = n
placeToFlip n [x1, x2] =
  if x1 /= x2
    then n
    else n + 1
placeToFlip n (x1:x2:xs) =
  if x1 /= x2
    then n
    else placeToFlip (n + 1) (x2 : xs)

flipPancakesStack :: Int -> [Bool] -> [Bool]
flipPancakesStack n pancakes =
   flipedPancakes ++ notFlipedPancakes
   where
     flipedPancakes = reverse . map not $ take (n + 1) pancakes
     notFlipedPancakes = drop (n + 1) pancakes

parsePancakesStack :: String -> [Bool]
parsePancakesStack str = map convert str
  where
    convert '+' = True
    convert '-' = False
