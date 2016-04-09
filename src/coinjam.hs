import Data.Char

main = do
  content <- getContents
  let lns =  tail . lines $ content
  let contents =  map start . zip [1..(length lns)] $ lns
  putStr (unlines contents)

start :: (Int, String) -> String
start (counter, str) =
  "Case #" ++ (show counter) ++ ": " ++ (splitOn " " str) !! 0
