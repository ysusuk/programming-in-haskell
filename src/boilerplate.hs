module Boilerplate where

appendCase = (++  "Case#")

output :: (Int, String) -> String
output (num, str) = appendCase $ show num ++ ": " ++ str

eachLine :: (String -> String) -> String -> String
eachLine f = unlines . map f . lines
