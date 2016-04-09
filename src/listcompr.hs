module ListCompr where

flatten :: [[a]] -> [a]
flatten xxs = [x | xs <- xxs, x <- xs]
