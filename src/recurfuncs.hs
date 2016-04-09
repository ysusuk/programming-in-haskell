module RecurFuncs where

factorial :: Int -> Int
factorial n = product [1..n]

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial (n - 1)

product' :: [Int] -> Int
product' [] = 1
product' (n:ns) = n * product ns
