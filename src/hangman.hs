import System.IO

main :: IO ()
main =
  do putStrLn "Think a word: "
     word <- sgetLine
     putStrLn "Try to gues it: "
     guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

guess :: String -> IO ()
guess word =
  do putStr "> "
     xs <- getLine
     if xs == word then
        putStrLn "You got it!"
     else
       do putStrLn (diff word xs)
          guess word

diff :: String -> String -> String
diff xs ys =
  [if x `elem` ys then x else '_' | x <- xs]
