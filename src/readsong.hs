import System.IO
import System.Environment(getArgs)
import Control.Concurrent (forkIO)

main = do
  args <- getArgs

  inh <- readFile (args !! 0)
  let str = concatWithBlah . tail . lines $ inh
  writeFile (args !! 1) str

concatWithBlah = unlines . map (++ " processed")
  -- inh <- openFile (args !! 0) ReadMode
  -- outh <- openFile (args !! 1) WriteMode
  -- content <- hGetContents inh
  -- hPutStr outh content
  -- hClose inh
  -- hClose outh
