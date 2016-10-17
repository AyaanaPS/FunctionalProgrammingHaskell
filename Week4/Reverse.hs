--
-- Reverse.hs
--
-- This function takes in file and outputs the lines in that file
-- in reverse order.
--

module Main where

import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: reverse filename"
-- This reads the filename and calls the function on the file.
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else reverseLines (head args) >> exitSuccess

-- This reverses the lines in the file and prints them.
reverseLines :: String -> IO ()
reverseLines str = do
    x <- readFile str
    let fileLines = lines x
    mapM_ putStrLn (reverse fileLines)
