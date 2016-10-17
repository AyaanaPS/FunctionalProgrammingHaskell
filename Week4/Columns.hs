--
-- Columns.hs
--

--
-- This program reads in a file and outputs the columns of each line
-- specified in the command line.
--

module Main where

import Data.Char
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: columns n1 n2 ... filename"
-- This reads in all the arguments and calls the helper functions and then
-- prints out the result.
--
main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then usage >> exitFailure
  else do
    fileInput <- checkFilename (last args)
    columns <- checkInts (init args)
    let fileLines = lines fileInput
    let result = getWords fileLines columns
    mapM_ putStrLn (result) >> exitSuccess

--
-- This checks if the filename passed in is a dash or something else and
-- then acts appropriately.
--

-- Hi! I don't understand how I can get rid of the return. I did and I just
-- get errors so i'm definitely not doing it right.
-- Don't I need it so that my function does indeed have IO String as it's 
-- return type? Can you clarify? Thank you very much!
checkFilename :: String -> IO String
checkFilename str = 
    if str == "-" then do
        result <- hGetContents stdin
        return result
    else do
        fileName <- readFile str
        return fileName

--
-- This checks if the columns passed in are digits and then 
-- acts appropriately.
--
checkInts :: [String] -> IO [Int]
checkInts [] = return []
checkInts lst = 
    if (all isDigit (concat lst)) then
        return (map (\x -> read x) lst)
    else
        usage >> exitFailure

--
-- This gets a list of the desired words in each line as strings.
--
getWords :: [String] -> [Int] -> [String]
getWords [] _ = return []
getWords (hd:tl) columns =
    let 
      current = unwords (selectWords (words hd) columns)
      rest = getWords tl columns 
    in
    current:rest


-- 
-- This gets a list of words in a line and returns a list of the desired
-- words from that line.
--
selectWords :: [String] -> [Int] -> [String]
selectWords _ [] = []
selectWords lst (hd:tl) =
    if ((hd-1) < length lst) then (lst!!(hd - 1) : (selectWords lst tl))
    else (selectWords lst tl)
