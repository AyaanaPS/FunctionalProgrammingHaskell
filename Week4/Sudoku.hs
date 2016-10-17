--
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
--import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = iter s (1, 1)
  where
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
    iter :: Sudoku -> (Int, Int) -> IO Bool
    iter _ (row, _) | row > 9 = return True
    iter brd (row, col) = do
      val <- readArray brd (row, col)
      if val == 0
        then do
          possibilities <- getOKValues brd (row, col)
          iter' brd (row, col) possibilities
      else
        iter brd (nextIndex(row, col))

    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
    iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
    iter' _ _ [] = return False
    iter' brd (row, col) (hd:tl) = do
      writeArray brd (row, col) hd
      solved <- iter brd (nextIndex (row,col))
      if solved then
        return True
      else do
        writeArray brd (row, col) 0
        iter' brd (row, col) tl
    
    nextIndex :: (Int, Int) -> (Int, Int)
    nextIndex (n, 9) = (n + 1, 1)
    nextIndex (n, m) = (n, m + 1)

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
    getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
    getOKValues brd (row, col) = do
      rVals <- getRow brd row
      cVals <- getCol brd col
      bVals <- getBox brd (row, col)
      return [vals | vals <- [1..9], vals `notElem` (rVals ++ cVals ++ bVals)]

    -- Return the ith row in a Sudoku board as a list of Ints.
    getRow :: Sudoku -> Int -> IO [Int]
    --check valid for row.
    getRow _ x | x > 9 || x < 1 = error "Row not valid"
    getRow brd row = mapM(\col -> readArray brd (row, col)) [1..9] >>=
      return .filter(/= 0)

    -- Return the ith column in a Sudoku board as a list of Ints.
    getCol :: Sudoku -> Int -> IO [Int]
    getCol _ x | x > 9 || x < 1 = error "Col not valid"
    getCol brd col = mapM(\row -> readArray brd (row, col)) [1..9] >>=
      return .filter(/= 0)

    -- Return the box covering location (i, j) as a list of Ints.
    getBox :: Sudoku -> (Int, Int) -> IO [Int]
    getBox _ (row, col) | row > 9 || row < 1 || col > 9 || col < 1 = 
      error "Location not valid"
    getBox brd (row, col) = do
      let bRow = (row-1) `div` 3
      let bCol = (col-1) `div` 3
      let uCol = (bCol+1) * 3
      let uRow = (bRow+1) * 3
      row1 <- mapM(\col -> readArray brd ((uRow-2), col)) [(uCol-2)..uCol]
      row2 <- mapM(\col -> readArray brd ((uRow-2)+1, col)) [(uCol-2)..uCol]
      row3 <- mapM(\col -> readArray brd ((uRow-2)+2, col)) [(uCol-2)..uCol]
      return (filter(/=0) (row1 ++ row2 ++ row3))

-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure
