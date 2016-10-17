module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tuple as T 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

{- Part C. Question 1 -}
{- This creates a sparse matrix from a list of index/element pairs
    and the array bounds. It uses a helper function called
    addToMatrix which takes in a list and a SparseMatrix and updates the 
    matrix if the head of the list has a value not equal to zero. 
-}

sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
  -- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix

sparseMatrix _ (row, column) | (row < 1 || column < 1) = error "Bounds invalid"
sparseMatrix lst (row, column) =
    addToMatrix lst (SM (row, column) S.empty S.empty M.empty) 
    where
        addToMatrix :: (Eq a, Num a) => [((Integer, Integer), a)] -> 
            SparseMatrix a -> SparseMatrix a
        addToMatrix [] sm = sm
        addToMatrix (((r, c), v):tl) (SM (row, column) rows cols vals) =
            if r > row || r < 1 || c > column 
                || c < 1 then error "Index not valid"

            else 
                if v /= 0 then 
                    (addToMatrix tl 
                        (SM (row, column) 
                            (S.insert r rows)
                            (S.insert c cols) 
                            (M.insert ((r, c)) (v) vals)))
                else
                    addToMatrix tl (SM (row, column) rows cols vals)

{- Part C. Question 2 -}
{- This takes two sparse matrices as an input and outputs the result of
    adding the two matrices together. First, the function ensures
    that the two matrices are compatible (same dimensions). If they are,
    it uses the unionWith function to add them.
-}

addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM (row1, col1) _ _ vals1) (SM (row2, col2) _ _ vals2) =
    if (row1 /= row2 || col1 /= col2) then error "Matrices not equal"
    else 
        let 
            vals = M.filter(/=0) (M.unionWith (+) vals1 vals2)
            rowsNew = S.fromList (map fst (M.keys vals))
            colsNew = S.fromList (map snd (M.keys vals))
        in
    (SM (row1, col1) rowsNew colsNew vals)

{- Part C. Question 3 -}
{- This negates the inputted sparse matrix using the M.map function and the
    built in negate function.
-}

negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM (row, col) rows cols vals) = (SM (row, col) rows cols 
    (M.map negate vals))

{- Part C. Question 4 -}
{- This subtracts two inputted matrices using the addSM function and the
    negateSM function defined above.
-}

subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM sm1 sm2 = addSM sm1 (negateSM sm2)

{- Part C. Question 5 -}

{- This is a helper function for the mulSM function. It gets a map of the non0
    values at locations in a single row of matrix. It does this using
    the getSM function defined below and inserting each [(row, col], value]
    into a map if the value is not equal to 0.
-}

getRow :: (Eq a, Num a) => SparseMatrix a -> Integer -> M.Map (Integer, Integer) a
getRow sm row = 
    generateMap row 1 sm M.empty
    where
        generateMap :: (Eq a, Num a) => Integer -> Integer -> 
            SparseMatrix a -> M.Map (Integer, Integer) a -> 
                M.Map (Integer, Integer) a

        generateMap _ col _ vals | (col > snd(bounds sm)) = vals
        generateMap row col sm vals = 
            if (getSM sm (row, col) == 0) then
                generateMap row (col + 1) sm vals
            else
                generateMap row (col + 1) sm 
                    (M.insert (row, col) (getSM sm (row, col)) vals)

{- This is a helper function for the mulSM function. It gets a map of the non0
    values at locations in a single column of matrix. It does this using
    the getSM function defined below and inserting each [(row, col], value]
    into a map if the value is not equal to 0.
-}

getCol :: (Eq a, Num a) => SparseMatrix a -> Integer -> M.Map (Integer, Integer) a
getCol sm col = 
    generateMap 1 col sm M.empty
    where
        generateMap :: (Eq a, Num a) => Integer -> Integer -> 
            SparseMatrix a -> M.Map (Integer, Integer) a -> 
                M.Map (Integer, Integer) a

        generateMap row _ _ vals | (row > fst(bounds sm)) = vals
        generateMap row col sm vals = 
            if (getSM sm (row, col) == 0) then
                generateMap (row + 1) col sm vals
            else
                generateMap (row + 1) (col) sm 
                    (M.insert (row, col) (getSM sm (row, col)) vals)

{- This multiplies a row and a column (each inputted as a map) using 
    multiple map functions.
-}

mulHelper :: (Num a) => M.Map (Integer, Integer) a -> M.Map (Integer, Integer) a -> 
    a
mulHelper rows cols = 
    let
        swapped = M.mapKeys T.swap rows
        newR = M.mapKeys fst swapped
        newC = M.mapKeys fst cols
    in
    M.fold (+) 0 (M.intersectionWith (*) newR newC)

{- This multiplies two sparse matrices using the above helper functions.
    It first ensures that the two matrices are compatible. If they are,
    the function uses sparseMatrix to build a matrix that is the result
    of multiplying the two inputted matrices together.
-}

mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM sm1 sm2 =
    if (snd(bounds sm1) /= fst(bounds sm2)) then error "Matrices not compatible"
    else 
        (sparseMatrix [((row, col), val) 
            | row <- (S.toList (rowIndices sm1)), 
                col <- (S.toList (colIndices sm2)), 
                val <- [(mulHelper (getRow sm1 row) (getCol sm2 col))]] 
            ((fst(bounds sm1)), (snd(bounds sm2))))

{- Part C. Question 6 -}
{- This gets the value of the inputted matrix at the specified location. -}
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM (row, col) _ _ vals) (x, y) 
    | x > row || y > col || x < 1 || y < 1 = error "Indices out of bounds"
    | otherwise = (M.findWithDefault 0 (x, y) vals)

{- This returns the number of rows in the sparse matrix -}
rowsSM :: Num a => SparseMatrix a -> Integer
rowsSM (SM (rows, _) _ _ _) = rows

{- This returns the number of columns in the sparse matrix -}
colsSM :: Num a => SparseMatrix a -> Integer
colsSM (SM (_, cols) _ _ _) = cols

{- Part C. Question 7 -}

{- This defines shortcut operators for the addSM, subSM, mulSM and getSM
    functions. -}
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) x y = addSM x y

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) x y = subSM x y

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) x y = mulSM x y

(<|!|>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<|!|>) x y = getSM x y

{- Part C. Question 8 -}

{- It doesn't make sense to define the SparseMatrix as an instance of the
    Num type class because some of the operators of the Num type class do 
    not work for SparseMatrices. For example, you can't take the 
    absolute value or the sign of a sparse matrix. Thus, the Num type
    class has extra operators that are unneeded for sparse matrices. -}
