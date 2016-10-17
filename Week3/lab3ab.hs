module Lab3ab where

{- Part A. Question 1 -}

{- 
data Nat = Zero | Succ Nat

The following defines the Eq and Show instances for the Nat class.

instance Eq Nat where 
    Zero == Zero = True
    Zero == Succ _ = False
    Succ _ == Zero = False
    Succ x == Succ y = (x == y)

instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ x) = "Succ (" ++ show x ++ ")"
-}

{- Part A. Question 2 -}

{- This automatically derives Eq and Show -}

data Nat = Zero | Succ Nat deriving (Eq, Show)

{- Part A. Question 3 -}

{- This is the explicit instance definition of the Ord type class for the
    Nat type. Automatically deriving this class would give us what we want.
    This is because Haskell automatically orders the struct in the way it
    was writen. Since the struct was written Zero followed by Succ Nat,
    Haskell will order it that way automatically. -}

instance Ord Nat where
    Zero <= _ = True
    Succ _ <= Zero = False
    Succ x <= Succ y = (x <= y)

{- Part A. Question 4 -}

{- This defines the Eq and Ord isntances for the SignedNat type.
    These could not be automatically derived since again Haskell
    does not recognize the Pos and Neg tags and thus will not
    order the numbers in the way we desire. -}

data SignedNat = 
    Neg Nat | Pos Nat deriving(Show)

instance Eq SignedNat where
    Pos Zero == Neg Zero = True
    Neg Zero == Pos Zero = True
    Pos x == Pos y = (x == y)
    Neg a == Neg b = (a == b)
    _ == _ = False

instance Ord SignedNat where
    Pos Zero <= Neg Zero = True
    Neg Zero <= Pos Zero = True
    Neg _ <= Pos _ = True
    Neg a <= Neg b = b <= a
    Pos x <= Pos y = x <= y
    _ <= _ = False

{- Part A. Question 5 -}

{- This adds two Nat numbers -}
addNat :: Nat -> Nat -> Nat
addNat Zero x = x
addNat x Zero = x
addNat (Succ x) y = Succ (addNat x y)

{- This subtracts two Nat numbers -}
subNat :: Nat -> Nat -> Nat
subNat Zero Zero = Zero
subNat Zero _ = error "Bad input"
subNat x Zero = x
subNat (Succ x) (Succ y) = subNat x y

{- This multiplies two Nat numbers -}
mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat _ Zero = Zero
mulNat (Succ Zero) n1 = n1
mulNat n1 (Succ Zero) = n1
mulNat x (Succ y) = (addNat x (mulNat x y)) 

{- This uses addNat to add two signedNat numbers -}
addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos n1) (Pos n2) = Pos (addNat n1 n2)
addSignedNat (Neg n1) (Neg n2) = Neg (addNat n1 n2)
addSignedNat (Neg n1) (Pos n2) | n1 <= n2 = Pos (subNat n2 n1)
                               | otherwise = Neg (subNat n1 n2)
addSignedNat n1 n2 = addSignedNat n2 n1

{- This uses mulNat to multiply two signedNat numbers-}
mulSignedNat :: SignedNat -> SignedNat -> SignedNat
mulSignedNat (Pos n1) (Pos n2) = Pos (mulNat n1 n2)
mulSignedNat (Neg n1) (Neg n2) = Pos (mulNat n1 n2)
mulSignedNat (Pos n1) (Neg n2) = Neg (mulNat n1 n2)
mulSignedNat (Neg n1) (Pos n2) = Neg (mulNat n1 n2)

{- This negates the inputted signedNat number -}
negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Pos n1) = (Neg n1)
negateSignedNat (Neg n1) = (Pos n1)

{- This gets the absolute value of the inputted signedNat number -}
absSignedNat :: SignedNat -> SignedNat
absSignedNat (Neg n1) = (Pos n1)
absSignedNat (Pos n1) = (Pos n1)

{- This returns the sign of the inputted signedNat number -}
signumSignedNat :: SignedNat -> SignedNat
signumSignedNat (Pos Zero) = (Pos Zero)
signumSignedNat (Neg Zero) = (Neg Zero)
signumSignedNat (Pos _) = (Pos (Succ Zero))
signumSignedNat (Neg _) = (Neg (Succ Zero))

{- This gets the respective Nat number from the inputted integer -}
natFromInt :: Integer -> Nat
natFromInt 0 = Zero
natFromInt x = Succ (natFromInt (x - 1))

{- This gets the respective signedNat number from the inputted integer -}
signedNatFromInt :: Integer -> SignedNat
signedNatFromInt x | x >= 0 = (Pos (natFromInt x))
                   | otherwise = (Neg (natFromInt (0 - x)))

{- This defines all the Num instances for the signedNat -}
instance Num SignedNat where
    (+) = addSignedNat
    (*) = mulSignedNat
    negate = negateSignedNat
    abs = absSignedNat
    signum = signumSignedNat
    fromInteger = signedNatFromInt

{- Part A. Question 6 -}

{- This gets the respective integer from the inputted Nat number -}
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + (natToInteger x)

{- This gets the respective integer from the inputted signedNat number -}
signedNatToInteger :: SignedNat -> Integer
signedNatToInteger (Pos x) = (natToInteger x)
signedNatToInteger (Neg x) = (0 - (natToInteger x))

{- Part A. Question 7 -}

{- Yes, this is ugly because of the Pos and Neg tag. To avoid this
    redundancy, we can use a Prev tag similar to the Succ tag for the 
    negative numbers. Thus, -2 would be Prev (Prev Zero)) and +2 would be
    Succ (Succ Zero)). 
-}

{- data Nat = Zero | Succ Nat | Prev Nat -}

{- This data declaration does create other issues, such as the fact that
    we can't have both Prev and Succ declarations in the same number. 
    Furthermore, this data declaration makes defining the operations
    on this data type quite tricky since they have to check for 
    conflicting types. A way to solve that problem would be to cancel out
    Prev Succ pairs in operations. 
-}

{- Part A. Question 8 -}

{- This computes the factorial of any instance of Num and Ord -}
factorial :: (Ord a, Num a) => a -> a
factorial 0 = 1
factorial n | n >= 0 = n * (factorial (n - 1))
            | otherwise = error "Can't have negative input"

{- Part B. Question 1 -}

{- 
    1: 
    The >#< operator is infix (non-associative). This is because the
    output isn't of the same type as the input.
-}

{-
    2:
    The operator +| is infixl, but could also be infixr. This is because
    this operation is essentially just addition, which is infixl, but could
    also be infixr.

    Consider the evaluation of 7 +| 10 +| 203.

    infixl evaluation:
    (7 +| 10) +| 204 = 7 +| 204 = 1

    infixr evaluation:
    7 +| (10 +| 204) = 7 +| 214 = 1

    Clearly, infixl and infixr evaluate to the same answer.
-}

{-
    3:
    The operator &< is just infixl. This is because it is essentially
    appending a number to the end of a list and the list is the first input
    to the operator. Thus, an expression would have to be evaluated from
    left to right since the first input to the operator is the list to append
    the value to.
-}

{-
    4:
    The operator >&& is infixr. This is because like the &< operator,
    it is essentially an operation that appends a number to a list. 
    However, this time the list is the second argument to the operator
    because the list is being appended to the beginning of the list.
    Thus, an expression would have to be evaluated from right to left since
    the second input to the operator is the list to append the value to.
-}

{- Part B. Question 2 -}

{- The +# operator should be either infixl or infixr, but is actually
    non-associative. This is because in a chain operation, it should
    sum all of the values and then report the number of digits. 
    However, if it was either left or right associative, it would
    fail in some cases. Consider the following examples:

    2000 +# 4 +# 300 = 4 (True Answer)

    Left Associative (Fails):
    (2000 +# 4) +# 300 = 4 +# 300 = 3

    Right Associative (Works):
    2000 +# (4 +# 300) = 2000 +# 3 = 4

    300 +# 4 +# 2000 = 4 (True Answer)
    
    Left Associative (Works):
    (300 +# 4) +# 2000 = 3 +# 2000 = 4

    Right Associative (Fails):
    300 +# (4 +# 2000) = 300 +# 4 = 3
    
    Since neither left nor right associative works for every case,
    this is non-associative.
-}
