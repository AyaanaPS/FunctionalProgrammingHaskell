-- Ayaana Patel Sikora
-- CS 115 - Lab 1

{- Part A. Question 1. -}

{- Definition of sum of squares operator. -}
(+*) :: Double -> Double -> Double
x +* y = (x * x) + (y * y)
infixl 7 +*

{- Definition of exclusive-OR operator -}
(^||) :: Bool -> Bool -> Bool
False ^|| b = b
True ^|| b = not b
infixr 3 ^||

{- Part A. Question 2 -}

{- Computes the product of all integers in the range of the two inputs -}

rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | x > y = error "second arg must be larger than first arg."
                 | x < y = x * (rangeProduct (x + 1) y)
                 | otherwise = x

{- Part A. Question 3 -}

{- Uses the foldr function to compute the product of all integers in a list -}

prod :: [Integer] -> Integer
prod = foldr (*) 1

{- Uses the prod function defined above to redefine the rangeProduct function -}

rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y | x > y = error "second arg must be larger than first arg."
                  | otherwise = prod [x..y]

{- Part A. Question 4 -}

{- Part 1: This defines a map function that combines two lists using the
    inputted operator. -}

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f (hd1:tl1) (hd2:tl2) = (f hd1 hd2) : (map2 f tl1 tl2)
map2 _ _ _ = []

{- Part 2: This defines a map function that combines three lists using the
    inputted operator. -}

map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 f (hd1:tl1) (hd2:tl2) (hd3:tl3) = (f hd1 hd2 hd3) : (map3 f tl1 tl2 tl3)
map3 _ _ _ _ = []

{- Part 3: -}

{- 

dot lst1 lst2
--> ((sum .) . map2 (*)) lst1 lst2
--> \x -> ((sum .) map2 (*) x) lst1 lst2
--> ((sum .) (map2 (*) lst1)) lst2
--> ((\x -> sum . x) (map2 (*) lst1)) lst2
--> (sum . (map2 (*) lst1)) lst2
--> (\x -> sum ((map2 (*) lst1) x)) lst2
--> sum ((map2 (*) lst1) lst2)
--> sum (map2 (*) lst1 lst2)

-}

{- Part A. Question 5 -}

{- This calculates the sum of all numbers under 1000 (inclusive), which are 
    multiples of 3 or 5 -}
val = sum [x | x <- [1..999], ((x `mod` 5 == 0) || (x `mod` 3 == 0))]

{- The result of executing this statement is 233168 -}

{- Part A. Question 6 -}

sieve :: [Integer] -> [Integer]
sieve (hd:tl) = hd : (sieve (filter (\x -> (x `mod` hd /= 0)) tl))
sieve [] = []

primes = sieve [2..]
result = sum (takeWhile (<10000) primes)

{- Part B. Question 1 -}

{- The style of the second pattern match is not very good. The following is
    a more stylistic version, since it accurately pattern matches. -}

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (hd:tl) = hd + (sumList tl)

{- Part B. Question 2 -}

{- The issue is that using the length function increases the time
    complexity of the function. This is unneccessary since we
    could just use hd:tl notation. -}

largest :: [Integer] -> Integer
largest [] = error "empty list"
largest [hd] = hd
largest (hd:tl) = max hd (largest tl)

{- Part C. Question 1 -}

{-
fib 3
--> fib (3 - 1) + fib (3 - 2)
--> fib 2 + fib (3 - 2)
--> (fib (2 - 1) + fib (2- 2)) + fib (3 - 2)
--> (fib (1) + fib (2 - 2)) + fib (3 - 2)
--> (1 + fib (2 - 2)) + fib (3 - 2)
--> (1 + fib (0)) + fib (3 - 2)
--> (1 + 0) + fib (3 - 2)
--> 1 + fib (3 - 2)
--> 1 + fib (1)
--> 1 + 1
--> 2
-}

{- Part C. Question 2 -}

{- The issue with this code is the order of the cases. Since fact n comes
    before fact 0, it will always be chosen, since fact n is always true. This
    can easily be fixed by rewriting the code so that the case fact 0 comes
    first -}

{-

fact 3
--> 3 * (fact (3 - 1))
--> 3 * (fact (2))
--> 3 * (2 * fact (2 - 1))
--> 3 * (2 * fact (1))
--> 3 * (2 * (1 * fact (1 - 1)))
--> 3 * (2 * (1 * fact (0))) 

- NOTE: Ideally, this code should evaluate fact 0 to 1. Then, the code would
    evaluate to:
    3 * (2 * (1 * 1))
    3 * (2 * 1)
    3 * 2
    6

    However, the function would pattern match fact 0 with fact n and keep going.

--> 3 * (2 * (1 * (0 * fact (0 - 1))))
--> 3 * (2 * (1 * (0 * fact (-1))))
-}

{- Eventually, this would evaluate to 0 because of the multiplication with 0 -}

{- Part C. Question 3 -}

{-
reverse [1, 2, 3]
--> iter [1, 2, 3] []
--> iter [2, 3] (1:[])
--> iter [2, 3] [1]
--> iter [3] (2:[1])
--> iter [3] [2, 1]
--> iter [] [3:[2, 1]]
--> iter [] [3, 2, 1]
--> [3, 2, 1]
-}

{- The asymptotic time complexity of this is O(n), where n is the size of the
    inputted list. This is because for each element in the list, there is 
    an operation that must be executed. -}

{- Part C. Question 4 -}

{-
reverse [1, 2, 3]
--> reverse [2, 3] ++ [1]
--> (reverse [3] ++ [2]) ++ [1]
--> ((reverse [] ++ [3]) ++ [2]) ++ [1]
--> (([] ++ [3]) ++ [2]) ++ [1]
--> (([3]) ++ [2]) ++ [1]
--> (3 : ([] ++ [2])) ++ [1]
--> 3 : (([] ++ [2]) ++ [1])
--> 3 : ([2] ++ [1])
--> 3 : (2 : ([] ++ [1]))
--> 3 : (2 : [1])
--> 3 : [2, 1]
--> [3, 2, 1] 
-}

{- The asymptotic time complexity of this is actually O(n^2), where n is 
    the size of the inputted list. This is because first reverse [1, 2, 3] 
    is expanded into [] ++ [3] ++ [2] ++ [1]. Then, with larger lists passed to
    ++, more steps are required since ++ is a recursive operator, with a time
    complexity of O(n). Ben Bitdiddle made his mistake by assuming the n in 
    O(n) will be 1 since we are evaluating lists of length 1. However, 
    in the evaluation we see that n is not always 1 in the calls to ++ and thus,
    the time complexity is O(n^2).
    -}


{- Part C. Question 5 -} 

{-
head (isort [3, 1, 2, 5, 4])
--> head (insert 3 (isort [1, 2, 5, 4]))
--> head (insert 3 (insert 1 (isort [2, 5, 4])))
--> head (insert 3 (insert 1 (insert 2 (isort [5, 4]))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (isort [4])))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 (isort []))))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 ([]))))))
--> head (insert 3 (insert 1 (insert 2 (insert 5 ([4])))))
--> head (insert 3 (insert 1 (insert 2 (4 : insert 5 []))))
--> head (insert 3 (insert 1 (2 : (4 : insert 5 []))))
--> head (insert 3 (1 : (2 : (4 : insert 5 []))))
--> head (1 : insert 3 (2 : (4 : insert 5 [])))
--> 1
-}

{- Part C. Question 6 -}

{-
foldr max 0 [1, 5, 3, -2, 4]
--> max 1 (foldr max 0 [5, 3, -2, 4])
--> max 1 (max 5 (foldr max 0 [3, -2, 4]))
--> max 1 (max 5 (max 3 (foldr max 0 [-2, 4])))
--> max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
--> max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
--> max 1 (max 5 (max 3 (max -2 (max 4 (0)))))
--> max 1 (max 5 (max 3 (max -2 (4))))
--> max 1 (max 5 (max 3 (4)))
--> max 1 (max 5 (4))
--> max 1 (5)
--> 5
-}

{-
foldl max 0 [1, 5, 3, -2, 4]
--> foldl max (max 0 1) [5, 3, -2, 4]
--> foldl max (max (max 0 1) 5) [3, -2, 4]
--> foldl max (max (max (max 0 1) 5) 3) [-2, 4]
--> foldl max (max (max (max (max 0 1) 5) 3) -2) 4
--> foldl max (max (max (max 1 5) 3) -2) 4
--> foldl max (max (max 5 3) -2) 4
--> foldl max (max 5 -2) 4
--> foldl max 5 4
--> foldl

-}

{- foldr and foldl have the space complexity because of lazy evaluation.
    They both have O(n) pending operations since foldl does not evaluate
    it's max statements right away.
-}
