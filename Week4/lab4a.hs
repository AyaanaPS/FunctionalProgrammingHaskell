import Data.Char

{- Part A. Question 1 -}

--myPutStrLn :: String -> IO ()
--myPutStrLn "" = putChar '\n'
--myPutStrLn (c:cs) = (putChar c) >> (myPutStrLn cs)

{- Part A. Question 2 -}

{- This shouldn't have the do. It is unnecessary because it is only followed
    by one line. -}

--greet :: String -> IO ()
--greet name = putStrLn ("Hello, " ++ name ++ "!")

{- Part A. Question 3 -}

{- Simple Desguaring -}

--greet2 :: IO ()
--greet2 = 
--    putStr "Enter your name: " >> (getLine >>= (\name -> (putStr "Hello, " >> 
--        putStr name >> putStrLn "!")))

{- Complex Desugaring -}

--greet2 :: IO ()
--greet2 = 
--    putStr "Enter your name: " >>
--        getLine >>= 
--            \y -> case y of
--                name -> (putStr "Hello " >> putStr name >> putStrLn "!")

{- The two desugared versions behave the same way because the do the same
    thing. This is because the complex desugaring is unnecessary since 
    there won't every be a pattern match failure (which is also why
    that case is not included.) -}

{- Part A. Question 4 -}

{- greet3 simple desugared version -}

--greet3 :: IO ()
--greet3 = 
--    putStr "Enter your name: " >>
--        getLine >>= 
--            \(n:ns) ->
--                let name = toUpper n : ns in 
--                    (putStr "Hello, " >> putStr name >> putStrLn "!")

{- greet3 complex desugared version -}

--greet3 :: IO ()
--greet3 = 
--    putStr "Enter your name: " >>
--        getLine >>=
--            \y -> case y of
--                (n:ns) -> 
--                    let name = toUpper n : ns in
--                        (putStr "Hello, " >> putStr name >> putStrLn "!")
--                _ -> fail "Pattern match failure in do expression"

{- Yes, complex desugaring does have effects here. This is because there
    is actually a kind of user input that would cause a pattern match
    failure: an empty string.}
