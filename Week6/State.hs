import Control.Monad
import Control.Monad.State
import Data.IORef

-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)


-- This function uses the IO monad to compute the factorial of the inputted
-- number.
factIO :: Integer -> IO Integer
factIO n = 
    if n < 0 then error "invalid input"
      else
        do  counter <- newIORef n
            total <- newIORef 1
            whileIO 
                (do c' <- readIORef counter
                    return (c' > 0))
                (do c' <- readIORef counter
                    t' <- readIORef total
                    writeIORef total (c' * t')
                    writeIORef counter (c' - 1))
            readIORef total

-- This function uses the State monad to compute the factorial of the inputted
-- number.
factState :: Integer -> Integer
factState n = if n < 0 then error "invalid input"
  else evalState factStateHelp (n, 1)
    where
        factStateHelp :: State (Integer, Integer) Integer
        factStateHelp = do
            whileState (\(i, _) -> i > 0)
                (do (i, j) <- get
                    put (i - 1, i * j))
            (_, j) <- get
            return j


-- This function finds the n'th fibonacci number using the IO monad.
fibIO :: Integer -> IO Integer
fibIO n =
  if n < 0 then error "invalid input"
    else
      do  counter <- newIORef n
          prev1 <- newIORef 1
          prev2 <- newIORef 0
          whileIO
              (do c' <- readIORef counter
                  return (c' > 0))
              (do c' <- readIORef counter
                  p1 <- readIORef prev1
                  p2 <- readIORef prev2
                  writeIORef counter (c' - 1)
                  writeIORef prev1 (p1 + p2)
                  writeIORef prev2 (p1))
          readIORef prev2

-- This function finds the n'th fibonacci number using the State monad.
fibState :: Integer -> Integer
fibState n = if n < 0 then error "invalid input"
  else evalState fibStateHelp (n, 1, 0)
    where
        fibStateHelp :: State (Integer, Integer, Integer) Integer
        fibStateHelp = do
            whileState (\(c, _, _) -> c > 0)
                (do (c, p1, p2) <- get
                    put ((c - 1), (p1 + p2), p1))
            (_, _, p2) <- get
            return p2



{- Part B -}

{- 
f :: a -> Reader r b
g :: b -> Reader r c

We want to compose them to give a function with the type signature
h :: a -> Reader r c

We can rewrite these into a non-monadic form:

f' :: (a, r) -> b
g' :: (b, r) -> c
h' :: (a, r) -> c

We can easily define h' in terms of f' and g'
h' :: (a, r) -> c
h' (x, rd) = 
    let y = f' (x, rd)
        z = g' (y, rd)
    in z

Going back to the original functions f, g and h we have

h = f >=> g, which is equivalent to
h x = f x >>= g, which is equivalent to
h x = f x >>= \y -> g y
h x = do y <- f x
         g y

Let's go back to f' and g' and write curried versions of them:

f'' :: a -> r -> b
g'' :: b -> r -> c

f'' :: a -> r -> b
f'' x rd = f' (x, rd)

g'' :: b -> r -> c
g'' y rd = g' (y, rd)

Or, written slightly differently:
f'' x = \rd -> f' (x, rd)
g'' y = \rd -> g' (y, rd)

Then, if we wrap the right-hand sides of f'' and g'' in a Reader constructor,
    we have the definitions of f and g in terms of f' and g':

f :: a -> Reader r b
f x = Reader (\rd -> f' (x, rd))

g :: b -> Reader r c
g y = Reader (\rd -> g' (y, rd))

Similarily, we can define the monadic composition of f and g(h) in terms
of the composition of f' and g'(h') as follows:

h :: a -> Reader r c
h x = Reader (\rd -> h' (x, rd))

Now, we can derive the >>= operator of the (Reader r) monad.

We know that h = f >=> g, which is equivalent to
h x = f x >>= g
Reversing this equation, we have:
f x >>= g = h x
Expanding h x, we have:
f x >>= g = Reader(\rd -> h' (x, rd))

Then, let us calculate: 
f x >>= g
  = Reader (\rd -> h' (x, rd))
  = Reader (\rd ->  -- expanded using definition of h'
      let y = f' (x, rd)
          z = g' (y, rd)
          in z
  = Reader (\rd ->
      let y = f' (x, rd) in
        g' (y, rd))

Continuing:

f x >>= g
  = Reader (\rd -> 
      let y = f' (x, rd) in
          g' (y, rd))

Recall: f x = Reader (\rd -> f' (x, rd))
  = Reader (\rd ->
       let (Reader ff) = f x
            y = ff rd
        in g' (y, rd))

Notice that the above definition is no longer dependent on f', but only on f.
Let us eliminate g' in the same way.

f x >>= g
    = Reader (\rd ->
        let (Reader ff) = f x
             y = ff rd
             (Reader gg) = g y
        in gg rd

We can now substitute mv for f x to get:

mv >>= g
    = Reader (\r ->
        let (Reader ff) = mv
            x = ff r
            (Reader hh) = g x
        in hh r

The above is our desired result. Thus, we have derived the >>= operator.

Note that this can also be written as:

mv >>= g
   = Reader (\r ->
        let x = runReader mx r in
            runReader (f x) r)
-}

{- Now, we will derive the return operator:

Recall: the return method for a particular monad is the monadic version of
    the identity function.

Monadic functions in the (Reader r) monad have type signatures of the form
a -> Reader r b

The non-monadic version has type signatures of the form:
(a, r) -> b

The identity function in this form would be:

id_reader (x, rd) = (x, rd)
id_reader' x rd = (x, rd)
id_reader' x = \st -> (x, rd)

id_reader_monad :: a -> Reader r a
id_reader_monad x = Reader (\rd -> (x, rd))

This is the identity function in the (Reader rd) monad.
Therefore, it also has the return method:
return :: a -> Reader r a
return x = Reader (\rd -> (x, rd))

Thus, we have derived the return definition for the Reader monad.

-}
