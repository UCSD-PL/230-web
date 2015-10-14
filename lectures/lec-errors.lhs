The Error Monad
===============

\begin{comment}
\begin{code}
{-@ LIQUID "--no-termination" @-}
module Errors where
\end{code}
\end{comment}

The `Error` monad is a generalization
of the `Maybe` monad that allows for
encoding *exceptions* as monads.

Maybe Recap
-----------

Recall the `Maybe` type:

~~~~~{.haskell}
data Maybe a = Just a | Nothing
~~~~~

We used it to define a *safe* version of division

\begin{code}
safeDiv     :: Int -> Int -> Maybe Int
safeDiv n m =  if m == 0 then Nothing else Just (n `div` m)
\end{code}

How to program with `Maybe`? Lets write an evaluator:

\begin{code}
data Expr = Val Int 
          | Div Expr Expr 
             deriving (Show)
\end{code}

Here's a super yucky but safe evaluator:

\begin{code}
evalY           ::  Expr -> Maybe Int
evalY (Val n)   =  Just n
evalY (Div x y) =  case evalY x of 
                       Nothing -> Nothing
                       Just n1 -> case evalY y of
                                    Nothing -> Nothing
                                    Just n2 -> n1 `safeDiv` n2
\end{code}

But thankfully, we can declare `Maybe` to be an instance of a `Monad`

~~~~~{.haskell}
instance Monad Maybe where
   -- return      :: a -> Maybe a
   return x       =  Just x

   -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ =  Nothing
   (Just x) >>= f =  f x
~~~~~

and then cleanup the evaluator into:

\begin{code}
evalM           ::  Expr -> Maybe Int
evalM (Val n)   =  return n
evalM (Div x y) =  do n1 <- evalM x
                      n2 <- evalM y
                      n1 `safeDiv` n2
\end{code}

From Maybe to Error
-------------------

Wouldn't it be nice to not just get `Nothing` but instead have some *clue*
as to what went wrong? Lets tweak the type of `Maybe`:

\begin{code}
data Error a = Err String
             | Ok a 
             deriving (Eq, Ord, Show)
\end{code}

This type is *almost* like `Maybe` except that instead of `Nothing` we can
carry around an `String` that can perhaps give a clue as to what the error was.

We can use the `Monad Maybe` instance to turn this into a monad:

\begin{code}
instance Monad Error where
   -- return    :: a -> Error a
   return x     =  Ok x

   -- (>>=)     :: Error a -> (a -> Error b) -> Error b
   Err z  >>= _ =  Err z   -- just pass the error along 
   (Ok x) >>= f =  f x
\end{code}

Now we can write our evaluator as:

\begin{code}
ediv      :: Int -> Int -> Error Int
ediv _ 0 = throw "Oops Div By Zero"
ediv n m = return (n `div` m)

evalE          ::  Expr -> Error Int
evalE (Val n)   =  return n
evalE (Div x y) =  do n1 <- evalE x
                      n2 <- evalE y
                      ediv n1 n2
                        `catch` \_ ->
                           throw $  "The bad expr is:" ++ show y
\end{code}


Now, we can do:

~~~~~{.haskell}
ghci> evalE ((Val 10) `Div` (Val 5))
Ok 2

ghci> evalE ((Val 10) `Div` (Val 2 `Div` Val 4))
Err "Div By Zero: Div (Val 2) (Val 4)"
~~~~~

Error as Exceptions
-------------------

Finally, note that we can use the `Error` Monad to encode exceptions:

\begin{code}
throw :: String -> Error a
throw = Err

catch :: Error a -> (String -> Error a) -> Error a
catch (Err s) f = f s
catch (Ok x)  _ = Ok x
\end{code}

And returning to our evaluator:

\begin{code}
evalEx ::  Expr -> Error Int
evalEx e = evalE e `catch` \_ -> Ok (-1)
\end{code}

and now, instead we get:

Now, we can do:

~~~~~{.haskell}
ghci> evalEx ((Val 10) `Div` (Val 5))
Ok 2

ghci> evalEx ((Val 10) `Div` (Val 2 `Div` Val 4))
Ok (-1)
~~~~~



