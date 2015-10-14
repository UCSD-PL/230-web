% CSE 230: Winter 2013
% Introduction
% Ranjit Jhala, UC San Diego 


# A Simple Function


## In Haskell, Functions Are Defined as **equations**

\begin{code}
pos :: Integer -> Bool 
pos x = x > 0
\end{code}

<br>
<br>
<br>

## Write functions over *tuples* using **pattern-matching**

\begin{code}
pat :: (Int, Int, Int) -> Int
pat (x, y, z) = x * (y + z )
\end{code}


# A Recursive Function

Use `if-then-else` expressions to *split cases*

\begin{code}
clone :: a -> Int -> [a]
clone x n = if (n == 0)                 
              then []                   -- base case
              else (x : clone x (n-1))  -- inductive case
\end{code}

# Eliminate Ugly `if-then-else` with Patterns

One Equation Per Case

\begin{code}
clone' x 0 = []                -- base      case
clone' x n = x : clone x (n-1) -- inductive case
\end{code}

# You can write an `if-then-else` function!

Equations are rather more general...

\begin{code}
ifThenElse True  thenE elseE = thenE
ifThenElse False thenE elseE = elseE
\end{code}


# A Brief Word on *Laziness*

Is the following JavaScript function ...

~~~~~{.javascript}
function ifThenElse(cond, thenB, elseB) { 
  return cond ? thenB : elseB ;
}

var z = ifThenElse(true, 1, alert("DIE DIE DIE"));
~~~~~


<br>

... the *same-as* the Haskell equivalent?

~~~~~{.haskell}
ifThenElse True  thenE elseE = thenE
ifThenElse False thenE elseE = elseE

z = ifThenElse True 1 (error "DIE DIE DIE")
~~~~~



# Wrap Up: A "Hello World" Program

\begin{code}
main = do putStrLn "What is your name ?"
          n <- getLine
          putStrLn ("Happy New Year " ++ n)
\end{code}

