% CSE 230: Winter 2013
% Higher Order Functions 
% Ranjit Jhala, UC San Diego 

\begin{code}
module HigherOrder where
import Prelude hiding ((++))
\end{code}

## Plan

- [Functions Are Data](#functions-are-data)

- [Partial Application](#partial-application)

- [Anonymous Functions](#lambda-the-function-that-has-no-name)

- [Infix Operators and Sections](#anonymous-functions)

# Functions Are Data

## Functions Are Data

Lets define two functions ...

\begin{code}
plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1
\end{code}

... we can manipulate functions like ordinary data

\begin{code}
aPairOfFuns   = (plus1, minus1)
aListOfFuns   = [plus1, minus1, plus1, minus1]
aBigListoFuns = replicate 10 plus1 ++ replicate 5 minus1
\end{code}

### The Type is Very Descriptive

~~~~~{.haskell}
ghci> :type aBigListoFuns
aBigListoFuns :: [Int -> Int]
~~~~~

## A Big Deal: Can *Pass Functions Around*

- Take functions as **inputs**

- Return functions as **outputs**

## Taking Functions as Input

\begin{code}
doTwice f x = f (f x)
\end{code}

**Takes** as input

- a function `f`
- an input `x` 

and **returns** as output

- result of applying `x` to `f` **two times**

## Taking Functions as Input

\begin{code}
doTwice f x = f (f x)
\end{code}

Lets run it,

~~~~~{.haskell}
ghc> doTwice plus1 10
12
ghc> doTwice minus1 100
98
~~~~~

## Taking Functions as Input

\begin{code}
doTwice f x = f (f x)
\end{code}

### Execution = Substitute Equals for Equals

~~~~~{.haskell}
doTwice plus1 10 
  == {- unfold doTwice -} 
     plus1 (plus1 10)	
  == {- unfold plus1 -}
     plus1 (10 + 1)
  == {- unfold plus1 -}
     (10 + 1) + 1
  == {- old-school arithmetic -}
     12
~~~~~

## Returning Functions as Output

Instead of writing `plus1`, `plus2`, `plus3` ...

\begin{code}
plusn :: Int -> (Int -> Int)
plusn n = f
          where f x = x + n
\end{code}

We can now use the above to write

\begin{code}
plus2   = plusn 2
plus10  = plusn 10
minus20 = plusn (-20)       -- why the parens?
\end{code}

which can be used ...

~~~~~{.haskell}
ghc> plus10 100
110

ghc> minus20 1000
980 
~~~~~

## Returning Functions as Output

You can *derive* the result by substitutions...

~~~~~{.haskell}
plus10 100
  
  == {- unfold plus10 -}
     (plusn 10) 100 
  
  == {- unfold plusn  -}
     f 100
     where f x = x + 10 
  
  == {- unfold f -}
     100 + 10 
  
  == {- arithmetic -}
     110
~~~~~

# Partial Application

## Partial Application

`-` and `->` are operators

### `-` is an operator on **integers**

- **Takes** two integers
- **Returns** an integer

### `->` is an operator on **types**
  
- **Takes** two types ("input", "output") 
- **Returns** a type  ("function")

## Partial Application

`-` and `->` are operators

### `-` is  Left-Associative

~~~~~{.haskell}
2 - 1 - 1 == (2 - 1) - 1 == 0
~~~~~
      
<br><br>

### `->` is Right-Associative

~~~~~{.haskell}
Int -> Int -> Int == Int -> (Int -> Int)
~~~~~
 
## Partial Application

\begin{code}
plus :: Int -> Int -> Int
plus n x = n + x
\end{code}

<br>

Here, `plus` actually has type (Haskell hides ugly parens)

~~~~~{.haskell}
plus :: Int -> (Int -> Int)
~~~~~

<br>

So `plus` 

- **takes** a single input `n`, **returns** a function that ...
    - ... **takes** a single input `x`, **returns** the sum of `x` and `n`

## Partial Application

\begin{code}
plus :: Int -> Int -> Int
plus n x = n + x
\end{code}

### Needn't pass *all* arguments to `plus`
- Can **partially apply** just the first args 
- Get a function **waiting for** remaining args

### A Partially Applied `plus`

\begin{code}
plus100 = plus 100
\end{code}

- **waiting for** `x` 
- **will return** `100 + x`

~~~~~{.haskell}
ghci> plus100 566 
666
~~~~~

## Quiz: Have you been following so far?

\begin{code}
ex1 = doTwice (plus 99)
\end{code}

1. What is the **type** of `ex1` ? 

2. What is the **value** of `ex1 100` ?

# Anonymous Functions 

## Functions With No Name 

Haskell lets you create functions that **have no name**

~~~~~{.haskell}
plus 1000
~~~~~

Very useful for **functions-that-are-used-once**

## Lambda: Creating Nameless Functions

Create nameless functions with **lambda** (just a `\`)

~~~~~{.haskell}
\ param -> body
~~~~~

<br>
For example, a function that returns `1` more than its input 

~~~~~{.haskell}
\x -> x + 1
~~~~~

<br>
We can call it, just like any other function

~~~~~{.haskell}
ghci> (\x -> x + 1) 100
101

ghci> doTwice (\x -> x + 1) 100
102
~~~~~

## Lambda: Creating Nameless Functions

Lambda-functions are **just values** ... so you *can* name them

\begin{code}
plus42 = \x -> x + 42
\end{code}

## Lambda: Creating Nameless Functions

Lambda-functions are **just values** ... so you *can* name them

In general, an old-school **equation-style** function definition

~~~~~{.haskell}
f x1 x2 ... xn = e 
~~~~~

<br>
is **equivalent to** the **lambda-style** definition

~~~~~{.haskell}
f = \x1 -> \x2 -> ... xn -> e 
~~~~~

<br>
which can be **shortened** to

~~~~~{.haskell}
f = \x1 x2 ... xn -> e 
~~~~~

# Infix Operators and Sections


## Infix Operators 

To **improve readability** Haskell lets you use some functions as **infix**

1. Any function whose name is surrounded in **backticks**

2. Any function defined with **parens**


## Infix Operators with Backticks

Instead of `plus 2 10` you can write

~~~~~{.haskell}
ghci> 2 `plus` 10 
12
~~~~~

<br>

or, instead of `clone "thirty" 3` you can say

~~~~~{.haskell}
ghci> "thirty" `clone` 3
["thirty", "thirty", "thirty"]
~~~~~


## Infix Operators with Parentheses 

For some heavily used functions, convenient to skip the ticks

\begin{code}
-- What does this function do?
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)
\end{code}

<br>

which lets us write

\begin{code}
ex2 = [1,2,3] ++ [4,5,6]
\end{code}

<br>

**Quiz:** What is the value of `ex2` ?


## Sections

<br>

To **further improve** readability, partially apply infix operators

~~~~~{.haskell}
ghci> doTwice (+1) 0
2
~~~~~

<br>

`:` the list cons operator, is also infix, so... 

\begin{code}
ex3 = doTwice (0:) [7]
\end{code}

<br>

**Quiz:** what is the value of `ex3` ? 

## Recap: Higher Order Functions 

- [Functions Are Data](#functions-are-data)

- [Partial Application](#partial-application)

- [Anonymous Functions](#lambda-the-function-that-has-no-name)

- [Infix Operators and Sections](#anonymous-functions)
