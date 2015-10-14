% CSE 230: Winter 2013
% Polymorphism and Computation Patterns
% Ranjit Jhala, UC San Diego 

\begin{code}
module HigherOrder where
import Prelude hiding ((++))
\end{code}

## Plan

- Polymorphic Functions (#polymorphism)

- Polymorphic Data (#polymorphic-data-structures)

- Bottling Computation Patterns (#bottling-computation-patterns)

# Polymorphic Functions

## Polymorphic functions

Many functions are **oblivious** to the type of their arguments

\begin{code}
doTwice f x = f (f x)
\end{code}

<br>

Works on **many** kinds of values

~~~~~{.haskell}
ghci> :type doTwice
doTwice :: (a -> a) -> a -> a
~~~~~~

### For ALL Types `a`

- **takes**   function `f :: a -> a`
- **takes**   value    `x` :: a`
- **returns** an       `a`

## Polymorphic functions

Many functions are **oblivious** to the type of their arguments

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

### Can call with `Int` values 

~~~~~{.haskell}
ghci> doTwice (+1) 0
2
~~~~~

<br>
Type parameter `a` is **instantiated** as `Int` 

## Polymorphic functions

Many functions are **oblivious** to the type of their arguments

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

### Or with on `String` values 

~~~~~{.haskell}
ghci> doTwice (++ "I don't care! ") "Isn't this neat ?"

"Isn't this neat ? I don't care! I don't care! "
~~~~~

<br>
Type parameter `a` is **instantiated** as `String` 

## `doTwice` is Polymorphic

Works on **many** kinds of values

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

### Crucial for **abstraction** 

- Doing twice is **independent of** operation and data
- Don't need **separate** versions for `Int` , `String` ...

## Polymorphism Enables Reuse

Works on **many** kinds of values

~~~~~{.haskell}
doTwice     :: (a -> a) -> a -> a  
doTwice f x = f (f x)
~~~~~

<br>

**Reuse** same `doTwice` across different **operators** and **data**

## Polymorphism: With Great Power ...

... comes great responsibility!

### Recall infix **sections**

~~~~~{.haskell}
ghci> (10 <) 12
True
~~~~~

<br>

**Quiz:** What is the value of?

~~~~~{.haskell}
ghci> doTwice (10 <) 100
~~~~~

## Polymorphism: With Great Power ...

... comes great responsibility!

~~~~~{.haskell}
ghci> doTwice (10 <) 100
~~~~~

<br>

### Nasty Type Error

~~~~~{.haskell}
doTwice :: (a -> a) -- operator 
        -> a        -- input
        -> a        -- output
~~~~~

<br>

Operator should return **same** type as output ...

... but `(10 <) ::  Int -> Bool`

## Quiz

Are you following so far ?

<br>

\begin{code}
ex1 = doTwice doTwice
\end{code}

<br>

1. What is the **type of** `ex1` ?

2. What is the **type parameter** of `doTwice` **instantiated** as?


# Polymorphic Data  

## Polymorphic Data Types 

Of course, you can also create polymorphic data types

~~~~~{.haskell}
data List a 
  = Nil             -- empty list
  | Cons a (List a) -- "cons" of a hd :: a and tl :: List a
~~~~~

<br>

also known as (defined in standard library)

~~~~~{.haskell}
data [a] 
  = []        -- empty list
  | (:) a [a] -- "cons" of a hd :: a and tl :: List a
~~~~~

## Functions Over Polymorphic Data 

Polymorphic functions and data go hand in hand ...

\begin{code}
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs
\end{code}

<br>

or our friends from last time

\begin{code}
(++) :: [a] -> [a] -> [a] 
head :: [a] -> a
tail :: [a] -> [a]
\end{code}

<br>

**Note:** 

1. All the above **oblivious** to type of values **inside** list
2. Types are pretty awesome specifications of behavior 

# FP's tag-team = **Polymorphism** + **Higher-Order Funs**

## Computation Pattern 1: Iteration

<br>

### Convert Strings to UPPERCASE

~~~~~{.haskell}
toUpperString []     = []
toUpperString (c:cs) = (??? c) : toUpperString cs
~~~~~

Er, what to put in `???` Let's ask [Hoogle!](http://haskell.org/hoogle)

## Computation Pattern 1: Iteration

<br>

### Recall

\begin{code}
type XY      = (Double, Double)
type Polygon = [XY]
\end{code}

<br>

### A function that shifts a Polygon

~~~~~{.haskell}
shiftPoly d []       = []
shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys
~~~~~

<br>

### Helper That Translates *One* Vertex

\begin{code}
shiftXY (dx, dy) (x, y) = (x + dx, y + dy)
\end{code}

## A recurring theme: iteration!

<br>

### Like humans and monkeys `toUpperString` and `shiftPoly` are 93% same!

~~~~~{.haskell}
shiftPoly d []       = []
shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys
~~~~~

<br>

~~~~~{.haskell}
toUpperString []     = []
toUpperString (c:cs) = (toUpper c) : toUpperString cs
~~~~~

## A recurring theme: iteration!

<br>

### Like humans and monkeys `toUpperString` and `shiftPoly` are 93% same!

~~~~~{.haskell}
shiftPoly d []       = []
shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys


toUpperString []     = []
toUpperString (c:cs) = (toUpper c) : toUpperString cs
~~~~~

<br> 

### Capture the Pattern in a Bottle!

~~~~~{.haskell}
map          :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = (f x) : (map f xs)
~~~~~

## Capture the Iteration Pattern in a Bottle!

<img src="../static/fairy.png" width=100 align="middle"/> 

~~~~~{.haskell}
map          :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = (f x) : (map f xs)
~~~~~

<br>

### Instantiate the pattern

~~~~~{.haskell}
toUpperString = map toUpper
shiftPoly     = map shiftXY
~~~~~

<br>

**Quiz:** Dude, where are the parameters?

# Patterns everywhere!

## Computation Pattern 2: Folding

<br> 

### Function that *Adds* elements of a list

~~~~~{.haskell}
listAdd []     = 0
listAdd (x:xs) = x + (listAdd xs)
~~~~~

## Computation Pattern 2: Folding

<br> 

### Function that *Multiplies* elements of a list

~~~~~{.haskell}
listMul []     = 1
listMul (x:xs) = x * (listMul xs)
~~~~~

## Can you spot the pattern?

<br> 

### Pattern = Shared DNA

~~~~~{.haskell}
listAdd []     = 0
listAdd (x:xs) = x + (listAdd xs)

listMul []     = 1
listMul (x:xs) = x * (listMul xs)
~~~~~

**Quiz:** What are **differences** between `listAdd` and `listMul`?

## Pattern = Shared DNA = `foldr`

<br> 

~~~~~{.haskell}
listAdd []     = 0
listAdd (x:xs) = x + (listAdd xs)

listMul []     = 1
listMul (x:xs) = x * (listMul xs)
~~~~~

<br> 

~~~~~{.haskell}
foldr op base []     = base
foldr op base (x:xs) = x `op` (foldr op base xs) 
~~~~~

<br> 

\begin{code}
listAdd = foldr (+) 0   -- op = (+), base = 0
listMul = foldr (*) 1   -- op = (*), base = 1
\end{code}

## Pattern = Shared DNA = `foldr`

<br>

~~~~~{.haskell}
foldr op base []     = base
foldr op base (x:xs) = x `op` (foldr op base xs) 
~~~~~

### Recall

~~~~~{.haskell}
len []     = 0
len (x:xs) = 1 + len xs
~~~~~

<br>

### Quiz: Rewrite `len` with `foldr`!

~~~~~{.haskell}
len = foldr ex2 ex3 
~~~~~

What should `ex2` and `ex3` be?

# Public Service Announcement

## Public Service Announcement

<br>

What is more readable: **HOF** or **Recursion** ?

# Spotting Patterns In the "Real World"

## Spotting Patterns In the "Real World"

Patterns appear not just in **toy** functions but in **real code**

To practice, lets develop a small library that *swizzles* text files

1. Beginner's version, riddled with [explicit recursion](../lectures/swizzle-v0.html)

2. Intermediate version, recursion eliminated with [higher-order functions](../lectures/swizzle-v1.html)

3. Advanced version, `swizzle` and `unswizzle` [without duplication](../lectures/swizzle-v2.html)









[2]: http://haskell.org/hoogle "Hoogle Query: Char -> Char"
