---
title: Higher-Order Functions I
---

<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--no-termination" @-}

module Lec2 where

import Data.Char
\end{code}
</div>

Functions Are Data
==================

In all functional languages, functions are *first-class* values, meaning,
that they can be treated just as you would any other data. That is, you can
pass functions around to in *any* manner that you can pass any other data
around in. For example, suppose you have a simple functions `plus1` and `minus1`
defined via the equations

\begin{code}
plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1
\end{code}

Now, you can make a pair containing two instances of the function

\begin{code}
funp = (plus1, minus1)
\end{code}

Lets see what the type of the pair is

~~~~~{.haskell}
ghc> :type funp
funp :: (Int -> Int, Int -> Int)
~~~~~

or you can make a list containing five copies

\begin{code}
quiz1 = (plus1, minus1, plus1, "cat")

f2i :: Double -> Int
f2i = undefined
\end{code}

**QUIZ**

What is the type of `quiz1` ?


A. `Int -> Int`
B. `[Int -> Int]`
C. `[Int] -> [Int]`
D. `(Int -> Int, Int -> Int, Int -> Int)`
E. *Type Error*




<div class="hidden">

Indeed, note that the type is most descriptive

~~~~~{.haskell}
ghc> :type funs
funs :: [Int -> Int]
~~~~~

</div>

Taking Functions as Input
-------------------------

This innocent looking feature makes a langage surprisingly brawny and
flexible, because now, we can write *higher-order* functions that
other functions as input and return functions as output! Consider

\begin{code}
doTwice f x = f (f x)
\end{code}





Here, `doTwice` takes two inputs a function `f` and value  `x`,
and returns the the result of applying `f` to `x`, and feeding that
result back into `f` to get the final output, thereby twice applying
`f` to `x` in a manner of speaking. Note how the raw code is clearer
to understand than my long-winded English description!

Lets run it,

~~~~~{.haskell}
ghc> doTwice plus1 10
12
ghc> doTwice minus1 100
98
~~~~~

Did I mention that the execution model is just *substitute equals for equals*

~~~~~{.haskell}
doTwice plus1 10 == {- unfold doTwice -}
                    plus1 (plus1 10)
                 == {- unfold plus1 -}
		    plus1 (10 + 1)
		 == {- unfold plus1 -}
		    (10 + 1) + 1
		 == {- old-school arithmetic -}
		    12
~~~~~

Returning Functions as Output
-----------------------------

Similarly, it can be useful to write functions that return new
functions as output. For example, rather than writing different
versions `plus1`, `plus2`, `plus3` *etc.* we can just write a
single function `plusn` as

\begin{code}
plusn :: Int -> (Int -> Int)
plusn n = f
          where f x = x + n
\end{code}


plus10 1000 ==  (plusn 10) 1000
            ==  (f
                   where f x = x + 10) 1000
            ==   (f 1000) where f x  = x + 10
            ==   1000 + 10
            ==   1010

That is, `plusn` returns as output a function `f` which itself
takes as input an integer `x` and adds `n` to it. Lets use it

\begin{code}
plus10  = plusn 10
minus20 = plusn (-20)
\end{code}

Note the types of the above

~~~~~{.haskell}
plus10 :: Int -> Int
~~~~~

That is, `plus10` will take an input integer and return the
incremented-by-10 integer as output.


~~~~~{.haskell}
ghc> plus10 100
110

ghc> minus20 1000
980
~~~~~

You can also directly use `plusn`

~~~~~{.haskell}
ghc> (plusn 25) 100
125
~~~~~

What happened? You do the math

~~~~~{.haskell}
(plusn 25) 100 == {- unfold plusn -}
                  f 100
                  where f x = x + 25
               == {- unfold f -}
                  100 + 25
               == {- arithmetic -}
                  125
~~~~~

Super easy!


Partial Application
-------------------

In regular arithmetic, the `-` operator is *left-associative*. Hence,

~~~~~{.haskell}
	2 - 1 - 1 == (2 - 1) - 1 == 0
~~~~~

(and not `2 - (1 - 1) == 2` !). Just like `-` is an arithmetic operator
that takes two numbers and returns an number, in Haskell, `->` is a
*type operator* that takes two types, the input and output and returns
a new function type.

However, `->` is *right-associative* : the type

~~~~~{.haskell}
	Int -> Int -> Int
~~~~~

is equivalent to

~~~~~{.haskell}
	Int -> (Int -> Int)
~~~~~

That is, the first type of function, which takes two integers, is in
reality a function that takes a single integer as input, and returns
as *output* a function from integers to integers! Enriched with this
knowledge, consider the function

\begin{code}
plus     :: Int -> Int -> Int
plus n x = n + x
\end{code}

Thus, whenever we use `plus` we can either pass in both the inputs
at once

~~~~~{.haskell}
ghci> plus 10 20
30
~~~~~

or instead, we can  *partially apply* the function, by just passing
in only one input

\begin{code}
plusfive :: Int -> Int
plusfive = plus 5
\end{code}

thereby getting as output a function that is *waiting* for the second
input (at which point it will produce the final result.)

~~~~~{.haskell}
ghci> plus5 1000
1005
~~~~~

Again, how ? (Cough) *Substitute equals for equals*

~~~~~{.haskell}
plusfive 1000 == {- definition of plusfive -}
	         plus 5 1000
	      == {- unfold plus -}
	         5 + 1000
              == 1005
~~~~~

Finally, by now it should be pretty clear that `plusn n` is equivalent
to the partially applied `plus n`.

**QUIZ**

If you have been following so far, you should know
what is the value of `quiz2` ?

\begin{code}
quiz2   = zog 100
  where
    zog = doTwice (plus 20)
\end{code}


A. *Type Error*
B. `40`
C. `100`
D. `120`
E. `140`









First, see if you can figure out the type.

\begin{code}
doTwicePlus20 = doTwice (plus 20)
\end{code}

~~~~~{.haskell}
doTwicePlus20 :: Int -> Int
~~~~~

Next, see if you can figure out how this evaluates.

~~~~~{.haskell}
ghci> doTwicePlus20 0
40
~~~~~

Anonymous Functions
-------------------

As we have seen, with Haskell, it is quite easy to create function values
that are not bound to any name. For example the expression `plus 1000`
yields a function value that is not bound to any name.

We will see many situations where a particular function is only used once,
and hence, there is no need to explicitly name it. Haskell provides a
mechanism to create such *anonymous* functions. For example,

~~~~~{.haskell}
\x -> x + 1
~~~~~

is an expression that corresponds to a function that takes an argument `x`
and returns as output the value `x + 2`. The function has no name, but we
can use it in the same place where we would write a function.


~~~~~{.haskell}
ghci> (\x -> x + 1) 100
101
ghci> doTwice (\x -> x + 1) 100
102
~~~~~

Of course, we could name the function if we wanted to

\begin{code}
plus1' :: Int -> Int
plus1' = \x -> x + 1
\end{code}

Indeed, in general, a function defining equation

~~~~~{.haskell}
f x1 x2 ... xn = e
~~~~~

is equivalent to

~~~~~{.haskell}
f = \x1 x2 ... xn -> e
~~~~~

Infix Operations and Sections
-----------------------------

In order to improve readability, Haskell allows you to use certain
functions as *infix* operations: a function whose name appears in
parentheses can be used as an infix operation. My personal favorite
infix operator is the *pipeline* function defined thus

\begin{spec}
(|>) x f = f x
\end{spec}

Huh? Doesn't seem so compelling does it? Actually, its very handy
because I can now write deeply nested function applications in an
easy to read unix-style pipes manner. For example

~~~~~{.haskell}
ghci> 0 |> plus 1
3
ghci> 0 |> plus 1 |> plus5
6
~~~~~

We can get a triple-repeating version of `doTwice` as

\begin{code}
doThrice f x = x |> f |> f |> f

append (x:xs) ys = x : append xs ys
append _      ys = ys
\end{code}

It is easy to check that

~~~~~{.haskell}
doThrice f x == f (f (f x))
~~~~~

We will see many other such operators in the course of the class;
indeed many standard operators including that we have used already
are defined in this manner in the standard library.

~~~~~{.haskell}
ghci> :type (+)
(+) :: (Num a) => a -> a -> a
ghci> :type (++)
(++) :: [a] -> [a] -> [a]
ghci> :type (:)
(:) :: a -> [a] -> [a]
~~~~~

Furthermore, Haskell allows you to use *any* function as an infix
operator, simply by wrapping it inside backticks.

~~~~~{.haskell}
ghci> 2 `plus` 3
5
~~~~~

Recall the function from the last lecture

\begin{code}
clone         :: Int -> a -> [a]
clone n x
  | n <= 0    = []
  | otherwise = x : clone (n-1) x
\end{code}


We invoke it in an infix-style like so

~~~~~{.haskell}
ghci> 30 `clone` 3
[30,30,30]
~~~~~

To further improve readability, Haskell allows you
to use *partially applied* infix operators, ie  infix
operators with only a single argument. These
are called *sections*. Thus, the section `(+1)`

is simply a function that takes as input a number, the
argument missing on the left of the `+` and returns
that number plus `1`.



goober = (`clone` 3) ==> \n -> clone n 3

n `clone` x =======> clone n x

(`clone` x) =======> (\n -> clone n x)
(n `clone`) =======> (\x -> clone n x)

quiz7 = goober 2 ======> (`clone` 3) 2 ===> (2 `clone` 3) ====

A. TYPE ERROR
B. [2,2,2]
C. [3,3]
D. [3,3,3]
E. [2,2]














~~~~~{.haskell}
ghci> doThrice (+1) 0
3
~~~~~

Similarly, the section `(1:)` takes a list of numbers
and returns a new list with `1` followed by the input
list. Consequently,

~~~~~{.haskell}
ghci> doTwice (1:) [2..5]
[1,1,3,4,5]
~~~~~

Polymorphism
============

We used to `doTwice` to repeat an arithmetic operation, but the actual body
of the function is oblivious to how `f` behaves. For example,

~~~~~{.haskell}
ghci> doTwice (+1) 0
2

ghci> doTwice (++ "I don't care! ") "Isn't this neat ?"
"Isn't this neat ? I don't care! I don't care! "
~~~~~

Thus, `doTwice` is *polymorphic* in that it works with different
kinds of values, eg functions that increment integers and concatenate
strings. This is vital for *abstraction*. The general notion of repeating,
ie  *doing twice* is entirely independent of the *specific* operation that
is being repeated, and so we shouldn't have to write separate repeaters for
integers and strings. Polymorphism allows us to *reuse* the same abstraction
`doTwice` in different settings.

Of course, with great power, comes great responsibility.

The section `(10 <)` takes an integer and returns `True`
iff the integer is greater than `10`

\begin{code}
greaterThan10 :: Int -> Bool
greaterThan10 = (10 <)
\end{code}

**QUIZ**

What is the value of:

\begin{spec}
quiz3 = doTwice greaterThan10 100
\end{spec}

A. `True`
B. `False`
C. *Type Error*
D. `Run-time Error`
E. `101`








Indeed, because the input and output types are different, it doesn't make
sense to try `doTwice greaterThan10`

~~~~~{.haskell}
ghci> doTwice greaterThan10 100


<interactive>:36:9:
    Couldn't match type ‘Bool’ with ‘Int’
    Expected type: Int -> Int
      Actual type: Int -> Bool
    In the first argument of ‘doTwice’, namely ‘greaterThan10’
    In the expression: doTwice greaterThan10 100
~~~~~

Urgh!!! However, a quick glance at the type of doTwice would have spared us
this grief.

~~~~~{.haskell}
ghci> :type doTwice
doTwice :: (t -> t) -> t -> t
~~~~~

The `t` above is a *type variable*. The signature above states that the
first argument to `doTwice` must be a function that maps values of type
`t` to `t`, ie  must produce an output that has the same type as its
input (so that that output can be fed into the function again!)
The second argument must also be a `t` at which point we may are guaranteed
that the result from `doTwice` will also be a `t`. The above holds *for
any* `t` which allows us to safely re-use `doTwice` in different settings.
Of course, if the input and output type of the input function are
different, as in `greaterThan10` then the function is incompatible with
`doTwice`.


**QUIZ**

Ok, to make sure you're following, what is the type of:

\begin{code}
foo x f = f x
\end{code}

What is the type of `foo` ?
A. `a -> a`
B. `(a -> a) -> a`
C. `a -> b -> a -> b`
D. `a -> (a -> b) -> b`  -- ($) f x = f x
E. `a -> b -> a`





-- the "second" function is the thing with the first argument "filled in"

plus     :: Int -> (Int -> Int)
plus     :: (Int, Int) -> Int
plus x y = x + y




**QUIZ**

\begin{code}
x |> f   = f x
do2 f x  = x |> f |> f
do3 f x  = x |> f |> f |> f

quiz4    = do3
\end{code}

A. `a -> a`
B. `(a, a) -> (a, a)`
C. `(a -> a) -> a -> a`
D. `a -> (a, a)`
E. `a -> a -> a -> a`


**QUIZ**

And what is the *value* of:

\begin{code}
quiz5   = foo (+ 10) 0
  where
    foo = do2 do3
\end{code}

A. `50`
B. *Type Error*
C. `60`






Polymorphic Data Structures
---------------------------

Polymorphic functions which can *operate* on different kinds values
are often associated with polymorphic data structures which can
*contain* different kinds of values. These are also represented
by types containing type variables.

For example, the list length function

\begin{code}
len        :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs
\end{code}

doesn't peep inside the actual contents of the list; it only
counts how many there are. This property is crisply specified
in the function's signature, which states that we can invoke `len`
on any kind of list. The type variable `a` is a placeholder that is
replaced with the actual type of the list at different application
sites. Thus, in the below instances, `a` is replaced with `Double`,
`Char` and `[Int]` respectively.

~~~~~{.haskell}
ghci> len [1.1, 2.2, 3.3, 4.4]
4

ghci> len "mmm donuts!"
11

ghci> len [[], [1], [1,2], [1,2,3]]
4
~~~~~

Most standard list manipulating functions, for example those in
the standard library [Data.List][1]
have generic types. You'll find that the type signature contains a
surprising amount of information about how the function behaves.

~~~~~{.haskell}
(++) :: [a] -> [a] -> [a]
head :: [a] -> a
tail :: [a] -> [a]
~~~~~

Bottling Computation Patterns With Polymorphic Higher-Order Functions
=====================================================================

The tag-team of polymorphism and higher-order functions is the secret
sauce that makes FP so tasty. It allows us to take arbitrary, *patterns
of computation* that reappear in different guises in different places,
and crisply specify them as safely reusable strategies. That sounds very
woolly (I hope), lets look at some concrete examples.


Computation Pattern: Iteration
------------------------------

Lets write a function that converts a string to uppercase.
Recall that in Haskell, a `String` is just a list of `Char`.
We must start with a function that will convert an individual
`Char` to its uppercase version. Once we find this function,
we will simply *jog over the list*, and apply the function to
each `Char`.

How might we find such a transformer? Lets query [Hoogle][2]
for a function of the appropriate type! Ah, we see that the
module `Data.Char` contains a function.

~~~~~{.haskell}
toUpper :: Char -> Char
~~~~~

and so now, we can write the simple recursive function

~~~~~{.haskell}
toUpperString []     = []
toUpperString (c:cs) = toUpper c : toUpperString cs
~~~~~

As you might imagine, this sort of recursion appears all over
the place. For example, suppose I represent a using a pair
of `Double` (for the x- and y- coordinates.) and I have a list
of points that represent a polygon.

\begin{code}
type XY      = (Double, Double)
type Polygon = [XY]
\end{code}

Now, its easy to write a function that *shifts* a
point by a specific amount

\begin{code}
shiftXY :: XY -> XY -> XY
shiftXY (dx, dy) (x, y) = (x + dx, y + dy)
\end{code}

How would we translate a polygon? Just jog over all the
points in the polygon and translate them individually

~~~~~{.haskell}
shiftPoly :: XY -> Polygon -> Polygon

shiftPoly d []     = []
shiftPoly d (x:xs) = shiftXY d x : shiftPoly d xs

uppercase []       = []
uppercse (x:xs)   = toUpper x : uppercase xs

shiftPoly d    = thing (shiftXY d)
uppercase      = thing toUpper

~~~~~

Now, in a lesser language, you might be quite happy with
the above code. But what separates a good programmer from
a great one, is the ability to *abstract*.

Like humans and monkeys, the functions `toUpperString`
and `shiftPoly` [share 93% of their DNA][3] -- the notion
of *jogging* over the list. The common pattern is described
by the polymorphic higher-order function `map`

~~~~~{.haskell}
map f []     = []
map f (x:xs) = (f x) : (map f xs)
~~~~~

How did we arrive at this? Well, you find what is enshrine in the
function's body that which is *common*  to the different instances,
namely the recursive jogging strategy; and the bits that are different,
simply become the function's parameters! Thus, the `map` function
abstracts, or if you have a vivid imagination, locks up in a bottle,
the extremely common pattern of jogging over the list.

![Fairy In a Bottle](/static/fairy.png)

Verily, the type of `map` tells us exactly what it does

~~~~~{.haskell}
ghci> :type map
map :: (a -> b) -> [a] -> [b]
~~~~~

That is, it takes an `a -> b` transformer and list of `a` values,
and transforms each value to return a list of `b` values. We can
now safely reuse the pattern, by *instantiating* the transformer
with different specific operations.

\begin{code}
toUpperString = map toUpper
shiftPoly     = map shiftXY
\end{code}

Much better.

By the way, what happened to the parameters of `toUpperString` and `shiftPoly`?
Two words: *partial application*. In general, in Haskell, a function
definition equation

~~~~~{.haskell}
f x = e x
~~~~~

is identical to

~~~~~{.haskell}
f = e
~~~~~

as long as `x` doesn't appear in `e`. Thus, to save ourselves the trouble
of typing, and the blight of seeing, the vestigial `x` we *often* prefer
to just leave it out altogether.

As an exercise, to prove to yourself using just equational reasoning
(using the different equality laws we have seen) that the above versions
of `toUpperString` and `shiftPoly` are equivalent.

Computation Pattern: Folding
----------------------------

Once you've put on the FP goggles, you start seeing computation
patterns  everywhere.

Lets write a function that *adds* all the elements of a list.

~~~~~{.haskell}
listAdd []     = 0
listAdd (x:xs) = x + listAdd xs
~~~~~

Next, a function that *multiplies* the elements of a list.

~~~~~{.haskell}
listMul []     = 1
listMul (x:xs) = x * (listMul xs)
~~~~~

Can you see the pattern? Again, the only bits that are different are the
`base` case value, and the `op` being performed at each step. We'll just
turn those into parameters, and lo!

~~~~~{.haskell}
foldr op base []     = base
foldr op base (x:xs) = x `op` (foldr op base xs)
~~~~~

Now, each of the individual functions are just specific instances of the
general `foldr` pattern.

\begin{code}
listAdd = foldr (+) 0
listMul = foldr (*) 1
\end{code}

To develop some intuition about `foldr` lets "run" it a few times by hand.

~~~~~{.haskell}
foldr op base [x1,x2,...,xn]
  == {- unfold -}
     x1 `op` (foldr op base [x2,...,xn])
  == {- unfold -}
     x1 `op` (x2 `op` (foldr op base [...,xn]))
  == {- unfold -}
     x1 `op` (x2 `op` (... `op` (xn `op` base)))
~~~~~

Aha! It has a rather pleasing structure that mirrors that of lists; the `:`
is replaced by the `op` and the `[]` is replaced by `base`. Thus, can you
see how to use it to eliminate recursion from the recursion from

\begin{spec}
listLen []     = 0
listLen (x:xs) = 1 + listLen xs
\end{spec}

**QUIZ**

Which of these is a valid implementation of `listLen`

A. `listLen = foldr (\n -> n + 1) 0`
B. `listLen = foldr (+ 1) 0`
C. `listLen = foldr (\_ n -> n + 1) 0`
D. `listLen = foldr (\x xs -> 1 + listLen xs) 0`
E. All of the above


How would you use it to eliminate the recursion from

~~~~~{.haskell}
factorial 0 = 1
factorial n = n * factorial (n-1)
~~~~~


\begin{code}
factorial n = foldr (*) 1 [1..n]
\end{code}




One last pattern exercise. How about this fellow from last lecture:

~~~~~{.haskell}
fuseActions :: [IO ()] -> IO ()
fuseActions []        = return ()
fuseActions (a1:acts) = do a1
                           fuseActions acts
~~~~~

Can you spot the pattern?

\begin{code}
fuseActions :: [IO ()] -> IO ()
fuseActions = foldr (>>) (return ())
\end{code}

Which is more readable? HOFs or Recursion
-----------------------------------------

As a beginner, you might think that the recursive versions of `toUpperString` and
`shiftPoly` are easier to follow than the `map` versions. Certainly, `fold` takes
a bit of getting used to.

However, as you get used to the light, you will
find the latter is infact far easier to follow, because once
abstraction gets into your bones you'll know exactly what an
instance does at a single glance.

In contrast, recursion is lower-level, so every time you see
a recursive function, you have to understand how the knots are
tied, and worse, there is potential for making silly off-by-one
type errors if you re-jigger the basic strategy every time.

As an added bonus, it can be quite useful and profitable to
*parallelize* and *distribute* the computation patterns (like
`map` and `fold`) in just one place, thereby allowing arbitrary
hundreds or thousands of instances to [benefit in a single shot!][4]

[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html "Data.List"
[2]: http://haskell.org/hoogle "Hoogle Query: Char -> Char"
[3]: http://www.livescience.com/health/070412_rhesus_monkeys.html
[4]: http://en.wikipedia.org/wiki/MapReduce "MapReduce"
