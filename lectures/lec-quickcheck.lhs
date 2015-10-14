---
title: QuickCheck: Type-directed Property Testing
---

> {-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}
> module Testing where 
> import Test.QuickCheck hiding ((===))
> import Control.Monad
> import Data.List
> import qualified Data.Map as M 
> import Control.Monad.State hiding (when)
> import Control.Applicative ((<$>))
 
In this lecture, we will look at [QuickCheck][1], a technique that
cleverly exploits typeclasses and monads to deliver a powerful 
automatic testing methodology. 

Quickcheck was developed by [Koen Claessen][0] and [John Hughes][11] more
than ten years ago, and has since been ported to other languages and
is currently used, among other things to find subtle [concurrency bugs][3]
in [telecommunications code][4].

The key idea on which QuickCheck is founded, is *property-based testing*. 
That is, instead of writing individual test cases (eg unit tests 
corresponding to input-output pairs for particular functions) one 
should write *properties* that are desired of the functions, and 
then *automatically* generate *random* tests which can be run to
verify (or rather, falsify) the property.

By emphasizing the importance of specifications, QuickCheck yields 
several benefits:

1. The developer is forced to think about what the code *should do*,

2. The tool finds corner-cases where the specification is violated, 
   which leads to either the code or the specification getting fixed,

3. The specifications live on as rich, machine-checkable documentation
   about how the code should behave.

Properties
==========

A QuickCheck property is essentially a function whose output is a boolean.
The standard "hello-world" QC property is

> prop_revapp :: [Int] -> [Int] -> Bool
> prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys


That is, a property looks a bit like a mathematical theorem that the
programmer believes is true. A QC convention is to use the prefix `"prop_"`
for QC properties. Note that the type signature for the property is not the 
usual polymorphic signature; we have given the concrete type `Int` for the
elements of the list. This is because QC uses the types to generate random
inputs, and hence is restricted to monomorphic properties (that don't
contain type variables.)

To *check* a property, we simply invoke the function

~~~~~{.haskell}
quickCheck :: (Testable prop) => prop -> IO ()
  	-- Defined in Test.QuickCheck.Test
~~~~~

lets try it on our example property above

~~~~~{.haskell}
ghci> quickCheck prop_revapp 
*** Failed! Falsifiable (after 2 tests and 1 shrink):     
[0]
[1]
~~~~~

Whats that ?! Well, lets run the *property* function on the two inputs

~~~~~{.haskell}
ghci> prop_revapp [0] [1] 
False
~~~~~

QC has found a sample input for which the property function *fails* ie,
returns `False`. Of course, those of you who are paying attention will
realize there was a bug in our property, namely it should be

> prop_revapp_ok :: [Int] -> [Int] -> Bool
> prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

because `reverse` will flip the order of the two parts `xs` and `ys` of 
`xs ++ ys`. Now, when we run 

~~~~~{.haskell}
*Main> quickCheck prop_revapp_ok
+++ OK, passed 100 tests.
~~~~~

That is, Haskell generated 100 test inputs and for all of those, the
property held. You can up the stakes a bit by changing the number of
tests you want to run

> quickCheckN   :: (Testable p) => Int -> p -> IO () 
> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

and then do

~~~~~{.haskell}
*Main> quickCheckN 10000 prop_revapp_ok
+++ OK, passed 10000 tests.
~~~~~

QuickCheck QuickSort
--------------------

Lets look at a slightly more interesting example. Here is the canonical 
implementation of *quicksort* in Haskell.

> qsort        :: (Ord a) => [a] -> [a]
> qsort []     = []
> qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
>   where lhs  = [y | y <- xs, y < x]
>         rhs  = [z | z <- xs, z > x]

Really doesn't need much explanation! Lets run it "by hand" on a few inputs

~~~~~{.haskell}
ghci> [10,9..1]
[10,9,8,7,6,5,4,3,2,1]
ghci> qsort [10,9..1]
[1,2,3,4,5,6,7,8,9,10]

ghci> [2,4..20] ++ [1,3..11]
[2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11]
ghci> qsort $ [2,4..20] ++ [1,3..11]
[1,2,3,4,5,6,7,8,9,10,11,12,14,16,18,20]
~~~~~

Looks good -- lets try to test that the output is in 
fact sorted. We need a function that checks that a 
list is ordered

> isOrdered ::         (Ord a) => [a] -> Bool
> isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
> isOrdered _          = True

and then we can use the above to write a property

> prop_qsort_isOrdered :: [Int] -> Bool
> prop_qsort_isOrdered = isOrdered . qsort

Lets test it!

~~~~~{.haskell}
ghci> quickCheckN 10000 prop_qsort_isOrdered 
+++ OK, passed 10000 tests.
~~~~~

Conditional Properties
----------------------

Here are several other properties that we 
might want. First, repeated `qsorting` should not
change the list. That is, 

> prop_qsort_idemp ::  [Int] -> Bool 
> prop_qsort_idemp xs = qsort (qsort xs) == qsort xs


Second, the head of the result is the minimum element
of the input

> prop_qsort_min :: [Int] -> Bool
> prop_qsort_min xs = head (qsort xs) == minimum xs

However, when we run this, we run into a glitch


~~~~~{.haskell}
ghci> quickCheck prop_qsort_min 
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):  
[]
~~~~~

But of course! The earlier properties held *for all inputs*
while this property makes no sense if the input list is empty! 
This is why thinking about specifications and properties has the 
benefit of clarifying the *preconditions* under which a given 
piece of code is supposed to work. 

In this case we want a *conditional properties* where we only want 
the output to satisfy to satisfy the spec *if* the input meets the
precondition that it is non-empty.

> prop_qsort_nn_min    :: [Int] -> Property
> prop_qsort_nn_min xs = 
>   not (null xs) ==> head (qsort xs) == minimum xs
>
> prop_qsort_nn_max    :: [Int] -> Property
> prop_qsort_nn_max xs = 
>   not (null xs) ==> head (reverse (qsort xs)) == maximum xs

We can write a similar property for the maximum element too. This time
around, both the properties hold

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_qsort_nn_min
+++ OK, passed 1000 tests.

ghci> quickCheckN 1000 prop_qsort_nn_max
+++ OK, passed 1000 tests.
~~~~~

Note that now, instead of just being a `Bool` the output
of the function is a `Property` a special type built into 
the QC library. Similarly the *implies* combinator `==>` 
is on of many QC combinators that allow the construction 
of rich properties.


Testing Against a Model Implementation
--------------------------------------

We could keep writing different properties that capture 
various aspects of the desired functionality of `qsort`. 
Another approach for validation is to test that our `qsort` 
is *behaviourally* identical to a trusted *reference 
implementation* which itself may be too inefficient or 
otherwise unsuitable for deployment. In this case, lets 
use the standard library's `sort` function

> prop_qsort_sort    :: [Int] -> Bool
> prop_qsort_sort xs =  qsort xs == sort xs

which we can put to the test

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_qsort_sort
*** Failed! Falsifiable (after 4 tests and 1 shrink):     
[-1,-1]
~~~~~

Say, what?!

~~~~~{.haskell}
ghci> qsort [-1,-1]
[-1]
~~~~~

Ugh! So close, and yet ... Can you spot the bug in our code?

~~~~~{.haskell}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs  = [y | y <- xs, y < x]
        rhs  = [z | z <- xs, z > x]
~~~~~

We're assuming that the *only* occurrence of (the value) `x` 
is itself! That is, if there are any *copies* of `x` in the 
tail, they will not appear in either `lhs` or `rhs` and hence
they get thrown out of the output. 


Is this a bug in the code? What *is* a bug anyway? Perhaps the
fact that all duplicates are eliminated is a *feature*! At any 
rate there is an inconsistency between our mental model of how 
the code *should* behave as articulated in `prop_qsort_sort` 
and the actual behavior of the code itself.

We can rectify matters by stipulating that the `qsort` produces
lists of distinct elements

> isDistinct ::(Eq a) => [a] -> Bool
> isDistinct (x:xs) = not (x `elem` xs) && isDistinct xs
> isDistinct _      = True
>
> prop_qsort_distinct :: [Int] -> Bool 
> prop_qsort_distinct = isDistinct . qsort  

and then, weakening the equivalence to only hold on inputs that 
are duplicate-free 

> prop_qsort_distinct_sort :: [Int] -> Property 
> prop_qsort_distinct_sort xs = 
>   (isDistinct xs) ==> (qsort xs == sort xs)

QuickCheck happily checks the modified properties

~~~~~{.haskell}
ghci> quickCheck prop_qsort_distinct
+++ OK, passed 100 tests.

ghci> quickCheck prop_qsort_distinct_sort 
+++ OK, passed 100 tests.
~~~~~


The Perils of Conditional Testing
---------------------------------

Well, we managed to *fix* the `qsort` property, but beware! Adding
preconditions leads one down a slippery slope. In fact, if we paid
closer attention to the above runs, we would notice something

~~~~~{.haskell}
ghci> quickCheckN 10000 prop_qsort_distinct_sort 
...
(5012 tests; 248 discarded)
...
+++ OK, passed 10000 tests.
~~~~~

The bit about some tests being *discarded* is ominous. In effect, 
when the property is constructed with the `==>` combinator, QC 
discards the randomly generated tests on which the precondition 
is false. In the above case QC grinds away on the remainder until 
it can meet its target of `10000` valid tests. This is because 
the probability of a randomly generated list meeting the precondition 
(having distinct elements) is high enough. This may not always be the case.

The following code is (a simplified version of) the `insert` function 
from the standard library 

~~~~~{.haskell}
insert x []                 = [x]
insert x (y:ys) | x > y     = x : y : ys
                | otherwise = y : insert x ys
~~~~~

Given an element `x` and a list `xs`, the function walks along `xs` 
till it finds the first element greater than `x` and it places `x` 
to the left of that element. Thus

~~~~~{.haskell}
ghci> insert 8 ([1..3] ++ [10..13])
[1,2,3,8,10,11,12,13]
~~~~~

Indeed, the following is the well known [insertion-sort][5] algorithm

> isort :: (Ord a) => [a] -> [a]
> isort = foldr insert []

We could write our own tests, but why do something a machine can do better?!

> prop_isort_sort    :: [Int] -> Bool
> prop_isort_sort xs = isort xs == sort xs

~~~~~{.haskell}
ghci> quickCheckN 10000 prop_isort_sort 
+++ OK, passed 10000 tests.
~~~~~

Now, the reason that the above works is that the `insert` 
routine *preserves* sorted-ness. That is while of course 
the property 

> prop_insert_ordered'      :: Int -> [Int] -> Bool
> prop_insert_ordered' x xs = isOrdered (insert x xs)

is bogus

~~~~~{.haskell}
ghci> quickCheckN 10000 prop_insert_ordered' 
*** Failed! Falsifiable (after 4 tests and 1 shrink):     
0
[0,-1]

ghci> insert 0 [0, -1]
[0, 0, -1]
~~~~~

the output *is* ordered if the input was ordered to begin with

> prop_insert_ordered      :: Int -> [Int] -> Property 
> prop_insert_ordered x xs = 
>   isOrdered xs ==> isOrdered (insert x xs)

Notice that now, the precondition is more *complex* -- the property 
requires that the input list be ordered. If we QC the property

~~~~~{.haskell}
ghci> quickCheckN 10000 prop_insert_ordered
*** Gave up! Passed only 35 tests.
~~~~~

Ugh! The ordered lists are so *sparsely* distributed 
among random lists, that QC timed out well before it 
found 10000 valid inputs!

*Aside* the above example also illustrates the benefit of 
writing the property as `p ==> q` instead of using the boolean
operator `||` to write `not p || q`. In the latter case, there is 
a flat predicate, and QC doesn't know what the precondition is,
so a property may hold *vacuously*. For example consider the 
variant

> prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
> prop_insert_ordered_vacuous x xs = 
>   not (isOrdered xs) || isOrdered (insert x xs)

QC will happily check it for us

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_insert_ordered_vacuous
+++ OK, passed 10000 tests.
~~~~~

Unfortunately, in the above, the tests passed *vacuously* 
only because their inputs were *not* ordered, and one 
should use `==>` to avoid the false sense of security 
delivered by vacuity.

QC provides us with some combinators for guarding against 
vacuity by allowing us to investigate the *distribution* 
of test cases

~~~~~{.haskell}
collect  :: Show a => a -> Property -> Property
classify :: Bool -> String -> Property -> Property
~~~~~

We may use these to write a property that looks like

> prop_insert_ordered_vacuous' :: Int -> [Int] -> Property 
> prop_insert_ordered_vacuous' x xs = 
>   -- collect (length xs) $
>   classify (isOrdered xs) "ord" $
>   classify (not (isOrdered xs)) "not-ord" $
>   not (isOrdered xs) || isOrdered (insert x xs)

When we run this, as before we get a detailed breakdown
of the 100 passing tests

~~~~~{.haskell}
ghci> quickCheck prop_insert_ordered_vacuous'
+++ OK, passed 100 tests:
 9% 1, ord
 2% 0, ord
 2% 2, ord
 5% 8, not-ord
 4% 7, not-ord
 4% 5, not-ord
 ...
~~~~~

where a line `P% N, COND` means that `p` percent of the inputs had length
`N` and satisfied the predicate denoted by the string `COND`. Thus, as we
see from the above, a paltry 13% of the tests were ordered and that was
because they were either empty (`2% 0, ord`) or had one (`9% 1, ord`).
or two elements (`2% 2, ord`). The odds of randomly stumbling upon a 
beefy list that is ordered are rather small indeed!


Generating Data
===============

Before we start discussing how QC generates data (and how we can help it
generate data meeting some pre-conditions), we must ask ourselves a basic
question: how does QC behave *randomly* in the first place?!

~~~~~{.haskell}
ghci> quickCheck prop_insert_ordered'
*** Failed! Falsifiable (after 4 tests and 2 shrinks):    
0
[0,-1]

ghci> quickCheck prop_insert_ordered'
*** Failed! Falsifiable (after 5 tests and 5 shrinks):    
0
[1,0]
~~~~~

Eh? This seems most *impure* -- same inputs yielding two totally different
outputs! Well, this should give you a clue as to one of the key techniques
underlying QC -- **monads!** 

The Generator Monad
-------------------

A Haskell term that generates a (random value) of type `a` has the type
[`Gen a`][6] which is defined as

~~~~~{.haskell}
newtype Gen a = MkGen { unGen :: StdGen -> Int -> a }
~~~~~

In effect, the term is a function that takes as input a random number
generator `StdGen` and a seed `Int` and returns an `a` value. One can
easily (and we shall see, profitably!) turn `Gen` into a `Monad` by

~~~~~{.haskell}
instance Monad Gen where
  return x =
    MkGen (\_ _ -> x)
  
  MkGen m >>= k =
    MkGen (\r n ->
      let (r1, r2)  = split r
          MkGen m' = k (m r1 n)
       in m' r2 n
    )
~~~~~

The function `split` simply *forks* the random number generator into two
parts; which are used by the left and right parameters of the bind
operator `>>=`. (*Aside* you should be able to readily spot the 
similarity between random number generators and the `ST` monad -- 
in both cases the basic action is to grab some value and transition 
the *state* to the next-value. For more details see [Chapter 14, RWH][7])

The Arbitrary Typeclass
-----------------------

QC uses the above to define a typeclass for types for which
random values can be generated!

~~~~~{.haskell}
class Show a where
  show :: a -> String

class Arbitrary a where
  arbitrary :: Gen a
~~~~~

> gimmeInts :: IO [Int]
> gimmeInts = sample' arbitrary

Thus, to have QC work with (ie generate random tests for) values of type
`a` we need only make `a` an instance of `Arbitrary` by defining an
appropriate `arbitrary` function for it. QC defines instances for base
types like `Int` , `Float`, lists etc and lifts them to compound types
much like we did for `JSON` a [few lectures back](/lectures/lec5.html)

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = do x <- arbitrary
                 y <- arbitrary 
                 return (x,y)
~~~~~

or more simply

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = liftM2 (,) arbitrary arbitrary

do x <- mx
   y <- my
   return $ f x y
~~~~~


Generator Combinators
---------------------

QC comes loaded with a set of combinators that allow us to create 
custom instances for our own types.

The first of these combinators is `choose`

~~~~~{.haskell}
choose :: (System.Random.Random a) => (a, a) -> Gen a
~~~~~

which takes an *interval* and returns an random element from that interval.
(The typeclass `System.Random.Random` describes types which can be
*sampled*. For example, the following is a randomly chosen set of numbers
between `0` and `3`.

~~~~~{.haskell}
ghci> sample $ choose (0, 3)
2
1
0
2
1
0
2
3
0
0
~~~~~

<!--

QUIZ
----

What is a plausible type for `sample`? 


a. `Gen a -> [a]`
b. `Gen a -> Gen [a]`
c. `Gen a -> IO [a]`
d. `Gen a -> IO a`
e. `a -> Gen [a]`

-->

A second useful combinator is `elements` 

~~~~~{.haskell}
elements :: [a] -> Gen a
~~~~~

fmap :: (a -> b) -> (m a) -> (m b)
fmap f m = do x <- m 
              return (f x)

elements xs = do i <- choose (0, (length xs) - 1)
                 return (xs !! i)

elements xs = (xs !!) <$> choose (0, length xs - 1)


oneOf     :: [Gen a] -> Gen a
oneOf gs = do g <- elements gs
              x <- g
              return x


which returns a generator that produces values drawn from the input list

~~~~~{.haskell}
ghci> sample $ elements [10, 20..100]
60
70
30
50
30
20
20
10
100
80
10
~~~~~

A third combinator is `oneof` 

~~~~~{.haskell}
oneof :: [Gen a] -> Gen a
~~~~~

which allows us to randomly choose between multiple generators

~~~~~{.haskell}
ghci> sample $ oneof [elements [10,20,30], choose (0,3)]
10
0
10
1
30
1
20
2
20
3
30
~~~~~

EXERCISE
--------

Lets try to figure out the **implementation** of `oneOf`

~~~~~{.haskell}
oneOf :: [Gen a] -> Gen a
oneOf = error "LETS DO THIS IN CLASS"
~~~~~


Finally, `oneOf` is generalized into the `frequency` combinator 

~~~~~{.haskell}
frequency :: [(Int, Gen a)] -> Gen a
~~~~~

which allows us to build weighted combinations of individual generators.


Generating Ordered Lists
------------------------

We can use the above combinators to write generators for lists 

> genList1 ::  (Arbitrary a) => Gen [a]
> genList1 = liftM2 (:) arbitrary genList1

Can you spot a problem in the above? 

~~~~~{.haskell}
-- btw, don't freak out, remember that 

liftM2 f mx my = do x <- m1
                    y <- m2
                    return $ f x y

-- So the above is the same as

genList1 = do x  <- arbitrary
              xs <- gentList1
              return $ x : xs
~~~~~



**Problem**: `genList1` only generates infinite lists! Hmm. Lets try again,

> genList2 ::  (Arbitrary a) => Gen [a]
> genList2 = oneof [ return []
>                  , liftM2 (:) arbitrary genList2]

This is not bad, but we may want to give the generator a higher 
chance of not finishing off with the empty list, so lets use

> genList3 ::  (Arbitrary a) => Gen [a]
> genList3 = frequency [ (1, return [])
>                      , (7, liftM2 (:) arbitrary genList2) ]

We can use the above to build a custom generator that always returns
*ordered lists* by piping the generate list into the `sort` function

> genOrdList :: (Ord a, Arbitrary a) => Gen [a]
> genOrdList = sort <$> genList3 

~~~~~{.haskell}
-- again, remember that, <$> is just `fmap` where:

fmap f m = do {x <- m; return (f x)} 

-- so really the above is the same as:

genOrdList = do { x <- genList3 ; return (sort x) }
~~~~~

To *check* the output of a custom generator we can use the `forAll` combinator

~~~~~{.haskell}
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
~~~~~

For example, we can check that in fact, the combinator only produces
ordered lists

~~~~~
ghci> quickCheckN 1000 $ forAll genOrdList isOrdered 
+++ OK, passed 1000 tests.
~~~~~

and now, we can properly test the `insert` property

> prop_insert :: Int -> Property 
> prop_insert x = forAll genOrdList $ \xs -> isOrdered xs && isOrdered (insert x xs)

~~~~~
ghci> quickCheckN 1000 prop_insert 
+++ OK, passed 1000 tests.
~~~~~

Case Study : Checking Compiler Optimizations
============================================

Next, lets look at how QC can be used to generate structured data, 
by doing a small case-study on checking a compiler optimization. 

Recall the small *While* language that you wrote an evaluator for 
in [HW2](/homeworks/hw2.html). 

While: Syntax
-------------

The languages had arithmetic expressions 

> data Expression 
>   = Var   Variable
>   | Val   Value
>   | Plus  Expression Expression
>   | Minus Expression Expression
>   deriving (Eq, Ord)

where the atomic expressions were either variables or values

> data Variable = V String deriving (Eq, Ord)
>
> data Value 
>   = IntVal Int
>   | BoolVal Bool
>   deriving (Eq, Ord)

We used the expressions to define imperative *statements* which are either
assignments, if-then-else, a sequence of two statements, or a while-loop.

> data Statement
>   = Assign   Variable   Expression
>   | If       Expression Statement  Statement
>   | While    Expression Statement
>   | Sequence Statement  Statement
>   | Skip
>   deriving (Eq, Ord)


While: Semantics
----------------

The behavior of *While* programs was given using a *state* which is simply
a map from variables to values. Intuitively, a statement will *update* the
state by modifying the values of the variables appropriately.

> type WState = M.Map Variable Value

Your assignment was to (use the `State` monad to) write an evaluator 
(aka interpreter) for the language that took as input a program and 
a starting state and returned the final state.

> execute ::  WState -> Statement -> WState
> execute env = flip execState env . evalS

Since you wrote the code for the HW (you **DID** didn't you?) we won't go
into the details now -- scroll down to the bottom to see how `evalS` is
implemented.

Generating While Programs
-------------------------

We could painstakingly write manual test cases, but instead lets write some
simple generators for *While* programs, so that we can then check
interesting properties of the programs and the evaluator.

First, lets write a generator for variables.

> instance Arbitrary Variable where 
>   arbitrary = do x <- elements ['A'..'Z']
>                  return $ V [x]

thus, we assume that the programs are over variables drawn from the
uppercase alphabet characters. That is, our test programs range over 26
variables (you can change the above if you like.)

Second, we can write a generator for constant values (that can appear in 
expressions). Our generator simply chooses between randomly generated 
`Bool` and `Int` values.

> instance Arbitrary Value where 
>   arbitrary = oneof [ IntVal  <$> arbitrary
>                     , BoolVal <$> arbitrary ]


Third, we define a generator for `Expression` and `Statement` which 
selects from the different cases.

> -- instance Arbitrary Expression where
> --   arbitrary = sized arbnE
>
> -- arbE = frequency [ (1, Var   <$> arbitrary)
> --                  , (1, Val   <$> arbitrary)
> --                  , (5, Plus  <$> arbitrary <*> arbitrary)
> --                  , (5, Minus <$> arbitrary <*> arbitrary) ]

Finally, we need to write a generator for `WState` so that we can run 
the *While* program from some arbitrary input configuration. 

> instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (M.Map a b) where
>   arbitrary = M.fromList <$> arbitrary

In the above, `xvs` is a randomly generated list of key-value tuples,
which is turned into a `Map` by the `fromList` function.

Program Equivalence
-------------------

Let `p1` and `p2` be two *While* programs. We say that `p1` 
is *equivalent to* `p2` if for all input configurations `st` the
configuration resulting from executing `p1` from `st` is the same 
as that obtained by executing `p2` from `st`. Formally,

> (===) ::  Statement -> Statement -> Property
> p1 === p2 = forAll arbitrary $ \st -> execute st p1 == execute st p2 

Checking An Optimization: Zero-Add-Elimination
----------------------------------------------

Excellent! Lets take our generators our for a spin, by checking some
*compiler optimizations*. Intuitively, a compiler optimization (or 
transformation) can be viewed as a *pair* of programs -- the input 
program `p_in` and a transformed program `p_out`. A transformation 
`(p_in, p_out)`is *correct* iff `p_in` is equivalent to `p_out`.

Here's are some simple *sanity* check properties that correspond to
optimizations. 

> prop_add_zero_elim :: Variable -> Expression -> Property
> prop_add_zero_elim x e = 
>   (x `Assign` (e `Plus` Val (IntVal 0))) === (x `Assign` e) 
>
> prop_sub_zero_elim :: Variable -> Expression -> Property
> prop_sub_zero_elim x e =
>   (x `Assign` (e `Minus` Val (IntVal 0))) === (x `Assign` e) 

Lets check the properties!

~~~~~{.haskell}
ghci> quickCheck prop_add_zero_elim 
(0 tests)
...
~~~~~

Uh? whats going on? Well, lets look at the generator for expressions.

~~~~~{.haskell}
arbE = frequency [ (1, liftM Var arbitrary)
                 , (1, liftM Val arbitrary)
                 , (5, liftM2 Plus  arbitrary arbitrary)
                 , (5, liftM2 Minus arbitrary arbitrary) ]
~~~~~

in effect, its will generate infinite expressions with high probability!
(do the math!) So we need some way to control the size, either by biasing 
the `Var` and `Val` constructors (which terminate the generation) or by
looking at the *size* of the structure during generation. We can do this
with the combinator

~~~~~{.haskell}
sized :: (Int -> Gen a) -> Gen a
~~~~~

which lets us write functions that parameterize the generator with an
integer (and then turn that into a flat generator.)

> arbnE :: Int -> Gen Expression
> arbnE 0 = oneof     [ liftM Var arbitrary
>                     , liftM Val arbitrary ]
> arbnE n = frequency [ (1, liftM Var arbitrary)
>                     , (1, liftM Val arbitrary)
>                     , (5, liftM2 Plus  (arbnE n_by_2) (arbnE n_by_2))
>                     , (5, liftM2 Minus (arbnE n_by_2) (arbnE n_by_2)) ]
>   where n_by_2 = n `div` 2

In the above, we keep *halving* the number of allowed nodes, and when that
number goes to `0` we just return an atomic expression (either a variable
or a constant.) We can now update the generator for expressions to

> -- instance Arbitrary Expression where
> --   arbitrary = sized arbnE

And now, lets check the property again

~~~~~{.haskell}
ghci> quickCheck prop_add_zero_elim 
*** Failed! Falsifiable (after 10 tests):  
W
True
fromList [(R,False)]
~~~~~

whoops! Forgot about those pesky boolean expressions! If you think about it,

~~~~~{.haskell}
X := True + 0
~~~~~

will assign `0` to the variable while

~~~~~{.haskell}
X := True 
~~~~~

will assign `True` to the variable! Urgh. Ok, lets limit ourselves to *Integer* 
expressions

> intE :: Gen Expression
> intE = sized arbnEI 
>   where arbnEI 0 = oneof [ liftM Var arbitrary
>                          , liftM (Val . IntVal) arbitrary ]
>         arbnEI n = oneof [ liftM Var arbitrary
>                          , liftM (Val . IntVal) arbitrary
>                          , liftM2 Plus  (arbnEI n_by_2) (arbnEI n_by_2)
>                          , liftM2 Minus (arbnEI n_by_2) (arbnEI n_by_2) ]
>                    where n_by_2 = n `div` 2

using which, we can tweak the property to limit ourselves to integer
expressions

> prop_add_zero_elim'   :: Variable -> Property
> prop_add_zero_elim' x = 
>   forAll intE $ \e -> (x `Assign` (e `Plus` Val (IntVal 0))) === (x `Assign` e)


O, Quickcheck, what say you now?

~~~~~{.haskell}
ghci> quickCheck prop_add_zero_elim'
*** Failed! Falsifiable (after 16 tests):  
V
N
fromList [(A,-36),(B,True),(D,False),(E,44),(L,-32),(M,22),(N,True),(O,False),(Q,True),(S,True),(W,50)]
~~~~~

Of course! in the input state where `N` has the value `True`, the result of
executing `V := N` is quite different from executing `V := N + 0`. Oh well,
so much for that optimization, I guess we need some type information before
we can eliminate the additions-to-zero!


Checking An Optimization: Constant Folding (sort of) 
----------------------------------------------------

Well, that first one ran aground because *While* was untyped (tsk tsk.)
and so adding a zero can cause problems if the expression is a boolean. 
Lets look at another optimization that is not plagued by the
int-v-bool conflict. Suppose you have two back-to-back 
assignments

~~~~~{.haskell}
X := E
Y := E
~~~~~

It is silly to recompute `E` twice, since the result is already stored in
`X`. So, we should be able to optimize the above code to 

~~~~~{.haskell}
X := E
Y := X
~~~~~

Lets see how we might express the correctness of this transformation 
as a QC property

> prop_const_prop :: Variable -> Variable -> Expression -> Property
> prop_const_prop x y e = 
>   ((x `Assign` e) `Sequence` (y `Assign` e))
>   ===
>   ((x `Assign` e) `Sequence` (y `Assign` Var x))

Mighty QC, do you agree ?

~~~~~{.haskell}
*Main> quickCheck prop_const_prop 
*** Failed! Falsifiable (after 35 tests):  
Z
P
O + A + J + G + C + False + O + K + 6965 + True + True + W + T + K + 5266 + J + Z + R + -1588 + P + R + 3667 + B + Q + T + Y + Z + X + M + False + -1191 + 6124 + H + B + 1351 + S + T + E + R + 6969
fromList [(B,True),(D,3714),(E,True),(P,2455),(Q,True),(Y,-7341)]
~~~~~

Shrinking 
---------

Holy transfer function!! It fails?!! And what is that bizarre test? It
seems rather difficult to follow. Turns out, QC comes with a *test
shrinking* mechanism; all we need do is add to the `Arbitrary` instance
a function of type

~~~~~{.haskell}
shrink :: a -> [a]
~~~~~

which will take a candidate and generate a list of *smaller* candidates
that QC will systematically crunch through till it finds a minimally
failing test!

> instance Arbitrary Expression where
>   arbitrary = sized arbnE
>
>   shrink (Plus e1 e2)  = [e1, e2]
>   shrink (Minus e1 e2) = [e1, e2]
>   shrink _             = []

Lets try it again to see if we can figure it out!

~~~~~{.haskell}
ghci> quickCheck prop_const_prop 
*** Failed! Falsifiable (after 26 tests and 4 shrinks):    
D
U
A + D
fromList [(D,-638),(G,256),(H,False),(K,False),(O,True),(R,True),(S,-81),(T,926)]
~~~~~

Aha! Consider the two programs

~~~~~{.haskell}
D := A + D; 
U := A + D
~~~~~

and 

~~~~~{.haskell}
D := A + D; 
U := D
~~~~~

are they equivalent? Pretty subtle, eh. 


Well, I hope I've convinced you that QuickCheck is pretty awesome. 
The astonishing thing about it is its sheer simplicity -- a few 
fistfuls of typeclasses and a tiny pinch of monads and lo! a 
shockingly useful testing technique that can find a bunch of 
subtle bugs or inconsistencies in your code. 

Moral of the story -- types can go a long way towards making 
your code *obviously correct*, but not the whole distance. 
Make up the difference by writing properties, and have the 
machine crank out thousands of tests for you!

There is a lot of literature on QuickCheck on the web. 
It is used for a variety of commercial applications, 
both in Haskell and in pretty much every modern language, 
including [Perl][10]. 
Even if you don't implement a system in Haskell, you 
can use QuickCheck to test it, by just using the nifty 
[data generation][9] facilities. 


Appendix: Helper Code
=====================


Evaluator Code 
--------------

We don't have exceptions, so if a variable is not found, return value 0

> evalE :: Expression -> State WState Value
> evalE (Var x)       = get >>= return . M.findWithDefault (IntVal 0) x
> evalE (Val v)       = return v
> evalE (Plus e1 e2)  = return (intOp (+) 0 IntVal) `ap` evalE e1 `ap` evalE e2
> evalE (Minus e1 e2) = return (intOp (-) 0 IntVal) `ap` evalE e1 `ap` evalE e2
>
> evalS :: Statement -> State WState ()
> evalS w@(While e s)    = evalS (If e (Sequence s w) Skip)
> evalS Skip             = return ()
> evalS (Sequence s1 s2) = evalS s1 >> evalS s2
> evalS (Assign x e )    = do v <- evalE e
>                             m <- get
>                             put $ M.insert x v m
>                             return ()
> evalS (If e s1 s2)     = do v <- evalE e
>                             case v of 
>                               BoolVal True  -> evalS s1
>                               BoolVal False -> evalS s2
>                               _             -> return ()

Return `0` for arithmetic operations over a `Bool` value.

> intOp :: (Int -> Int -> a) -> a -> (a -> Value) -> Value -> Value -> Value
> intOp op _ c (IntVal x) (IntVal y) = c $ x `op` y
> intOp _  d c _          _          = c d 
> 


Pretty Printing Code
--------------------

> blank   :: Int -> String 
> blank n = replicate n ' '
> 
> instance Show Variable where
>   show (V x) = x
> 
> instance Show Value where
>   show (IntVal  i) = show i
>   show (BoolVal b) = show b
> 
> instance Show Expression where
>   show (Var v)       = show v
>   show (Val v)       = show v
>   show (Plus e1 e2)  = show e1 ++ " + " ++ show e2
>   show (Minus e1 e2) = show e1 ++ " + " ++ show e2
> 
> instance Show Statement where
>   show = showi 0
>
> showi :: Int -> Statement -> String 
> showi n (Skip)       = blank n ++ "skip"
> showi n (Assign x e) = blank n ++ show x ++ " := " ++ show e
> showi n (If e s1 s2) = blank n ++ "if " ++ show e ++ " then\n" ++ 
>                        showi (n+2) s1 ++
>                        blank n ++ "else\n" ++
> 					     showi (n+2) s2 ++
> 					     blank n ++ "endif"
> 
> showi n (While e s)  = blank n ++ "while " ++ show e ++ " do\n" ++ 
>                        showi (n+2) s
> showi n (Sequence s1 s2) = showi n s1 ++ "\n" ++ showi n s2 

Generating Statements
---------------------

> instance Arbitrary Statement where
>   arbitrary = oneof [ liftM2 Assign   arbitrary arbitrary
>                     , liftM3 If       arbitrary arbitrary arbitrary
>                     , liftM2 While    arbitrary arbitrary
>	              , liftM2 Sequence arbitrary arbitrary
>                     , return Skip ]

[0]: http://www.cse.chalmers.se/~koen/
[1]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[2]: http://www.cs.york.ac.uk/fp/smallcheck/
[3]: http://video.google.com/videoplay?docid=4655369445141008672#
[4]: http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf
[5]: http://en.wikipedia.org/wiki/Insertion_sort
[6]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/src/Test-QuickCheck-Gen.html#Gen
[7]: http://book.realworldhaskell.org/read/monads.html
[8]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
[9]: http://www.haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator
[10]: http://community.moertel.com/~thor/talks/pgh-pm-talk-lectrotest.pdf
[11]: http://www.cse.chalmers.se/~rjmh
