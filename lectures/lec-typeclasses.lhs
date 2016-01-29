---
title: Typeclasses
---

<div class="hidden">
\begin{code}
{-# LANGUAGE OverlappingInstances, IncoherentInstances, FlexibleInstances, TypeSynonymInstances #-}
import Control.Arrow
\end{code}
</div>

We have already seen that the `+` operator works for a bunch of different
underlying data types. For example


~~~~~{.haskell}
ghci> 2 + 3
5
ghci> :type it
it :: Integer

ghci> 2.9 + 3.5
6.4
ghci> :type it
it :: Double
~~~~~

Similarly we can compare all sorts of values

~~~~~{.haskell}
ghci> 2 == 3
False

ghci> [2.9, 3.5] == [2.9, 3.5]
True
~~~~~

"So?", I can *hear* you shrug.

Indeed, this is quite unremarkable, since languages since the dawn of time
has supported some form of operator "overloading" to support this kind of
**ad--hoc polymorphism**.

However, in Haskell, there is no caste system. There is no distinction
between operators and functions. All are first class citizens in Haskell.


Well then, what type do we give to *functions* like `+` and `==` ? Something like

~~~~~{.haskell}
(+) :: Integer -> Integer -> Integer
~~~~~

would be too anemic, since we want to add two doubles as well! Can type
variables help?

~~~~~{.haskell}
(+) :: a -> a -> a
~~~~~

Nope. Thats a bit too aggressive, since it doesn't make sense, to add two
functions to each other! Haskell solves this problem with an *insanely slick*
mechanism called typeclasses, introduced by [Wadler and Blott][1].

**BTW:** The paper is one of the best examples of academic writing I have seen.
The next time you hear a curmudgeon say all the best CS was done in the 60s,
just point them to the above.

Qualified Types
===============

To see the right type, lets just (politely, always) ask

~~~~~{.haskell}
ghci> :type (+)
(+) :: (Num a) => a -> a -> a
~~~~~

We call the above a *qualified type*. Read it as, `+` takes in two `a`
values and returns an `a` value for any type `a` that *is a `Num`*
or *is an instance of `Num`*.

The name `Num` can be thought of as a *predicate* over types.
Some types *satisfy* the `Num` predicate. Examples include
`Integer`, `Double` etc, and any values of those types can
be passed to `+`. Other types *do not* satisfy the predicate.
Examples include `Char`, `String`, functions etc, and so values
of those types cannot be passed to `+`.

~~~~~{.haskell}
ghci> 'a' + 'b'

<interactive>:1:0:
    No instance for (Num Char)
      arising from a use of `+' at <interactive>:1:0-8
    Possible fix: add an instance declaration for (Num Char)
    In the expression: 'a' + 'b'
    In the definition of `it': it = 'a' + 'b'
~~~~~

As promised, now these kinds of error messages should make sense. Basically
Haskell is complaining that `a` and `b` are of type `Char` which is *not*
an instance of `Num`.

OK, so what is a Typeclass?
---------------------------

In a nutshell, a typeclass is a collection of operations (functions)
that must exist for the underlying type. For example, lets look at
possibly the simplest typeclass `Eq`

~~~~~{.haskell}
class  Eq a  where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
~~~~~

That is, a type `a` can be an instance of `Eq` as long as there are two
functions that determine if two `a` values are respectively equal or
disequal. Similarly, the typeclass `Show` captures the requirements
that make a particular datatype be viewable,

~~~~~{.haskell}
class  Show a  where
  show :: a -> String
~~~~~

Indeed, we can test this on different (built-in) types

~~~~~{.haskell}
ghci> show 2
"2"

ghci> show 3.14
"3.14"

ghci> show (1, "two", ([],[],[]))
"(1,\"two\",([],[],[]))"
~~~~~

When we type an expression into ghci, it computes the value
and then calls `show` on the result. Thus, if we create a
*new* type by

\begin{code}
data Unshowable = A | B | C
\end{code}

then we can create values of the type,

~~~~~{.haskell}
ghci> let x = A
ghci> :type x
x :: Unshowable
~~~~~

but can't view or compare them

~~~~~{.haskell}
ghci> x

<interactive>:1:0:
    No instance for (Show Unshowable)
      arising from a use of `print' at <interactive>:1:0
    Possible fix: add an instance declaration for (Show Unshowable)
    In a stmt of a 'do' expression: print it

ghci> x == x

<interactive>:1:0:
    No instance for (Eq Unshowable)
      arising from a use of `==' at <interactive>:1:0-5
    Possible fix: add an instance declaration for (Eq Unshowable)
    In the expression: x == x
    In the definition of `it': it = x == x
~~~~~

Again, the previously incomprehensible type error message should
make sense to you.

**EXERCISE** Lets *create* an `instance` for `Show Unshowable`

Automatic Derivation
--------------------

Of course, this is lame; we *should* be able to compare and view them.
To allow this, Haskell allows us *automatically derive* functions for
certain key type classes, namely those in the standard library.

To do so, we simply dress up the data type definition with

\begin{code}
data Showable = A' | B' | C' deriving (Eq, Show)
\end{code}

and now we have

~~~~~{.haskell}
ghci> let x' = A'

ghci> :type x'
x' :: Showable

ghci> x'
A'

ghci> x' == x'
True
~~~~~

Standard Typeclass Hierarchy
----------------------------

Let us now peruse the definition of the `Num` typeclass.

~~~~~{.haskell}
ghci> :info Num
class (Eq a, Show a) => Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
~~~~~

There's quite a bit going on there. A type `a` can only be deemed an
instance of `Num` if

1. The type is *also* an instance of `Eq` and `Show`, and
2. There are functions for adding, multiplying, subtracting, negating
   etc values of that type.

In other words in addition to the "arithmetic" operations, we can
compare two `Num` values and we can view them (as a `String`.)

Haskell comes equipped with a rich set of built-in classes.

![Standard Typeclass Hierarchy](/static/classes.gif)

In the above picture, there is an edge from `Eq` and `Show` to `Num`
because for something to be a `Num` it must also be an `Eq` and `Show`.
There are a few other ones that we will come to know (and love!) in due
course...


Using Typeclasses
=================

Lets now see how slickly typeclasses integrate with the rest of Haskell's
type system by building a small library for *Maps* (aka associative arrays,
lookup tables etc.)

\begin{code}
data BST k v = Empty
             | Node k v (BST k v) (BST k v)
\end{code}

Did you get that?

Quiz
----

What is the type of:

\begin{code}
zoo Empty            = []
zoo (Node key _ l r) = zoo l ++ [key] ++ zoo r
\end{code}

a. `BST k v -> k`
b. `BST k v -> [k]`
c. `BST k v -> [(k, v)]`
d. `BST k v -> [v]`
e. `BST k v -> v`


~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~


Exercise
--------

Fill in the definition of:

\begin{code}
foldBST op base Empty          = base
foldBST op base (Node k v l r) = op k v ll rr
  where
   ll                          = foldBST op base l
   rr                          = foldBST op base r
\end{code}

so that the following functions behave as the names suggest!

\begin{code}

safeDiv n 0 = Nothing
safeDiv n m = Just (n `div` m)


keysOfTree'  = foldBST (\k _ lr rr -> lr ++ [k] ++ rr) []
valsOfTree'  = foldBST (\_ v lr rr -> lr ++ [v] ++ rr) []
totalOfTree' = foldBST (\_ v lr rr -> lr + v + rr)     0
\end{code}


Binary Search Ordering
----------------------

We will call this type `BST` to abbreviate [Binary Search Tree][2] which
are trees where keys are ordered such that at each node, the keys appearing
in the *left* and *right* subtrees are respectively *smaller* and *larger*
than than the key at the node. For example, this is what a tree that maps
the strings `"burrito"`, `"chimichanga"` and `"frijoles"` to their prices
might look like

![BST example](/static/lec5_bst.png)

The organization of the BST allows us to efficiently search the tree
for a key.

\begin{code}
find k (Node k' v' l r)
  | k == k'    = Just v'
  | k <  k'    = find k l
  | otherwise  = find k r
find k Empty = Nothing
\end{code}

We must ensure that the invariant is preserved by the `insert` function.
In the functional setting, the `insert` will return a brand new tree.

Lets fill in the blanks to develop a function that **adds** a new key-value
binding to the tree:

~~~~~{.haskell}
insert :: k -> v -> BST k v -> BST k v
~~~~~

Quiz
----

~~~~~{.haskell}
insert k v Empty = undefined
~~~~~

What shall we fill in for `undefined`?

a. `Empty`
b. `Node k v`
c. `v`
d. `Node k v Empty Empty`
e. `(k, v)`

~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~


Quiz
----

Ok, now lets move to the more interesting case:

\begin{code}
insert k v (Node k' v' l r)
  | k == k'      = undefined
\end{code}

What shall we fill in for `undefined`?

a. `Empty`
b. `Node k v  l     r`
c. `Node k v' l     r`
d. `Node k v  Empty Empty`
e. `Node k v' Empty Empty`

~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~



Quiz
----

And finally,

\begin{code}
insert k' v' (Node k v l r)
  | k' < k  = undefined
\end{code}

What shall we fill in for `undefined`?

a. `Empty                `
b. `insert k' v' l       `
c. `insert k' v' r       `
d. `Node k v (insert k' v' l) r`
e. `Node k v l (insert k' v' r)`

insert "peanutSauce" 0.75 menu

===

~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~


All in one place:

~~~~~{.haskell}
insert k v Empty = Node k v Empty Empty
insert k v (Node k' v' l r)
  | k == k'      = Node k v l r
  | k <  k'      = Node k' v' (insert k v l) r
  | otherwise    = Node k' v' l (insert k v r)
~~~~~

The BST ordering obviates the need for any backtracking. If additionally
if the tree is kept *balanced* we ensure very efficient searching.

Now, we can create a particular lookup table like so

\begin{code}
t0 = insert "burrito"     4.50 Empty
t1 = insert "chimichanga" 5.25 t0
t2 = insert "frijoles"    2.75 t1
\end{code}

**NOTE:** Each `insert` returns a brand new `BST`, this is not Java!

Of course this is a bit tedious, so it may be easier to write an `ofList`
function that will turn an association list into an appropriate `BST`.

\begin{code}
ofList = foldl (\t (k, v) -> insert k v t) Empty
\end{code}

Now, we can just do

\begin{code}
t = ofList [ ("chimichanga", 5.25)
           , ("burrito"    , 4.50)
           , ("frijoles"   , 2.75)]
\end{code}

After which we can query the table

~~~~~{.haskell}
ghci> :type t
t :: BST [Char] Double

ghci> find "burrito" t
Just 4.5

ghci> find "birria" t
Nothing
~~~~~

Similarly, it makes sense to implement a `toList` which will convert the
map into an association list, we can reuse `foldBST` from before:

\begin{code}
toList =  foldBST (\k v l r -> l ++ [(k, v)] ++ r) []
\end{code}

Quiz
----

Recall that

~~~~~{.haskell}
t = ofList [ ("chimichanga", 5.25)
           , ("burrito"    , 4.50)
           , ("frijoles"   , 2.75)]
~~~~~

What does `toList t` return?

a. `[("burrito", 4.50)    , ("chimichanga", 5.25) , ("frijoles", 2.75)]`
b. `[("chimichanga", 5.25), ("burrito", 4.50)     , ("frijoles", 2.75)]`
c. `[("frijoles", 2.75)   , ("burrito", 4.50)     , ("chimichanga", 5.25)]`
d. `[]`
e. none of the above.

~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~


Constraint Propagation
----------------------

Notice that we didn't write down the types of any of the functions.
Lets see what the types are

~~~~~{.haskell}
ghci> :type insert
insert :: (Ord a) => a -> v -> BST a v -> BST a v

ghci> :type find
find :: (Ord a) => a -> BST a t -> Maybe t

ghci> :type ofList
insert :: (Ord a) => a -> v -> BST a v -> BST a v
~~~~~

Whoa! Look at that, Haskell tells us that we can use any `a` value as a *key*
as long as the value is an instance of the `Ord` typeclass. You might guess
from the name, that a type is an instance of `Ord` if there are functions
that allow us to compare values of the type. In particular

~~~~~{.haskell}
ghci> :info Ord
class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

ghci>  :info Ordering
data Ordering = LT | EQ | GT 	-- Defined in GHC.Ordering
~~~~~

How, did the engine figure this out? Easy enough, if you look at the body
of the `insert` and `find` functions, you'll see that we compare two key
values.

Exercise
--------
Write a `delete` function of type

~~~~~{.haskell}
delete :: (Ord k) => k -> BST k v -> BST k v
~~~~~

Explicit Signatures
-------------------

While Haskell is pretty good about inferring types in general, there are
cases when the use of type classes requires explicit annotations (which
change the behavior of the code.)

For example, `Read` is a built-in typeclass, where any instance `a` of
`Read` has a function

~~~~~{.haskell}
read :: (Read a) => String -> a
~~~~~

which can parse a string and turn it into an `a`. Thus, `Read` is, in a
sense, the opposite of `Show`.

Quiz
----

What does the expression `read "2"` evaluate to?

a. compile time error
b. `"2" :: String`
c. `2   :: Integer`
d. `2.0 :: Double`
e. run-time exception


~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~





Haskell is foxed, because it doesn't know what to convert the string to!
Did we want an `Int` or a `Double` ? Or maybe something else altogether.
Thus, we get back the complaint

~~~~~{.haskell}
interactive>:1:0:
    Ambiguous type variable `a' in the constraint:
      `Read a' arising from a use of `read' at <interactive>:1:0-9
    Probable fix: add a type signature that fixes these type variable(s)
~~~~~

which clearly states what the issue is. Thus, here an explicit type
annotation is needed to tell it what to convert the string to. Thus,
if we play nice and add the types we get

~~~~~{.haskell}
ghci> (read "2") :: Int
2

ghci> (read "2") :: Float
2.0
~~~~~

Note the different results due to the different types.


Instantiating Typeclasses
=========================

So far we have seen Haskell's nifty support for overloading
by observing that

1. some standard types are instances of standard type classes, and
2. new types can be automatically made instances of standard type classes.

However, in many situations the automatic instantiation doesn't quite cut
it, and instead we need to (and get to!) create our own instances.

For example, you might have noticed that we didn't bother with adding `Eq`
to the deriving clause for our `BST` type. Thus, we can't compare two
`BST`s for equality (!)

~~~~~{.haskell}
*Main> Empty == Empty

<interactive>:1:0:
    No instance for (Eq (BST k v))
      arising from a use of `==' at <interactive>:1:0-13
    Possible fix: add an instance declaration for (Eq (BST k v))
    In the expression: Empty == Empty
    In the definition of `it': it = Empty == Empty
~~~~~

Suppose we had added

~~~~~{.haskell}
data BST k v = Empty
             | Node k v (BST k v) (BST k v)
             deriving (Eq, Show)
~~~~~

Now, we *can* compare two `BST` values

~~~~~{.haskell}
ghci> Empty == Empty
True
~~~~~

Quiz
----

Recall that

~~~~~{.haskell}
t = ofList [ ("chimichanga", 5.25)
           , ("burrito"    , 4.50)
           , ("frijoles"   , 2.75)]
~~~~~

What does

~~~~~{.haskell}
ghci> t == ofList (toList t)
~~~~~

evaluate to?

a. `True`
b. `False`
c. Compile-time error (`No instance of Eq...`)
d. Other type error
e. Run time exception

~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~


The equality test is rather too *structural*, as in,
are the two trees *exactly* the same, rather than what
we might want, which is, are the two underlying *maps*
exactly the same. Consequently we get

~~~~~{.haskell}
ghci> t == ofList (toList t)
False
~~~~~

Ugh! Why did that happen? Well, lets see

~~~~~{.haskell}
ghci> t
Node "chimichanga" 5.25 (Node "burrito" 4.5 Empty Empty) (Node "frijoles" 2.75 Empty Empty)

ghci> ofList (toList t)
Node "burrito" 4.5 Empty (Node "chimichanga" 5.25 Empty (Node "frijoles" 2.75 Empty Empty))
~~~~~

The trees are different because they contain the keys in different
(*valid!*) orders. To get around this, we can explicitly make `BST` an
instance of the `Eq` typeclass, by implementing the relevant functions for
the typeclass.

To undertand how, let us look at the full definition of the `Eq` typeclass.
Ah! the typeclass definition also provides *default implementations* of each
operation (in terms of the other operation.) Thus, all we need to do is
define `==` and we will get `/=` (not-equals) for free!

~~~~~{.haskell}
class Eq a  where
    (==)           :: a -> a -> Bool
    (/=)           :: a -> a -> Bool

    {- Default Implementations -}

    x == y         = not (x /= y)
    x /= y         = not (x == y)
~~~~~

Quiz
----

Thus, to define our own equality (and disequality) procedures
that are *robust* to ordering we might write:

\begin{code}
instance Eq (BST k v) where
  t1 == t2 = toList t1 == toList t2
\end{code}

Does it work?

a. *Yes*
b. No, because the orders may differ.
c. No, because it does not compile.
d. No, because it is too slow.
e. None of the above.

~~~~~{.haskell}
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
.
~~~~~

Well we can only compare two values of type `[(k, v)]` ...

- if we can compare two values of `(k, v)` ...
- if we can compare the two `k` and the two `v`.

Hence, we fix the above definition:

The above instance declaration states that

- **if** `k` and `v` are instances of `Eq` (i.e. can be compared for equality),
- **then** `BST k v` can be compared for equality, via the given procedure.


Thus, once we have supplied the above we get

~~~~~{.haskell}
ghci> t == ofList (toList t)
True
~~~~~

In general, when instantiating a typeclass, Haskell will check that we have
provided a *minimal implementation* containing enough functions from which
the remaining functions can be obtained (via their default implementations.)

~~~~~{.haskell}
ghci> t /= Empty
True
~~~~~

Laws
----

In addition to the explicit type requirements, a typeclass also encodes a
set of *laws* that describe the relationships between the different operations.
For example, the intention of the `Eq` typeclass is that the supplied
implementations of `==` and `/=` satisfy the law

~~~~~{.haskell}
forall t1 t2, t1 == t2 <==> not t1 /= t2
~~~~~

Unfortunately, there is no way for Haskell to *verify* that your implementations satisfy
the laws, so this is something to be extra careful about, when using typeclasses.


\begin{code}
class JEQ a where
  equals    :: a -> a -> Bool
  notEq     :: a -> a -> Bool
  notEq x y = not (equals x y)
\end{code}

Creating Typeclasses
====================

It turns out that typeclasses are useful for *many* different things. We
will see some of those over the next few lectures, but let us conclude
today's class with a quick example that provides a (very) small taste of
their capabilities.


JSON
----

*JavaScript Object Notation* or [JSON][3] is a simple format for
transferring data around. Here is an example:


~~~~~{.javascript}
{ "name"    : "Ranjit"
, "age"     : 38
, "likes"   : ["guacamole", "coffee", "bacon"]
, "hates"   : [ "waiting" , "grapefruit"]
, "lunches" : [ {"day" : "monday",    "loc" : "zanzibar"}
              , {"day" : "tuesday",   "loc" : "farmers market"}
              , {"day" : "wednesday", "loc" : "harekrishna"}
              , {"day" : "thursday",  "loc" : "faculty club"}
              , {"day" : "friday",    "loc" : "coffee cart"} ]
}
~~~~~

In brief, each JSON object is either
- a *base* value like a string, a number or a boolean,
- an (ordered) *array* of objects, or
- a set of *string-object* pairs.

Thus, we can encode (a subset of) JSON values with the datatype

\begin{code}
data JVal = JStr String
          | JNum Double
          | JBln Bool
          | JObj [(String, JVal)]
          | JArr [JVal]
          deriving (Eq, Ord, Show)
\end{code}

Thus, the above JSON value would be represented by the `JVal`

\begin{code}
js1 =
  JObj [("name", JStr "Ranjit")
       ,("age",  JNum 33)
       ,("likes",   JArr [ JStr "guacamole", JStr "coffee", JStr "bacon"])
       ,("hates",   JArr [ JStr "waiting"  , JStr "grapefruit"])
       ,("lunches", JArr [ JObj [("day",  JStr "monday")
                                ,("loc",  JStr "zanzibar")]
                         , JObj [("day",  JStr "tuesday")
                                ,("loc",  JStr "farmers market")]
                         , JObj [("day",  JStr "wednesday")
                                ,("loc",  JStr "hare krishna")]
                         , JObj [("day",  JStr "thursday")
                                ,("loc",  JStr "faculty club")]
                         , JObj [("day",  JStr "friday")
                                ,("loc",  JStr "coffee cart")]
                         ])
       ]
\end{code}

Serializing Haskell Values to JSON
----------------------------------

Next, suppose that we want to write a small library to
serialize Haskell values as JSON. We could write a bunch
of functions like

\begin{code}
doubleToJSON :: Double -> JVal
doubleToJSON = JNum
\end{code}

similarly, we have

\begin{code}
stringToJSON :: String -> JVal
stringToJSON = JStr

boolToJSON   :: Bool -> JVal
boolToJSON   = JBln
\end{code}

But what about collections, namely objects and arrays? We might try

\begin{code}
doublesToJSON    :: [Double] -> JVal
doublesToJSON xs = JArr (map doubleToJSON xs)

boolsToJSON      :: [Bool] -> JVal
boolsToJSON xs   = JArr (map boolToJSON xs)

stringsToJSON    :: [String] -> JVal
stringsToJSON xs = JArr (map stringToJSON xs)
\end{code}

which of course, you could abstract by making the
*individual-element-converter* a parameter

\begin{code}
xsToJSON :: (a -> JVal) -> [a] -> JVal
xsToJSON f xs = JArr (map f xs)

xysToJSON :: (a -> JVal) -> [(String, a)] -> JVal
xysToJSON f kvs = JObj [ (k, f v) | (k, v) <- kvs ]
\end{code}


but still, this is getting rather tedious, since we have to redefine
versions for each Haskell type, and instantiate them by hand for each
conversion

~~~~~{.haskell}
ghci> doubleToJSON 4
JNum 4.0

ghci> xsToJSON stringToJSON ["coffee", "guacamole", "bacon"]
JArr [JStr "coffee",JStr "guacamole",JStr "bacon"]

ghci> xysToJSON stringToJSON [("day", "monday"), ("loc", "zanzibar")]
JObj [("day",JStr "monday"),("loc",JStr "zanzibar")]
~~~~~

and this gets more hideous when you have richer objects like

\begin{code}
lunches = [ [("day", "monday"),    ("loc", "zanzibar")]
          , [("day", "tuesday"),   ("loc", "farmers market")]
          , [("day", "wednesday"), ("loc", "hare krishna")]
          , [("day", "thursday"),  ("loc", "faculty club")]
          , [("day", "friday"),    ("loc", "coffee cart")]
          ]
\end{code}

because we have to go through gymnastics like

~~~~~{.haskell}
ghci> xsToJSON (xysToJSON stringToJSON) lunches
JArr [JObj [("day",JStr "monday"),("loc",JStr "zanzibar")],JObj [("day",JStr "tuesday"),("loc",JStr "farmers market")]]
~~~~~

*Ugh!* So much for *readability*. Isn't there a better way? Is it too much
to ask for a magical `toJSON` that *just works?*


Typeclasses To The Rescue!
--------------------------

Of course there is a better way, and the the route is paved by typeclasses!

Lets define a typeclass that describes any type that can be converted to
JSON.

\begin{code}
class JSON a where
  toJSON :: a -> JVal
\end{code}

Easy enough. Now, we can make all the above instances of `JSON` like so

\begin{code}
instance JSON Double where
  toJSON = JNum

instance JSON Bool where
  toJSON = JBln

instance JSON String where
  toJSON = JStr
\end{code}

Now, we can just say

~~~~~{.haskell}
ghci> toJSON 4
JNum 4.0

ghci> toJSON True
JBln True

ghci> toJSON "guacamole"
JStr "guacamole"
~~~~~



Bootstrapping Instances
-----------------------

The real fun begins when we get Haskell to automatically
bootstrap the above functions to work for lists and
association lists!

\begin{code}
instance JSON a => JSON [a] where
  toJSON xs = JArr [toJSON x | x <- xs]
\end{code}

Whoa!

The above says, if `a` is an instance of `JSON`, that is,
if you can convert `a` to `JVal` then here's a generic
recipe to convert lists of `a` values!

~~~~~{.haskell}
ghci> toJSON [True, False, True]
JArr [JBln True,JBln False,JBln True]

ghci> toJSON ["cat", "dog", "Mouse"]
JArr [JStr "cat",JStr "dog",JStr "Mouse"]

ghci> toJSON [["cat", "dog"], ["mouse", "rabbit"]]
JArr [JArr [JStr "cat",JStr "dog"],JArr [JStr "mouse",JStr "rabbit"]]
~~~~~

Of course, we can pull the same trick with key-value lists

\begin{code}
instance (JSON a) => JSON [(String, a)] where
  toJSON = JObj . map (second toJSON)
\end{code}

after which, we are all set!

~~~~~{.haskell}
ghci> toJSON lunches
JArr [JObj [("day",JStr "monday"),("loc",JStr "zanzibar")],JObj [("day",JStr "tuesday"),("loc",JStr "farmers market")]]
~~~~~

It is also useful to bootstrap the serialization for tuples (upto some
fixed size) so we can easily write "non-uniform" JSON objects where keys
are bound to values with different shapes.

\begin{code}
instance (JSON a, JSON b) => JSON ((String, a), (String, b)) where
  toJSON ((k1, v1), (k2, v2)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2)]

instance (JSON a, JSON b, JSON c) => JSON ((String, a), (String, b), (String, c)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3)]

instance (JSON a, JSON b, JSON c, JSON d) => JSON ((String, a), (String, b), (String, c), (String,d)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3), (k4, v4)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3), (k4, toJSON v4)]

instance (JSON a, JSON b, JSON c, JSON d, JSON e) => JSON ((String, a), (String, b), (String, c), (String,d), (String, e)) where
  toJSON ((k1, v1), (k2, v2), (k3, v3), (k4, v4), (k5, v5)) =
    JObj [(k1, toJSON v1), (k2, toJSON v2), (k3, toJSON v3), (k4, toJSON v4), (k5, toJSON v5)]
\end{code}

Now, we can simply write

\begin{code}
hs = (("name"   , "Ranjit")
     ,("age"    , 33 :: Double)
     ,("likes"  , ["guacamole", "coffee", "bacon"])
     ,("hates"  , ["waiting", "grapefruit"])
     ,("lunches", lunches)
     )
\end{code}

which is a Haskell value that describes our running JSON example, and can
convert it directly like so

\begin{code}
js2 = toJSON hs
\end{code}

This value is exactly equal to the old "hand-serialized" JSON object `js1`.

~~~~~{.haskell}
ghci> js1 == js2
True
~~~~~

Exercise
--------

Why did we have to write a type annotation `33 :: Double` in the above
example? Can you figure out a way to remove it?


To wrap everything up, lets write a routine to serialize our `BST`
maps.

\begin{code}
instance (JSON v) => JSON (BST String v) where
  toJSON m = undefined
\end{code}

Now lets make up a complex Haskell value with an embedded `BST`.

\begin{code}
hs' = (("name"    , "el gordo taqueria")
      ,("address" , "213 Delicious Street")
      ,("menu"    , t))
\end{code}

and presto! our serializer *just works*

~~~~~{.haskell}
ghci> t
Node "chimichanga" 5.25
  (Node "burrito" 4.5 Empty Empty)
  (Node "frijoles" 2.75 Empty Empty)

ghci> :type hs'
hs' :: (([Char], [Char]),
        ([Char], [Char]),
        ([Char], BST [Char] Double))

ghci> toJSON hs'
JObj [("name", JStr "el gordo taqueria")
     ,("address", JStr "213 Delicious Street")
	 ,("menu", JObj [("burrito", JNum 4.5)
	                ,("chimichanga", JNum 5.25)
					,("frijoles",JNum 2.75)])]
~~~~~

Thats it for today. We will see much more typeclass awesomeness
in the next few lectures...

Exercise
--------
Why does Haskell reject the following expression?

~~~~~{.haskell}
(1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,1) == (1,2,3,4,5,6,7,8,9,10,1,1,1,1,1,1)
~~~~~

Exercise
--------
What are the mysterious incantations in the `LANGUAGE` block at the top of
the file? Can you figure out why they are needed?


[1]: http://portal.acm.org/citation.cfm?id=75283 "How to make ad-hoc polymorphism less ad-hoc"
[2]: http://en.wikipedia.org/wiki/Binary_search_tree "Wikipedia: Binary Search Trees"
[3]: http://www.json.org/ "JavaScript Object Notation"
