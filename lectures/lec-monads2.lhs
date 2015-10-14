---
title: Programming With Effects II
---

> import Data.Map

**Formatted and Extended version of the lecture notes by [Graham Hutton][0], January 2011**

Recap: Monads
=============

The notion of a monad can now be captured as follows:

~~~~~{.haskell}
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
~~~~~

That is, a monad is a parameterised type `m` that supports `return`
and `>>=` functions of the specified types.  The fact that `m` must
be a parameterised type, rather than just a type, is inferred from its
use in the types for the two functions.   Using this declaration,
it is now straightforward to make `Maybe` into a monadic type:

~~~~~{.haskell}
instance Monad Maybe where
   -- return      :: a -> Maybe a
   return x       =  Just x

   -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ =  Nothing
   (Just x) >>= f =  f x
~~~~~

(*Aside*: types are not permitted in instance declarations, but we
include them as comments for reference.)  It is because of this
declaration that the `do` notation can be used to sequence `Maybe`
values.  More generally, Haskell supports the use of this notation
with any monadic type.  In the next few sections we give some 
further examples of types that are monadic, and the benefits
that result from recognising and exploiting this fact.

The List Monad
--------------

The maybe monad provides a simple model of computations that can
fail, in the sense that a value of type `Maybe a` is either `Nothing`,
which we can think of as representing failure, or has the form
`Just x` for some `x` of type `a`, which we can think of as success.

The list monad generalises this notion, by permitting multiple
results in the case of success.  More precisely, a value of
`[a]` is either the empty list `[]`, which we can think of as
failure, or has the form of a non-empty list `[x1,x2,...,xn]`
for some `xi` of type `a`, which we can think of as success.
Making lists into a monadic type is straightforward:

~~~~~{.haskell}
instance Monad [] where
   -- return :: a -> [a]
   return x  =  [x]

   -- (>>=)  :: [a] -> (a -> [b]) -> [b]
   xs >>= f  =  concat (map f xs)
~~~~~

(*Aside*: in this context, `[]` denotes the list type `[a]` without
its parameter.)  That is, return simply converts a value into a
successful result containing that value, while `>>=` provides a
means of sequencing computations that may produce multiple
results: `xs >>= f` applies the function f to each of the results
in the list xs to give a nested list of results, which is then
concatenated to give a single list of results.

As a simple example of the use of the list monad, a function
that returns all possible ways of pairing elements from two 
lists can be defined using the do notation as follows:

~~~~~{.haskell}
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys =  do x <- xs
                  y <- ys
                  return (x, y)
~~~~~

That is, consider each possible value `x` from the list `xs`, and 
each value `y` from the list `ys`, and return the pair `(x,y)`. It
is interesting to note the similarity to how this function
would be defined using the list comprehension notation:

~~~~~{.haskell}
pairs xs ys = [(x,y) | x <- xs, y <- ys]
~~~~~

or in Python syntax:

~~~~~{.haskell}
def pairs(xs, ys): return [(x,y) for x in xs for y in ys]
~~~~~

In fact, there is a formal connection between the `do` notation
and the comprehension notation.  Both are simply different 
shorthands for repeated use of the `>>=` operator for lists.
Indeed, the language *Gofer* that was one of the precursors
to Haskell permitted the comprehension notation to be used 
with any monad.  For simplicity however, Haskell only allows
the comprehension notation to be used with lists.


Imperative Functional Programming
=================================

Consider the following problem. I have a (finite) list of values, e.g.

> vals0 :: [Char]
> vals0 = ['d', 'b', 'd', 'd', 'a']

that I want to *canonize* into a list of integers, where each *distinct*
value gets the next highest number. So I want to see something like

~~~~~{.haskell}
ghci> canonize vals0 
[0, 1, 0, 0, 2]
~~~~~

similarly, I want:

~~~~~{.haskell}
ghci> canonize ["zebra", "mouse", "zebra", "zebra", "owl"] 
[0, 1, 0, 0, 2]
~~~~~

**DO IN CLASS** 
How would you write `canonize` in Python?

**DO IN CLASS** 
How would you write `canonize` in Haskell? 

Now, lets look at another problem. Consider the following tree datatype.

~~~~~{.haskell}
data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)
~~~~~

Lets write a function

~~~~~{.haskell}
leafLabel :: Tree a -> Tree (a, Int)
~~~~~

that assigns each leaf a distinct integer value, so we get the following
behavior

~~~~~{.haskell}
ghci> leafLabel (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
                (Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2)))
~~~~~

**DO IN CLASS** 
How would you write `leafLabel` in Haskell? 


The State Monad
===============

Now let us consider the problem of writing functions that
manipulate some kind of state, represented by a type whose
internal details are not important for the moment:

~~~~~{.haskell}
type State = ... 
~~~~~

The most basic form of function on this type is a *state transformer* 
(abbreviated by ST), which takes the current state as its argument, 
and produces a modified state as its result, in which the modified 
state reflects any side effects performed by the function:

~~~~~{.haskell}
type ST = State -> State
~~~~~

In general, however, we may wish to return a result value in
addition to updating the state.  For example, a function for
incrementing a counter may wish to return the current value
of the counter.  For this reason, we generalise our type of
state transformers to also return a result value, with the
type of such values being a parameter of the `ST` type:

~~~~~{.haskell}
type ST a = State -> (a, State)
~~~~~

Such functions can be depicted as follows, where `s` is the input
state, `s'` is the output state, and `v` is the result value:
                              


  <img src="../static/monad1.png" width="300"/>



The state transformer may also wish to take argument values.
However, there is no need to further generalise the `ST` type
to take account of this, because this behaviour can already
be achieved by exploiting currying.  For example, a state
transformer that takes a character and returns an integer
would have type `Char -> ST Int`, which abbreviates the 
curried function type 

~~~~~{.haskell}
Char -> State -> (Int, State)
~~~~~

depicted by:



  <img src="../static/monad2.png" width="300"/>



Returning to the subject of monads, it is now straightforward
to make `ST` into an instance of a monadic type:

~~~~~{.haskell}
instance Monad ST where
   -- return :: a -> ST a
   return x  =  \s -> (x,s)

   -- (>>=)  :: ST a -> (a -> ST b) -> ST b
   st >>= f  =  \s -> let (x,s') = st s in f x s'
~~~~~

That is, `return` converts a value into a state transformer that
simply returns that value without modifying the state:




  <img src="../static/monad3.png" width="300"/>




In turn, `>>=` provides a means of sequencing state transformers:
`st >>= f` applies the state transformer `st` to an initial state
`s`, then applies the function `f` to the resulting value `x` to
give a second state transformer `(f x)`, which is then applied
to the modified state `s'` to give the final result:




  <img src="../static/monad4.png" width="400"/>



Note that `return` could also be defined by `return x s = (x,s)`.  
However, we prefer the above definition in which the second 
argument `s` is shunted to the body of the definition using a
lambda abstraction, because it makes explicit that `return` is
a function that takes a single argument and returns a state
transformer, as expressed by the type `a -> ST a`:  A similar
comment applies to the above definition for `>>=`.

We conclude this section with a technical aside.  In Haskell,
types defined using the `type` mechanism cannot be made into
instances of classes.  Hence, in order to make ST into an
instance of the class of monadic types, in reality it needs
to be redefined using the "data" mechanism, which requires
introducing a dummy constructor (called `S` for brevity):

> data ST0 a = S0 (State -> (a, State))

It is convenient to define our own application function for
this type, which simply removes the dummy constructor:

> apply0        :: ST0 a -> State -> (a, State)
> apply0 (S0 f) x = f x

In turn, ST is now defined as a monadic type as follows:

> instance Monad ST0 where
>   -- return :: a -> ST a
>   return x   = S0 (\s -> (x,s))
>
>   -- (>>=)  :: ST a -> (a -> ST b) -> ST b
>   st >>= f   = S0 (\s -> let (x, s') = apply0 st s in apply0 (f x) s')

(*Aside*: the runtime overhead of manipulating the dummy constructor
S can be eliminated by defining ST using the `newtype` mechanism
of Haskell, rather than the `data` mechanism.)

A simple example
----------------

Intuitively, a value of type `ST a` (or `ST0 a`) is simply an *action* that
returns an `a` value. The sequencing combinators allow us to combine simple
actions to get bigger actions, and the `apply0` allows us to *execute* an
action from some initial state.

To get warmed up with the state-transformer monad, consider the simple
*sequencing* combinator

~~~~~{.haskell}
(>>) :: Monad m => m a -> m b -> m b
~~~~~

in a nutshell, `a1 >> a2` takes the actions `a1` and `a2` and returns the
*mega* action which is `a1`-then-`a2`-returning-the-value-returned-by-`a2`.


In other words, the function can be defined using the notion of a state 
transformer, in which theinternal state is simply the next fresh integer

> type State = Int

In order to generate a fresh integer, we define a special
state transformer that simply returns the current state as
its result, and the next integer as the new state:

> fresh :: ST0 Int
> fresh =  S0 (\n -> (n, n+1))

Note that `fresh` is a *state transformer* (where the state 
is itself just `Int`), that is an *action* that happens to 
return integer values. What do you think the following does:

> wtf1 = fresh >> 
>        fresh >> 
>        fresh >> 
>        fresh

**DO IN CLASS** 
What do you think this would return:

~~~~~{.haskell}
ghci> apply0 wtf1 0
~~~~~

Indeed, we are just chaining together four `fresh` actions to get a single
action that "bumps up" the counter by `4`.

Now, the `>>=` sequencer is kind of like `>>` only it allows you to
"remember" intermediate values that may have been returned. Similarly, 

~~~~~{.haskell}
return :: a -> ST0 a
~~~~~

takes a value `x` and yields an *action* that doesnt actually transform the
state, but just returns the same value `x`. So, putting things together,
how do you think this behaves?

> wtf2 = fresh >>= \n1 ->
>        fresh >>= \n2 ->  
>        fresh >>
>        fresh >>
>        return [n1, n2]

> wtf2' = do { n1 <- fresh;  
>              n2 <- fresh;
>              fresh ;
>              fresh ;
>              return [n1, n2];
>            }



**DO IN CLASS** 
What do you think this would return:

~~~~~{.haskell}
ghci> apply0 wtf2 0
~~~~~

Now, the `do` business is just nice syntax for the above:

> wtf3 = do n1 <- fresh
>           fresh
>           fresh
>           fresh
>           return n1

is just like `wtf2`.


A More Interesting Example
--------------------------

By way of an example of using the state monad, let us define
a type of binary trees whose leaves contains values of some type a:

> data Tree a = Leaf a 
>             | Node (Tree a) (Tree a)
>             deriving (Eq, Show)

Here is a simple example:

> tree :: Tree Char
> tree =  Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

Now consider the problem of defining a function that labels each 
leaf in such a tree with a unique or "fresh" integer.  This can
be achieved by taking the next fresh integer as an additional 
argument to the function, and returning the next fresh integer
as an additional result. In short, we can use

~~~~~{.haskell}
fresh :: ST0 Int
~~~~~

to get distinct integer values, together with the `return` and `>>=` 
primitives that are provided by virtue of `ST` being a monadic type.
It is now straightforward to define a function that takes a tree as its
argument, and returns a state transformer that produces the
same tree with each leaf labelled by a fresh integer:

> mlabel            :: Tree a -> ST0 (Tree (a,Int))
> mlabel (Leaf x)   =  do n <- fresh
>                         return (Leaf (x,n))
> mlabel (Node l r) =  do l' <- mlabel l
>                         r' <- mlabel r
>                         return (Node l' r')

Note that the programmer does not have to worry about the tedious
and error-prone task of dealing with the plumbing of fresh labels,
as this is handled automatically by the state monad.

Finally, we can now define a function that labels a tree by
simply applying the resulting state transformer with zero as
the initial state, and then discarding the final state:

> label  :: Tree a -> Tree (a, Int)
> label t = fst (apply0 (mlabel t) 0)

For example, `label tree` gives the following result:

~~~~~{.haskell}
ghci> label tree
Node (Node (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
~~~~~

Exercise
--------

- Define a function `app :: (State -> State) -> ST0 State`, such 
  that fresh can be redefined by `fresh = app (+1)`.

- Define a function `run :: ST0 a -> State -> a`, such that label
  can be redefined by `label t = run (mlabel t) 0`.


A Generic State Transformer
===========================

Often, the *state* that we want to have will have multiple 
components, eg multiple variables whose values we might want 
to *update*. This is easily accomplished by using a different
type for `State` above, for example, if we want two integers, 
we might use the definition

~~~~~{.haskell}
type State = (Int, Int)
~~~~~

and so on. 

Since state is a handy thing to have, the standard library 
includes a [module][6] `Control.Monad.State` that defines a 
parameterized version of the state-transformer monad above. 
(You will use this library in [HW3](/homeworks/hw3.html).)

We will only allow clients to use the functions declared below

~~~~~{.haskell}
module MyState (ST, get, put, apply) where
~~~~~

The type definition for a generic state transformer is very simple:

> data ST s a = S (s -> (a, s))

is a parameterized state-transformer monad where the state is
denoted by type `s` and the return value of the transformer is 
the type `a`. We make the above a monad by declaring it to be
an instance of the `monad` typeclass

> instance Monad (ST s) where
>   return x   = S (\s -> (x, s))
>   st >>= f   = S (\s -> let (x, s') = apply st s 
>                         in apply (f x) s')


where the function `apply` is just

> apply :: ST s a -> s -> (a, s)
> apply (S f) x = f x

Accessing and Modifying State
-----------------------------

Since our notion of state is generic, it is useful to write a `get`
and `put` function with which one can *access* and *modify* the state. 
We can easily `get` the *current* state via 



> get = S (\s -> (s, s))





That is, `get` denotes an action that leaves the state unchanged, 
but returns the state itself as a value. What do you think the type
of `get` is? 

Dually, to *modify* the state to some new value `s'` we can write
the function

> put s' = S (\_ -> ((), s'))



fresh  = S0 (\n -> (n, n+1))

> realfresh :: ST Int Int
> realfresh = do n <- get
>                put (n+1)
>                return n 








which denotes an action that ignores (ie blows away the old state) and
replaces it with `s'`. Note that the `put s'` is an action that itselds 
yields nothing (that is, merely the unit value.)



Using a Generic State Transformer
=================================

Let us use our generic state monad to rewrite the tree labeling function 
from above. Note that the actual type definition of the generic transformer
is *hidden* from us, so we must use only the publicly exported functions:
`get`, `put` and `apply` (in addition to the monadic functions we get for
free.)


Recall the action that returns the next fresh integer. Using the generic
state-transformer, we write it as:

> freshS :: ST Int Int
> freshS = do n <- get
>             put (n+1)
>             return n


Now, the labeling function is straightforward

> mlabelS :: Tree a -> ST Int (Tree (a,Int))
> mlabelS (Leaf x)   =  do n <- freshS
>                          return (Leaf (x, n))
> mlabelS (Node l r) =  do l' <- mlabelS l
>                          r' <- mlabelS r
>                          return (Node l' r')





Easy enough!

~~~~~{.haskell}
ghci> apply (mlabelS tree) 0
(Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2)), 3)
~~~~~

We can *execute* the action from any initial state of our choice

~~~~~{.haskell}
ghci> apply (mlabelS tree) 1000
(Node (Node (Leaf ('a',1000)) (Leaf ('b',1001))) (Leaf ('c',1002)),1003)
~~~~~

Now, whats the point of a generic state transformer if we can't have richer
states. Next, let us extend our `fresh` and `label` functions so that 

- each node gets a new label (as before),
- the state also contains a map of the *frequency* with which each 
  leaf value appears in the tree.

Thus, our state will now have two elements, an integer denoting the *next*
fresh integer, and a `Map a Int` denoting the number of times each leaf
value appears in the tree.

> data MySt a = M { index :: Int
>                 , freq  :: Map a Int }
>               deriving (Eq, Show)

We write an *action* that returns the next fresh integer as

> freshM = do 
>   s     <- get              
>   let n  = index s
>   put $ s { index = n + 1 }  
>   return n 

Similarly, we want an action that updates the frequency of a given element `k`

> updFreqM k = do 
>   s    <- get               
>   let f = freq s 
>   let n = findWithDefault 0 k f
>   put $ s {freq = insert k (n + 1) f}

And with these two, we are done

> mlabelM (Leaf x)   =  do updFreqM x 
>                          n <- freshM
>                          return $ Leaf (x, n)
>
> mlabelM (Node l r) =  do l' <- mlabelM l
>                          r' <- mlabelM r
>                          return $ Node l' r'

Now, our *initial* state will be something like

> initM = M 0 empty

and so we can label the tree

~~~~~{.haskell}
ghci> let tree2   = Node tree tree 
ghci> let (lt, s) = apply (mlabelM tree) $ M 0 empty 

ghci> lt
Node (Node (Node (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))) (Node (Node (Leaf ('a',3)) (Leaf ('b',4))) (Leaf ('c',5)))

ghci> s
M {index = 6, freq = fromList [('a',2),('b',2),('c',2)]}
~~~~~

The IO Monad
============

Recall that interactive programs in Haskell are written using the
type `IO a` of "actions" that return a result of type `a`, but may
also perform some input/output.  A number of primitives are
provided for building values of this type, including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()
~~~~~

The use of return and `>>=` means that `IO` is monadic, and hence
that the do notation can be used to write interactive programs.
For example, the action that reads a string of characters from
the keyboard can be defined as follows:

~~~~~{.haskell}
getLine :: IO String
getLine =  do x <- getChar
              if x == '\n' then
                 return []
              else
                 do xs <- getLine
                    return (x:xs)
~~~~~

It is interesting to note that the `IO` monad can be viewed as a
special case of the state monad, in which the internal state is
a suitable representation of the "state of the world":

~~~~~{.haskell}
   type World = ...

   type IO a  = World -> (a, World)
~~~~~

That is, an action can be viewed as a function that takes the
current state of the world as its argument, and produces a value
and a modified world as its result, in which the modified world
reflects any input/output performed by the action.  In reality,
Haskell systems such as Hugs and GHC implement actions in a more
efficient manner, but for the purposes of understanding the
behaviour of actions, the above interpretation can be useful.


Derived primitives
==================

An important benefit of abstracting out the notion of a monad into 
a single typeclass, is that it then becomes possible to define a 
number of useful functions that work in an arbitrary monad.  


We've already seen this in the `pairs` function


> pairs xs ys = do
>   x <- xs
>   y <- ys
>   return (x, y)


What do you think the type of the above is ? (I left out an annotation
deliberately!)

~~~~~{.haskell}
ghci> :type pairs
pairs: (monad m) => m a -> m b -> m (a, b)
~~~~~

It takes two monadic values and returns a single *paired* monadic value.
Be careful though! The function above will behave differently depending
on what specific monad instance it is used with! If you use the `Maybe`
monad

~~~~~{.haskell}
ghci> pairs (Nothing) (Just 'a')
Nothing

ghci> pairs (Just 42) (Nothing)
Just 42

ghci> pairs (Just 2) (Just 'a')
Just (2, a)
~~~~~

this generalizes to the list monad 

~~~~~{.haskell}
ghci> pairs [] ['a'] 
[]

ghci> pairs [42] []
[]

ghci> pairs [2] ['a'] 
[(2, a)]

ghci> pairs [1,2] "ab" 
[(1,'a') , (2, 'a'), (1, 'b'), (2, 'b')]
~~~~~

However, the behavior is quite different with the `IO` monad

~~~~~{.haskell}
ghci> pairs getChar getChar
40('4','0')
~~~~~

For example, the `map` function on lists can be generalised 
as follows:

> liftM     :: Monad m => (a -> b) -> m a -> m b
> liftM f mx = do x <- mx
>                 return (f x)

Similarly, `concat` on lists generalises to:

> join    :: Monad m => m (m a) -> m a
> join mmx = do mx <- mmx
>               x  <- mx
>               return x



As a final example, we can define a function that transforms
a list of monadic expressions into a single such expression that
returns a list of results, by performing each of the argument
expressions in sequence and collecting their results:

~~~~~{.haskell}
sequence          :: Monad m => [m a] -> m [a]
sequence []       =  return []
sequence (mx:mxs) =  do x  <- mx
                        xs <- sequence mxs
                        return (x:xs)
~~~~~


Monads As Programmable Semicolon
--------------------------------

It is sometimes useful to sequence two monadic expressions,
but discard the result value produced by the first:

~~~~~{.haskell}
(>>)     :: Monad m => m a -> m b -> m b
mx >> my =  do _ <- mx
               y <- my
               return y
~~~~~

For example, in the state monad the `>>` operator is just normal
sequential composition, written as `;` in most languages.

Indeed, in Haskell the entire `do` notation with or without `;` is 
just [syntactic sugar][4] for `>>=` and `>>`. For this reason, we can 
legitimately say that Haskell has a [*programmable semicolon*][5].


Exercise
--------

- Define `liftM` and `join` more compactly by using `>>=`.

- Explain the behaviour of sequence for the maybe monad.

- Define another monadic generalisation of map:
	
~~~~~{.haskell}
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
~~~~~

- Define a monadic generalisation of foldr:

~~~~~{.haskell}
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
~~~~~

The monad laws
==============

Earlier we mentioned that the notion of a monad requires that the
return and `>>=` functions satisfy some simple properties.    The
first two properties concern the link between return and `>>=`:

~~~~~{.haskell}
return x >>= f  =  f x	--	(1)

mx >>= return   =  mx	--	(2)
~~~~~

Intuitively, equation (1) states that if we return a value `x` and
then feed this value into a function `f`, this should give the same
result as simply applying `f` to `x`.  Dually, equation (2) states
that if we feed the results of a computation `mx` into the function
return, this should give the same result as simply performing `mx`.
Together, these equations express --- modulo the fact that the
second argument to `>>=` involves a binding operation --- that
return is the left and right identity for `>>=`.

The third property concerns the link between `>>=` and itself, and
expresses (again modulo binding) that `>>=` is associative:

~~~~~{.haskell}
(mx >>= f) >>= g  =  mx >>= (\x -> (f x >>= g)) 	-- (3)
~~~~~

Note that we cannot simply write `mx >>= (f >>= g)` on the right 
hand side of this equation, as this would not be type correct.

As an example of the utility of the monad laws, let us see how
they can be used to prove a useful property of the `liftM` function
from the previous section, namely that it distributes over the
composition operator for functions, in the sense that:

~~~~~{.haskell}
liftM (f . g)  =  liftM f . liftM g
~~~~~

This equation generalises the familiar distribution property of
map from lists to an arbitrary monad.  In order to verify this
equation, we first rewrite the definition of `liftM` using `>>=`:

~~~~~{.haskell}
liftM f mx  =  mx >>= \x -> return (f x)
~~~~~

Now the distribution property can be verified as follows:

~~~~~{.haskell}
(liftM f . liftM g) mx
   = {-   applying . -}
     liftM f (liftM g mx)
   = {-   applying the second liftM -}
     liftM f (mx >>= \x -> return (g x))
   = {-   applying liftM -} 
     (mx >>= \x -> return (g x)) >>= \y -> return (f y)
   = {-   equation (3) -}
     mx >>= (\z -> (return (g z) >>= \y -> return (f y)))
   = {-   equation (1) -}
     mx >>= (\z -> return (f (g z)))
   = {-   unapplying . -}
     mx >>= (\z -> return ((f . g) z)))
   = {-   unapplying liftM -}
     liftM (f . g) mx
~~~~~

Exercise
--------

Show that the maybe monad satisfies equations (1), (2) and (3).


Exercise
--------

Given the type

> data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

of expressions built from variables of type `a`, show that this
type is monadic by completing the following declaration:

~~~~~{.haskell}
   instance Monad Expr where
      -- return       :: a -> Expr a
      return x         = ...

      -- (>>=)        :: Expr a -> (a -> Expr b) -> Expr b
      (Var a)   >>= f  = ...
      (Val n)   >>= f  = ...
      (Add x y) >>= f  = ...
~~~~~

Hint: think carefully about the types involved.  With the aid of an 
example, explain what the `>>=` operator for this type does.

Other topics
------------

The subject of monads is a large one, and we have only scratched
the surface here.  If you are interested in finding out more, 
two suggestions for further reading would be to look at "monads
with a zero a plus" (which extend the basic notion with two 
extra primitives that are supported by some monads), and "monad
transformers" (which provide a means to combine monads.)  For
example, see sections 3 and 7 of the following article, which
concerns the monadic nature of [functional parsers][3]
For a more in-depth exploration of the IO monad, see Simon Peyton
Jones' excellent article on the ["awkward squad"][2]


[0]: http://www.cs.nott.ac.uk/~gmh/monads
[1]: http://en.wikipedia.org/wiki/Gofer_(software) "Gofer Language"
[2]: http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/ "Awkward Squad"
[3]: http://www.cs.nott.ac.uk/~gmh/monparsing.pdf "Functional Parsers"
[4]: http://book.realworldhaskell.org/read/monads.html#monads.do
[5]: http://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/
[6]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2
