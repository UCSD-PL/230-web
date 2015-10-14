---
title: Monad Transformers
---

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, OverlappingInstances, FlexibleInstances #-}

> import Control.Monad.Error
> import Control.Monad.State
> import Control.Monad.Writer
> import Debug.Trace
> import Control.Applicative 

Monads Can Do Many Things
=========================

Lets recall the simple language of divisions. 

> data Expr = Val Int
>           | Div Expr Expr
>           deriving (Show)

Today, we will see how monads can be used to write (and compose) 
*evaluators* for such languages.

Remember the vanilla *unsafe* evaluator 

> eval            ::  Expr -> Int
> eval (Val n)   =  n
> eval (Div x y) =  eval x `div` eval y

Here are two terms that we will use as running examples. 

> ok  = Div (Div (Val 1972) (Val 2)) (Val 23)
> err = Div (Val 2) (Div (Val 1) (Div (Val 2) (Val 3)))

The first evaluates properly and returns a valid answer, 
and the second fails with a divide-by-zero exception.

~~~~~{.haskell}
ghci> eval ok 
42

ghci> eval err
*** Exception: divide by zero
~~~~~

We didn't like this `eval` because it can just blow up 
with a divide by zero error without telling us how it 
happened. Worse, the error is a *radioactive* value
that, spread unchecked through the entire computation.

We used the `Maybe` type to capture the failure case: a
`Nothing` result meant that an error happened somewhere,
while a `Just n` result meant that evaluation succeeded
yielding `n`. Morever, we saw how the `Maybe` monad could
be used to avoid ugly case-split-staircase-hell.

> evalMaybe ::  Expr -> Maybe Int
> evalMaybe (Val n)   = return n
> evalMaybe (Div x y) = do n <- evalMaybe x
>                          m <- evalMaybe y
>                          if m == 0 
>                            then Nothing
>                            else return (n `div` m)

> evalExn (Val n)   = return n
> evalExn (Div x y) = do n <- evalExn x
>                        m <- evalExn y
>                        if m == 0 
>                            then throwExn "EEKES DIVIDED BY ZERO" 
>                            else return (n `div` m)

> throwExn = Exn



which behaves thus

~~~~~{.haskell}
ghci> evalMaybe ok 
Just 42

ghci> evalMaybe err
Nothing
~~~~~


Error Handling Via Exception Monads
-----------------------------------

The trouble with the above is that it doesn't let us know
*where* the divide by zero occurred. It would be nice to 
have an *exception* mechanism where, when the error occurred,
we could just saw `throw x` for some value `x` which would, 
like an exception go rocketing back to the top and tell us
what the problem was.

If you think for a moment, you'll realize this is but a small
tweak on the `Maybe` type; all we need is to jazz up the 
`Nothing` constructor so that it carries the exception value.

> data Exc a = Exn  String
>            | Result a
>            deriving (Show)





Here the `Exn` is like `Nothing` but it carries a string 
denoting what the exception was. We can make the above a 
`Monad` much like the `Maybe` monad.

> instance Monad Exc where
>   (Exn s ) >>= _ = Exn s
>   (Result x) >>= f = f x
>   return           = Result 

> instance Functor Exc where
>   fmap f (Exn s)  = Exn s
>   fmap f (Result x) = Result (f x) 

**Throwing Exceptions**

Let's write a function to `throw` an exception 

~~~~~{.haskell}
throw = Exn
~~~~~

and now, we can use our newly minted monad to write 
a better exception throwing evaluator

> evalExc ::  Expr -> Exc Int
> evalExc (Val n)   = return n
> evalExc (Div x y) = do n <- evalExc x
>                        m <- evalExc y
>                        if m == 0 
>                          then throw  $ errorS y m 
>                          else return $ n `div` m

where the sidekick `errorS` generates the error string. 

> errorS y m = "Error dividing by " ++ show y ++ " = " ++ show m

Note that this is essentially like the first evaluator; 
instead of bailing with `Nothing` we return some (hopefully)
helpful message, but the monad takes care of ensuring that 
the exception is shot back up.

~~~~~{.haskell}
ghci> evalExc ok 
Result 42

ghci> evalExc err
Exn "Error dividing by Div (Val 2) (Val 3) = 0"
~~~~~


**Catching Exceptions**

Its all well and good to *throw* an exception, but it would be nice if we
could gracefully *catch* them as well. For example, wouldn't it be nice if
we could write a function like this:

> evalExcc ::  Expr -> Exc (Maybe Int)
> evalExcc e = tryCatch (Just <$> (evalExc e)) $ \err -> 
>                     return (trace ("oops, caught an exn" ++ err) Nothing)

Thus, in `evalExcc` we have just *caught* the exception to return a `Maybe` value
in the case that something went wrong. Not the most sophisticated form of
error handling, but you get the picture.

QUIZ 
----

What should the **type** of `tryCatch` be?

a. `Exc a -> (a -> Exc b) -> Exc b`
d. `Exc a -> (String -> a) -> a`
e. None of the above

~~~~~{.haskell}





~~~~~



> tryCatch :: Exc a -> (String -> Exc a) -> Exc a

And next, lets write it!

> tryCatch (Exn  err) f = f err
> tryCatch r@(Result _) _ = r 

And now, we can run it of course...

~~~~~{.haskell}
ghci> evalExcc ok
Result (Just 42)

ghci> evalExcc err
Caught Error: Error dividing by Div (Val 2) (Val 3) = 0
Result Nothing
~~~~~





Profiling Operations Via State Monads
-------------------------------------

Next, lets stop being so paranoid about errors and instead 
try to do some **profiling**. Lets imagine that the `div` 
operator is very expensive, and that we would like to 
*count* the number of divisions that are performed while
evaluating a particular expression.

As you might imagine, our old friend the state-transformer 
monad is likely to be of service here!

> type StateST = Int
> data ST a    = S (StateST -> (a, StateST))


> instance Monad ST where
>   return x     = S $ \s -> (x, s)
>   (S st) >>= f = S $ \s -> let (x, s') = st s 
>                                S st'   = f x
>                            in st' s'


Next, lets write the useful `runStateST` which executes the monad from 
an initial state, `getST` and `putST` which allow us to access and
modify the state, respectively.


~~~~~{.haskell}
getST = S (\s -> (s, s))
putST = \s' -> S (\_ -> ((), s'))
~~~~~


Armed with the above, we can write a function


> tickST = do n <- getST
>             putST (n+1)

Now, we can write a profiling evaluator


> evalST           :: Expr -> ST Int
> evalST (Val n)   = return n
> evalST (Div x y) = do n <- evalST x
>                       m <- evalST y
>                       tickST
>                       return (n `div` m)

<!--                         if m == 0 
                         then throw "AAARRCHGGG!!" 
                         else do {tickST; return (n `div` m)}

  -->

and by judiciously making the above an instance of `Show`

> instance Show a => Show (ST a) where
>   show (S st) = "value: " ++ show x ++ ", count: " ++ show s
>     where (x, s) = st 0
	   
we can get observe our profiling evaluator at work

~~~~~{.haskell}
ghci> evalST ok
value: 42, count: 2
~~~~~

But, alas, to get the profiling we threw out the nifty 
error handling that we had put in earlier

~~~~~{.haskell}
ghci> evalST err 
value: *** Exception: divide by zero
~~~~~

(Hmm. Why does it print `value` this time around?)


Transformers: Making Monads Multitask
=====================================

So it looks like Monads can do many thigs, but only 
*one thing at a time* -- you can either use a monad 
to do the error management plumbing *OR* to do the 
state manipulation plumbing, but not at the same time. 
Is it too much ask for both? I guess we could write a 
*mega-state-and-exception* monad that supports the 
operations of both, but that doesn't sound like any 
fun at all! Worse, if later we decide to add yet 
another feature, then we would have to make up yet 
another mega-monad. 

 
  <img src="../static/lec-tx-transforming.png" width="400"/>


We shall take a different approach, where we will keep
*wrapping* or decorating monads with extra features, so 
that we can take a simple monad, and then add the 
Exception monad's features to it, and then add the 
State monad's features and so on. 

The key to doing this is to not define exception 
handling, state passing etc as monads, but as
**functions from monads to monads.** 
This will require a little more work up-front 
(most of which is done already in well-designed libraries)
but after that we can add new features in a modular manner.
For example, to get a mega state- and exception- monad,
we will start with a dummy `Identity` monad, apply it to 
the `StateT` monad transformer (which yields state-passing monad)
and pass the result to the `ExcT` monad transformer which yields
the desired mega monad. Incidentally, the above should remind 
some of you of the [Decorator Design Pattern][2] and others 
of [Python's Decorators][3].

Concretely, we will develop mega-monads in *four* steps:

-  **Step 1: Description** First we will define typeclasses that describe the
   *enhanced* monads, i.e. by describing their *extra* operations,

-  **Step 2: Use** Second we will see how to write functions that *use* the mega
   monads, simply by using a combination of their features -- here the
   functions' type signatures will list all the constraints on the
   corresponding monad,

Next, we need to **create** monads with the special features. We will do
this by starting with a basic *powerless* monad, and then

-  **Step 3: Add Features** thereby adding extra operations to the simpler
   monad to make it more powerful, and

-  **Step 4: Preserver Features** Will make sure that the addition of
   features allows us to *hold onto* the older features, so that at the
   end, we get a mega monad that is just the *accumulation* of all the
   added features.

Next, lets look at each step in turn.

Step 1: Describing Monads With Special Features
-----------------------------------------------

The first step to being able to compose monads is to 
define typeclasses that describe monads armed with 
the special features. For example, the notion of an 
*exception monad* is captured by the typeclass

> class Monad m => MonadExc m where
>   throw :: String -> m a 

which corresponds to monads that are also equipped with 
an appropriate `throw` function (you can add a `catch` 
function too, if you like!) Indeed, we can make `Exc` an
instance of the above by

> instance MonadExc Exc where 
>   throw = Exn

I urge you to directly enter the body of `evalExc` above into 
GHCi and see what type is inferred for it!

Similarly, we can bottle the notion of a *state(-transforming) 
monad* in the typeclass

> class Monad m => MonadST m where
>   runStateST :: m a -> StateST -> m (a, StateST)
>   getST      :: m StateST 
>   putST      :: StateST -> m ()

which corresponds to monads that are kitted out with the
appropriate execution, extraction and modification functions.
Needless to say, we can make `ST` an instance of the above by

> instance MonadST ST where
>   runStateST (S f)  = return . f 
>   getST             = S (\s -> (s, s))
>   putST             = \s' -> S (\_ -> ((), s'))

Once again, if you know whats good for you, enter the body of
`evalST` into GHCi and see what type is inferred.

Step 2: Using Monads With Special Features
------------------------------------------

Armed with these two typeclasses, we can write our evaluator
quite easily

> evalMega (Val n)   = return n
> evalMega (Div x y) = do n <- evalMega x
>                         m <- evalMega y
>                         tickST
>                         if m == 0 
>                           then throw $ errorS y m 
>                           else return $ n `div` m

QUIZ
----

What is the type of `evalMega` ?

a. `Expr -> ST Int`
b. `Expr -> Exc Int`
c. `(MonadST m) => Expr -> m Int`
d. `(MonadExc m) => Expr -> m Int`
e. None of the above

~~~~~{.haskell}






~~~~~


Note that it is simply the combination of the two evaluators
from before -- we use the `throw` from `evalExc` and the 
`tickST` from `evalST`. Meditate for a moment on the type of 
above evaluator; note that it works with *any monad* that 
is **both** a exception- and a state- monad! 

Indeed, if, as I exhorted you to, you had gone back and studied the types of
`evalST` and `evalExc` you would find that each of those functions required the
underlying monad to be a state-manipulating and exception-handling monad
respectively.  In contrast, the above evaluator simply demands both features.

**Next:** But, but, but ... how do we create monads with **both** features?


Step 3: Injecting Special Features into Monads
----------------------------------------------

To *add* special features to existing monads, we will use
*monad transformers*, which are type operators `t` that 
map a monad `m` to a monad `t m`. The key ingredient of 
a transformer is that it must have a function `promote`
that can take an `m` value (ie action) and turn it into a 
`t m` value (ie action):

> class Transformer t where
>   promote :: Monad m => m a -> (t m) a

Now, that just defines the *type* of a transformer, lets see
some real transformers!

**A Transformer For Exceptions**

Consider the following type

> newtype ExcT m a = MkExc (m (Exc a))

it is simply a type with two parameters -- the first
is a monad `m` inside which we will put the exception 
monad `Exc a`. In other words, the `ExcT m a` simply 
*injects* the `Exc a` monad *into* the value slot 
of the `m` monad.

It is easy to formally state that the above is a 
bonafide transformer

> instance Transformer ExcT where
>   promote = MkExc . promote_ 

where the generic `promote_` function simply injects
the value from the outer monad `m`  into the inner 
monad `m1` :

> promote_ ::  (Monad m, Monad m1) => m t -> m (m1 t)
> promote_ m = do x <- m
>                 return $ return x

Consequently, any operation on the input monad `m` can be 
directly promoted into an action on the transformed monad, 
and so the transformation *preserves* all the operations
on the original monad.

Now, the real trick is twofold, we ensure that if `m` 
is a monad, then transformed `ExcT m` is an 
*exception monad*, that is an `MonadExc`.

First, we show the transformer output is a monad:

> instance Monad m => Monad (ExcT m) where
>   return x = promote $ return x 
>   p >>= f  = MkExc $ strip p >>= r 
>      where r (Result x)    = strip  $ f x
>            r (Exn  s)    = return $ Exn s
>            strip (MkExc m) = m

and next we ensure that the transformer is an 
exception monad by equipping it with `throw`

> instance Monad m => MonadExc (ExcT m) where
>   throw s = MkExc $ return $ Exn s

**A Transformer For State**

Next, we will build a transformer for the state monad,
following, more or less, the recipe for exceptions. Here 
is the type for the transformer

> newtype STT m a =  MkSTT (StateST -> m (a, StateST))

Thus, in effect, the enhanced monad is a state-update where 
the output is the original monad as we do the state-update
and return as output the new state wrapped inside the 
parameter monad.

> instance Transformer STT where
>   promote f = MkSTT $ \s -> do x <- f 
>                                return (x, s)

Next, we ensure that the transformer output is a monad:

> instance Monad m => Monad (STT m) where
>   return  = promote . return
>   m >>= f = MkSTT $ \s -> do (x, s') <- strip m s
>                              strip (f x) s'  
>     where strip (MkSTT f) = f 

and next we ensure that the transformer is a state 
monad by equipping it with the operations from `MonadST` 

> instance Monad m => MonadST (STT m) where
> --runStateST :: STT m a -> StateST -> STT m (a, StateST)
>   runStateST (MkSTT f) s = MkSTT $ \s0 -> do (x,s') <- f s
>                                              return ((x,s'), s0)
> --getST :: STT m StateST
> --getST :: MkSTT (StateST -> m (StateST, StateST))
>   getST = MkSTT $ \s -> return (s, s)
>
> --putST :: StateST -> STT m () 
> --putST :: StateST -> MkSTT (StateST -> m ((), StateST))
>   putST s = MkSTT (\_ -> return ((), s)) 

Step 4: Preserving Old Features of Monads
-----------------------------------------

Of course, we must make sure that the original features
of the monads are not lost in the transformed  monads. 
For this purpose, we will just use the `promote` 
operation to directly transfer operations from 
the old monad into the transformed monad. 

Thus, we can ensure that if a monad was already 
a state-manipulating monad, then the result of 
the exception-transformer is *also* a 
state-manipulating monad.

> instance MonadExc m => MonadExc (STT m) where
>   throw s = promote (throw s)

> instance MonadST m => MonadST (ExcT m) where
>   getST = promote getST
>   putST = promote . putST
>   runStateST (MkExc m) s = MkExc $ do (ex, s') <- runStateST m s
>                                       case ex of
>                                         Result x  -> return $ Result (x, s')
>                                         Exn err -> return $ Exn err 

Step 5: Whew! Put together and Run
----------------------------------

Finally, we can put all the pieces together and run the transformers.
We could *order* the transformations differently (and that can have
different consequences on the output as we will see.)

> evalExSt :: Expr -> STT Exc Int
> evalExSt = evalMega
>
> evalStEx :: Expr -> ExcT ST Int
> evalStEx = evalMega

which we can run as

~~~~~{.haskell}
ghci> d1
Exn:Error dividing by Div (Val 2) (Val 3) = 0

ghci> evalStEx ok
Count: 2
Result 42

ghci> evalStEx err
Count: 2
Exn "Error dividing by Div (Val 2) (Val 3) = 0"

ghci> evalExSt ok
Count:2
Result: 42

ghci> evalExSt err
Exn:Error dividing by Div (Val 2) (Val 3) = 0
~~~~~

where the rendering functions are

> instance Show a => Show (STT Exc a) where
>   show (MkSTT f) = case (f 0) of 
>                      Exn s         -> "Exn:" ++ s ++ "\n"
>                      Result (v, cnt) -> "Count:" ++ show cnt ++ "\n" ++
>                                         "Result: " ++ show v ++ "\n"
>
> instance Show a => Show (ExcT ST a) where
>   show (MkExc (S f)) = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
>     where (r, cnt) = f 0


The Monad Transformer Library
=============================

While it is often *instructive* to roll your own versions 
of code, as we did above, in practice you should reuse as 
much as you can from standard libraries. 


Error Monads and Transformers 
-----------------------------

The above sauced-up exception-tracking version of `Maybe` 
already exists in the standard type [Either][1]

~~~~~{.haskell}
ghci> :info Either 
data Either a b = Left a | Right b 	-- Defined in Data.Either
~~~~~

The `Either` type is a generalization of our `Exc` type, 
where the exception is polymorphic, rather than just being
a `String`. In other words the hand-rolled `Exc a` corresponds 
to the standard `Either String a` type.

The standard [MonadError][6] typeclass corresponds directly with
`MonadExc` developed above.

~~~~~{.haskell}
ghci> :info MonadError
class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a
  	-- Defined in Control.Monad.Error.Class
instance (Monad m, Error e) => MonadError e (ErrorT e m)
  -- Defined in Control.Monad.Error
instance (Error e) => MonadError e (Either e)
  -- Defined in Control.Monad.Error
instance MonadError IOError IO -- Defined in Control.Monad.Error
~~~~~

Note that `Either String` is an instance of `MonadError` much 
like `Exc` is an instance of `MonadExc`. Finally, the `ErrorT`
transformer corresponds to the `ExcT` transformer developed above
and its output is guaranteed to be an instance of `MonadError`.

State Monads and Transformers
-----------------------------

Similarly, the `ST` monad that we wrote above is but a pale reflection 
of the more general [State][4] monad. 

~~~~~{.haskell}
ghci> :info State
newtype State s a = State {runState :: s -> (a, s)}
  	-- Defined in Control.Monad.State.Lazy
~~~~~

The `MonadST` typeclass that we developed above corresponds directly 
with the standard [MonadState][5] typeclass.

~~~~~{.haskell}
ghci> :info MonadState
class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  	-- Defined in Control.Monad.State.Class

instance (Monad m) => MonadState s (StateT s m)
  -- Defined in Control.Monad.State.Lazy

instance MonadState s (State s)
  -- Defined in Control.Monad.State.Lazy
~~~~~

Note that `State s` is already an instance of `MonadState` much 
like `ST` is an instance of `MonadST`. Finally, the `StateT`
transformer corresponds to the `STT` transformer developed above
and its output is guaranteed to be an instance of `MonadState`.

Thus, if we stick with the standard libraries, we can simply write

> tick = do {n <- get; put (n+1)}

> eval1 (Val n)   = return n
> eval1 (Div x y) = do n   <- eval1 x
>                      m   <- eval1 y
>                      if m == 0 
>                        then throwError $ errorS y m
>                        else do tick
>                                return  $ n `div` m



> evalSE :: Expr -> StateT Int (Either String) Int
> evalSE = eval1

~~~~~{.haskell}
ghci> runStateT (evalSE ok) 0
Right (42,2)

ghci> runStateT (evalSE err) 0
Left "Error dividing by Div (Val 2) (Val 3) = 0"
~~~~~

You can stack them in the other order if you prefer

> evalES :: Expr -> ErrorT String (State Int) Int
> evalES = eval1

which will yield a different result

~~~~~{.haskell}
ghci> runState (runErrorT (evalES ok)) 0
(Right 42,2)

ghci> runState (runErrorT (evalES err)) 0
(Left "Error dividing by Div (Val 2) (Val 3) = 0",2)
~~~~~

see that we actually get the division-count (upto the point of
failure) even when the computation bails.


Tracing Operations Via Logger Monads
------------------------------------

Next, we will spice up our computations to also *log* messages (a *pure* 
variant of the usual method where we just *print* the messages to the
screen.) This can be done with the standard [Writer][7] monad, which
supports a `tell` action that logs the string you want (and allows you to
later view the entire log of the computation.

To accomodate logging, we juice up our evaluator directly as

> eval2 v = 
>   case v of 
>     Val n   -> do tell $ msg v n 
>                   return n
>     Div x y -> do n <- eval2 x
>                   m <- eval2 y
>                   if m == 0 
>                     then throwError $ errorS y m 
>                     else do tick 
>                             tell    $ msg v (n `div` m)  
>                             return  $ n `div` m

where the `msg` function is simply

> msg t r = "term: " ++ show t ++ ", yields " ++ show r ++ "\n"

Note that the only addition to the previous evaluator is the `tell`
operations! We can run the above using

> evalWSE :: Expr -> WSE Int
> evalWSE = eval2

where `WSE` is a type abbreviation

> type WSE a = WriterT String (StateT Int (Either String)) a 

That is, we simply use the `WriterT` transformer to decorate the underlying 
monad that carries the state and exception information.

~~~~~{.haskell}
ghci> runStateT (runWriterT (evalWSE ok)) 0
Right ((42,"term: Val 1972, yields 1972\nterm: Val 2, yields 2\nterm: Div (Val 1972) (Val 2), yields 986\nterm: Val 23, yields 23\nterm: Div (Div (Val 1972) (Val 2)) (Val 23), yields 42\n"),2)

ghci> runStateT (runWriterT (evalWSE err)) 0
Left "Error dividing by Div (Val 2) (Val 3) = 0"
~~~~~

That looks a bit ugly, so we can write our own pretty-printer

> instance Show a => Show (WSE a) where
>   show m = case runStateT (runWriterT m) 0 of 
>              Left s            -> "Error: " ++ s
>              Right ((v, w), s) -> "Log:\n"  ++ w       ++ "\n" ++
>                                   "Count: " ++ show s  ++ "\n" ++
>                                   "Value: " ++ show v  ++ "\n"

after which we get

~~~~~{.haskell}
ghci> print $ evalWSE ok

Log:
term: Val 1972, yields 1972
term: Val 2, yields 2
term: Div (Val 1972) (Val 2), yields 986
term: Val 23, yields 23
term: Div (Div (Val 1972) (Val 2)) (Val 23), yields 42

Count: 2
Value: 42

ghci> print $ evalWSE err
Error: Error dividing by Div (Val 2) (Val 3) = 0
~~~~~

*How come we didn't get any log in the error case?* 

The answer lies in the *order* in which we compose the transformers; 
since the error wraps the log, if the computation fails, the log gets 
thrown away. Instead, we can just wrap the other way around

> type ESW a = ErrorT String (StateT Int (Writer String)) a          
>
> evalESW :: Expr -> ESW Int
> evalESW = eval2

after which, everything works just fine!

~~~~~{.haskell}
ghci> evalESW err
Log:
term: Val 2, yields 2
term: Val 1, yields 1
term: Val 2, yields 2
term: Val 3, yields 3
term: Div (Val 2) (Val 3), yields 0

Count: 1
Error: Error dividing by Div (Val 2) (Val 3) = 0
~~~~~

> instance Show a => Show (ESW a) where 
>   show m = "Log:\n"  ++ log ++ "\n" ++ 
>            "Count: " ++ show cnt ++ "\n" ++
>            result
>     where ((res, cnt), log) = runWriter (runStateT (runErrorT m) 0)
>           result   = case res of 
>                        Left s -> "Error: " ++ s
>                        Right v -> "Value: " ++ show v

Moral of the story
------------------

  <img src="../static/lec-tx-stacking.png" width="200"/>

There are many useful monads, and if you play your cards right, Haskell
will let you *stack* them nicely on top of each other, so that you can get
*mega-monads* that have all the powers of the individual monads. See for
yourself in [Homework 3][8].


[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Either
[2]: http://oreilly.com/catalog/hfdesignpat/chapter/ch03.pdf
[3]: http://en.wikipedia.org/wiki/Python_syntax_and_semantics#Decorators
[4]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#v:state
[5]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Class.html#t:MonadState
[6]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error-Class.html#t:MonadError
[7]: http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Writer-Lazy.html#t:Writer
[8]: homeworks/hw3.html 
