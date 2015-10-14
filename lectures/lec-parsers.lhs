---
title: Monadic Parsing 
---

<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}
 
{-# LANGUAGE LambdaCase #-}
import Data.Char
import Data.Functor
import Control.Monad
\end{code}
</div>

Before we continue, a word from our sponsors: 

			**Don't Fear Monads**

They are simply an (extremely versatile) abstraction, like `map` or `fold`.

What is a Parser?
-----------------

A parser is a piece of software that takes a raw `String` (or sequence of
bytes) and returns some structured object, for example, a list of options, 
an XML tree or JSON object, a program's Abstract Syntax Tree and so on. 
Parsing is one of the most basic computational tasks. *Every* serious 
software system has a parser tucked away somewhere inside, for example

            System    Parses
    --------------    ------------------------------
     Shell Scripts    Command-line options
          Browsers    HTML
             Games    Level descriptors
           Routers    Packets


(Indeed I defy you to find any serious system that *does not* do some
parsing somewhere!)

The simplest and most accurate way to think of a parser is as a function 

~~~~~{.haskell}
type Parser = String -> StructuredObject
~~~~~

Composing Parsers
-----------------

The usual way to build a parser is by specifying a grammar and using a
parser generator (eg yacc, bison, antlr) to create the actual parsing
function. While elegant, one major limitation of the grammar based 
approach is its lack of modularity. For example, suppose I have two 
kinds of primitive values `Thingy` and `Whatsit`. 

~~~~~{.haskell}
Thingy : rule 	{ action } 
;

Whatsit : rule  { action }
;
~~~~~

If you want a parser for *sequences of* `Thingy` and `Whatsit` we have to 
painstakingly duplicate the rules as

~~~~~{.haskell}
Thingies : Thingy Thingies  { ... } 
           EmptyThingy      { ... }
;

Whatsits : Whatsit Whatsits { ... }
           EmptyWhatsit     { ... }
;
~~~~~

This makes sub-parsers hard to reuse. Next, we will see how to *compose* 
mini-parsers for sub-values to get bigger parsers for complex values.

To do so, we will generalize the above parser type a little bit, by noting 
that a (sub-)parser need not (indeed, will not) consume consume *all* of 
its input, and so we can simply have the parser return the unconsumed input

~~~~~{.haskell}
type Parser = String -> (StructuredObject, String) 
~~~~~

Of course, it would be silly to have different types for parsers for
different kinds of objects, and so we can make it a parameterized type


~~~~~{.haskell}
type Parser a = String -> (a, String) 
~~~~~

One last generalization: the parser could return multiple results, for
example, we may want to parse the string

~~~~~{.haskell}
"2 - 3 - 4"
~~~~~

either as

~~~~~{.haskell}
Minus (Minus 2 3) 4
~~~~~

or as 

~~~~~{.haskell}
Minus 2 (Minus 3 4)
~~~~~

So, we can have our parsers return a *list* of possible results (where the
empty list corresponds to a failure to parse.)

\begin{code}
newtype Parser a = P (String -> [(a, String)])
\end{code}

The above is simply the parser (*cough* action) the actual parsing is done by

\begin{code}
doParse (P p) s = p s
\end{code}

Lets build some parsers!


Parse A Single character
-------------------------

QUIZ
----

Recall

~~~~~{.haskell}
newtype Parser a = P (String -> [(a, String)])
~~~~~

Which of the following is a valid single-character-parser that returns the 
**first** `Char` from a string (if one exists.)

~~~~~{.haskell}
-- a
oneChar = P $ \cs -> head cs

-- b
oneChar = P $ \case -> {[] -> [('', []) | c:cs -> (c, cs)}

-- c
oneChar = P $ \cs -> (head cs, tail cs)

-- d
oneChar = P $ \cs -> [(head cs, tail cs)]

-- e
oneChar = P $ \case -> { [] -> [] | cs -> [(head cs, tail cs)]}
~~~~~

~~~~~{.haskell}





~~~~~

Yes, we can!

\begin{code}
oneChar :: Parser Char
oneChar = P (\cs -> case cs of
               c:cs' -> [(c, cs')]
               _     -> [])
\end{code}

Lets run the parser

~~~~~{.haskell}
ghci> doParse oneChar "hey!"
[('h',"ey!")]

ghci> doParse oneChar ""
[]
~~~~~

Now we can write another parser that grabs a **pair** of `Char` values

~~~~~{.haskell}
twoChar :: Parser (Char, Char)
twoChar  = P (\cs -> case cs of
             c1:c2:cs' -> [((c1, c2), cs')]
             _         -> [])
~~~~~

Lets run the parser

~~~~~{.haskell}
ghci> doParse twoChar "hey!"
[(('h', 'e'), "y!")]

ghci> doParse twoChar "h"
[]
~~~~~


Parser Composition
------------------

QUIZ
----

Recall

~~~~~{.haskell}
twoChar :: Parser (Char, Char)
twoChar  = P (\cs -> case cs of
             c1:c2:cs' -> [((c1, c2), cs')]
             _         -> [])
~~~~~

Suppose we had some `foo` such that behaved **identically** to `twoChar`.

~~~~~{.haskell}
twoChar' = foo oneChar oneChar 
~~~~~

What must the type of `foo` be?

a. `Parser (Char, Char)` 
b. `Parser Char -> Parser (Char, Char)`
c. `Parser a -> Parser a -> Parser (a, a)` 
d. `Parser a -> Parser b -> Parser (a, b)` 
e. `Parser a -> Parser (a, a)` 

~~~~~{.haskell}






~~~~~

Indeed, `foo` is a **parser combinator** that takes two parsers and returns a 
new parser that returns a pair of values:

~~~~~{.haskell}
pairP ::  Parser a -> Parser b -> Parser (a, b)
pairP p1 p2 = P (\cs -> 
  [((x,y), cs'') | (x, cs' ) <- doParse p1 cs, 
                   (y, cs'') <- doParse p2 cs']
  )
~~~~~

Now we can more cleanly write:

\begin{code}
twoChar = pairP oneChar oneChar 
\end{code}

which would run like this

~~~~~{.haskell}
ghci> doParse twoChar "hey!"
[(('h','e'), "y!")]
~~~~~

**EXERCISE:** Can you explain why we get the following behavior?

~~~~~{.haskell}
ghci> doParse twoChar "h"
[]
~~~~~


Now we could keep doing this, but often to go forward, it is helpful to
step back and take a look at the bigger picture.

Here's the the **type** of a parser

~~~~~{.haskell}
newtype Parser a = P (String -> [(a, String)])
~~~~~

it should remind you of something else, remember this?

~~~~~{.haskell}
type ST a = S (State -> (a, State))
~~~~~

*(drumroll...)*



Parser is A Monad
-----------------

Indeed, a parser, like a state transformer, [is a monad!][2] 
if you squint just the right way. 

We need to define the `return` and `>>=` functions. 

The bind is a bit tricky, but we just saw it above!

~~~~~{.haskell}
:type bindP 
bindP :: Parser a -> (a -> Parser b) -> Parser b
~~~~~

so, we need to suck the `a` values out of the first 
parser and invoke the second parser with them on the 
remaining part of the string.


QUIZ
----

Recall

~~~~~{.haskell}
doParse           :: Parser a -> String -> [(a, String)]
doParse (P p) str = p str
~~~~~

Consider the function `bindP`:

~~~~~{.haskell}
bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP p1 fp2 = P $ \cs -> [(y, cs'') | (x, cs')  <- undefined -- 1 
                                     , (y, cs'') <- undefined -- 2
                          ]
~~~~~

What shall we fill in for the two `undefined` to get the code to typecheck?

a. `p1 cs` and `fp2 x cs`
b. `doParse p1 cs` and `doParse (fp2 x) cs'`
c. `p1 cs` and `fp2 x cs'`
d. `doParse p1 cs` and `doParse (fp2 x) cs`
e. `doParse p1 cs` and `doParse fp2 x cs'`


~~~~~{.haskell}





~~~~~

Indeed, we can define the `bindP` function for `Parser`s as:

\begin{code}
bindP p1 fp2 = P $ \cs -> [(y, cs'') | (x, cs')  <- doParse p1 cs
                                     , (y, cs'') <- doParse (fp2 x) cs']
\end{code}

See how we suck the `a` values out of the first 
parser (by running `doParse`) and invoke the second 
parser on each possible `a` (and the remaining string) 
to obtain the final `b` and remainder string tuples.

The `return` is very simple, we can let the types guide us

~~~~~{.haskell}
:type returnP
returnP :: a -> Parser a
~~~~~

which means we must ignore the input string and just return the input element

\begin{code}
returnP x = P (\cs -> [(x, cs)])
\end{code}

Armed with those, we can officially brand parsers as monads

\begin{code}
instance Monad Parser where
  (>>=)  = bindP
  return = returnP
\end{code}

This is going to make things really sweet...

Parser Combinators
------------------

Since parsers are monads, we can write a bunch of **high-level combinators**
for composing smaller parsers into bigger ones.

For example, we can use our beloved `do` notation to rewrite `pairP` as

\begin{code}
pairP       :: Parser a -> Parser b -> Parser (a, b)
pairP px py = do x <- px
                 y <- py
                 return (x, y)
\end{code}

shockingly, exactly like the `pairs` function [from here](/lectures/monads2.html).

Next, lets flex our monadic parsing muscles and write some new 
parsers. It will be helpful to have a a *failure* parser that 
always goes down in flames, that is, returns `[]` -- **no** 
successful parses.

\begin{code}
failP = P $ const []
\end{code}

Seems a little silly to write the above, but its helpful to build 
up richer parsers like the following which parses a `Char` *if* 
it satisfies a predicate `p`

\begin{code}
satP ::  (Char -> Bool) -> Parser Char
satP p = do c <- oneChar 
            if p c then return c else failP
\end{code}

we can write some simple parsers for particular characters 

\begin{code}
lowercaseP = satP isAsciiLower
\end{code}

~~~~~{.haskell}
ghci> doParse (satP ('h' ==)) "mugatu"
[]

ghci> doParse (satP ('h' ==)) "hello"
[('h',"ello")]
~~~~~

The following parse alphabet and numeric characters respectively

\begin{code}
alphaChar = satP isAlpha
digitChar = satP isDigit
\end{code}

and this little fellow returns the first digit in a string as an `Int`




\begin{code}
digitInt  = do c <- digitChar
               return ((read [c]) :: Int)
\end{code}





which works like so

~~~~~{.haskell}
ghci> doParse digitInt "92"
[(9,"2")]

ghci> doParse digitInt "cat"
[]
~~~~~

Finally, this parser will parse only a particular `Char` passed in as input

\begin{code}
char c = satP (== c)
\end{code}

**EXERCISE:** Write a function `strP :: String -> Parser String` such that
`strP s` parses **exactly** the string `s` and nothing else, that is,

~~~~~{.haskell}
ghci> dogeP = strP "doge"

ghci> doParse dogeP "dogerel"
[("doge", "rel")]

ghci> doParse dogeP "doggoneit"
[]
~~~~~


A Nondeterministic Choice Combinator
------------------------------------

Next, lets write a combinator that takes two sub-parsers and 
**non-deterministically chooses** between them. 

~~~~~{.haskell}
chooseP :: Parser a -> Parser a -> Parser a
~~~~~

That is, we want `chooseP p1 p2` to return a succesful parse
if *either* `p1` or `p2` succeeds. 

We can use `chooseP` to build a parser that returns either 
an alphabet or a numeric character

\begin{code}
alphaNumChar = alphaChar `chooseP` digitChar
\end{code}

After defining the above, we should get something like:

~~~~~{.haskell}
ghci> doParse alphaNumChar "cat"
[('c', "at")]
ghci> doParse alphaNumChar "2cat"
[('2', "cat")]
ghci> doParse alphaNumChar "230"
[('2', "30")]
~~~~~

**QUIZ**

How would we go about encoding **choice** in our parsers? 

~~~~~{.haskell}
-- a 
p1 `chooseP` p2 = do xs <- p1
                     ys <- p2
                     return (x1 ++ x2) 
-- b
p1 `chooseP` p2 = do xs <- p1 
                     case xs of 
                       [] -> p2 
                       _  -> return xs

-- c
p1 `chooseP` p2 = P $ \cs -> doParse p1 cs ++ doParse p2 cs

-- d
p1 `chooseP` p2 = P $ \cs -> case doParse p1 cs of
                               [] -> doParse p2 cs
                               rs -> rs
~~~~~

~~~~~{.haskell}






~~~~~


\begin{code}

chooseP :: Parser a -> Parser a -> Parser a
p1 `chooseP` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)
\end{code}

Thus, what is even nicer is that if *both* parsers succeed, 
you end up with all the results. 

Here's a parser that grabs `n` characters from the input 

\begin{code}
grabn :: Int -> Parser String 
grabn n 
  | n <= 0    = return ""
  | otherwise = do c  <- oneChar  
                   cs <- grabn (n-1)
                   return (c:cs)
\end{code}

**DO IN CLASS** How would you nuke the nasty recursion from `grabn` ?

**QUIZ**

Lets now use our choice combinator to define:

\begin{code}
foo = grabn 2 `chooseP` grabn 4
\end{code}

What does the following evaluate to?

~~~~~{.haskell}
ghci> doParse foo "mickeymouse"
~~~~~

a. `[]`
b. `[("mi","ckeymouse")]`
c. `[("mick","eymouse")]`
d. `[("mi","ckeymouse"),("mick","eymouse")]`
e. `[("mick","eymouse"), ("mi","ckeymouse")]`


~~~~~{.haskell}



~~~~~

and only one result if thats possible

~~~~~{.haskell}
ghci> doParse grab2or4 "mic"
[("mi","c")]

ghci> doParse grab2or4 "m"
[]
~~~~~

Even with the rudimentary parsers we have at our disposal, we can start
doing some rather interesting things. For example, here is a little
calculator. First, we parse the operation

\begin{code}
intOp      = plus `chooseP` minus `chooseP` times `chooseP` divide 
  where 
    plus   = char '+' >> return (+)
    minus  = char '-' >> return (-)
    times  = char '*' >> return (*)
    divide = char '/' >> return div
\end{code}

**DO IN CLASS** 
Can you guess the type of the above parser?


Next, we can parse the expression

\begin{code}
calc = do x  <- digitInt
          op <- intOp
          y  <- digitInt 
          return $ x `op` y
\end{code}

which, when run, will both parse and calculate

~~~~~{.haskell}
ghci> doParse calc "8/2"
[(4,"")]

ghci> doParse calc "8+2cat"
[(10,"cat")]

ghci> doParse calc "8/2cat"
[(4,"cat")]

ghci> doParse calc "8-2cat"
[(6,"cat")]

ghci> doParse calc "8*2cat"
[(16,"cat")]
~~~~~

**QUIZ**

What does the following return:

~~~~~{.haskell}
ghci> doParse calc "99bottles"
~~~~~

a. Type error
b. `[]`
c. `[(9, "9bottles")]`
d. `[(99, "bottles")]`
e. Run-time exception


Recursive Parsing
-----------------

To start parsing interesting things, we need to add recursion 
to our combinators. For example, its all very well to parse 
individual characters (as in `char` above) but it would a lot 
more swell if we could grab particular `String` tokens. 

Lets try to write it! 

~~~~~{.haskell}
string :: String -> Parser String
string ""     = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)
~~~~~

**DO IN CLASS**
Ewww! Is that explicit recursion ?! Lets try again (can you spot the pattern)

\begin{code}
string :: String -> Parser String
string = undefined -- fill this in
\end{code}


Much better!

~~~~~{.haskell}
ghci> doParse (string "mic") "mickeyMouse"
[("mic","keyMouse")]

ghci> doParse (string "mic") "donald duck"
[]
~~~~~

Ok, I guess that wasn't really recursive then after all! 

Lets try again.

Lets write a combinator that takes a parser `p` that 
returns an `a` and returns a parser that returns *many* 
`a` values. That is, it keeps grabbing as many `a` values 
as it can and returns them as a `[a]`.

\begin{code}
manyP     :: Parser a -> Parser [a]
manyP p   = many1 `chooseP` many0 
  where 
    many0 = return []
    many1 = do x  <- p
               xs <- manyP p
               return (x:xs)
\end{code}

But beware! The above can yield *many* results

~~~~~{.haskell}
ghci> doParse (manyP digitInt) "123a" 
[([], "123a"), ([1], "23a"),([1, 2], "3a"),([1, 2, 3], "a")]
~~~~~

which is simply all the possible ways to extract sequences 
of integers from the input string.

Deterministic Maximal Parsing
-----------------------------

Often we want a single result, not a set of results. For example,
the more intuitive behavior of `many` would be to return the maximal
sequence of elements and not *all* the prefixes.

To do so, we need a *deterministic* choice combinator

\begin{code}
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case doParse (p1 `chooseP` p2) cs of
                         []  -> []
                         x:_ -> [x]
\end{code}

The above runs choice parser but returns only the first result. 
Now, we can revisit the `manyP` combinator and ensure that it 
returns a single, maximal sequence

\begin{code}
mmanyP     :: Parser a -> Parser [a]
mmanyP p   = mmany1 <|> mmany0
  where 
    mmany0 = return []
    mmany1 = do x  <- p
                xs <- mmanyP p
                return (x:xs)
\end{code}

**DO IN CLASS** 
Wait a minute! What exactly is the difference between the above and 
the original `manyP`? How do you explain this:

~~~~~{.haskell}
ghci> doParse (manyP digitInt) "123a" 
[([1,2,3],"a"),([1,2],"3a"),([1],"23a"),([],"123a")]

ghci> doParse (mmanyP digitInt) "123a" 
[([1,2,3],"a")]
~~~~~

Lets use the above to write a parser that will return an entire integer
(not just a single digit.)



~~~~~{.haskell}
oneInt :: Parser Integer
oneInt = do xs <- mmanyP digitChar 
            return $ ((read xs) :: Integer)
~~~~~

*Aside*, can you spot the pattern above? We took the 
parser `mmanyP digitChar` and simply converted its output
using the `read` function. This is a recurring theme, and 
the type of what we did gives us a clue

~~~~~{.haskell}
(a -> b) -> Parser a -> Parser b
~~~~~

Aha! a lot like `map`. Indeed, there is a generalized version
of `map` that we have seen before (`lift1`) and we bottle up 
the pattern by declaring `Parser` to be an instance of the 
`Functor` typeclass

\begin{code}
instance Functor Parser where
  fmap f p = do x <- p
                return (f x)
\end{code}

after which we can rewrite

\begin{code}
oneInt ::  Parser Int
oneInt = read `fmap` mmanyP digitChar 
\end{code}

Lets take it for a spin

~~~~~{.haskell}
ghci> doParse oneInt "123a"
[(123, "a")]
~~~~~


Parsing Arithmetic Expressions
------------------------------


Lets use the above to build a small calculator, that
parses and evaluates arithmetic expressions. In essence, 
an expression is either binary operand applied to two 
sub-expressions or an integer. We can state this as

\begin{code}
calc0      ::  Parser Int
calc0      = binExp <|> oneInt 
  where 
    binExp = do x <- oneInt
                o <- intOp 
                y <- calc0 
                return $ x `o` y
\end{code}

This works pretty well!

~~~~~{.haskell}
ghci> doParse calc0 "1+2+33"
[(36,"")]

ghci> doParse calc0 "11+22-33"
[(0,"")]
~~~~~

but things get a bit strange with minus

~~~~~{.haskell}
ghci> doParse calc0 "11+22-33+45"
[(-45,"")]
~~~~~

Huh? Well, if you look back at the code, you'll realize the 
above was parsed as

~~~~~{.haskell}
11 + ( 22 - (33 + 45))
~~~~~

because in each `binExp` we require the left operand to be 
an integer. In other words, we are assuming that each 
operator is *right associative* hence the above result. 


I wonder if we can try to fix it just by flipping the order

\begin{code}
calc1      ::  Parser Int
calc1      = binExp <|> oneInt 
  where 
    binExp = do x <- calc1 
                o <- intOp 
                y <- oneInt
                return $ x `o` y
\end{code}

**QUIZ**

What does the following evaluate to?

~~~~~{.haskell}
ghci> doParse calc1 "11+22-33+45"
~~~~~

a. `[( 11 , "+22-33+45")]`
b. `[( 33 , "-33+45")]`
c. `[( 0, "+45")]`
d. `[( 45 , "")]`
e. None of the above 



~~~~~{.haskell}






~~~~~

Indeed, there is a bug here ... can you figure it out? 

**Hint:** what will the following return?

~~~~~{.haskell}
ghci> doParse calc1 "2+2"
~~~~~

Even worse, we have no precedence, and so

~~~~~{.haskell}
ghci> doParse calc0 "10*2+100"
[(1020,"")]
~~~~~

as the string is parsed as

~~~~~{.haskell}
10 * (2 + 100)
~~~~~





Precedence
----------

We can add both associativity and precedence, by stratifying the 
parser into different levels. Here, lets split our operations 
into addition- 

\begin{code}
addOp       = plus `chooseP` minus 
  where 
    plus    = char '+' >> return (+)
    minus   = char '-' >> return (-)
\end{code}

and multiplication-precedence.

\begin{code}
mulOp       = times `chooseP` divide 
  where 
    times   = char '*' >> return (*)
    divide  = char '/' >> return div
\end{code}

Now, we can stratify our language into (mutually recursive) sub-languages, 
where each top-level expression is parsed as a **sum-of-products** 

\begin{code}
sumE     = addE <|> prodE 
  where 
    addE = do x <- prodE 
              o <- addOp
              y <- sumE 
              return $ x `o` y

prodE    = mulE <|> factorE
  where 
    mulE = do x <- factorE
              o <- mulOp
              y <- prodE 
              return $ x `o` y

factorE = parenP sumE <|> oneInt
\end{code}

We can run this 

~~~~~{.haskell}
ghci> doParse sumE "10*2+100"
[(120,"")]

ghci> doParse sumE "10*(2+100)"
[(1020,"")]
~~~~~

Do you understand why the first parse returned `120` ?
What would happen if we *swapped* the order of `prodE`
and `sumE` in the body of `addE` (or `factorE` and `prodE` 
in the body of `prodE`) ? Why?

**QUIZ**

Recall that in the above,

~~~~~{.haskell}
factorE :: Parser Int
factorE = parenP sumE <|> oneInt
~~~~~

What is the type of `parenP` ?

a. `Parser Int`
b. `Parser a -> Parser a`
c. `a -> Parser a`
d. `Parser a -> a`
e. `Parser Int -> Parser a` 

~~~~~{.haskell}




~~~~~

Lets write `parenP`

\begin{code}
parenP p = do char '(' 
              x <- p
              char ')'
              return x
\end{code}

Parsing Pattern: Chaining
-------------------------

There is not much point gloating about combinators if we are 
going to write code like the above -- the bodies  of `sumE` 
and `prodE` are almost identical!

Lets take a closer look at them. In essence, a `sumE` is 
of the form

~~~~~{.haskell}
prodE + < prodE + < prodE + ... < prodE >>>
~~~~~

that is, we keep chaining together `prodE` values and 
adding them for as long as we can. Similarly a `prodE` 
is of the form

~~~~~{.haskell}
factorE * < factorE * < factorE * ... < factorE >>>
~~~~~

where we keep chaining `factorE` values and multiplying 
them for as long as we can. There is something unpleasant 
about the above: the addition operators are right-associative

~~~~~{.haskell}
ghci> doParse sumE "10-1-1"
[(10,"")]
~~~~~

Ugh! I hope you understand why: its because the above was 
parsed as `10 - (1 - 1)` (right associative) and not
`(10 - 1) - 1` (left associative). You might be tempted 
to fix that simply by flipping the order of `prodE` and 
`sumE`

~~~~~{.haskell}
sumE     = addE <|> prodE 
  where 
    addE = do x <- sumE 
              o <- addOp
              y <- prodE 
              return $ x `o` y
~~~~~

but this would prove disastrous. Can you see why? 

The  parser for `sumE` directly (recursively) calls itself 
**without consuming any input!** Thus, it goes off the deep 
end and never comes back. Instead, we want to make sure we 
keep consuming `prodE` values and adding them up (rather 
like fold) and so we could do

\begin{code}
sumE1       = prodE1 >>= addE1
  where 
    addE1 x = grab x <|> return x
    grab  x = do o <- addOp
                 y <- prodE1 
                 addE1 $ x `o` y

prodE1      = factorE1 >>= mulE1 
  where 
    mulE1 x = grab x <|> return x
    grab  x = do o <- mulOp
                 y <- factorE1 
                 mulE1 $ x `o` y

factorE1 = parenP sumE1 <|> oneInt
\end{code}

It is easy to check that the above is indeed left associative.

~~~~~{.haskell}
ghci> doParse sumE1 "10-1-1"
[(8,"")]
~~~~~

and it is also very easy to spot and bottle the chaining computation
pattern: the only differences are the *base* parser 
(`prodE1` vs `factorE1`) and the binary operation (`addOp` vs `mulOp`).
We simply make those parameters to our *chain-left* combinator

\begin{code}
p `chainl` pop = p >>= rest
   where 
     rest x = grab x <|> return x 
     grab x = do o <- pop
                 y <- p
                 rest $ x `o` y 
\end{code}

after which we can rewrite the grammar in three lines

\begin{code}
sumE2    = prodE2   `chainl` addOp
prodE2   = factorE2 `chainl` mulOp
factorE2 = parenP sumE2 <|> oneInt 
\end{code}

~~~~~{.haskell}
ghci> doParse sumE2 "10-1-1"
[(8,"")]

ghci> doParse sumE2 "10*2+1"
[(21,"")]

ghci> doParse sumE2 "10+2*1"
[(12,"")]
~~~~~

That concludes our in-class exploration of monadic parsing. 
This is merely the tip of the iceberg. Though parsing is a 
very old problem, and has been studied since the dawn of 
computing, we saw how monads bring a fresh perspective 
which have recently been transferred from Haskell to 
[many other languages][3]. 
There have been several exciting [recent][4] [papers][5] 
on the subject, that you can explore on your own. 
Finally, Haskell comes with several parser combinator 
libraries including [Parsec][3] which you will play 
around with in [HW2](/homeworks/Hw2.html). 

[2]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[3]: http://www.haskell.org/haskellwiki/Parsec
[4]: http://www.cse.chalmers.se/~nad/publications/danielsson-parser-combinators.html
[5]: http://portal.acm.org/citation.cfm?doid=1706299.1706347
λ> 