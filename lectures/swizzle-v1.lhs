
> import System
> import Data.Maybe
> import Data.Char

Swizzling One Character
-----------------------

First make a lookup table or *association list* of characters; each
element is a list is a pair of the char and its swizzled version. Its
easy enough to add caps as well, but note how we maintain readability
with the `where`.

> code :: [(Char, Char)]
> code = smalls ++ caps 
>   where smalls   = zip ['a' .. 'z'] shuffles 
>         caps     = zip ['A' .. 'Z'] (map toUpper shuffles) 
>         shuffles = "thequickbrownfxjmpsdvlazyg"

Now, the pattern matching with `case` is nice, but an expert knows how 
to wield the standard library. In the old code, we have a *default* `Char`
and the `lookup` will return a `Maybe Char` and we want to output the
latter if it is not `Nothing`. In other words, we require a function of
type `Char -> Maybe Char -> Char` and [Hoogle](http://haskell.org/hoogle) 
is your friend.

> swizzleChar :: Char -> Char
> swizzleChar c = fromMaybe c (lookup c code) 

Swizzling One Line
------------------

To swizzle one line, we will swizzle each character on the line. You should
be able to spot this as a `map`.

> swizzleLine :: String -> String
> swizzleLine = map swizzleChar

Swizzling Many Lines
--------------------

To swizzle a file, we will first reverse the lines in the file and 
then swizzle each line of the file. 

~~~~~{.haskell}
swizzleContent fileString = unlines (swizzleLines (reverse (lines fileString)))
~~~~~

That looks a bit ugly doesn't it. Since we are just gluing (ie pipelining) 
together three operations, the intermediate variables are somewhat
pointless, indeed, we can eliminate them altogether. Recall that 
the infix *compose* function

~~~~~{.haskell}
(.) f g x = f (g x)
~~~~~

Now, you see why compose is useful, we can define 

~~~~~{.haskell}
swizzleContent fileString = (unlines . swizzleLines . reverse . lines) fileString
~~~~~

and further simplify it by eliminating the parameter altogether (via
partial application)

~~~~~{.haskell}
swizzleContent = unlines . swizzleLines . reverse . lines
~~~~~

Now, of course, the helper `swizzleLines` is merely a `map` over `swizzleLine`

~~~~~{.haskell}
swizzleLines = map swizzleLine
~~~~~

Think of `map` as a magic wand that enables a function (eg `swizzleLine`) 
to operate over lists. Thus, we can just use equational reasoning and boil
`swizzleContent` down to

> swizzleContent = unlines . map swizzleLine . reverse . lines

I defy you to describe `swizzleContent` more elegantly than that! 

Doing the IO
------------

Of course, its all very well to manipulate strings, but in the end,
the rubber must hit the road. The next function takes a filename as
input and returns an action that corresponds to the swizzling of the 
file.

> swizzleFile :: FilePath -> IO ()
> swizzleFile f = do d  <- readFile f
>                    writeFile (f ++ ".swz") (swizzleContent d) 

Hmm, it would be nice to be able to swizzle many files at one shot.
Can you find a way to use patterns to eliminate the 
explicit recursion altogether?

~~~~~{.haskell}
swizzleFiles :: [FilePath] -> IO ()
swizzleFiles []     = return ()
swizzleFiles (f:fs) = do swizzleFile f 
                         swizzleFiles fs
~~~~~

Actually, there are *two* different things that are intermingled in the
above code. First, we are creating the individual actions for each file.
Second, we are stitching together the individual actions. 
So we can rewrite the code as

~~~~~{.haskell}
swizzleFiles :: [FilePath] -> IO ()
swizzleFiles fs = fuseActions (map swizzleFile fs) 
~~~~~

Now, this is still improvable. Never rewrite when 
you can reuse! Recall that

~~~~~{.haskell}
fuseActions :: [IO a] -> IO ()
~~~~~

When you query [Hoogle][1] with that type you find the (aptly named) 
library function `sequence_` that does exactly the same thing! So.

~~~~~{.haskell}
swizzleFiles fs = sequence_ (map swizzleFile fs) 
~~~~~

And by now, you should be able to spot the compose pattern (by the ugly
parens!) So really the code is just

> swizzleFiles = sequence_ . map swizzleFile

Finally, not much more to do here

> main = do files <- getArgs 
>           swizzleFiles files

Note that except for the very end, the code is completely pure. 
Merely by inspecting the function's type we can know that it 
doesnt so much as breathe on the filesystem. 

Now of course, there is a gigantic problem, we have code for 
swizzling, but not unswizzling!! [Lets see](swizzle-v2.html) how to fix that.


[1]: http://haskell.org/hoogle "Hoogle"
