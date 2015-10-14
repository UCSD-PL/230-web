\begin{code}
import System.Environment
import Data.Maybe 
\end{code}

Swizzling One Character
-----------------------

First make a lookup table or *association list* of characters; each
element is a list is a pair of the char and its swizzled version.

That is, we want a list that looks like:

\begin{spec}
  [ ('a', 't')
  , ('b', 'h')
  , ('c', 'e')
  , ('d', 'q')
  , ('e', 'u')
  , ...
  ]
\end{spec}


How do we create it?

\begin{code}
makePairs                 :: [a] -> [b] -> [(a, b)]
makePairs [] []           = [] 
makePairs (c:cs) (cc:ccs) = (c, cc) : makePairs cs ccs 
\end{code}

\begin{code}
code :: [(Char, Char)]

code = makePairs ['a' .. 'z'] "thequickbrownfxjmpsdvlazyg"
\end{code} 

Now, we can use the `code` to translate a **single** character.


So that, 

\begin{spec}
swizzleChar  'a' code == 't'     -- 'a' encoded as 't'
swizzleChar  'b' code == 'h'     -- 'b' encoded as 'h'
swizzleChar  'λ' code == 'λ'     -- non-lower case encoded as itself.
\end{spec}
 
\begin{code}
findInCode                :: Char -> [(Char, Char)] -> Char
findInCode c []           = c
findInCode c ((k,v) : kvs)
  | c == k                = v
  | otherwise             = findInCode c kvs

swizzleChar c = findInCode c code
\end{code}

Lets try to rewrite `findInCode` so that it doesn't just use `Char`.




Representing Failure 
--------------------

It is common for functions to be *partial* by 

+ failing 
+ being undefined 

on some inputs. 

Instead of throwing an **exception** the more Haskelly solution is:

\begin{spec}
data Maybe a = Nothing    -- failure, no value  
             | Just a     -- success, with a value
\end{spec}

Lets rewrite the above recursive `swizzleChar` to use `Maybe`

\begin{code}
findInList x kvs = undefined
\end{code}


**QUIZ** What is the type of `findInList` ?

A. `Char -> [(Char, v)] -> v`
B. `Char -> [(Char, v)] -> Maybe v`
C. `k -> [(k, v)] -> Maybe v`
D. `k -> [(k, v)] -> v`
E. `v -> k -> [(k, v)] -> v`



Now lets rewrite `swizzleChar` with `findInList`

\begin{code}
swizzleChar' c code = findInList c code 
\end{code}


**EXERCISE**: Can you think of a simple way to check that each (lower case) 
character is in fact mapped to a distinct character, ie that there
are no collisions?



Swizzling One Line
------------------

To swizzle one line, we will swizzle each character on the line.

\begin{code}
swizzleLine        :: String -> String
swizzleLine []     = []
swizzleLine (c:cs) = swizzleChar c : swizzleLine cs
\end{code}

Swizzling Many Lines
--------------------

To swizzle a file, we will first reverse the lines in the file and then 
swizzle each line of the file. 

\begin{code}
swizzleContent :: String -> String
swizzleContent fileString = 
  let fileLines        = lines fileString
      fileLinesRev     = reverse fileLines
      fileLinesRevSwiz = swizzleLines fileLinesRev
      fileStringSwiz   = unlines fileLinesRevSwiz
  in fileStringSwiz
\end{code}

where the auxiliary function `swizzleLines` 
simply swizzles the content of each line.

\begin{code}
swizzleLines        :: [String] -> [String]
swizzleLines []     = []
swizzleLines (l:ls) = (swizzleLine l) : (swizzleLines ls)
\end{code}

Doing the IO
------------

Of course, its all very well to manipulate strings, but in the end,
the rubber must hit the road. The next function takes a filename as
input and returns an action that corresponds to the swizzling of the 
file.

\begin{code}
swizzleFile :: FilePath -> IO ()
swizzleFile f = do d <- readFile f
                   writeFile (f ++ ".swz") (swizzleContent d) 
\end{code}


Hmm, it would be nice to be able to swizzle many files at one shot.

\begin{code}
swizzleFiles :: [FilePath] -> IO ()
swizzleFiles []     = return ()
swizzleFiles (f:fs) = do swizzleFile f 
                         swizzleFiles fs
\end{code}

Finally, we put it all together.

\begin{code}
main = do files <- getArgs 
          swizzleFiles files
\end{code}

Note that except for the very end, the code is completely pure. 
Merely by inspecting the function's type we can know that it 
doesnt so much as breathe on the filesystem. However, as we will 
see, this code is full of superfluous recursion. In the [next
version](swizzle-v1.html) we will eliminate the recursion by
spotting and applying the right computation patterns.

