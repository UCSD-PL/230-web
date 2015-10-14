\begin{code}
import System.Environment
import Data.Maybe
import Data.Char
\end{code}

\begin{code}
code = smalls ++ caps 
  where 
    smalls   = zip ['a' .. 'z'] shuffles 
    caps     = zip ['A' .. 'Z'] (map toUpper shuffles) 
    shuffles = "thequickbrownfxjmpsdvlazyg"
\end{code}

First, we will just make two operations for swizzling and unswizzling
characters.

\begin{code}
txChar kvs c  = fromMaybe c (lookup c kvs)
swizzleChar   = txChar code 
unswizzleChar = txChar [(y,x) | (x,y) <- code]
\end{code}

To (un/)swizzle one line, we will (un/)swizzle each character 
on the line. We simply make actual operation a parameter. 

\begin{code}
txLine op     = map op 
\end{code}

To swizzle a file, we will first reverse the lines in the file and 
then swizzle each line of the file. 

\begin{code}
txContent op  = unlines . map (txLine op) . reverse . lines
\end{code}

To actually transform a file

\begin{code}
txFile op f   = do d  <- readFile f
                   writeFile (f ++ ".swz") (txContent op d) 
\end{code}

And to transform many files

\begin{code}
txFiles op = sequence_ . map (txFile op)
\end{code}

Finally, at the very top, the command line args tell us whether to
swizzle or unswizzle

\begin{code}
main = do args <- getArgs 
          case args of 
            ("-s":files) -> txFiles swizzleChar files
            ("-u":files) -> txFiles unswizzleChar files
            (_)          -> putStrLn "usage: swizzle -s file1 file2 ... OR swizzle -u file1 file2 ..."
\end{code}



[1]: http://haskell.org/hoogle "Hoogle"
