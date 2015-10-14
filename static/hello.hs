#!/usr/bin/env runhaskell

main = do putStrLn "What is your name ?"
          n <- getLine
          putStrLn ("Happy New Year " ++ n)


-- main =  putStrLn "Hello World!"


megaAct = do act1
             act2
             act3

act1 = putStrLn "This is a string on a line"
act2 = putStrLn "This is another string on a line"
act3 = putStrLn "This is the last string i promise you"


