> pos :: Integer -> Bool 
> pos x = x > 0

> pat :: (Int, Int, Int) -> Int
> pat (x, y, z) = x * (y + z )

A recursive function

> clone :: a -> Int -> [a]
> clone x n = ifThenElse (n == 0) [] (x : clone x (n-1)) 

Cleaner, with "pattern matching"

> clone' x 0 = []
> clone' x n = x : clone x (n-1)

An If-Then-Else function

> ifThenElse True thenExpr elseExpr  = thenExpr
> ifThenElse False thenExpr elseExpr = elseExpr

if True then 1 else error "DIE DEI DIE"
    ===> 1

In JavaScript:

function ifThenElse(cond, thenB, elseB) { 
  return cond ? thenB : elseB ;

ifThenElse(true, 1, alert("DIE DIE DIE"));

> rangeIf lo hi = if lo > hi 
>                   then [] 
>                   else lo : (rangeIf (lo+1) hi)

> range lo hi 
>   | lo > hi   = []
>   | otherwise = lo : range (lo + 1) hi


> listAdd        :: [Int] -> Int
> listAdd []     = 0
> listAdd (x:xs) = x + listAdd xs



data Circle = C Double Double Double
data Square = S Double Double Double
areaCircle (C x y r) = pi * r * r
areaSquare (S x y d) = d * d


> data Shape  = Circle Double Double Double
>             | Square Double Double Double
>             | Poly [(Double, Double)]
>             deriving (Show)

> area (Circle _ _ r)    = pi * r * r
> area (Square _ _ d)    = d * d
> area (Poly ps)         = areaPoly ps
> 
> areaPoly (p1:p2:p3:rest) = areaTriangle p1 p2 p3 + areaPoly (p1:p3:rest)
> areaPoly _               = 0


> areaTriangle = undefined

> c0 = Circle 0 0 2.3
> c1 = Circle 0 1 1.31
> s0 = Square 12 1 12312
> p0 = Poly   [(0,0), (4,4), (92, 92)]



> act1 = putStrLn "This is a string on a line"
> act2 = putStrLn "This is another string on a line"
> act3 = putStrLn "This is the last string i promise you"









main = do putStrLn "What is your name ?"
          n <- getLine
          putStrLn ("Happy New Year " ++ n)
