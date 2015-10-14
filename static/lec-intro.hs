#!/usr/bin/env runhaskell

{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--diff"           @-}

import Prelude hiding (sum)

e1 :: Double  
e1 = 31 * (42 + 56)
       
e2 = 3 * (4.2 + 5.6) :: Double

e3 :: Bool
e3 = True

e4 :: ((Int, Double), Bool)
e4 = ((7, 5.2), True)



zero :: Int
zero = 0

x = 12

pos   :: Integer -> Bool 
pos x = x > 0

-- pos 12 == 12 > 0 == True

pat3 :: Int -> Int -> Int -> Int
pat3 x y z = x * (y + z )

zog = pat3 2 10


pat :: (Int, Int, Int) -> Int
pat (x, y, z) = x * (y + z)

{- 
   mys ("cat", "dog", 49)   ======> 49
   mys (1, 2, 3)            ======> 3
   mys (True, False, "bob") ======> "bob"
-}

-- PATTERN MATCHING

mys           :: (a, b, c) -> c 
mys (x, y, z) = z





e5 = ['1', '2', 'c']

e6 :: [[Double]]
e6 = [[1], [2, 3], [4.2]]

emp = []

e7 :: [Int]
e7 = 1 : emp

e8 :: [String]
e8 = "cat" : emp

e9 :: [Double]
e9 = 4.9 : emp 


cons2          :: a -> a -> [a] -> [a] 
cons2 x1 x2 xs = x1 : x2 : xs 



-- firstElem [1,2,3]                ===> 1
-- firstElem ["cat", "dog", "muse"]          ===> "cat"
-- firstElem ["cat", "dog", "goose", "muse"] ===> "cat"

firstElem (x:_) = x 









-- | A recursive function

-- HEREHERE

-- >>> clone 0 'a'
-- []

-- >>> clone 1 'a'
-- ['a']

-- >>> clone 2 'a'
-- ['a','a']

-- >>> clone 3 'a'
-- ['a','a','a']

clone         :: Int -> thing -> [thing] 
clone n x     = ite (n > 0) (x : clone (n-1) x) [] 

 -- if n > 0
 --               then x : clone (n-1) x
 --               else []

bob1 = if 1 > 2              then "cat"    else "mouse"
bob2 = if [1,2,3] == 1:2:3:4:[] then "hellow" else "world"

ite True  e1 _ = e1
ite False _ e2 = e2


{- 

function ite(b, e1, e2){
  return b ? e1 : e2;
}

function clone(n, x){
  ite(n > 0, tail.push(x), new Array());
}

-}
{- 







               
clone n x   = if n == 0 
                then [] 
                else x : (clone (n-1) x)

--}

-- | Cleaner, with "guards"

clone' n x 
  | n > 0     = x : clone' (n-1) x
  | otherwise = [] 

-- | Cleaner, with "pattern matching"

clone'' x 0 = []
clone'' x n = x : clone'' x (n-1)



-- append :: [a] -> [a] -> [a]
-- append [1,2,3] [4,5,6] =========> [1,2,3,4,5,6]




-- | An If-Then-Else function

ifThenElse True thenExpr elseExpr  = thenExpr
ifThenElse False thenExpr elseExpr = elseExpr



clone''' x n = ifThenElse (n == 0) [] (x : clone''' x (n-1)) 


-- if True then 1 else error "DIE DEI DIE"
--     ===> 1
--
-- In JavaScript:
--
-- function ifThenElse(cond, thenB, elseB) { 
--   return cond ? thenB : elseB ;
-- }
-- var z = ifThenElse(true, 1, alert("DIE DIE DIE"));


range lo hi 
  | lo <= hi  = lo : rest
  | otherwise = []
  where
    rest      = range (lo+1) hi




-- | Generate the elements in a `range lo hi`

-- >>> range 1 5
-- [1,2,3,4,5]

-- >>> range 5 1
-- []


-- range :: ???












range lo hi 
  | lo > hi   = []
  | otherwise = lo : range (lo + 1) hi





-- | `sum` the elements of a list

-- >>> sum []
-- 0

-- >>> sum [1]
-- 1

-- >>> sum [1,2,3]
-- 6

-- sum :: ???










sum []     = 0
sum (x:xs) = x + sum xs






-------------------------------------------------------------
-- | Creating Types -----------------------------------------
-------------------------------------------------------------

data Shape  = C XPos YPos Radius 
            | S XPos YPos Side

type XPos = Double
type YPos = Double
type Side = Double
type Radius = Double
 
area :: Shape -> Double
area (S _ _ side) = side * side
area (C _ _ radius) = pi * radius * radius


-- ADDS TWO NUMBERS!!!
docrazymath x y  = x `undefined` y

-- "READS THE USERS INPUT"
getUserInput x y = undefined











-- data Circle = C (Double, Double, Double)
-- C :: (Double, Double, Double) -> Circle
-- radius (C (_,_,r)) = r 
-- C (0,1,55)
--
--
--
--
--
-- data Circle = C Double Double Double
-- data Square = S Double Double Double

-- areaCircle (C x y r) = pi * r * r
-- areaSquare (S x y d) = d * d




-- data [a] = [] 
--          | (:) a [a]


-- data Circle = Kanye Double Double Double
-- Circle :: Double -> Double -> Double -> Circle


-- data Shape  = Circle Double Double Double
--             | Square Double Double Double
--             | Poly [(Double, Double)]
--             deriving (Show)





{- 

area (Circle _ _ r)    = pi * r * r
area (Square _ _ d)    = d * d
-- area (Poly ps)      = areaPoly ps

areaPoly (p1:p2:p3:rest) = areaTriangle p1 p2 p3 + areaPoly (p1:p3:rest)
areaPoly _               = 0


areaTriangle = undefined -- fill in as exercise

c0 = Circle 0 0 2.3
c1 = Circle 0 1 1.31
s0 = Square 12 1 12312
p0 = Poly   [(0,0), (4,4), (92, 92)]

-}

-- | IO


main = putStrLn "Hello World!"




-- main = do putStrLn "What is your name ?"
--           n <- getLine
--           putStrLn ("Happy New Year " ++ n)


-- | Lets break it down. These just *actions*, not executed.
          
act1 = putStrLn "This is a string on a line"
act2 = putStrLn "This is another string on a line"
act3 = putStrLn "This is the last string i promise you"

-- | acts is just a LIST-OF actions (which is NOT an action)

acts = [act1, act2, act3]

-- | bigAct is the result of COMPOSING the actions

bigAct = do act1
            act2
            act3

      
act :: IO ()
act = do putStrLn "Hey? " 
         name <- getLine 
         putStrLn ("Hello " ++ name)

