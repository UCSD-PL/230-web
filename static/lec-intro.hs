#!/usr/bin/env runhaskell
module Lec3 where

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


--------------------------------------------------------------------------------

-- | Get first element of list
--
-- >>> firstElem [1,2,3]
-- 1
--
-- >>> firstElem ["cat", "dog"]
-- "cat"

firstElem (x:_) = x

-- firstElem ["cat", "dog", "cat"]






--
-- >>> firstElem []
-- ???



--------------------------------------------------------------------------------

-- | Clone a value multiple times
--
-- >>> clone 4 "cat"
-- ["cat", "cat", "cat", "cat"]
--
-- >>> clone 3 'a'
-- "aaa"
--
-- >>> clone 1 3.14
-- [3.14]

-- >>> clone 0 3.14
-- []

clone :: Int -> t -> [t]
clone 0 x = []
clone n x = x : clone (n-1) x

(***) :: Int -> t -> [t]
(***) 0 x = []
(***) n x = x : (***) (n-1) x


--------------------------------------------------------------------------------

-- | Add up the elements of a list
--
-- >>> listAdd [1,2,3,4]
-- 10

listAdd :: [Int] -> Int
listAdd []    = 0
listAdd (h:t) = h + listAdd t



--------------------------------------------------------------------------------

-- | Generate the values between lo and hi
--
-- >>> range 0 5
-- [0, 1, 2, 3, 4]

range :: Int -> Int -> [Int]
range lo hi   = ite (lo >= hi) [] (lo : rest)
  where
    lo'       = lo + 1
    rest      = range lo' hi

--------------------------------------------------------------------------------

-- | An If-Then-Else function (!)

-- >>> ifThenElse (1 < 2) "cat" "dog"
-- "cat"
--
-- >>> ifThenElse (10 < 2) "cat" "dog"
-- "dog"

ite :: Bool -> t -> t -> t
ite True  x _ = x
ite False _ x = x

{-

function range(lo, hi){
  return ite(lo >= hi, [], cons(lo, range(lo+1, hi)));
}

function ite(cond, x, y) {
   return cond ? x : y;
}

function cons(x, xs) {
  var ys = xs;
  ys.push(x);
  return ys;
}


-}









--------------------------------------------------------------------------------

-- | Rewrite @range@ , @clone@ using @ite@

range' = undefined

clone' = undefined


--------------------------------------------------------------------------------

-- | Can we write such an @ite@ in JavaScript ?

{-

function ite(b, e1, e2){
  return b ? e1 : e2;
}

function clone(n, x){
  ite(n > 0, tail.push(x), new Array());
}

-}





--------------------------------------------------------------------------------
-- | Appending two lists

-- >>> append [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]


append :: [a] -> [a] -> [a]
append = undefined



type Pos    = Double
type Radius = Double
data CircleT = Circle Pos Pos Radius
               deriving (Eq, Show)
type Size    = Double
data SquareT = Square Pos Pos Size
               deriving (Eq, Show)

data ShapeT = C CircleT
            | S SquareT deriving (Eq, Show)


area :: ShapeT -> Double
area (C c) = areaCircle c
area (S s) = areaSquare s

areaSquare :: SquareT -> Double
areaSquare (Square _ _ s) = s * s

areaCircle :: CircleT -> Double
areaCircle (Circle _ _ r) = pi * r * r


--------------------------------------------------------------------------------
-- | Creating Types ------------------------------------------------------------
--------------------------------------------------------------------------------

data Shape  = C XPos YPos Radius
            | S XPos YPos Side

type XPos = Double
type YPos = Double
type Side = Double
type Radius = Double

area :: Shape -> Double
area (S _ _ side) = side * side
area (C _ _ radius) = pi * radius * radius







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

--------------------------------------------------------------------------------
-- | IO
--------------------------------------------------------------------------------

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
