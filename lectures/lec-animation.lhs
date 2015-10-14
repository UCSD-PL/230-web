---
title: Functional Animations
---

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import SOE hiding (Region)
> import qualified SOE as G (Region)
> import System.Environment (getArgs)
> import Shape
> import Draw
> import Picture

-------------------------------------------------------------

Graphics Recap
==============

Representing Regions
--------------------

Recall the `Shape` type from a few lectures back (here we'll
just use the definitions in the SOE library.)


~~~~~{.haskell}
data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
     deriving Show

type Radius = Float
type Side   = Float
type Vertex = (Float,Float)
~~~~~

We can create specific shape values like 

> s1 = Rectangle 3 2
> s2 = Ellipse 1 1.5
> s3 = RtTriangle 3 2
> s4 = Polygon [ (-2.5, 2.5)
>              , (-3  ,   0)
>              , (-1.7,-1.0)
>              , (-1.1, 0.2)
>              , (-1.5, 2.0) ]


We can convert a `Shape` into a more general `Region` 
that contains atomic shapes, which can be operated on 
in various ways.

~~~~~{.haskell}
data Region 
  = Shape Shape		          -- primitive shape
  | Translate Vector Region   -- translated region
  | Scale     Vector Region   -- scaled region
  | Complement Region	      -- inverse of region
  | Region `Union` Region     -- union of regions
  | Region `Intersect` Region -- intersection of regions
  | Region `Xor` Region       -- XOR of regions
  | Empty                     -- empty region
  deriving Show
~~~~~

We can convert our basic shapes to atomic regions

> r1 = Shape s1
> r2 = Shape s2
> r3 = Shape s3
> r4 = Shape s4

and combine the atomic regions 

> reg1  = r3 `Union` (r1 `Intersect` (Complement r2 `Union` r4)) 


Representing Pictures
---------------------

Finally, we have a `Picture` type which comprises colored regions,
which can be layered over each other

~~~~~{.haskell}
data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
     deriving Show
~~~~~

Here is an atomic picture

> pic1  = Region Red reg1

Here's another region

> r5   = Shape $ Rectangle 1 1
> r6   = Shape $ Ellipse 0.5 0.5
> reg2 = (Scale (2,2) r6) `Union` 
>        (Translate (2,1) r6) `Union` 
>        (Translate (-2,0) r5)

which we color yellow

> pic2 = Region Yellow reg2

and we can layer the two pictures over each other

> pic3 = pic2 `Over` pic1

Drawing Pictures
----------------

We can render a picture in a window via the function `draw` 

> main1 = draw "Picture 1" pic1

Lets render the other pictures 

> main2 = draw "Picture 2" pic2

and 

> main3 = draw "Picture 3" $ pic3 

From Pictures To Animations
===========================

So far, our pictures have been *static*, nothing moves. 

<img src="../static/Animexample.png" width=100 align="middle"/> 
<img src="../static/Animexample.png" width=100 align="middle"/> 

Suppose we wanted to create animations. The standard trick 
for how moving images work is by exploiting *persistence 
of vision* wherein an illusion of motion is created by
rapidly rendering a [*series* of images][1]. This seems 
like an inherently *imperative* activity

0. initialize image
1. render image
2. pause
3. change image
4. go to step 1.

However, this is hideously low-level: it is hard to *compose* 
animations from simpler ones, hard to *reuse* animations, and
generally rather brittle. Instead, we will take a radically 
different view, where we will represent an animation as a 
purely functional value. How? A type is worth a thousand pictures...

> type Animation a = Time -> a
> type Time        = Float

That is, an animation is *a function* that tells us, at 
each time instant, what the image to be rendered at that 
instant is!


Lets write a simple `Shape` animation...

> rubberBall :: Animation Shape
> rubberBall = \t -> Ellipse (sin t) (cos t)

or `Region` animations

> revolvingBall :: Animation Region
> revolvingBall = \t -> Translate (sin t, cos t) ball 
>   where ball  = Shape (Ellipse 0.2 0.2)

or `Picture` animations

> planets ::  Animation Picture
> planets t = p1 `Over` p2
>   where p1 = Region Red    $ Shape (rubberBall t) 
>         p2 = Region Yellow $ revolvingBall t


or just plain old `String` animations

> ticker :: Animation String
> ticker t = "The time is :" ++ show t

How do we render all these animations? Well, all we need
is a function that renders graphics, that is, takes a 
window name, an `Animation Graphic` and returns an 
action (the rendering.)

> animate :: String -> Animation Graphic -> IO ()

with this, rendering is trivial. First, lets create an
appropriate animation

> blueBall ::  Animation Graphic
> blueBall = withColor Blue . shapeToGraphic . rubberBall









The above type looks a bit surprising, but lets slowly 
step through the expression.

~~~~~{.haskell}
withColor Blue :: Graphic -> Graphic
shapeToGraphic :: Shape -> Graphic
~~~~~

and so 

~~~~~{.haskell}
withColor Blue . shapeToGraphic :: Shape -> Graphic
~~~~~

Finally, 

~~~~~{.haskell}
rubberBall :: Time -> Shape 
~~~~~

and so

~~~~~{.haskell}
withColor Blue . shapeToGraphic . rubberBall :: Time -> Graphic
~~~~~

Now, lets run some animations! 

> main4 = animate "Shape"   $ blueBall -- withColor Blue . shapeToGraphic . rubberBall 

> main5 = animate "Text"    $ text (100,200) . ticker 

> main6 = animate "Region"  $ withColor Yellow . regionToGraphic . revolvingBall

> main7 = animate "Picture" $ picToGraphic . planets

A Brief Digression: `$` and `.`
-------------------------------

~~~~~{.haskell}
($) :: (a -> b) -> a -> b
($) f x = f x


foo x = bar (baz (goo (moo (poo x))))

foo   = bar . baz . goo . moo . poo 

foo x = bar (baz x)

foo   = bar . baz


(.) ::  (b -> c) -> (a -> b) -> (a -> c)
(.) f g = \x -> f(g(x))

foo x = bar (baz x)
foo x = bar $ baz x
foo   = bar . baz

incr x = x + 1

incr $ 5 -- OK
incr . 5 -- BAD

foo (bar (baz (goo)))

foo $ bar $ baz $ goo

foo (bar)

foo $ bar

($) f x = f x
~~~~~


A DSL for Animations
====================

We can build animations up at a low-level, by constructing functions
from `Time` to `Graphic` but this is a bit ugly. Next, we will use 
type classes to design some generic operators for animations, which
will allow us to write animations much more easily and smoothly.

When we are done, we will be able to create very rich animations, 
from very simple ones, by elegantly *composing* the animations, 
thereby allowing us *reuse* sub-animations and animation 
combinators in a safe and flexible way.

1. We start with a trivial behavior which is a stationary object.

> demo1 = animateB "1" $ reg yellow $ ballB

2. Next, we can simply *translate* a picture behavior with an 
   xy-coordinate behavior thereby giving us a shifted animation.

> demo2 = animateB "2" $ reg yellow $ bounce $ ballB

> bounce = tx (0, sin time)

3. We can trivially *reuse* the bouncing mechanism, by 
   translating a different object with the same rotation 
   behavior.

> demo3 = animateB "3" $ reg red $ bounce $ pentaB

4. We can think of the color itself as a behavior and if 
   we replace `red` with `flash` then the rendered image's 
   color now changes dynamically!

> demo4 = animateB "4" $ reg flash $ tx (0, sin time) ballB 

5. Next, we can layer multiple animations by simply putting 
   them *over* each other.

> demo5 = animateB "5" $ a1 `over` a2 
>   where a1 = reg red    $ tx (0, sin time) ballB
>         a2 = reg yellow $ tx (sin time, 0) pentaB

6. We can design a rotation combinator to rotate each 
   individual element

> demo6 = animateB "6" $ a1 `over` a2 
>   where a1 = reg red    $ tx (0, sin time) ballB
>         a2 = reg yellow $ lift2 turn (pi * sin time) pentaB


7. We can even think of *time as a behavior* and can manipulate it. 
   For example, here is an animation layered with itself `2` 
   seconds in the future,

> demo7 = animateB "7" $ a1 `over` a2 
>   where a1 = reg red $ tx (sin time, cos time) ballB
>         a2 = timeTx (2 + time) a1 

8. Or we can speed up time (fast-forward) 

> demo8 = animateB "8" $ a1 `over` a2 
>   where a1 = reg red $ tx (sin time, cos time) ballB
>         a2 = timeTx (2 * time) a1 

9. or slow it down (slow-motion)

> demo9 = animateB "9" $ a1 `over` a2 `over` a3 
>   where a1 = reg red $ tx (sin time, cos time) ballB
>         a2 = timeTx (2 * time) a1 
>         a3 = timeTx (0.5 * time) a1 

10. or go backwards in time (rewind) 

> demo10 = animateB "10" $ a1 `over` a2 `over` a3 `over` a4 
>   where a1 = reg red $ tx (sin time, cos time) ballB
>         a2 = timeTx (2 * time) a1 
>         a3 = timeTx (0.5 * time) a1 
>         a4 = timeTx (-1 * time) a1 

Pretty sweet! You can imagine a variety of other combinators for
manipulating animations. Seems pretty magical doesn't it? It turns
out, its not hard, once you put on the Haskell goggles. 

Our *strategy* for creating the DSL for animations, will be to

- make behaviors a first-class value, by defining a special 
  type for behaviors, and

- defining a variety of numeric and graphical operations 
  over behaviors by making the above type an *instance* 
  of various classes which define the numerical and 
  graphical operations.

A Type For Behaviors
--------------------

First, lets define a *newtype* which is basically a *wrapper* 
around the old animation type. 

> newtype Behavior a = Beh (Time -> a)

There are several reasons for doing this. 

1. The typeclass instantiation is not very happy with type 
   synonyms (we just saw an example, but that was after 
   pleading with haskell via some command line options.) 
   There are rather technical reasons for this, which we 
   will avoid going into right now.

2. More importantly, we will want to *hide* the actual 
   implementation of behaviors from clients (ie make the 
   definition *private*), so that they can *only* be 
   constructed using the high-level operations that we 
   provide. To use the `BST` example, we would want to 
   hide the implementation of the `BST` to prevent a 
   client from creating a *raw* `BST` that does not 
   satisfy the binary-search-ordered invariant.

Now, we can easily define an animation function for this 
new behavior type.

> animateB ::  String -> Behavior Picture -> IO ()
> animateB s (Beh f) = animate s (picToGraphic. f)


















Next, lets look at how to build our DSL for writing
`Behavior Picture` values.


Lifting Operations To Behaviors
-------------------------------

Remember the good old `map` function? It showed how to convert 
a function from `a` to `b` into one from `[a]` to `[b]`. We say 
that `map` *lifts* a function from `a` to `b` into one from 
`[a]` to `[b]`.

The notion of *lifting* is quite general (and has been studied 
formally.) Rather than getting too abstract, lets just replace 
`[]` above with `Behavior` and `map` with `lift1` and we can 
imagine functions like


> lift1 :: (a -> b) -> Behavior a -> Behavior b
> lift1 f (Beh a) = Beh $ \t -> f (a t)

thus, `lift1` lifts 1-ary functions similarly, we can lift 
2-ary functions and 3-ary functions

> lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
> lift2 f (Beh a) (Beh b) = Beh $ \t -> f (a t) (b t)
>
> lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
> lift3 f (Beh a) (Beh b) (Beh c) = Beh $ \t -> f (a t) (b t) (c t)


What do you think `lift0` might do? Yes, it just "lifts" 
a constant value into a behaviour that returns that value
at each instant in time.

> lift0 :: a -> Behavior a
> lift0 x = Beh $ \_ -> x

and a function that lifts a function over a list.

> liftXs ::  ([t] -> a) -> [Behavior t] -> Behavior a
> liftXs f bs = Beh (\t -> f [b t | Beh b <- bs])

Now, we will make `Behavior` an instance of `Num` by simply
lifting the various operations over `a` to `Behavior a` 
(whenever `Num a` holds.) 

> instance Num a => Num (Behavior a) where
>   (+)         = lift2 (+)
>   (*)         = lift2 (*)
>   negate      = lift1 negate
>   abs         = lift1 abs
>   signum      = lift1 signum
>   fromInteger = lift0 . fromInteger

We can do the same for operations defined over `Fractional` 
and `Floating` values.

> instance Fractional a => Fractional (Behavior a) where
>   (/) = lift2 (/)
>   fromRational = lift0 . fromRational

> instance Floating a => Floating (Behavior a) where
>   pi    = lift0 pi
>   sqrt  = lift1 sqrt
>   exp   = lift1 exp
>   log   = lift1 log
>   sin   = lift1 sin
>   cos   = lift1 cos
>   tan   = lift1 tan
>   asin  = lift1 asin
>   acos  = lift1 acos
>   atan  = lift1 atan
>   sinh  = lift1 sinh
>   cosh  = lift1 cosh
>   tanh  = lift1 tanh
>   asinh = lift1 asinh
>   acosh = lift1 acosh
>   atanh = lift1 atanh

As a result of the above, the operations that we perform on 
floating point numbers (eg adding them, multiplying them, 
taking their `sin`) can be lifted to *behaviors* on floating
point numbers. For example, if we define the behavior

> time :: Behavior Time
> time = Beh $ \t -> t

then `sin time` is the behavior

~~~~~{.haskell}
sin time == {- unfold overloading for time, sin -} 
            lift1 sin (\t -> t) 
         == {- unfold definition of lift1 -}
            \t -> sin ((\t -> t) t) 
         == {- unfold the anonymous functions -}
            \t -> sin t 
~~~~~

Similarly, we can lift the basic shape and color constructors
to `Behavior`

> reg   ::  Behavior Color -> Behavior Region -> Behavior Picture
> reg   = lift2 Region

> shape ::  Behavior Shape -> Behavior Region
> shape = lift1 Shape

> poly  ::  [Behavior Vertex] -> Behavior Shape
> poly  = liftXs Polygon

> ell   ::  Behavior Radius -> Behavior Radius -> Behavior Shape
> ell   = lift2 Ellipse

> red, yellow, green, blue ::  Behavior Color
> red    = lift0 Red
> yellow = lift0 Yellow
> green  = lift0 Green 
> blue   = lift0 Blue

> tx :: (Behavior Float, Behavior Float) -> Behavior Region -> Behavior Region
> tx (Beh a1, Beh a2) (Beh r)
>        = Beh (\t -> Translate (a1 t, a2 t) (r t))

Now, we can define a simple ball and pentagon behavior

> ballB    = shape $ ell 0.2 0.2

> triB     = shape $ poly (map lift0 vs)
>   where vs = [ ( 0.0,  0.8)
>              , ( 0.3, -0.5)
>              , (-0.3, -0.5) 
>              ]


> pentaB   = shape $ poly (map lift0 vs) 
>   where vs = [ ( 0.0,  0.8)
>              , ( 0.5,  0.2)
>              , ( 0.3, -0.5)
>              , (-0.3, -0.5) 
>              , (-0.5,  0.2)
>              ]

we can define a function that takes any `Behavior Region` 
and makes it go around in a circle

> revolveRegion = tx (sin time, cos time) 

after which we get the spinning behaviors

> revBallB  = revolveRegion ballB
> revPentaB = revolveRegion pentaB


~~~~~{.haskell}
reg :: Behavior Color -> Behavior Region -> Behavior Picture
~~~~~

defined above, takes a `Behavior Color` and a `Behavior Region` 
and returns a `Behavior Picture` which is the colored version 
of the region, thereby allowing us build a simple *atomic* animation

> anim1 = animateB "Yellow Revolving Sphere" $ reg yellow revBallB

Next, we can define a *multiplexer* or *conditional* combinator 
for behaviors like so

> cond :: Behavior Bool -> Behavior a -> Behavior a -> Behavior a
> cond = lift3 $ \b x y -> if b then x else y 

Huh? Simple. We made a *function* of three arguments that
uses the first argument to choose between the second or third,
and simply hoist it up to work over behaviors!

We can similarly lift primitive comparison operators up
into `Behavior` land

> (>*) = lift2 (>)
> (<*) = lift2 (<)

Now, we can use these combinators to define different
`Behavior Bool` values, and hence, get different 
`Behavior Color` values

> flash   = cond (cos time >* 0) red yellow
> flash'  = cond (cos time >* 0) green blue

which we can drop into our old animation to get different color 
schemes!

> anim2 = animateB "Revolving Sphere" $ reg flash   revBallB
> anim3 = animateB "Revolving Sphere" $ reg flash'  revBallB

Indeed, we can conditionally compose the two flashing colors, 
to get a four-way flash

> flash'' = cond (sin time >* 0) flash flash' 
> anim4 = animateB "Revolving Sphere" $ reg flash'' revBallB


New Typeclasses For Graphical Operations
----------------------------------------

Next, lets see how we can *layer* behaviors. To do so, lets 
define a typeclass that supports layering

> class Combine a where
>   empty :: a
>   over  :: a -> a -> a

We can layer *multiple* values using the combinator

> overMany = foldr over empty

**NOTE:** the type for `overMany`, behold automatic 
propagation at work! We can make `Picture` and `Behavior` 
instances of `Combine`

> instance Combine Picture where
>   empty = EmptyPic
>   over  = Over

> instance Combine a => Combine (Behavior a) where
>   empty = lift0 empty
>   over  = lift2 over

The names are quite self-explanatory. By virtue of the above,
we can use `overMany` to layer multiple `Picture` and `Behavior`
values

> anim5 = animateB "Many Spheres" $ overMany [b1,b2,b3]
>   where b1 = reg flash   $ tx ((sin time)-1, cos time) ballB
>         b2 = reg flash'  $ tx ((sin time)+1, cos time) ballB
>         b3 = reg flash'' $ tx (2 * sin time, cos time) pentaB

Similarly, we can define a richer class of operations on Shapes
and lift them to behaviors

> class Deformable a where
>   turn    :: Float -> a -> a
>   stretch :: Float -> a -> a 

that is, we can turn (rotate) and stretch (resize) by some amount.
For now, lets just define these for `Polygon`s

> instance Deformable Shape where
>   turn theta (Polygon ps) = Polygon (map (rotate theta) ps)
>   stretch x  (Polygon ps) = Polygon (map (\(a, b) -> (a*x, b*x)) ps)
>
> instance Deformable Region where
>   turn theta (Shape sh) = Shape (turn theta sh)
>   stretch x  (Shape sh) = Shape (stretch x  sh)

where the helper function `rotate` is defined as

> rotate :: Float -> Coordinate -> Coordinate
> rotate theta (x,y) = (x*c + y*s, y*c - x*s) 
>     where (s, c) = (sin theta, cos theta)

We can lift the above operations to `Picture`

> instance Deformable Picture where
>   turn theta (Region c r)   = Region c (turn theta r)
>   turn theta (p1 `Over` p2) = turn theta p1 `Over` turn theta p2
>   turn theta EmptyPic       = EmptyPic
>   stretch x (Region c r)    = Region c (stretch x r)
>   stretch x (p1 `Over` p2)  = stretch x p1 `Over` stretch x p2
>   stretch x EmptyPic        = EmptyPic

and then finally, to `Behavior`

> instance Deformable a => Deformable (Behavior a) where
>   turn theta (Beh b) = Beh (\t -> turn theta (b t))
>   stretch x  (Beh b) = Beh (\t -> stretch x (b t))

Now, we can pull even neater effects from our DSL

> anim6 = animateB "Pendulum" $ swirly pentaB -- triB 

> swirly = reg flash'' 
>        . lift2 stretch (1.2 + sin time) 
>        . lift2 turn (pi * sin time) 


Time Travel Via Time Transformation 
-----------------------------------

As one last trick, let us see how we do *time travel* as it were, 
by using the following function, that translates a behavior 
*in time*.

> timeTx (Beh tb) (Beh b) = Beh (b . tb)

   
Lets look at the type of this function, it takes a `Behavior Time` 
and any other `Behavior a`, and returns a new `Behavior a` which 
is the old one translated according to the first signal! That is,
at any given instant `t` the time behavior is used to look up 
a `t'` from which the new behavior draws its value. This can 
really provide for some interesting effects.


For example, to *fast-forward* an animation, we write a combinator 

> fastForward = timeTx (2 * time)

to watch it in *slow-motion*

> slowMotion  = timeTx (0.5 * time)

and to *rewind* an animation, we might do

> rewind      = timeTx (-1  * time)

Check it out!

> anim7 = animateB "Many Spheres" $ overMany [b1,b2,b3,b4]
>   where b1  = swirly triB 
>         b2  = rewind b1 
>         b3  = fastForward b1 
>         b4  = slowMotion b1 

Of course, we can apply the time transformation to the 
individual behaviors, for example

> anim8 = animateB "Fast Flash" $ overMany [b1,b2]
>   where b1 = reg (fflash 4) $ tx ((sin time)-1, cos time) ballB
>         b2 = reg (fflash 8) $ tx ((sin time)+1, cos time) ballB

where `fflash` is `flash` sped up by some factor

> fflash n = timeTx (n * time) flash''

And one last example

> anim9 = animateB "Last!" $ overMany (map stagger [0..7])
>   where stagger = \t -> timeTx (lift0 (t * pi/4) + time) bB
>         bB      = reg (fflash 8) revBallB


Thats all for lecture, see the detailed implementation of a 
Kaleidoscope animation given in SOE.

Details
=======

See the book for more info about these helper functions.


> main = do
>   [n] <- getArgs
>   case n of
>     "main1"  -> main1
>     "main2"  -> main2
>     "main3"  -> main3
>     "main4"  -> main4
>     "main5"  -> main5
>     "main6"  -> main6
>     "main7"  -> main7
>     "demo1"  -> demo1
>     "demo2"  -> demo2
>     "demo3"  -> demo3
>     "demo4"  -> demo4
>     "demo5"  -> demo5
>     "demo6"  -> demo6
>     "demo7"  -> demo7
>     "demo8"  -> demo8
>     "demo9"  -> demo9
>     "demo10" -> demo10
>     "anim1"  -> anim1
>     "anim2"  -> anim2
>     "anim3"  -> anim3
>     "anim4"  -> anim4
>     "anim5"  -> anim5
>     "anim6"  -> anim6
>     "anim7"  -> anim7
>     "anim8"  -> anim8
>     "anim9"  -> anim9
>     _        -> putStrLn "Eh, don't understand that command!"


Dummy Instances for `Eq` and `Show`
-----------------------------------

We need to make `Behavior a` an instance of `Eq` and `Show` 
in order that it to satisfy the requirements of `Num`.
Of course these operations don't really make sense so 
we'll just make a dummy instantiation.

> instance Eq (Behavior a) where
>   a1 == a2 = error "Can't compare behaviors."
>
> instance Show (Behavior a)  where
>   showsPrec n a1 = error "<< Behavior >>"
>




Conversion Between Graphic Values
---------------------------------

> regionToGraphic :: Region -> Graphic
> regionToGraphic = drawRegion . regionToGRegion

> picToGraphic :: Picture -> Graphic
> picToGraphic (Region c r)
>   = withColor c (regionToGraphic r)
> picToGraphic (p1 `Over` p2)
>   = picToGraphic p1 `overGraphic` picToGraphic p2
> picToGraphic EmptyPic 
>   = emptyGraphic


Rendering 
---------

> animate title anim = runGraphics $ do
>   w  <- openWindowEx title (Just (0,0)) (Just (xWin, yWin)) drawBufferedGraphic 
>   t0 <- timeGetTime
>   animateLoop w t0 anim


> animateLoop w t0 anim = do 
>   t <- timeGetTime
>   let ft = intToFloat (fromInteger (toInteger (t - t0))) / 1000
>   setGraphic w (anim ft)
>   spaceCloseEx w $ animateLoop w t0 anim


> spaceCloseEx w loop = do 
>   k <- maybeGetWindowEvent w
>   case k of
>     Just (Key c d) | c == ' ' && d -> closeWindow w
>     Just Closed                    -> closeWindow w
>     Nothing                        -> loop
>     _                              -> spaceCloseEx w loop



[1]: http://en.wikipedia.org/wiki/Animation "Wikipedia: Animation"
