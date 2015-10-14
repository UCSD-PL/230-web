% CSE 230: Winter 2013
% Functional Animations 
% Ranjit Jhala, UC San Diego 

## Plan

- Graphics Recap (#graphics-recap)

- From Pictures To Animations (#from-pictures-to-animations)

- A DSL For Animations (#a-dsl-for-animations)

- Time Travel (#time-travel)

# Graphics Recap 

## SOE Graphics Recap

We use a 3-layered representation for pictures:

1. `Shape`   : *Rectangle*, *Ellipse*, *Polygon* etc.

2. `Region`  : Compose many `Shape`s

3. `Picture` : `Region` plus *color*

## SOE Graphics Recap

Recall the `Shape` type 

~~~~~{.haskell}
data Shape 
  = Rectangle Side Side
  | Ellipse Radius Radius
  | RtTriangle Side Side
  | Polygon [Vertex]
  deriving Show

type Radius = Float
type Side   = Float
type Vertex = (Float,Float)
~~~~~

## 1. Create Specific `Shape`

~~~~~{.haskell}
s1 = Rectangle 3 2
s2 = Ellipse 1 1.5
s3 = RtTriangle 3 2
s4 = Polygon [ (-2.5, 2.5 )
             , (-3  , 0   )
             , (-1.7, -1.0)
             , (-1.1, 0.2 )
             , (-1.5, 2.0 ) ]
~~~~~


## 2. `Region`s Combine Multiple `Shape`s

A General `Region` combines `Shape` in different ways

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

## 2. `Region`s Combine Multiple `Shape`s

<br>

We can turn a `Shape` into a basic `Region`

~~~~~{.haskell}
r1 = Shape s1
r2 = Shape s2
r3 = Shape s3
r4 = Shape s4
~~~~~

<br>

Or directly 

~~~~~{.haskell}
[r1, r2, r3, r4] = map Shape [s1, s2, s3, s4]
~~~~~

## 2. `Region`s Combine Multiple `Shape`s

<br>

And combine the atomic regions 

~~~~~{.haskell}
reg1  = r3 `Union` (r1 `Intersect` (Complement r2 `Union` r4)) 
~~~~~



## 3. Color `Region` to get a `Picture`

~~~~~{.haskell}
data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
     deriving Show
~~~~~

### An atomic picture

~~~~~{.haskell}
pic1  = Region Red reg1
~~~~~

## 3. Color `Region` to get a `Picture`

~~~~~{.haskell}
data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
     deriving Show
~~~~~

### A compound picture

First take a region 

~~~~~{.haskell}
reg2 = (Scale (2,2) r6) `Union` (Translate (2,1) r6) `Union` (Translate (-2,0) r5)
  where r5   = Shape $ Rectangle 1 1
        r6   = Shape $ Ellipse 0.5 0.5
~~~~~

and color it Yellow

~~~~~{.haskell}
pic2 = Region Yellow reg2
~~~~~

## 3. Color `Region` to get a `Picture`

We can also **layer** pictures to get complex images

~~~~~{.haskell}
pic3 = pic2 `Over` pic1
~~~~~

## Drawing Pictures

Finally, the function `draw` renders the pictures on the screen

~~~~~{.haskell}
main1 = draw "Picture 1" pic1
main2 = draw "Picture 2" pic2
main3 = draw "Picture 3" pic3 
~~~~~

**Exercise:** Can anyone *guess* the type of `draw` ?


# From Pictures To Animations

## From Pictures To Animations

So far, our `Picture`s are **static**, nothing **moves**

<img src="../static/Animexample.png" width=100 align="middle"/> 

## From Pictures To Animations

How to create **motion** ?

> 0. **Initialize** image
> 1. **Render** image
> 2. **Pause**
> 3. **Update** image
> 4. Go to step 1.

> Seems **super imperative** ... **ICKY-ALERT**

## Functional Animations

> 1. **Represent** `Animation` as a *purely functional value*
> 2. **Render** `Animation` via `IO`

## Representing Animations 

We represent `Animation` as a **purely functional value**

~~~~~{.haskell}
type Animation a = Time -> a
~~~~~

<br>

where `Time` is just a type alias
           
~~~~~{.haskell}
type Time        = Float
~~~~~

## Representing Animations 

`Animation` is a **more accurate** description ...

~~~~~{.haskell}
type Animation a = Time -> a
~~~~~

...of real animations!

<img src="../static/Animexample.png" width=100 align="middle"/> 

# `Animation` is Polymorphic

## We Can Animate `Shape`

~~~~~{.haskell}
rubberBall :: Animation Shape
rubberBall = \t -> Ellipse (sin t) (cos t)
~~~~~

or ...

## We Can Animate `Region`

~~~~~{.haskell}
revolvingBall :: Animation Region
revolvingBall = \t -> Translate (sin t, cos t) ball 
  where ball  = Shape (Ellipse 0.2 0.2)
~~~~~

or ...

## We Can Animate `Picture`

~~~~~{.haskell}
planets ::  Animation Picture
planets t = p1 `Over` p2
  where p1 = Region Red    $ Shape (rubberBall t) 
        p2 = Region Yellow $ revolvingBall t
~~~~~

(**Hey** whats with that funny `$` ?)

## We Can Animate ... `String`

Or just boring old `String` values

~~~~~{.haskell}
ticker :: Animation String
ticker t = "The time is :" ++ show t
~~~~~

or just plain old `String` animations

> ticker :: Animation String
> ticker t = "The time is :" ++ show t

## Rendering Animations

Of course, need `IO` to **see** the `Animation` !

~~~~~{.haskell}
animate :: String -> Animation Graphic -> IO ()
~~~~~

## Rendering Animations

Of course, need `IO` to **see** the `Animation` !

~~~~~{.haskell}
blueBall ::  Animation Graphic
blueBall = withColor Blue . shapeToGraphic . rubberBall 
~~~~~

where the helper functions are

~~~~~{.haskell}
withColor      :: Color -> Graphic -> Graphic
shapeToGraphic :: Shape -> Graphic
~~~~~

> Lets slowly step through the code for `blueBall` ...

## Rendering Animations

Of course, need `IO` to **see** the `Animation` !

~~~~~{.haskell}
main4 = animate "Shape" blueBall 

main5 = animate "Text"    $ text (100,200) . ticker 

main6 = animate "Region"  $ withColor Yellow . regionToGraphic . revolvingBall

main7 = animate "Picture" $ picToGraphic . planets
~~~~~

**Remember**  Our old friends `.` and `$`

# A DSL For Animations

## A DSL For Animations

Those animations were pretty, but very **low level**

> - Low-level were writing functions `\t -> ...`

> - Very hard to **reuse** simple animations 

## A DSL For Animations

Lets design high-level operators for fancy animations...

> - ... by **composing** simple animations

> - ... allowing **safe reuse** of sub-animations 

## A DSL For Animations

> 1. First, lets see examples

> 2. Then, we'll look at the implementation 

## DSL Examples: A Stationary Object

A **stationary** object

~~~~~{.haskell}
demo1 = animateB "1" $ reg yellow $ ballB
~~~~~

## DSL Examples: Mashup With Motion

Lets **mashup** `tx` with an XY-coordinate behavior ...

~~~~~{.haskell}
demo2 = animateB "2" $ reg yellow $ tx (0, sin time) ballB
~~~~~

<br>

> - `translate` original `ballB` animation using XY animation

> - **Result** ?

## DSL Examples: Refactor Bouncing

Lets pull out the  **bouncing** behavior ...

~~~~~{.haskell}
bounce = tx (0, sin time)
~~~~~

<br>

and rewrite the `demo2`

~~~~~{.haskell}
demo2  = animateB "2" $ reg yellow $ bounce ballB
~~~~~

## DSL Examples: Reuse Bouncing


Lets **reuse** the  **bouncing** behavior ...

~~~~~{.haskell}
demo3  = animateB "3" $ reg red $ bounce pentaB
~~~~~

<br>

... with a different object!


## DSL Examples: Lets Get Flashy!

**Color** itself can be a **behavior** 

> can change over time ...

## DSL Examples: Lets Get Flashy!

**Color** itself can be a **behavior** 

> Can change over time ...

> Why be a boring `red` all the time?

~~~~~{.haskell}
demo4 = animateB "4" $ reg flash $ bounce ballB 
~~~~~

## DSL Examples: Lets Mix Animations

We can **layer** multiple animations ...

~~~~~{.haskell}
demo5  = animateB "5" $ a1 `over` a2 
  where 
    a1 = reg red      $ tx (0, sin time) ballB
    a2 = reg yellow   $ tx (sin time, 0) pentaB
~~~~~

## DSL Examples: Lets Rotate Animations

An operator that will `turn` things around

~~~~~{.haskell}
demo6  = animateB "6" $ a1 `over` a2 
  where 
    a1 = reg red    $ bounce ballB
    a2 = reg yellow $ lift2 turn (pi * sin time) pentaB
~~~~~

# Now, the coolest part, **time** is an **animation** ...

## DSL Examples: Lets Rotate Animations

An animation layered with **itself**, 2 sec in the **future**

~~~~~{.haskell}
demo7  = animateB "7" $ a1 `over` a2 
  where 
    a1 = reg red $ tx (sin time, cos time) ballB
    a2 = timeTx (2 + time) a1 
~~~~~

**Ex:** Can you **generalize** this into a `meAndMyFutureN` function?


# Now, the coolest part, **time** is an **animation** ...

## DSL Examples: Fast-Forwarding Animations

Just make `time` move extra fast (*"on the double"*)

~~~~~{.haskell}
demo8  = animateB "8" $ a1 `over` a2 
  where
    a1 = reg red $ tx (sin time, cos time) ballB
    a2 = timeTx (2 * time) a1 
~~~~~

> Can anyone **guess** the next slide?

## DSL Examples: Slow-Motion Animations

Just make `time` move extra **slow**

~~~~~{.haskell}
demo9  = animateB "9" $ a1 `over` a2 `over` a3 
  where 
    a1 = reg red $ tx (sin time, cos time) ballB
    a2 = timeTx (2 * time) a1 
    a3 = timeTx (0.5 * time) a1 
~~~~~

> Can anyone **guess** the *next* slide?

> My Mom used to say ...

## DSL Examples: Rewind

~~~~~{.haskell}
demo10 = animateB "10" $ a1 `over` a2 `over` a3 `over` a4 
  where 
    a1 = reg red $ tx (sin time, cos time) ballB
    a2 = timeTx (2 * time) a1 
    a3 = timeTx (0.5 * time) a1 
    a4 = timeTx (-1 * time) a1 
~~~~~


## A DSL For Animations

1. **Examples**

2. **Implementation**

> Next, lets see the *implementation*


## Animation DSL Implementation Strategy

> 1. Make `Behavior` a first class **value** (via a type)

> 2. Define `Behavior` **operators** 


## A Type For Behaviors

We use the `newtype` mechanism ...

~~~~~{.haskell}
newtype Behavior a = Beh (Time -> a)
~~~~~

> Many reasons:

> 1. Cannot define typeclass **instance** for **raw** function types
> 2. Want to **hide** implementation from users
> 3. Zero overhead, wrapper exists only at compile-time

## A Type For Behaviors

We use the `newtype` mechanism ...

~~~~~{.haskell}
newtype Behavior a = Beh (Time -> a)
~~~~~

To animate, use:

~~~~~{.haskell}
animateB ::  String -> Behavior Picture -> IO ()
animateB s (Beh f) = animate s (picToGraphic. f)
~~~~~


## Operators For Behaviors

Remember our old friend 

~~~~~{.haskell}
map :: (a -> b) -> [a] -> [b]
~~~~~

<br>

**Very** general idea, called **lifting**  lets write...

~~~~~{.haskell}
lift1 :: (a -> b) -> Beh a -> Beh b
~~~~~


## Operators For Behaviors

Can generalize this to **multiple** arguments...

~~~~~{.haskell}
lift2 :: (a -> b -> c) 
      -> Behavior a 
      -> Behavior b 
      -> Behavior c

lift3 :: (a -> b -> c -> d) 
      -> Behavior a 
      -> Behavior b 
      -> Behavior c 
      -> Behavior d
~~~~~

## Operators For Behaviors

Can generalize this to **many** arguments...

~~~~~{.haskell}
liftXs ::  ([t] -> a) -> [Behavior t] -> Behavior a
~~~~~

## Operators For Behaviors

> `liftX` lets to **hoist** operators to **Behaviors**

> So, we can make `Behavior` a `Num` (!)

## Operators For Behaviors

`liftX` lets us make `Behavior` a `Num`

~~~~~{.haskell}
instance Num a => Num (Behavior a) where
  (+)         = lift2 (+)
  (*)         = lift2 (*)
  negate      = lift1 negate
  abs         = lift1 abs
  signum      = lift1 signum
  fromInteger = lift0 . fromInteger
~~~~~

## Operators For Behaviors

`liftX` lets us make `Behavior` and `Fractional`

~~~~~{.haskell}
instance Fractional a => Fractional (Behavior a) where
  (/) = lift2 (/)
  fromRational = lift0 . fromRational
~~~~~

## Operators For Behaviors

`liftX` lets us make `Behavior` and `Floating`

~~~~~{.haskell}
instance Floating a => Floating (Behavior a) where
  pi    = lift0 pi
  sqrt  = lift1 sqrt
  exp   = lift1 exp
  log   = lift1 log
  sin   = lift1 sin
  cos   = lift1 cos
  tan   = lift1 tan
  asin  = lift1 asin
  acos  = lift1 acos
  ...
~~~~~

## Operators For Behaviors

So, we define the behavior `time` like

~~~~~{.haskell}
time :: Behavior Time
time = Beh $ \t -> t
~~~~~

and the instances let us do things like


> - `2 + time`
> - `2 * time`
> - `0.5 * time`

## Operators For Behaviors

Or even `sin time` ... !

~~~~~{.haskell}
sin time == {- unfold overloading for time, sin -} 
            lift1 sin (\t -> t) 
         == {- unfold definition of lift1 -}
            \t -> sin ((\t -> t) t) 
         == {- unfold the anonymous functions -}
            \t -> sin t 
~~~~~





## Operators For Behaviors

Can generalize this to **zero** arguments...

> What would be the type of `lift0` ?

## Operators For Behaviors

Can generalize this to **zero** arguments...

... **lift** a constant value into a `Behavior`

~~~~~{.haskell}
lift0 :: a -> Behavior a
~~~~~


















**Very** general idea, called **lifting**  lets write...

~~~~~{.haskell}
lift1 :: (a -> b) -> Beh a -> Beh b
~~~~~



























~~~~~{.haskell}
class Functor t where
  fmap :: (a -> b) -> t a -> t b
~~~~~

Here `t` is a **type constructor** (e.g. `[]` or `Maybe` or `Behavior`)




## Operators For Behaviors

We have predefined instances for various types:

~~~~~{.haskell}
ghci> :i Functor

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe -- Defined in `Data.Maybe'
instance Functor [] -- Defined in `GHC.Base'
instance Functor IO -- Defined in `GHC.Base'
~~~~~




