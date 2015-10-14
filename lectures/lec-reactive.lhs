---
title: Reactive Programming 
---

> import Data.Char
> import SOE hiding (Region, Event)
> import qualified SOE as G (Region, Event)

> import Animation (picToGraphic)
> import Shape
> import Picture
> import Memo1
> import Draw (xWin,yWin,intToFloat)
> import Control.Concurrent.Chan
> import System.Environment  (getArgs)
>
> infixr 1 =>>, ->>
> infixr 1 `untilB`, `switch`, `stepAccum`, `step`
> infixl 0 .|.
> infixr 4 <*, >*
> infixr 3 &&*
> infixr 2 ||*

Last time, we saw how to do *animation* in a functional style, 
where the meat of the animation was captured as a *behavior* which
was essentially a function from each time step to some output `a`.
We built up rich animations by composing different behaviors with 
an expressive set of combinators. Today, we will extend that model
to include *user actions* in a style known as *functional reactive
programming(FRP)*. 

Typically, reactive programs (eg GUIs) are written in an 
*event-driven* style where each user event triggers some 
function (eg a callback.) For example, in AJAX client-side 
programs one sees things like `onClick(...)` or 
`onMouseOver(...)` where the ellipses specify the 
callbacks that are triggered when particular user actions occur. 
However, such programs are inherently against the Haskell, 
functional style, and, more importantly, they are rather 
difficult to read and reason about because the flow of 
control leaps about all over the place. Its a bit like 
programming with `goto` !

Lets see how a bit of abstraction can help.

The basic idea is simple: in addition to the *continuous* 
`Behavior` type we will introduce a *discrete* `Event` type
that will be a stream of *actions* (user generated or otherwise.)

Our programs will simple compose behaviours and events into one 
giant behavior that will be rendered by the function

> reAct s b =  reactimate s $ lift1 picToGraphic b

Warmup: Drawing Pictures Via Behaviors
--------------------------------------

Ok, as a warm up, lets draw a simple animation with a 
circling red ball. 

> ball	= ell 0.2 0.2

Recall the function `translate` which takes a pair of float behaviors
and a picture behavior and generates a behavior where the picture is moved
to the relevant co-ordinates at each time step.

> circ  = translate (cos time, sin time) ball 

Finally, we can paint the circle with a color behavior and draw it on the
screen.

> main0 = reAct "0" $ paint red circ

Recall that this `red` is not just the color `Red` but the color
*lifted* into behaviors, that is, a function that returns red for
each time step.

Toggling Colors Via Event Sampling 
----------------------------------

Now, lets get the user involved. Suppose that I want a color
behavior where the color is red until the left mouse button 
is clicked, at which point, the color should turn blue. 
With our combinators we would write

> main1 = reAct "1" $ paint redBlue circ   
> redBlue = red `untilB` (lbp ->> blue)

Theres a lot going on here! First, lets look at

~~~~~{.haskell}
lbp :: Event ()
~~~~~

The name `lbp` represents an `Event ()` value which you can 
think of as an infinite stream of left-button-presses, where
each press returns a `()` value. Next, lets look at the 
combinator

~~~~~{.haskell}
(->>) :: Event a -> b -> Event b
~~~~~

This is a higher-order *sampling* combinator that says 
that if you have some event stream of `a` values, and a 
particular `b` value, then you can get an event stream 
of `b` values where at each event the `b` value is returned.
(Note that the `a` value at each event is *ignored* here.)
In this case, we sample the value

~~~~~{.haskell}
blue :: Behavior Color
~~~~~

which is a behavior, thus 

~~~~~{.haskell}
(lbp ->> blue) :: Event (Behavior Color)
~~~~~

meaning that the second argument to the `untilB` combinator is a stream 
of *behavior* events (in this case, the same `blue` behavior at each
button-click.) Lets look more closely at the the `untilB` combinator

~~~~~{.haskell}
untilB :: Behavior a -> Event (Behavior a) -> Behavior a
~~~~~

The type is *almost* completely descriptive (we will soon 
see another combinator with the same type that works rather 
differently.) In essence, the expression `b untilB e` is a 
behavior that is identical to `b` upto the point in time when
the first (behavior) event on the stream `e` is generated, 
at which point the behavior switches over to the new behavior
event. Thus,

~~~~~{.haskell}
red `untilB` (lbp ->> blue) :: Behavior Color 
~~~~~

is a behavior that is the color red *until* the first 
mouse-click, at which point the behavior toggles over 
to blue.

Recursive Toggling 
------------------

Now, we can fully exploit the power of the pure, combinator
based approach to manipulating events and behaviors. We can
write a simple color behavior that toggles between red and 
blue on each mouse click, by using recursion

> redBlueR = red  `untilB` lbp ->> 
>            blue `untilB` lbp ->> 
>            redBlueR
>
> main2    = reAct "2" $ paint redBlueR circ   

If you are confused, then note that the above expression is actually

~~~~~{.haskell}
redBlueR = red `untilB` (lbp ->> blue `untilB` (lbp ->> redBlueR))
~~~~~

and you can work out the types inside-out to deduce that 

~~~~~{.haskell}
red, blue, redBlueR :: Behavior Color
~~~~~

Recycling Colors 
----------------


Lets see if we can generalize the above to a behavior
that toggles through an arbitrary set of colors. Our 
friend in this endeavor will be the standard library 
function

~~~~~{.haskell}
cycle :: [a] -> [a]
~~~~~

which takes a list, and generates an infinite version
of the list that repeatedly *cycles* over the originial. 
In other words

~~~~~{.haskell}
cycle [1,2,3] = [1,2,3,1,2,3,1,2,3,1,2,3,...]
~~~~~

Now given a list of color (behaviors) we can first 
generate their cycled version and then at each click,
we just move onto the tail of the list.

> clickThru cs = toggle $ cycle cs 
>   where toggle (c:cs) = c `untilB` (lbp ->> toggle cs)

-- toggle' cs  = foldr (\c r -> c `untilB` (lbp ->> r)) 
--                    undefined cs

foldr op base (c1:c2:c3:c4:c5:...:[])
==> (c1 `op` c2 `op` c3 `op` c4 `op` c5 `op` ....`op` base)


  

**DO IN CLASS** What do you think the type of `clickThru` is ?

Now lets see it in action!

> main3 = reAct "s" $ paint rgby3 circ
> rgby3 = clickThru colors
> colors = [red, green, blue, yellow]

**DO IN CLASS**  Yuck! The above has recursion! Can you think
of a way to eliminate it entirely?



Fusing Event Streams
--------------------

Now the left mouse button is but one source of events. In general, 
one can imagine many others, the other mouse buttons, the keyboard, 
the network, on screen "collisions" and so on. Thus, to work with 
these sources, we need a means to combine the events generated by each
source. 

The following fuse-combinator `.|.` combinator provides this functionality.

~~~~~{.haskell}
(.|.) :: Event a -> Event a -> Event a
~~~~~

We can use it like so

> redBorY = red `untilB` ((lbp ->> blue) .|. (key ->> yellow))
> main4   = reAct "4" $ paint redBorY circ

Here we have two streams of (behavior) events

~~~~~{.haskell}
(lbp ->> blue)    :: Event (Behavior Color)
(key ->> yellow)) :: Event (Behavior Color)
~~~~~

which are fused into a single stream by the `.|.` 

~~~~~{.haskell}
((lbp ->> blue) .|. (key ->> yellow)) :: Event (Behavior Color)
~~~~~

which is then composed with the initial `red` behavior by the
`untilB` combinator. The result is a behavior that starts off 
as red, but upon receiving the first event from the fused stream,
toggles over to the color returned by the stream; blue if the 
mouse click happens first, or yellow if a key press happens.

Of course, the fuse combinator can be used recursively as well, 
so here is a behavior like the above, but which uses recursion to
allow to repeat toggling.

> main5 = reAct "5" $ paint redBorYR circ
>   where redBorYR = red `untilB` e 
>         e        = (lbp ->> blue   `untilB` e) .|. 
>                    (key ->> yellow `untilB` e)




Switching Between Many Choices
------------------------------

You know my feelings about explicit recursion; it is best caged inside a
few powerful, easy-to-read and understand patterns. The only reason we 
have to use it in the above is because the `untilB` combinator is anemic, 
it simply uses the behavior returned by the *first event* while instead, 
we want one that keeps switching over to the behavior returned by 
*each event* in the stream.

Such a functionality is provided by the `switch` combinator

~~~~~{.haskell}
switch :: Behavior a -> Event (Behavior a) -> Behavior a
~~~~~

Note that the type of `switch` is identical to that of `untilB`. 
So! the types don't always tell you everything, so choose your 
names carefully!

The expression `b switch e` is a behavior that starts off as `b` 
and then, at each event on the `e` stream, switches over to the 
behavior returned by that event. Thus, we can get our repeated 
toggling simply by

> main6 = reAct "6" $ paint redBorYR circ
>   where redBorYR = red `switch` ((lbp ->> blue) .|. (key ->> yellow))


Sampling Between Many Choices
-----------------------------

Let us revisit the example where we toggled over a bunch 
of colors. The pattern of using the same event stream to 
sample between an (infinite) list of values is so common, 
that we can capture it in a combinator 

~~~~~{.haskell}
withElem_ :: Event a -> [b] -> Event b
~~~~~

The type is rather descriptive. Given a stream of events, and an (infinite)
list of values, we can generate a stream of events where the values
returned are the corresponding elements from the list. Thus, the expression

~~~~~{.haskell}
lbp `withElem_` [1..]
~~~~~

corresponds to an event stream where subsequent mouse clicks return 
`1`, `2`, `3`, and so on. Thus, 

~~~~~{.haskell}
cycle colors :: [Behavior Color]
~~~~~

and so

~~~~~{.haskell}
lbp `withElem_` cycle colors :: Event (Behavior Color)
~~~~~

is a stream of events where each mouse click returns a color behavior!
When we compose this with an initial `red` behavior using `switch` we 
get a color behavior where each mouse click shifts over to the next color
in the cycle.

> colsClk = red `switch` (lbp `withElem_` cycle colors)
> main7   = reAct "7" $ paint colsClk circ

Using Event Data 
----------------

So far, we have mostly ignored the actual *data* being returned by the
events. This is fine for, say mouse click events, but for key-press events
we may want to know which key was pressed. 

Indeed, the handicap is apparent from the type of the sampling operator 

~~~~~{.haskell}
(->>) :: Event a -> b -> Event b
~~~~~

which ignores the `a` value altogether! We have an additional 
*value sampling* combinator

~~~~~{.haskell}
(=>>) :: Event a -> (a -> b) -> Event b
~~~~~

which bears a striking similarity to `map`. Indeed, `e =>> f` 
transforms a stream of `a` events to `b` events by using the 
supplied function. For example

~~~~~{.haskell}
(lbp `withElem_` [1..]) =>> (+ 1000)
~~~~~

is a stream of mouse click events that return the sequence 
of values `1001`, `1002`, `1003`, and so on.

Next, we can encapsulate the events generated by key-presses as

~~~~~{.haskell}
key :: Event Char
~~~~~

which returns the character corresponding to the pressed key. 
Thus, to allow us to change the color of the rendered object 
differently based on the key pressed, we can simply map 
the `key` event to the appropriate color behavior and compose
the result with `switch` like so

> main8        = reAct "8" $ paint rbChoose circ
>   where 
>     rbChoose = white `switch` (key =>> charColorB white)

where the function `charColorB` simply maps each `Char` to the
corresponding `Behavior Color`.

> charColorB _ 'R' = red
> charColorB _ 'r' = red
> charColorB _ 'B' = blue
> charColorB _ 'b' = blue
> charColorB _ 'Y' = yellow 
> charColorB _ 'y' = yellow 
> charColorB def _ = def 



Saving Old Values
-----------------

The above color behavior simply resets the color to white if you 
press the wrong color. Instead, it would be useful to *save* the
value of the behavior that held at the instant when the event 
occurred. We achieve this with the combinator 

~~~~~{.haskell}
snapshot :: Event a -> Behavior b -> Event (a, b)
~~~~~

Here, the type is rather descriptive: it is another sampling 
combinator that returns at each event, the value that the 
behavior had at that event.

Thus, we can now fix the above so that the old color is 
preserved whenever some incorrect key is pressed, by using 
the snapshot combinator to generate an event stream with the 
colors that held at each keypress, and then switching over 
the result.

> main9      = reAct "9" $ paint rbRem circ
>   where
>     rbRem  = white `switch` ((key `snapshot` rbRem) =>> charColorRemB)

where the function `charColorRemB` uses the old color as the *default*

> charColorRemB (c, oldc) = charColorB (lift0 oldc) c 


Boolean Events
--------------

So far, all the events we've seen have come from the user 
doing something like pressing a button or key or such. 
What if we want to *generate* events algorithmically, for 
example when two (animated) objects collide with each other?
In the reactive style, the trick is to think not in terms 
of one-off things being injected into the proceedings, but 
instead to step back and take a wider view in terms of 
behaviors and streams.

In essence, what we want is a combinator that takes some 
kind of continuous behavior signal, and generates an event 
stream with an event occurring at each time that the signal
makes a big change. Enter the `when` combinator 

~~~~~{.haskell}
when :: Behavior Bool -> Event ()
~~~~~

which takes a boolean behavior, intuitively, a signal that 
maps each time step to a boolean value, and returns a 
stream of `()` events every time the signal transitions from 
`False` to `True`.

Here's how you might use it. Recall the `time` behavior? 
The expression

~~~~~{.haskell}
(time >* 5) :: Behavior Bool
~~~~~

is a boolean behavior that toggles to true after `5` 
seconds, and we can turn it into an event by

~~~~~{.haskell}
(when (time >* 5)) :: Event Bool
~~~~~

which in effect is like a *timer* that generates a `True` 
event after `5` seconds. Now, we can compose it with the 
sample and `until` combinator to build a color behavior 
that toggles after `3` seconds.

> main10 = reAct "10" $ paint w3b circ 
>   where w3b = white `untilB` (when (time >* 3) ->> blue)

We could generate a repeated pulse event by applying some 
periodic operation to the `time` behavior. For example, 
consider the event

> pulse  = when (sin time >* 0)

and we can use `pulse` to switch the color

> main11   = reAct "11" $ paint rgby circ
>   where 
>     rgby = red `switch` (pulse `withElem_` colors)

Integrating over the past
-------------------------

Often, it is useful to have a primitive that can *accumulate* 
the values of previous behaviors. For example, recall the equations

~~~~~{.haskell}
s(t) = s0 + \Sum_0^t v(t).dt
v(t) = v0 + \Sum_0^t f(t).dt
~~~~~

which govern the position and velocity of a moving object at 
time `t` where `f` is some (constant) that denotes acceleration. 
We can define a notion of integration over time as a combinator for
behaviors

~~~~~{.haskell}
integral :: Behavior Float -> Behavior Float
~~~~~

where `integral b` is simply the behavior 

~~~~~{.haskell}
ib(t) = b(0) + \Sum_0^t b(t).dt
~~~~~

Armed with this combinator, we can write a very declarative description
of a bouncing ball animation.

> bball g hv  = translate (x, y) ball
>   where x   = -3  + integral hv
>         y   = 1.5 + integral vv
>         vv  = integral g `switch` 
>                 (when (y <* -1.5) `snapshot_` vv =>> \v' ->
>                  lift0 (-v' * 0.75) + integral g)
>
> main12 = reAct "bball" $ paint red (bball (-4) 0.5)

Note how the `x` coordinate is simply integrating the (constant)
horizontal velocity `hv` over time. The `y` coordinate integrates 
the vertical velocity `vv` over time, but `vv` is itself made of
two parts 

- the *falling* motion which is simply the integral of the 
  *gravity* acceleration `g`, and,

- every time the ball hits the bottom, that is the `y` value 
  falls below some threshold, the velocity is negated (ie the 
  direction changes) and we switch over to the new velocity 
  behavior.


Converting Events Into Behaviors
--------------------------------

Next, lets see how we can convert discrete events into `step` behaviors

> step :: a -> Event a -> Behavior a
> a `step` e = constB a `switch` e =>> constB


With this, we can define a ball behavior parameterized by a position
behavior:

> cball xy  = translate xy ball

we can put it to use with the following animation, where `mouse` is a
primitivee behavior generated by the position of the mouse cursor at
each point in time:

> main15 = reAct "15" $ paint colsClk $ translate mouse ball


and we can use the above to make the ball jump up and down levels based on 
key-presses.

> stepY hv = (x, y) 
>   where x = -3 + integral hv 
>         y = 0 `step` (key =>> ((0.3 *) . fromIntegral . digitToInt))
>
> main13 = reAct "cball" $ paint red $ cball (stepY 0.5)

Accumulating Events Into Behaviors
----------------------------------

Now, we can use events to *accumulate* behaviors, by having each event
return a function that transforms the behavior value. Concretely, consider 
the combinator:

> stepAccum :: a -> Event (a -> a) -> Behavior a
> a `stepAccum` e = b 
>    where b = a `step` (e `snapshot` b =>> \(f, x) -> f x)

Lets put it to use with a simple event accumulator that just "increments
the `y` co-ordinate:

> ladderY hv = (x, y)
>   where x = -3 + integral hv 
>         y = 0 `stepAccum` (lbp ->> (0.2 +))
>
> main14 = reAct "cball" $ paint red $ cball (ladderY 0.5)

PaddleBall
==========

Finally, lets put it all together and define a simple paddleball game!

> main16 = reAct "paddleball" $ paddleball 2
>
> paddleball speed = walls `over` paddle `over` pball speed

Thats it! Lets, look at the definitions for each element. First, the walls:

> walls = upper `over` left `over` right
>   where upper = paint green $ translate ( 0,1.7) (rec 4.4 0.05)
>         left  = paint green $ translate (-2.2,0) (rec 0.05 3.4)
>         right = paint green $ translate ( 2.2,0) (rec 0.05 3.4)

Next, the paddle:

> paddle = paint red $ translate (fst mouse, -1.7) (rec 0.5 0.05)

And finally, the ball:

> pball vel = paint yellow $ translate (xpos, ypos) ball
>   where xpos = integral xvel
>         ypos = integral yvel
>         xvel = vel `stepAccum` xhit ->> negate
>         yvel = vel `stepAccum` yhit ->> negate
>         xhit = when (xpos >*  2 ||* xpos <* -2)
>         yhit = when (ypos >* 1.5 
>                     ||* ypos      `between` (-2.0,-1.5) &&*
>                         fst mouse `between` (xpos-0.25, xpos+0.25))
>
> x `between` (a, b) = x >* a &&* x <* b


History
=======

FRP was invented in 1997 by Elliot and Hudak[1]. It has, over time, been
adopted in a variety of domains. Recent variants include FlapJax[2] which 
shows how FRP can be used to build elegant Ajax programs, and Microsoft 
has released the Reactive Extensions[3] for .Net that brings FRP to
the .Net platform. Finally, FRP is not only about GUIs, a recent effort
aims to use these ideas to make OpenFlow networks more easily
programmable[4].

[1]: http://conal.net/papers/icfp97/
[2]: http://www.flapjax-lang.org
[3]: http://msdn.microsoft.com/en-us/devlabs/ee794896
[4]: http://www.frenetic-lang.org


Implementation Details
======================

Since events are a *discrete* entity that will be composed with behaviors,
we will, for efficiency reasons want a sparse *sample-based* representation 
of the two kinds of values.

Behaviors
---------

The data type defining behaviors

> newtype Behavior a = Behavior (([Maybe UserAction], [Time]) -> [a])
> type Time          = Float
> type UserAction    = G.Event

The constant behavior

> constB :: a -> Behavior a
> constB x = Behavior (\_ -> repeat x)

various functions to lift operations over behaviors

> ($*) :: Behavior (a -> b) -> Behavior a -> Behavior b
> Behavior ff $* Behavior fb
>   = Behavior (\uts -> zipWith ($) (ff uts) (fb uts))

> lift0 :: a -> Behavior a
> lift0 = constB

> lift1 :: (a -> b) -> (Behavior a -> Behavior b)
> lift1 f b1 
>   = lift0 f $* b1

> lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
> lift2 f b1 b2 
>   = lift1 f b1 $* b2

> lift3 :: (a -> b -> c -> d) -> 
>          (Behavior a -> Behavior b -> Behavior c -> Behavior d)
> lift3 f b1 b2 b3 
>   = lift2 f b1 b2 $* b3

Joining and splitting Behaviors

> pairB :: Behavior a -> Behavior b -> Behavior (a, b)
> pairB = lift2 (,)

> fstB :: Behavior (a,b) -> Behavior a
> fstB  = lift1 fst
> sndB :: Behavior (a,b) -> Behavior b
> sndB  = lift1 snd

A few basic behaviors

> time :: Behavior Time
> time = Behavior (\(_, ts) -> ts)
>
> red, blue, yellow, green, white, black :: Behavior Color
> red    = lift0 Red
> blue   = lift0 Blue
> yellow = lift0 Yellow 
> green  = lift0 Green
> white  = lift0 White
> black  = lift0 Black

Various operators over behaviors. 

> paint :: Behavior Color -> Behavior Region -> Behavior Picture
> paint = lift2 Region
>
> shape :: Behavior Shape -> Behavior Region
> shape   = lift1 Shape
>
> ell, rec :: Behavior Float -> Behavior Float -> Behavior Region
> ell x y = shape (lift2 Ellipse   x y) 
> rec x y = shape (lift2 Rectangle x y)

> translate :: (Behavior Float, Behavior Float) 
>              -> Behavior Region -> Behavior Region
> translate (Behavior fx, Behavior fy) (Behavior fp)
>       = Behavior (\uts -> zipWith3 aux (fx uts) (fy uts) (fp uts))
>         where aux x y p = Translate (x,y) p
>
> (>*),(<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
> (>*) = lift2 (>)
> (<*) = lift2 (<)
>
> (&&*),(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
> (&&*) = lift2 (&&)
> (||*) = lift2 (||)
>
> over :: Behavior Picture -> Behavior Picture -> Behavior Picture
> over = lift2 Over

Events
------

The datatype definition

> newtype Event a    = Event (([Maybe UserAction],[Time]) -> [Maybe a])

The basic key press event

> lbp :: Event ()
> lbp = Event (\(uas,_) -> map getlbp uas)
>       where getlbp (Just (Button _ True True)) = Just ()
>             getlbp _                           = Nothing

High-Level Event Combinators
----------------------------

The transition combinator

> e ->> v = e =>> \_ -> v
>
> (=>>) :: Event a -> (a -> b) -> Event b
> Event fe =>> f = Event (map (fmap f) . fe)

Then `untilB` combinator

> untilB :: Behavior a -> Event (Behavior a) -> Behavior a
> Behavior fb `untilB` Event fe =
>   memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
>     where loop (_:us) (_:ts) ~(e:es) (b:bs) =
>             b : case e of 
>                   Nothing             -> loop us ts es bs
>                   Just (Behavior fb') -> fb' (us,ts)

The `switch` combinator

> switch :: Behavior a -> Event (Behavior a) -> Behavior a
> Behavior fb `switch` Event fe =
>   memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
>     where loop (_:us) (_:ts) ~(e:es) ~(b:bs) = 
>             b : case e of 
>                   Nothing             -> loop us ts es bs
>                   Just (Behavior fb') -> loop us ts es (fb' (us,ts))
>
> memoB :: Behavior a -> Behavior a
> memoB (Behavior fb) = Behavior (memo1 fb)

The `withElem_` combinator

> withElem  :: Event a -> [b] -> Event (a,b)
> withElem (Event fe) bs = Event (\uts -> loop (fe uts) bs)
>   where loop (Just a  : evs) (b:bs) = Just (a,b) : loop evs bs
>         loop (Nothing : evs)    bs  = Nothing    : loop evs bs
>
> withElem_ :: Event a -> [b] -> Event b
> withElem_ e bs = e `withElem` bs =>> snd


The `snapshot` combinator

> snapshot :: Event a -> Behavior b -> Event (a, b)
> Event fe `snapshot` Behavior fb
>   = Event (\uts -> zipWith' aux (fe uts) (fb uts))
>       where aux (Just x) y = Just (x, y)
>             aux Nothing  _ = Nothing

The `when` combinator

> when :: Behavior Bool -> Event ()
> when = unique . while
>
> while :: Behavior Bool -> Event ()
>
> while (Behavior fb) 
>   = Event (\uts -> map aux (fb uts))
>     where aux True  = Just ()
>           aux False = Nothing
>
> unique :: (Show a, Eq a) => Event a -> Event a
> unique (Event fe) =
>       Event (\uts -> aux (fe uts))
>       where aux xs = zipWith remdup (Nothing:xs) xs
>             remdup x y | x==y      = Nothing
>                        | otherwise = y

The `integral` combinator

> integral :: Behavior Float -> Behavior Float
> integral (Behavior fb)
>   = Behavior (\uts@(us,t:ts) -> 0 : loop t 0 ts (fb uts))
>       where loop t0 acc (t1:ts) (a:as) 
>                  = let acc' = acc + (t1-t0)*a
>                    in acc' : loop t1 acc' ts as




Other details
-------------

> main = do
>   [n] <- getArgs
>   case n of
>     "main0"  -> main0
>     "main1"  -> main1
>     "main2"  -> main2
>     "main3"  -> main3
>     "main4"  -> main4
>     "main5"  -> main5
>     "main6"  -> main6
>     "main7"  -> main7
>     "main8"  -> main8
>     "main9"  -> main9
>     "main10" -> main10
>     "main11" -> main11
>     "main12" -> main12
>     "main13" -> main13
>     "main14" -> main14
>     "main15" -> main15
>     "main16" -> main16
>     _        -> putStrLn "Eh, don't understand that command!"


> instance Fractional a => Fractional (Behavior a) where
>   (/) = lift2 (/)
>   fromRational = lift0 . fromRational

> instance Num a => Num (Behavior a) where
>   (+) = lift2 (+)
>   (*) = lift2 (*)
>   negate = lift1 negate
>   abs = lift1 abs
>   signum = lift1 signum
>   fromInteger = lift0 . fromInteger

> instance Show (Behavior a)  where
>   showsPrec n a s = "<< Behavior >>"

> instance Eq (Behavior a) where
>   a1 == a2 = error "Can't compare behaviors."

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


> (.|.) :: Event a -> Event a -> Event a
> Event fe1 .|. Event fe2 
>   = Event (\uts -> zipWith aux (fe1 uts) (fe2 uts))
>       where aux Nothing  Nothing  = Nothing
>             aux (Just x) _        = Just x
>             aux _        (Just y) = Just y

> key :: Event Char
> key = Event (\(uas,_) -> map getkey uas)
>       where getkey (Just (Key ch True)) = Just ch
>             getkey _                    = Nothing
>
> zipWith' f ~(x:xs) ~(y:ys) = f x y : zipWith' f xs ys

> snapshot_ :: Event a -> Behavior b -> Event b
> snapshot_ e b = e `snapshot` b =>> snd


> mm :: Event Coordinate
> mm = Event (\(uas,_) -> map getmm uas)
>      where getmm (Just (MouseMove pt)) = Just (gPtToPt pt)
>            getmm _                     = Nothing
>
>
> gPtToPt :: (Int, Int) -> Coordinate
> gPtToPt (x,y) = ( pixelToInch (x - 300)
>                 , pixelToInch (250 - y) )
>
> pixelToInch  :: Int -> Float
> pixelToInch n = intToFloat n / 100
>
> mouse :: (Behavior Float, Behavior Float)
>
> mouse = (fstB m, sndB m)
>   where m = (0,0) `step` mm
>
> reactimate :: String -> Behavior Graphic -> IO ()
> reactimate title franProg
>   = runGraphics $
>     do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
>               drawBufferedGraphic
>        (us,ts,addEvents) <- windowUser w
>        addEvents
>        let drawPic (Just g) = 
>              do setGraphic w g
>                 quit <- addEvents
>                 if quit 
>                   then return True
>                   else return False
>            drawPic Nothing  = return False
>        let Event fe = sample `snapshot_` franProg
>        run drawPic (fe (us,ts))
>        closeWindow w
>   where
>     run f (x:xs) = do
>       quit <- f x
>       if quit
>         then return ()
>         else run f xs
>     run f [] = return ()
>
> sample :: Event ()
> sample = Event (\(us,_) -> map aux us)
>   where aux Nothing  = Just ()
>         aux (Just _) = Nothing

> windowUser :: Window -> IO ([Maybe UserAction], [Time], IO Bool)
> windowUser w
>   = do (evs, addEv) <- makeStream
>        t0 <- timeGetTime
>        let addEvents =
>              let loop rt = do
>                    mev <- maybeGetWindowEvent w
>                    case mev of
>                      Nothing -> return False
>                      Just e  -> case e of
>                         Key ' ' True -> return True
>                         Closed -> return True
>                         _ -> addEv (rt, Just e) >> loop rt
>              in do t <- timeGetTime
>                    let rt = w32ToTime (t-t0)
>                    quit <- loop rt
>                    addEv (rt, Nothing)
>                    return quit
>        return (map snd evs, map fst evs, addEvents)
>
> w32ToTime t = intToFloat (fromInteger (toInteger t)) / 1000
>
> makeStream :: IO ([a], a -> IO ())
> makeStream = do
>   ch <- newChan
>   contents <- getChanContents ch
>   return (contents, writeChan ch)
