---
title: Homework #1, Due Monday 1/26/15
---

Haskell Formalities
-------------------

We declare that this is the Hw1 module and import some libraries:

<pre><span class=hs-linenum>10: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>module</span> <span class='hs-conid'>Hw1</span> <span class='hs-keyword'>where</span>
<span class=hs-linenum>11: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class=hs-error><span class='hs-conid'>SOE</span></span>
<span class=hs-linenum>12: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class=hs-error><span class='hs-conid'>Play</span></span>
<span class=hs-linenum>13: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class=hs-error><span class='hs-conid'>XMLTypes</span></span>
</pre>
Part 0: All About You
---------------------

Tell us your name, email and student ID, by replacing the respective
strings below

<pre><span class=hs-linenum>21: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myName</span>  <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Write Your Name  Here"</span>
<span class=hs-linenum>22: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myEmail</span> <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Write Your Email Here"</span>
<span class=hs-linenum>23: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>mySID</span>   <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Write Your SID   Here"</span>
</pre>

Preliminaries
-------------

Before starting this assignment:

* Download and install the [Haskell Platform](http://www.haskell.org/platform/).
* Download the [SOE code bundle](/static/SOE-cse230-wi15.tar.gz).

* Verify that it works by changing into the `SOE/src` directory and
   running `ghci Draw.lhs`, then typing `main0` at the prompt:
 
~~~
cd SOE/src
ghci Draw.lhs
*Draw> main0
~~~

  You should see a window with some shapes in it.

**NOTE:** If you have trouble installing SOE, [see this page](soe-instructions.html)

5. Download the required files for this assignment: [hw1.tar.gz](/static/hw1.tar.gz).
   Unpack the files and make sure that you can successfully run the main program (in `Main.hs`).
   We've provided a `Makefile`, which you can use if you like. You should see this output:

~~~
Main: Define me!
~~~

Part 1: Defining and Manipulating Shapes
----------------------------------------

You will write all of your code in the `hw1.lhs` file, in the spaces
indicated. Do not alter the type annotations --- your code must
typecheck with these types to be accepted.

The following are the definitions of shapes:

<pre><span class=hs-linenum>64: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Shape</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Rectangle</span> <span class='hs-conid'>Side</span> <span class='hs-conid'>Side</span>
<span class=hs-linenum>65: </span><span class='hs-varop'>&gt;</span>            <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Ellipse</span> <span class='hs-conid'>Radius</span> <span class='hs-conid'>Radius</span>
<span class=hs-linenum>66: </span><span class='hs-varop'>&gt;</span>            <span class='hs-keyglyph'>|</span> <span class='hs-conid'>RtTriangle</span> <span class='hs-conid'>Side</span> <span class='hs-conid'>Side</span>
<span class=hs-linenum>67: </span><span class='hs-varop'>&gt;</span>            <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Polygon</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Vertex</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>68: </span><span class='hs-varop'>&gt;</span>            <span class='hs-keyword'>deriving</span> <span class='hs-conid'>Show</span>
<span class=hs-linenum>69: </span><span class='hs-varop'>&gt;</span> 
<span class=hs-linenum>70: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Radius</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Float</span> 
<span class=hs-linenum>71: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Side</span>   <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Float</span>
<span class=hs-linenum>72: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Vertex</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>Float</span><span class='hs-layout'>,</span> <span class='hs-conid'>Float</span><span class='hs-layout'>)</span>
</pre>
1. Below, define functions `rectangle` and `rtTriangle` as suggested
   at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
   built with the Polygon constructor.

<pre><span class=hs-linenum>78: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>rectangle</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Side</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Side</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Shape</span>
<span class=hs-linenum>79: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>rectangle</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span> 
</pre>
<pre><span class=hs-linenum>81: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>rtTriangle</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Side</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Side</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Shape</span>
<span class=hs-linenum>82: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>rtTriangle</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span> 
</pre>
2. Define a function

<pre><span class=hs-linenum>86: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>sides</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Shape</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>87: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>sides</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
  which returns the number of sides a given shape has.
  For the purposes of this exercise, an ellipse has 42 sides,
  and empty polygons, single points, and lines have zero sides.

3. Define a function

<pre><span class=hs-linenum>95: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>bigger</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Shape</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Float</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Shape</span>
<span class=hs-linenum>96: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>bigger</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
  that takes a shape `s` and expansion factor `e` and returns
  a shape which is the same as (i.e., similar to in the geometric sense)
  `s` but whose area is `e` times the area of `s`.

4. The Towers of Hanoi is a puzzle where you are given three pegs,
   on one of which are stacked $n$ discs in increasing order of size.
   To solve the puzzle, you must move all the discs from the starting peg
   to another by moving only one disc at a time and never stacking
   a larger disc on top of a smaller one.
   
   To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:
   
   1. Move $n - 1$ discs from peg $a$ to peg $c$.
   2. Move the remaining disc from peg $a$ to peg $b$.
   3. Move $n - 1$ discs from peg $c$ to peg $b$.
   
   Write a function
   
<pre><span class=hs-linenum>116: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>hanoi</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>117: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>hanoi</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
  that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
  where a is the starting peg,
  emits the series of moves required to solve the puzzle.
  For example, running `hanoi 2 "a" "b" "c"`

  should emit the text

~~~  
move disc from a to c
move disc from a to b
move disc from c to b
~~~

Part 2: Drawing Fractals
------------------------

1. The Sierpinski Carpet is a recursive figure with a structure similar to
   the Sierpinski Triangle discussed in Chapter 3:

![Sierpinski Carpet](/static/scarpet.png)

Write a function `sierpinskiCarpet` that displays this figure on the
screen:

<pre><span class=hs-linenum>143: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>sierpinskiCarpet</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>144: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>sierpinskiCarpet</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Note that you either need to run your program in `SOE/src` or add this
path to GHC's search path via `-i/path/to/SOE/src/`.
Also, the organization of SOE has changed a bit, so that now you use
`import SOE` instead of `import SOEGraphics`.

2. Write a function `myFractal` which draws a fractal pattern of your
   own design.  Be creative!  The only constraint is that it shows some
   pattern of recursive self-similarity.

<pre><span class=hs-linenum>155: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFractal</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>156: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFractal</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Part 3: Recursion Etc.
----------------------

First, a warmup. Fill in the implementations for the following functions.

(Your `maxList` and `minList` functions may assume that the lists
they are passed contain at least one element.)

Write a *non-recursive* function to compute the length of a list

<pre><span class=hs-linenum>168: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>lengthNonRecursive</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>169: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>lengthNonRecursive</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`doubleEach [1,20,300,4000]` should return `[2,40,600,8000]`

<pre><span class=hs-linenum>173: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>doubleEach</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>174: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>doubleEach</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Now write a *non-recursive* version of the above.

<pre><span class=hs-linenum>178: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>doubleEachNonRecursive</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>179: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>doubleEachNonRecursive</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`pairAndOne [1,20,300]` should return `[(1,2), (20,21), (300,301)]`

<pre><span class=hs-linenum>183: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>pairAndOne</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>184: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>pairAndOne</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>

Now write a *non-recursive* version of the above.

<pre><span class=hs-linenum>189: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>pairAndOneNonRecursive</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>190: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>pairAndOneNonRecursive</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`addEachPair [(1,2), (20,21), (300,301)]` should return `[3,41,601]`

<pre><span class=hs-linenum>194: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>addEachPair</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>195: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>addEachPair</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span> 
</pre>
Now write a *non-recursive* version of the above.

<pre><span class=hs-linenum>199: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>addEachPairNonRecursive</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-layout'>(</span><span class='hs-conid'>Int</span><span class='hs-layout'>,</span> <span class='hs-conid'>Int</span><span class='hs-layout'>)</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>200: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>addEachPairNonRecursive</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span> 
</pre>
`minList` should return the *smallest* value in the list. You may assume the
input list is *non-empty*.

<pre><span class=hs-linenum>205: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>minList</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>206: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>minList</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Now write a *non-recursive* version of the above.

<pre><span class=hs-linenum>210: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>minListNonRecursive</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>211: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>minListNonRecursive</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`maxList` should return the *largest* value in the list. You may assume the
input list is *non-empty*.

<pre><span class=hs-linenum>216: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>maxList</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>217: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>maxList</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Now write a *non-recursive* version of the above.

<pre><span class=hs-linenum>221: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>maxListNonRecursive</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-conid'>Int</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>222: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>maxListNonRecursive</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Now, a few functions for this `Tree` type.

<pre><span class=hs-linenum>226: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Tree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Leaf</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Branch</span> <span class='hs-layout'>(</span><span class='hs-conid'>Tree</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Tree</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>227: </span><span class='hs-varop'>&gt;</span>               <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>,</span> <span class='hs-conid'>Eq</span><span class='hs-layout'>)</span>
</pre>
`fringe t` should return a list of all the values occurring as a `Leaf`.
So: `fringe (Branch (Leaf 1) (Leaf 2))` should return `[1,2]`

<pre><span class=hs-linenum>232: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>fringe</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Tree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>233: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>fringe</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`treeSize` should return the number of leaves in the tree. 
So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

<pre><span class=hs-linenum>238: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>treeSize</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Tree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>239: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>treeSize</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`treeSize` should return the height of the tree.
So: `height (Branch (Leaf 1) (Leaf 2))` should return `1`.

<pre><span class=hs-linenum>244: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>treeHeight</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Tree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>245: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>treeHeight</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Now, a tree where the values live at the nodes not the leaf.

<pre><span class=hs-linenum>249: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>ILeaf</span> <span class='hs-keyglyph'>|</span> <span class='hs-conid'>IBranch</span> <span class='hs-varid'>a</span> <span class='hs-layout'>(</span><span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span>
<span class=hs-linenum>250: </span><span class='hs-varop'>&gt;</span>                       <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>,</span> <span class='hs-conid'>Eq</span><span class='hs-layout'>)</span>
</pre>
`takeTree n t` should cut off the tree at depth `n`.
So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
should return `IBranch 1 ILeaf ILeaf`.

<pre><span class=hs-linenum>256: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>takeTree</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Int</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>257: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>takeTree</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
`takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

<pre><span class=hs-linenum>263: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>takeTreeWhile</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Bool</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>InternalTree</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>264: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>takeTreeWhile</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre> 
Write the function map in terms of foldr:

<pre><span class=hs-linenum>268: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myMap</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>269: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myMap</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"Define me!"</span>
</pre>
Part 4: Transforming XML Documents
----------------------------------

The rest of this assignment involves transforming XML documents.
To keep things simple, we will not deal with the full generality of XML,
or with issues of parsing. Instead, we will represent XML documents as
instances of the following simpliﬁed type:

~~~~
data SimpleXML =
   PCDATA String
 | Element ElementName [SimpleXML]
 deriving Show

type ElementName = String
~~~~

That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
data") node containing a string or else an `Element` node containing a
tag and a list of sub-nodes.

The file `Play.hs` contains a sample XML value. To avoid getting into
details of parsing actual XML concrete syntax, we'll work with just
this one value for purposes of this assignment. The XML value in
`Play.hs` has the following structure (in standard XML syntax):

~~~
<PLAY>
  <TITLE>TITLE OF THE PLAY</TITLE>
  <PERSONAE>
    <PERSONA> PERSON1 </PERSONA>
    <PERSONA> PERSON2 </PERSONA>
    ... -- MORE PERSONAE
    </PERSONAE>
  <ACT>
    <TITLE>TITLE OF FIRST ACT</TITLE>
    <SCENE>
      <TITLE>TITLE OF FIRST SCENE</TITLE>
      <SPEECH>
        <SPEAKER> PERSON1 </SPEAKER>
        <LINE>LINE1</LINE>
        <LINE>LINE2</LINE>
        ... -- MORE LINES
      </SPEECH>
      ... -- MORE SPEECHES
    </SCENE>
    ... -- MORE SCENES
  </ACT>
  ... -- MORE ACTS
</PLAY>
~~~

* `sample.html` contains a (very basic) HTML rendition of the same
  information as `Play.hs`. You may want to have a look at it in your
  favorite browser.  The HTML in `sample.html` has the following structure
  (with whitespace added for readability):
  
~~~
<html>
  <body>
    <h1>TITLE OF THE PLAY</h1>
    <h2>Dramatis Personae</h2>
    PERSON1<br/>
    PERSON2<br/>
    ...
    <h2>TITLE OF THE FIRST ACT</h2>
    <h3>TITLE OF THE FIRST SCENE</h3>
    <b>PERSON1</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
    <b>PERSON2</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
    <h3>TITLE OF THE SECOND SCENE</h3>
    <b>PERSON3</b><br/>
    LINE1<br/>
    LINE2<br/>
    ...
  </body>
</html>
~~~

You will write a function `formatPlay` that converts an XML structure
representing a play to another XML structure that, when printed,
yields the HTML speciﬁed above (but with no whitespace except what's
in the textual data in the original XML).

<pre><span class=hs-linenum>360: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>formatPlay</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>SimpleXML</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>SimpleXML</span>
<span class=hs-linenum>361: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>formatPlay</span> <span class='hs-varid'>xml</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>PCDATA</span> <span class='hs-str'>"WRITE ME!"</span>
</pre>
The main action that we've provided below will use your function to
generate a ﬁle `dream.html` from the sample play. The contents of this
ﬁle after your program runs must be character-for-character identical
to `sample.html`.

<pre><span class=hs-linenum>368: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>mainXML</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <span class='hs-varid'>writeFile</span> <span class='hs-str'>"dream.html"</span> <span class='hs-varop'>$</span> <span class='hs-varid'>xml2string</span> <span class='hs-varop'>$</span> <span class='hs-varid'>formatPlay</span> <span class='hs-varid'>play</span>
<span class=hs-linenum>369: </span><span class='hs-varop'>&gt;</span>              <span class='hs-varid'>testResults</span> <span class='hs-str'>"dream.html"</span> <span class='hs-str'>"sample.html"</span>
<span class=hs-linenum>370: </span><span class='hs-varop'>&gt;</span>
<span class=hs-linenum>371: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>firstDiff</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Eq</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Maybe</span> <span class='hs-layout'>(</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>,</span><span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span><span class='hs-layout'>)</span>
<span class=hs-linenum>372: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>firstDiff</span> <span class='hs-conid'>[]</span> <span class='hs-conid'>[]</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Nothing</span>
<span class=hs-linenum>373: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>firstDiff</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-conop'>:</span><span class='hs-varid'>cs</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-varid'>d</span><span class='hs-conop'>:</span><span class='hs-varid'>ds</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>374: </span><span class='hs-varop'>&gt;</span>      <span class='hs-keyglyph'>|</span> <span class='hs-varid'>c</span><span class='hs-varop'>==</span><span class='hs-varid'>d</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>firstDiff</span> <span class='hs-varid'>cs</span> <span class='hs-varid'>ds</span> 
<span class=hs-linenum>375: </span><span class='hs-varop'>&gt;</span>      <span class='hs-keyglyph'>|</span> <span class='hs-varid'>otherwise</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Just</span> <span class='hs-layout'>(</span><span class='hs-varid'>c</span><span class='hs-conop'>:</span><span class='hs-varid'>cs</span><span class='hs-layout'>,</span> <span class='hs-varid'>d</span><span class='hs-conop'>:</span><span class='hs-varid'>ds</span><span class='hs-layout'>)</span>
<span class=hs-linenum>376: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>firstDiff</span> <span class='hs-varid'>cs</span> <span class='hs-varid'>ds</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Just</span> <span class='hs-layout'>(</span><span class='hs-varid'>cs</span><span class='hs-layout'>,</span><span class='hs-varid'>ds</span><span class='hs-layout'>)</span>
<span class=hs-linenum>377: </span><span class='hs-varop'>&gt;</span> 
<span class=hs-linenum>378: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>testResults</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>379: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>testResults</span> <span class='hs-varid'>file1</span> <span class='hs-varid'>file2</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> 
<span class=hs-linenum>380: </span><span class='hs-varop'>&gt;</span>   <span class='hs-varid'>f1</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>readFile</span> <span class='hs-varid'>file1</span>
<span class=hs-linenum>381: </span><span class='hs-varop'>&gt;</span>   <span class='hs-varid'>f2</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>readFile</span> <span class='hs-varid'>file2</span>
<span class=hs-linenum>382: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyword'>case</span> <span class='hs-varid'>firstDiff</span> <span class='hs-varid'>f1</span> <span class='hs-varid'>f2</span> <span class='hs-keyword'>of</span>
<span class=hs-linenum>383: </span><span class='hs-varop'>&gt;</span>     <span class='hs-conid'>Nothing</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>384: </span><span class='hs-varop'>&gt;</span>       <span class='hs-varid'>putStr</span> <span class='hs-str'>"Success!\n"</span>
<span class=hs-linenum>385: </span><span class='hs-varop'>&gt;</span>     <span class='hs-conid'>Just</span> <span class='hs-layout'>(</span><span class='hs-varid'>cs</span><span class='hs-layout'>,</span><span class='hs-varid'>ds</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyword'>do</span>
<span class=hs-linenum>386: </span><span class='hs-varop'>&gt;</span>       <span class='hs-varid'>putStr</span> <span class='hs-str'>"Results differ: '"</span>
<span class=hs-linenum>387: </span><span class='hs-varop'>&gt;</span>       <span class='hs-varid'>putStr</span> <span class='hs-layout'>(</span><span class='hs-varid'>take</span> <span class='hs-num'>20</span> <span class='hs-varid'>cs</span><span class='hs-layout'>)</span>
<span class=hs-linenum>388: </span><span class='hs-varop'>&gt;</span>       <span class='hs-varid'>putStr</span> <span class='hs-str'>"' vs '"</span>
<span class=hs-linenum>389: </span><span class='hs-varop'>&gt;</span>       <span class='hs-varid'>putStr</span> <span class='hs-layout'>(</span><span class='hs-varid'>take</span> <span class='hs-num'>20</span> <span class='hs-varid'>ds</span><span class='hs-layout'>)</span>
<span class=hs-linenum>390: </span><span class='hs-varop'>&gt;</span>       <span class='hs-varid'>putStr</span> <span class='hs-str'>"'\n"</span>
</pre>
Important: The purpose of this assignment is not just to "get the job
done" --- i.e., to produce the right HTML. A more important goal is to
think about what is a good way to do this job, and jobs like it. To
this end, your solution should be organized into two parts:

1. a collection of generic functions for transforming XML structures
   that have nothing to do with plays, plus

2. a short piece of code (a single deﬁnition or a collection of short
   deﬁnitions) that uses the generic functions to do the particular
   job of transforming a play into HTML.

Obviously, there are many ways to do the ﬁrst part. The main challenge
of the assignment is to ﬁnd a clean design that matches the needs of
the second part.

You will be graded not only on correctness (producing the required
output), but also on the elegance of your solution and the clarity and
readability of your code and documentation.  Style counts.  It is
strongly recommended that you rewrite this part of the assignment a
couple of times: get something working, then step back and see if
there is anything you can abstract out or generalize, rewrite it, then
leave it alone for a few hours or overnight and rewrite it again. Try
to use some of the higher-order programming techniques we've been
discussing in class.

Submission Instructions
-----------------------

* If working with a partner, you should both submit your assignments
  individually.
* Make sure your `hw1.lhs` is accepted by GHC without errors or warnings.
* Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
  subject "HW1" (minus the quotes).
  *This address is unmonitored!*

Credits
-------

This homework is essentially Homeworks 1 & 2 from
<a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
