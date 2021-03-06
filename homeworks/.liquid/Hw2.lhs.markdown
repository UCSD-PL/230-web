---
title: Homework #2, Due Friday 2/13/15
---

<pre><span class=hs-linenum>5: </span><span class='hs-varop'>&gt;</span> <span class='hs-comment'>{-# LANGUAGE TypeSynonymInstances #-}</span>
<span class=hs-linenum>6: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>module</span> <span class='hs-conid'>Hw2</span> <span class='hs-keyword'>where</span>
</pre>
<pre><span class=hs-linenum>8: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Control</span><span class='hs-varop'>.</span><span class='hs-conid'>Applicative</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>empty</span><span class='hs-layout'>,</span> <span class='hs-layout'>(</span><span class='hs-varop'>&lt;|&gt;</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
<span class=hs-linenum>9: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Data</span><span class='hs-varop'>.</span><span class='hs-conid'>Map</span>
<span class=hs-linenum>10: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Control</span><span class='hs-varop'>.</span><span class='hs-conid'>Monad</span><span class='hs-varop'>.</span><span class='hs-conid'>State</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>when</span><span class='hs-layout'>)</span>
<span class=hs-linenum>11: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-conid'>State</span><span class='hs-layout'>,</span> <span class='hs-varid'>between</span><span class='hs-layout'>)</span>
<span class=hs-linenum>12: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span><span class='hs-varop'>.</span><span class='hs-conid'>Combinator</span> <span class='hs-varid'>hiding</span> <span class='hs-layout'>(</span><span class='hs-varid'>between</span><span class='hs-layout'>)</span>
<span class=hs-linenum>13: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span><span class='hs-varop'>.</span><span class='hs-conid'>Char</span>
<span class=hs-linenum>14: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>import</span> <span class='hs-conid'>Text</span><span class='hs-varop'>.</span><span class='hs-conid'>Parsec</span><span class='hs-varop'>.</span><span class='hs-conid'>String</span>
</pre>
This week's homework is presented as a literate Haskell file,
just like the lectures. This means that every line beginning with
`>` is interpreted as Haskell code by the compiler, while every other
line is ignored. (Think of this as the comments and code being reversed
from what they usually are.)

You can load this file into `ghci` and compile it with `ghc`
just like any other Haskell file, so long as you remember to save
it with a `.lhs` suffix.

To complete this homework, download [this file as plain text](Hw2.lhs) and
answer each question, filling in code where noted (i.e. where it says `error
"TBD"`).

Your code *must* typecheck against the given type signatures.
Feel free to add your own tests to this file to exercise the functions
you write.  Submit your homework by sending this file, filled in
appropriately, to `cse230@goto.ucsd.edu` with the subject "HW2"; you
will receive a confirmation email after submitting. 

Before starting this assignment:

1. Install `parsec` via the command `cabal install parsec`
2. Learn to read the [documentation](http://hackage.haskell.org)
3. Download the test files 
   [test.imp](/static/test.imp),
   [fact.imp](/static/fact.imp), 
   [abs.imp](/static/abs.imp), 
   [times.imp](/static/times.imp).

Problem 0: All About You
========================


Tell us your name, email and student ID, by replacing the respective
strings below

<pre><span class=hs-linenum>53: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myName</span>  <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Write Your Name  Here"</span>
<span class=hs-linenum>54: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myEmail</span> <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Write Your Email Here"</span>
<span class=hs-linenum>55: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>mySID</span>   <span class='hs-keyglyph'>=</span> <span class='hs-str'>"Write Your SID   Here"</span>
</pre>

Problem 1: All About `foldl`
============================

Define the following functions by filling in the "error" portion:

1. Describe `foldl` and give an implementation:

<pre><span class=hs-linenum>65: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFoldl</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>66: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFoldl</span> <span class='hs-varid'>f</span> <span class='hs-varid'>b</span> <span class='hs-varid'>xs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
2. Using the standard `foldl` (not `myFoldl`), define the list reverse function:

<pre><span class=hs-linenum>70: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myReverse</span> <span class='hs-keyglyph'>::</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span>
<span class=hs-linenum>71: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myReverse</span> <span class='hs-varid'>xs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
3. Define `foldr` in terms of `foldl`:

<pre><span class=hs-linenum>75: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFoldr</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>a</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span>
<span class=hs-linenum>76: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFoldr</span> <span class='hs-varid'>f</span> <span class='hs-varid'>b</span> <span class='hs-varid'>xs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
4. Define `foldl` in terms of the standard `foldr` (not `myFoldr`):

<pre><span class=hs-linenum>80: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFoldl2</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>b</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-keyglyph'>[</span><span class='hs-varid'>b</span><span class='hs-keyglyph'>]</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>81: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>myFoldl2</span> <span class='hs-varid'>f</span> <span class='hs-varid'>b</span> <span class='hs-varid'>xs</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
5. Try applying `foldl` to a gigantic list. Why is it so slow?
   Try using `foldl'` (from [Data.List](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#3))
   instead; can you explain why it's faster?

Part 2: Binary Search Trees
===========================

Recall the following type of binary search trees:

<pre><span class=hs-linenum>92: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Emp</span> 
<span class=hs-linenum>93: </span><span class='hs-varop'>&gt;</span>              <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Bind</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-layout'>(</span><span class='hs-conid'>BST</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>BST</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span> 
<span class=hs-linenum>94: </span><span class='hs-varop'>&gt;</span>              <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>)</span>
</pre>
Define a `delete` function for BSTs of this type:

<pre><span class=hs-linenum>98: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>delete</span> <span class='hs-keyglyph'>::</span> <span class='hs-layout'>(</span><span class='hs-conid'>Ord</span> <span class='hs-varid'>k</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=&gt;</span> <span class='hs-varid'>k</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>BST</span> <span class='hs-varid'>k</span> <span class='hs-varid'>v</span>
<span class=hs-linenum>99: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>delete</span> <span class='hs-varid'>k</span> <span class='hs-varid'>t</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
Part 3: An Interpreter for WHILE 
================================

Next, you will use monads to build an evaluator for
a simple *WHILE* language. In this language, we will
represent different program variables as 

<pre><span class=hs-linenum>108: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Variable</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>String</span>
</pre>
Programs in the language are simply values of the type

<pre><span class=hs-linenum>112: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Statement</span> <span class='hs-keyglyph'>=</span>
<span class=hs-linenum>113: </span><span class='hs-varop'>&gt;</span>     <span class='hs-conid'>Assign</span> <span class='hs-conid'>Variable</span> <span class='hs-conid'>Expression</span>          <span class='hs-comment'>-- x = e</span>
<span class=hs-linenum>114: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>If</span> <span class='hs-conid'>Expression</span> <span class='hs-conid'>Statement</span> <span class='hs-conid'>Statement</span>   <span class='hs-comment'>-- if (e) {s1} else {s2}</span>
<span class=hs-linenum>115: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>While</span> <span class='hs-conid'>Expression</span> <span class='hs-conid'>Statement</span>          <span class='hs-comment'>-- while (e) {s}</span>
<span class=hs-linenum>116: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Sequence</span> <span class='hs-conid'>Statement</span> <span class='hs-conid'>Statement</span>        <span class='hs-comment'>-- s1; s2</span>
<span class=hs-linenum>117: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Skip</span>                                <span class='hs-comment'>-- no-op</span>
<span class=hs-linenum>118: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>)</span>
</pre>
where expressions are variables, constants or 
binary operators applied to sub-expressions

<pre><span class=hs-linenum>123: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Expression</span> <span class='hs-keyglyph'>=</span>
<span class=hs-linenum>124: </span><span class='hs-varop'>&gt;</span>     <span class='hs-conid'>Var</span> <span class='hs-conid'>Variable</span>                        <span class='hs-comment'>-- x</span>
<span class=hs-linenum>125: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Val</span> <span class='hs-conid'>Value</span>                           <span class='hs-comment'>-- v </span>
<span class=hs-linenum>126: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Op</span>  <span class='hs-conid'>Bop</span> <span class='hs-conid'>Expression</span> <span class='hs-conid'>Expression</span>
<span class=hs-linenum>127: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>)</span>
</pre>
and binary operators are simply two-ary functions

<pre><span class=hs-linenum>131: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Bop</span> <span class='hs-keyglyph'>=</span> 
<span class=hs-linenum>132: </span><span class='hs-varop'>&gt;</span>     <span class='hs-conid'>Plus</span>     <span class='hs-comment'>-- (+)  :: Int  -&gt; Int  -&gt; Int</span>
<span class=hs-linenum>133: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Minus</span>    <span class='hs-comment'>-- (-)  :: Int  -&gt; Int  -&gt; Int</span>
<span class=hs-linenum>134: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Times</span>    <span class='hs-comment'>-- (*)  :: Int  -&gt; Int  -&gt; Int</span>
<span class=hs-linenum>135: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Divide</span>   <span class='hs-comment'>-- (/)  :: Int  -&gt; Int  -&gt; Int</span>
<span class=hs-linenum>136: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Gt</span>       <span class='hs-comment'>-- (&gt;)  :: Int -&gt; Int -&gt; Bool </span>
<span class=hs-linenum>137: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Ge</span>       <span class='hs-comment'>-- (&gt;=) :: Int -&gt; Int -&gt; Bool</span>
<span class=hs-linenum>138: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Lt</span>       <span class='hs-comment'>-- (&lt;)  :: Int -&gt; Int -&gt; Bool</span>
<span class=hs-linenum>139: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>Le</span>       <span class='hs-comment'>-- (&lt;=) :: Int -&gt; Int -&gt; Bool</span>
<span class=hs-linenum>140: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>)</span>
</pre>
<pre><span class=hs-linenum>142: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>data</span> <span class='hs-conid'>Value</span> <span class='hs-keyglyph'>=</span>
<span class=hs-linenum>143: </span><span class='hs-varop'>&gt;</span>     <span class='hs-conid'>IntVal</span> <span class='hs-conid'>Int</span>
<span class=hs-linenum>144: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyglyph'>|</span> <span class='hs-conid'>BoolVal</span> <span class='hs-conid'>Bool</span>
<span class=hs-linenum>145: </span><span class='hs-varop'>&gt;</span>   <span class='hs-keyword'>deriving</span> <span class='hs-layout'>(</span><span class='hs-conid'>Show</span><span class='hs-layout'>)</span>
</pre>
We will represent the *store* i.e. the machine's memory, as an associative
map from `Variable` to `Value` 

<pre><span class=hs-linenum>150: </span><span class='hs-varop'>&gt;</span> <span class='hs-keyword'>type</span> <span class='hs-conid'>Store</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>Map</span> <span class='hs-conid'>Variable</span> <span class='hs-conid'>Value</span>
</pre>
**Note:** we don't have exceptions (yet), so if a variable
is not found (eg because it is not initialized) simply return 
the value `0`. In future assignments, we will add this as a 
case where exceptions are thrown (the other case being type errors.)

We will use the standard library's `State` 
[monad](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2)
to represent the world-transformer.
Intuitively, `State s a` is equivalent to the world-transformer 
`s -> (a, s)`. See the above documentation for more details. 
You can ignore the bits about `StateT` for now.

Expression Evaluator
--------------------

First, write a function 

<pre><span class=hs-linenum>169: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalE</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Expression</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>State</span> <span class='hs-conid'>Store</span> <span class='hs-conid'>Value</span>
</pre>
that takes as input an expression and returns a world-transformer that
returns a value. Yes, right now, the transformer doesnt really transform
the world, but we will use the monad nevertheless as later, the world may
change, when we add exceptions and such.

**Hint:** The value `get` is of type `State Store Store`. Thus, to extract 
the value of the "current store" in a variable `s` use `s <- get`.

<pre><span class=hs-linenum>179: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalOp</span> <span class='hs-keyglyph'>::</span> <span class=hs-error><span class='hs-conid'>Op</span></span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Value</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Value</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Value</span>
<span class=hs-linenum>180: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalOp</span> <span class='hs-conid'>Plus</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-varid'>i</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-varid'>j</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-conid'>IntVal</span> <span class='hs-layout'>(</span><span class='hs-varid'>i</span><span class='hs-varop'>+</span><span class='hs-varid'>j</span><span class='hs-layout'>)</span>
<span class=hs-linenum>181: </span><span class='hs-varop'>&gt;</span> 
<span class=hs-linenum>182: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalE</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-varid'>x</span><span class='hs-layout'>)</span>      <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
<span class=hs-linenum>183: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalE</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-varid'>v</span><span class='hs-layout'>)</span>      <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
<span class=hs-linenum>184: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalE</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-varid'>o</span> <span class='hs-varid'>e1</span> <span class='hs-varid'>e2</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
Statement Evaluator
-------------------

Next, write a function

<pre><span class=hs-linenum>191: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalS</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Statement</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>State</span> <span class='hs-conid'>Store</span> <span class='hs-conid'>()</span>
</pre>
that takes as input a statement and returns a world-transformer that
returns a unit. Here, the world-transformer should in fact update the input
store appropriately with the assignments executed in the course of
evaluating the `Statement`.

**Hint:** The value `put` is of type `Store -> State Store ()`. 
Thus, to "update" the value of the store with the new store `s'` 
do `put s'`.

<pre><span class=hs-linenum>202: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalS</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-varid'>x</span> <span class='hs-varid'>e</span> <span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span> 
<span class=hs-linenum>203: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalS</span> <span class='hs-varid'>w</span><span class='hs-keyglyph'>@</span><span class='hs-layout'>(</span><span class='hs-conid'>While</span> <span class='hs-varid'>e</span> <span class='hs-varid'>s</span><span class='hs-layout'>)</span>    <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span> 
<span class=hs-linenum>204: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalS</span> <span class='hs-conid'>Skip</span>             <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
<span class=hs-linenum>205: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalS</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-varid'>s1</span> <span class='hs-varid'>s2</span><span class='hs-layout'>)</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
<span class=hs-linenum>206: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>evalS</span> <span class='hs-layout'>(</span><span class='hs-conid'>If</span> <span class='hs-varid'>e</span> <span class='hs-varid'>s1</span> <span class='hs-varid'>s2</span><span class='hs-layout'>)</span>     <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span> 
</pre>
In the `If` case, if `e` evaluates to a non-boolean value, just skip both
the branches. (We will convert it into a type error in the next homework.)
Finally, write a function 

<pre><span class=hs-linenum>212: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>execS</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Statement</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Store</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Store</span>
<span class=hs-linenum>213: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>execS</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
such that `execS stmt store` returns the new `Store` that results
from evaluating the command `stmt` from the world `store`. 
**Hint:** You may want to use the library function 

~~~~~{.haskell}
execState :: State s a -> s -> s
~~~~~

When you are done with the above, the following function will 
"run" a statement starting with the `empty` store (where no 
variable is initialized). Running the program should print 
the value of all variables at the end of execution.

<pre><span class=hs-linenum>228: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>run</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Statement</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>IO</span> <span class='hs-conid'>()</span>
<span class=hs-linenum>229: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>run</span> <span class='hs-varid'>stmt</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <span class='hs-varid'>putStrLn</span> <span class='hs-str'>"Output Store:"</span> 
<span class=hs-linenum>230: </span><span class='hs-varop'>&gt;</span>               <span class='hs-varid'>putStrLn</span> <span class='hs-varop'>$</span> <span class='hs-varid'>show</span> <span class='hs-varop'>$</span> <span class='hs-varid'>execS</span> <span class='hs-varid'>stmt</span> <span class='hs-varid'>empty</span>
</pre>
Here are a few "tests" that you can use to check your implementation.

<pre><span class=hs-linenum>234: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>w_test</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"X"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Plus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Minus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Plus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>2</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>3</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Plus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>3</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"Y"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>While</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Gt</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"X"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"Y"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Plus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"Y"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"X"</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"X"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Minus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"X"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
</pre>
<pre><span class=hs-linenum>236: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>w_fact</span> <span class='hs-keyglyph'>=</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"N"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>2</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"F"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>While</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Gt</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"N"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>0</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"X"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"N"</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"Z"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"F"</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>While</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Gt</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"X"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Sequence</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"F"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Plus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"Z"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"F"</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"X"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Minus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"X"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Assign</span> <span class='hs-str'>"N"</span> <span class='hs-layout'>(</span><span class='hs-conid'>Op</span> <span class='hs-conid'>Minus</span> <span class='hs-layout'>(</span><span class='hs-conid'>Var</span> <span class='hs-str'>"N"</span><span class='hs-layout'>)</span> <span class='hs-layout'>(</span><span class='hs-conid'>Val</span> <span class='hs-layout'>(</span><span class='hs-conid'>IntVal</span> <span class='hs-num'>1</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span><span class='hs-layout'>)</span>
</pre>
As you can see, it is rather tedious to write the above tests! They
correspond to the code in the files `test.imp` and `fact.imp`. When you are
done, you should get

~~~~~{.haskell}
ghci> run w_test
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> run w_fact
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~

Problem 4: A Parser for WHILE 
=============================

It is rather tedious to have to specify individual programs as Haskell
values. For this problem, you will use parser combinators to build a parser
for the WHILE language from the previous problem.

Parsing Constants
-----------------

First, we will write parsers for the `Value` type

<pre><span class=hs-linenum>264: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>valueP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Value</span>
<span class=hs-linenum>265: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>valueP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>intP</span> <span class='hs-varop'>&lt;|&gt;</span> <span class='hs-varid'>boolP</span>
</pre>
To do so, fill in the implementations of

<pre><span class=hs-linenum>269: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>intP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Value</span>
<span class=hs-linenum>270: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>intP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span> 
</pre>
Next, define a parser that will accept a 
particular string `s` as a given value `x`

<pre><span class=hs-linenum>275: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>constP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>String</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>a</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-conid'>Parser</span> <span class='hs-varid'>a</span>
<span class=hs-linenum>276: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>constP</span> <span class='hs-varid'>s</span> <span class='hs-varid'>x</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
and use the above to define a parser for boolean values 
where `"true"` and `"false"` should be parsed appropriately.

<pre><span class=hs-linenum>281: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>boolP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Value</span>
<span class=hs-linenum>282: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>boolP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
Continue to use the above to parse the binary operators

<pre><span class=hs-linenum>286: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>opP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Bop</span> 
<span class=hs-linenum>287: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>opP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre> 

Parsing Expressions 
-------------------

Next, the following is a parser for variables, where each 
variable is one-or-more uppercase letters. 

<pre><span class=hs-linenum>296: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>varP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Variable</span>
<span class=hs-linenum>297: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>varP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>many1</span> <span class='hs-varid'>upper</span>
</pre>
Use the above to write a parser for `Expression` values

<pre><span class=hs-linenum>301: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>exprP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Expression</span>
<span class=hs-linenum>302: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>exprP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span>
</pre>
Parsing Statements
------------------

Next, use the expression parsers to build a statement parser

<pre><span class=hs-linenum>309: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>statementP</span> <span class='hs-keyglyph'>::</span> <span class='hs-conid'>Parser</span> <span class='hs-conid'>Statement</span>
<span class=hs-linenum>310: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>statementP</span> <span class='hs-keyglyph'>=</span> <span class='hs-varid'>error</span> <span class='hs-str'>"TBD"</span> 
</pre>
When you are done, we can put the parser and evaluator together 
in the end-to-end interpreter function

<pre><span class=hs-linenum>315: </span><span class='hs-varop'>&gt;</span> <span class='hs-definition'>runFile</span> <span class='hs-varid'>s</span> <span class='hs-keyglyph'>=</span> <span class='hs-keyword'>do</span> <span class='hs-varid'>p</span> <span class='hs-keyglyph'>&lt;-</span> <span class='hs-varid'>parseFromFile</span> <span class='hs-varid'>statementP</span> <span class='hs-varid'>s</span>
<span class=hs-linenum>316: </span><span class='hs-varop'>&gt;</span>                <span class='hs-keyword'>case</span> <span class='hs-varid'>p</span> <span class='hs-keyword'>of</span>
<span class=hs-linenum>317: </span><span class='hs-varop'>&gt;</span>                  <span class='hs-conid'>Left</span> <span class='hs-varid'>err</span>   <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>print</span> <span class='hs-varid'>err</span>
<span class=hs-linenum>318: </span><span class='hs-varop'>&gt;</span>                  <span class='hs-conid'>Right</span> <span class='hs-varid'>stmt</span> <span class='hs-keyglyph'>-&gt;</span> <span class='hs-varid'>run</span> <span class='hs-varid'>stmt</span>
</pre>
When you are done you should see the following at the ghci prompt

~~~~~{.haskell}
ghci> runFile "test.imp"
Output Store:
fromList [("X",IntVal 0),("Y",IntVal 10)]

ghci> runFile "fact.imp" 
Output Store:
fromList [("F",IntVal 2),("N",IntVal 0),("X",IntVal 1),("Z",IntVal 2)]
~~~~~





