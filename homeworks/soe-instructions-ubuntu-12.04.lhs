---
title: SOE on Ubuntu 12.04
---

To install SOE on Ubuntu 12.04 simply do the following.


1. Install Haskell Platform 

~~~~~
$ sudo apt-get install haskell-platform
~~~~~

2. Install the Graphics Libraries

~~~~~
$ cabal install OpenGL
$ cabal install GLFW 
~~~~~

3. Grab [SOE](/static/SOE-CSE230-wi13.tgz)

~~~~~
$ wget http://cseweb.ucsd.edu/classes/wi13/cse230-a/static/SOE-CSE230-wi13.tgz
$ tar -zxvf SOE-CSE230-wi13.tgz
~~~~~


4. To test your install, run `ghci` with the extended *search path* 


~~~~~
$ ghci -i./SOE/src
~~~~~

5. Grab and load [soe-test.hs](/static/soe-test.hs) 

~~~~~
Prelude> :load soe-test.hs 
[1 of 1] Compiling Main             ( soe-test.hs, interpreted )
Ok, modules loaded: Main.
~~~~~

Finally, test the code with the following

~~~~~
*Main> test 
~~~~~

You should see a window that looks like this

![bullseye](/static/SOE-test.png)

(Hit any key over the window to close it.)
