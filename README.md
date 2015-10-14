Install Libraries
--------------------

   cabal install hakyll -fhighlighting pandoc


Build the site
--------------

make all
	rebuilds the site (locally)

make clean
	nukes all the temporary files 

make update
	updates the webpage on csefast


Add new lectures or homeworks
-----------------------------

To add a new lecture, create the new file

	lectures/lec-XXX.lhs

then add a link to it in 

	lectures.markdown 

and then do

	make && make update

The analogous instructions apply to create a new homework.

Todo
----
Auto-generate Lectures and Homeworks (and the links) 
from directories.



