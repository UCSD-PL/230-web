Website for CSE230
==================


To build, make sure you install [stack](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md)


Build the site
--------------

To *build* the site locally into `_site/`:

```
make 
```

To *delete* all temporary files:

```
make clean
```

To *update* the webpage remotely (FIXME):

```
make update
```



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

