The GHC builder
===============
The GHC builder is a client/server system that allows us to build and test GHC
on lots of different computers scattered around the world (the clients, or
build slaves), and aggregate the test results centrally (the server).

Building the build slave
------------------------

```
cd client/
cabal sandbox init
cabal sandbox add-source ../common/
cabal install --dependencies-only
cabal build
```

Controlling resource usage
--------------------------

It is recommended that you run the builder-client in a dedicated VM or
container environment to control resources.  However, in the absence of that
you can use ulimit on UNIX and UNIX-like systems to control the maximum
resources that builder-client will consume.  Windows System Resource Manager
can be used in a similar manner.

### Example: Run builder-client with a 2GB virtual memory limit

```
ulimit -v 2000000
builder-client -v
```

See Also:
https://ghc.haskell.org/trac/ghc/wiki/Builder
