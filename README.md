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

See Also:
https://ghc.haskell.org/trac/ghc/wiki/Builder
