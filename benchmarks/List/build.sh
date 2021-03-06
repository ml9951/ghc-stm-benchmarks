#!/bin/sh

cabal install random-shuffle cmdargs split async --with-ghc=/localdisk/ryates/ghc/ghc-7.6-build/bin/ghc --reinstall

~/ghc/ghc-7.6-build/bin/ghc -O2 -threaded -rtsopts Main.hs -outputdir .build -o Main

ghc -O2 -threaded -rtsopts Main.hs -outputdir .build-7.6.3 -o Main-7.6.3
