
GHC = /home/ml9951/repos/ghc-wip/bin/ghc

.PHONY:	fine tl2

all:	fine tl2

fine:
	$(GHC) -threaded -rtsopts -O2 -fno-omit-yields -fforce-recomp Main -o fine

tl2:
	$(GHC) -threaded -rtsopts -O2 -fno-omit-yields -fforce-recomp -DPASTMTL2 Main -o tl2

cabal:
	cabal install --with-ghc=$(GHC) /home/ml9951/repos/ghc-wip/ghc/libraries/pastm

