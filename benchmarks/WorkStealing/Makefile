
GHC = /home/ml9951/repos/ghc-wip/bin/ghc

LINKED = 

.PHONY:	fine tl2

all:	fine tl2

fine:
	$(GHC) -threaded -rtsopts -O2 -fno-omit-yields -fforce-recomp -D$(LINKED) Main -o fine$(LINKED)

tl2:
	$(GHC) -threaded -rtsopts -O2 -fno-omit-yields -fforce-recomp -D$(LINKED) -DPASTMTL2 Main -o tl2$(LINKED)

cabal:
	cabal install --with-ghc=$(GHC) /home/ml9951/repos/ghc-wip/ghc/libraries/pastm

