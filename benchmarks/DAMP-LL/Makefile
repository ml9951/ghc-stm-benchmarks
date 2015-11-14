

GHC = /home/ml9951/repos/ghc-wip/bin/ghc

.PHONY:	straight-tl2 straight-fine dissected-fine dissected-tl2 cas mlc

all: straight-tl2 straight-fine dissected-fine dissected-tl2 cas mlc

straight-tl2:	
	$(GHC) -fforce-recomp -rtsopts -threaded -O2 -fno-omit-yields -DSTRAIGHTSTM -DPASTMTL2 Main.hs -o straightforward-tl2

straight-fine:	
	$(GHC) -fforce-recomp -rtsopts -threaded -O2 -fno-omit-yields -DSTRAIGHTSTM Main.hs -o straightforward-fine

dissected-tl2:
	$(GHC) -fforce-recomp -rtsopts -threaded -O2 -fno-omit-yields -DDISSECTEDSTM -DPASTMTL2 Main.hs -o dissected-tl2

dissected-fine:
	$(GHC) -fforce-recomp -rtsopts -threaded -O2 -fno-omit-yields -DDISSECTEDSTM Main.hs -o dissected-fine

cas:
	$(GHC) -fforce-recomp -rtsopts -threaded -O2 -fno-omit-yields -DCAS_LL Main.hs -o cas

mlc:
	$(GHC) -fforce-recomp -rtsopts -threaded -O2 -fno-omit-yields -DMLCLL Main.hs -o mlc

pkgs:	
	cabal sandbox init
	cabal install --with-ghc=$(GHC) ../random/pcg-random optparse-applicative ../throughput vector \
	/home/ml9951/repos/ghc-wip/ghc/libraries/pastm /home/ml9951/repos/ghc-wip/ghc/libraries/stm -j49




