#!/bin/bash

bc=-f-byteCounter
#bc=-fbyteCounter
other=

for n in pastm-tl2; do
    if [ $n == "pastm-tl2" ] ; then
       ghc=/home/ml9951/repos/ghc-wip/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fpastmtl2
       other="cabal install --with-ghc $ghc /home/ml9951/repos/ghc-wip/ghc/libraries/pastm/ \
                            --extra-include-dirs /home/ml9951/repos/ghc-wip/ghc/rts/"
    elif [ $n == "pastm-fine" ] ; then
       ghc=/home/ml9951/repos/ghc-old/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fpastmfine
    elif [ $n == "ctrie" ] ; then
       ghc=/localdisk/ryates/ghc-7.10/ghc-no-invariants-build/bin/ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       d=-fctrie
    elif [ $n = "hashmapcas" ] || [ $n = "hashmaptvar" ] || [ $n = "hashmaptmvar" ] || [ $n = "hashmapmvar" ] || [ $n = "hashmap" ]; then
        ghc=/localdisk/ryates/ghc-7.10/ghc-no-invariants-build/bin/ghc
        sb=.cabal-sandbox-$n
        echo "------------- $n -------------------"
        d=-f$n
    else
       ghc=ghc
       sb=.cabal-sandbox-$n
       echo "------------- $n -------------------"
       if [ $n == "htm-mut" ] || [ $n == "htm-mut-fine" ] ; then
           echo Building with tstruct support.
           d=-ftstruct
       elif [ $n = "head" ] ; then
           d=-fhead
       else
           d=
       fi
    fi

    # cabal install optparse-applicative stm MonadRandom ../throughput/ --with-ghc $ghc
    cabal sandbox init --sandbox=$sb

    if [ $n == "pastm-tl2" ] ; then
        $other
        cabal install optparse-applicative /home/ml9951/repos/ghc-wip/ghc/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc -j48
    elif [ $n == "pastm-fine" ] ; then
        $other
        cabal install optparse-applicative /home/ml9951/repos/ghc-wip/ghc/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc -j48
    elif [ $n == "head" ] ; then
        cabal install optparse-applicative \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc -j48
    else
        $other
        cabal install optparse-applicative /home/ml9951/repos/ghc-wip/ghc/libraries/stm \
              $bc ../throughput/ ../random/pcg-random/ ./ $d \
              --disable-executable-stripping --with-ghc $ghc -j48
    fi

    cp $sb/bin/rbtree-throughput Main-$n
#    cabal install optparse-applicative stm mwc-random ../throughput/ --with-ghc $ghc
    
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#    cabal exec -- $ghc -O2 -threaded -rtsopts -eventlog Main-stm-containers.hs -outputdir .build-sc-$n -fno-omit-yields -o Main-sc-$n
#    $ghc -O2 -threaded -rtsopts -eventlog Main-32.hs -outputdir .build-$n -fno-omit-yields -o Main-32-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done
