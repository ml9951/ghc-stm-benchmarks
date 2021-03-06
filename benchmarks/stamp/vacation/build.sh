#!/bin/bash

set -e

bc=-f-byteCounter
#bc=-fbyteCounter

for n in pastm-tl2; do
# for n in htm-mut; do
# for n in htm-bloom; do

    ghc=/home/ml9951/repos/ghc-wip/bin/ghc
    sb=.cabal-sandbox-$n
    dist=dist-$n

    echo "------------- $n -------------------"

    cabal sandbox init --sandbox=$sb

    if [ $n == "htm-mut" ] || [ $n == "htm-mut-fine" ] ; then
        echo Building with tstruct support.
        d=-ftstruct
    elif [ $n == "pastm-tl2" ]; then
        echo "building tl2"
        d=-fpastmtl2
        cabal install --with-ghc=$ghc /home/ml9951/repos/ghc-wip/ghc/libraries/stm
        cabal install --with-ghc=$ghc /home/ml9951/repos/ghc-wip/ghc/libraries/pastm
    elif [ $n == "pastm-fine" ]; then
        echo "building pastm-fine"
        cabal install --with-ghc=$ghc /home/ml9951/repos/ghc-wip/ghc/libraries/stm
    else
        d=
    fi

    cabal sandbox add-source ../../throughput/
    cabal sandbox add-source ../../random/pcg-random/
    cabal install $bc ../../throughput --with-ghc=$ghc
    cabal install vacation.cabal $d --with-ghc=$ghc --builddir=$dist --disable-executable-stripping
    
    cp ./.cabal-sandbox-$n/bin/vacation ./.cabal-sandbox-$n/bin/vacation-$n

#    $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build-$n -fno-omit-yields -o Main-$n
#   $ghc -O2 -threaded -rtsopts -eventlog Main.hs -outputdir .build -o Main-event

done























