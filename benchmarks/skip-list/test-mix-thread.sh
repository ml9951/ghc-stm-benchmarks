#!/bin/bash

set -e

# sparse writes
n=100000
# n=1000
m=$2

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

# for exe in HashMap-no-invariants no-invariants coarse htm-bloom hle-bloom fine-hle htm-mut; do
# for exe in hashmap no-invariants fine-hle htm-mut ctrie; do
# for exe in hashmap hashmapcas no-invariants htm-mut; do
# for exe in hashmap hashmapcas hashmaptvar hashmaptmvar hashmapmvar no-invariants htm-mut; do
for exe in pastm-tl2 pastm-fine; do
# for exe in no-invariants htm-mut pastm-tl2-old pastm-fine pastm-tl2; do
# for exe in hashmapcas stmtrie-fine cuckoo stmtrie-htm no-invariants htm-mut; do
# for exe in htm-mut htm-mut-align; do
# for exe in no-invariants htm-bloom fine-hle htm-mut htm-mut-fine; do
# for exe in no-invariants htm-bloom fine-hle htm-mut; do
# for exe in htm-bloom; do
# for exe in IORef-no-invariants HashMap-no-invariants; do
    main=./Main-$exe
    for t in `seq 1 48` ; do
#    for ti in `seq 1 36` ; do
#        t=`ghc -e "$ti*2"`

        retry=
        if [ "$exe" == "cuckoo" ] || [ "$exe" == "stmtrie-htm" ]; then
          count=`ghc -e "max $t 10"`
          retry="--htm-retry=$count"
        fi
        if [ "$exe" == "htm-bloom" ] || [ "$exe" == "htm-mut" ] || [ "$exe" == "htm-mut-align" ] || [ "$exe" == "htm-mut-no-off" ]; then
          # retry="--hle-retry=25"
          count=`ghc -e "max $t 10"`
          retry="--htm-retry=$count"
        fi
        if [ "$exe" == "hle-bloom" ]; then
          count=`ghc -e "max $t 10"`
          retry="--hle-retry=$count"
        fi
        if [ "$exe" == "htm-mut-fine" ]; then
          retry="--htm-retry=0"
        fi
        if [ "$exe" == "fine-hle" ]; then
          # retry="--hle-retry=25 --htm-retry=25"
          # retry="--hle-retry=25 --htm-retry=0"
          retry="--htm-retry=10"
        fi

#-e 100000 -t 48 -m 10 -s 1000 +RTS -N48

#-e 100000 -t 1 -m 90 -s 1000 +RTS -N1 -qacadmium

#        cmd="$main -e 100000 -t $t -m 30 -s $s $i +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A8m $retry" # large high contention
#        cmd="$main -e 100000 -t $t -m $m -s $s $i +RTS -N$t -ki4k -kc64k -kb4k -A8m $retry" # large
        cmd="$main -e 100000 -t $t -m $m -s $s $i +RTS -N$t -qacadmium"
#        cmd="$main -e 10000  -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -ki4k -kc64k -kb4k -A8m $retry" # small
#        cmd="$main -e 100000 -t $t -m $m -s $s $i +RTS -N$t -ki4k -kc64k -kb4k -A8m $retry" # TL2 Special
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -ki8m -kc128m -kb8m -A8m -V0 $retry"
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -H -K512m -ki4k -kc64k -kb4k -A8m -V0"
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -H -K512m -A2m -V0"
#        cmd="$main -e $n -t $t -m $m -s $s $i +RTS --stm-stats $q -N$t -K128m -A8m -V0"
        echo $cmd
        $cmd &>> $1
    done
done


