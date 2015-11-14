#!/bin/bash

set -e

# Phases
#p=1
p=2

# topo
#q=-qatopo-cores-threads-sockets
q=-qacadmium

echo "Benchmark running with log to logs/$1"

rm -f logs/$1 &> /dev/null

for exe in pastm-tl2 pastm-fine; do
  main=./.cabal-sandbox-$exe/bin/vacation-$exe
  for t in `seq 1 48` ; do

    retry=
    if [ "$exe" == "htm-bloom" ] || [ "$exe" == "htm-mut" ] || [ "$exe" == "htm-mut-align" ]; then
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

#"$main -c 1 -n 40 -q 90 -u 98 -r 16384 -t 0 -s 100000 -p 2 +RTS -qacadmium -N1 -ki4k -kc64k -kb4k -A8m"
#-c 48 -n 40 -q 90 -u 98 -t 0 -s 1000 -p 2 -r 16384 +RTS -N48
#-c 48 -n 40 -q 90 -u 98 -t 0 -s 1000 -p 2 -r 1048576 +RTS -N48



# low
#    cmd="$main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS --stm-stats $q -N$t -H -K32m $retry"
    cmd="$main -c $t -n 2 -q 90 -u 98 -r 16384 -t 0 -s 1000 -p $p +RTS $q -N$t -ki4k -kc64k -kb4k -A8m"
    echo $cmd
    $cmd &>> logs/$1
  
  done
done

