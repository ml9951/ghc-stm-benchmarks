#!/bin/bash

set -e

# sparse writes
n=1000
m=90

# topo
#q=-qatopo-cores-threads-sockets
q=-qatopo-cores-sockets-threads
#q= 

# init?
i=
#i=-i

s=1000

# for exe in no-invariants coarse htm-bloom hle-bloom; do
for exe in IORef-no-invariants; do
    main=./Main-$exe
    for t in `seq 1 72` ; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -m $m -s $s $i +RTS --stm-stats -lsu $q -N$t

        ./countUserTransactions ${main}.eventlog
    done
done


