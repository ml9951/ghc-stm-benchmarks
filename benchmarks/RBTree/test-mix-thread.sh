#!/bin/bash

set -e

# heavy-write
# r=10
# g=4
# n=1000
# m=10

# light-write
# r=10
# g=10
# n=2000
# m=90

# small-tree
r=1000
g=4
n=100
m=90

# topo
#q=-qatopo-cores-threads-sockets
#q=-qatopo-cores-sockets-threads
q= 

# init?
i=
#i=-i

# for exe in htm-bloom htm-bloom-C-reads; do
for exe in no-invariants coarse htm-bloom; do
# for exe in no-invariants; do
# for exe in coarse htm-bloom; do
# for exe in htm-bloom; do
    main=./Main-$exe
    for t in `seq 1 72` ; do
        perf stat -e tx-start,tx-capacity,tx-conflict -- $main -e $n -t $t -r $r -g $g -m $m $i +RTS --stm-stats $q
    done
done


