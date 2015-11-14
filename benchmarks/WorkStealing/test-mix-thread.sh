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

for exe in fine tl2; do
    main=./$exe
    for t in `seq 1 48` ; do
        cmd="$main -e 100000 -t $t +RTS -N$t"
        echo $cmd
        $cmd &>> $1
    done
done


