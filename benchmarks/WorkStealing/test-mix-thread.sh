#!/bin/bash

set -e

s=1000
b=10
D=8
q=-qacadmium-topo
echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

for exe in fineLINKED fineCHASELEV tl2LINKED tl2CHASELEV ; do
    main=./$exe
    for t in `seq 1 48` ; do
        cmd="$main -s $s -b $b -D $D -t $t +RTS -N$t $q" 
        echo $cmd
        $cmd &>> $1
    done
done


