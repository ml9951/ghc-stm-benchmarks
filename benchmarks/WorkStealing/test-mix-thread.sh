#!/bin/bash

set -e

s=1000
b=10
D=8

echo "Benchmark running with log to $1"

rm -f $1 &> /dev/null

for exe in fine tl2; do
    main=./$exe
    for t in `seq 1 48` ; do
        cmd="$main -s $s -b $b -D $D -t $t +RTS -N$t"
        echo $cmd
        $cmd &>> $1
    done
done


