#!/bin/bash

iters=5


for m in 10 50 90
do
    for i in $(eval echo {1..$iters})
    do
	./test-mix-thread.sh skiplist/$m-reads$i.txt $m
	sleep 1
    done
done




