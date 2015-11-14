#!/bin/bash

b=$1

for i in `seq 1 5` ; do
#    sleep 2
    ./test-low.sh low-$b-${i}.log
#    ./plot.sh $b-${i}.log $b-${i}.pdf
	echo ""
done

for i in `seq 1 5` ; do
#    sleep 2
    ./test-high.sh high-$b-${i}.log
#    ./plot.sh $b-${i}.log $b-${i}.pdf
done

#./plotMulti.sh max $b-max.pdf $b-*.log
#./plotMulti.sh ave $b-ave.pdf $b-*.log
#./plotMulti.sh mean $b-mean.pdf $b-*.log
#./plotMulti.sh min $b-min.pdf $b-*.log

