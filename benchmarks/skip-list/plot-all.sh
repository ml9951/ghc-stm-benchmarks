#!/bin/bash



for r in 10 50 90
do
    ./parse rbtree/$r-reads* -T
    cd figures
    pdflatex -interaction=nonstopmode fig.tex
    cp fig.pdf rbtree-$r-reads.pdf
    cd ../
    
done


