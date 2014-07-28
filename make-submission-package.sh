#!/bin/bash

touch *.lisp
./make-image.sh
./create-programs.sh

mkdir -p submission

rm -rf submission/solution
mkdir submission/solution
cp lambdaman.gcc ghost0.ghc ghost1.ghc submission/solution

rm -rf submission/code
mkdir submission/code
git archive master | (cd submission/code; tar x)

(cd submission; tar cfz solution.tar.gz solution code)
