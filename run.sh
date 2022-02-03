#!/bin/bash

# mkdir -p makes if it does not already exist
mkdir -p build_artifacts

cp Graph.hs build_artifacts/Graph.hs
cd build_artifacts

# clean up any previous build artifacts
rm *.svg
rm *.png
rm *.dot

ghc Graph.hs

./Graph

# This will produce some .dot files. bw-graph-01-images.dot bw-graph-02-images.dot ... etc.

# call graphviz on each .dot file.
# -O automatically names the files bw-graph-01-images.dot --> bw-graph-01-images.dot.svg
neato -O -Tpng *-images.dot
neato -O -Tpng original.dot
dot -O -Tpng final.dot

cd ..

mkdir -p result

cp build_artifacts/final.dot.png result/final.dot.png

# "Eye of Gnome" image viewer.
# -s : slideshow view
eog result/*.png


