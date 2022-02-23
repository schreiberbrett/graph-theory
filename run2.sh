#!/bin/bash

# TODO: Convert this into a Makefile

# mkdir -p makes if it does not already exist
mkdir -p build_artifacts

cp Book.lhs build_artifacts/Book.lhs
cd build_artifacts

# clean up any previous build artifacts
rm *.svg
rm *.png
rm *.dot

ghc Book.lhs

./Book

# This will produce some .dot files. bw-graph-01-images.dot bw-graph-02-images.dot ... etc.

# call graphviz on each .dot file.
# -O automatically names the files bw-graph-01-images.dot --> bw-graph-01-images.dot.svg
neato -O -Tpng *-images.dot
neato -O -Tpng original.dot
neato -O -Tpng final.dot

cd ..

mkdir -p result

cp build_artifacts/final.dot.png result/final.dot.png

# "Eye of Gnome" image viewer.
# -s : slideshow view
# eog result/*.png

eog result/final.dot.png
