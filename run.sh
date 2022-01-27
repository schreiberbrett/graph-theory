#!/bin/bash

ghc Graph.hs

./Graph

# This will produce some .dot files. bw-graph-01-images.dot bw-graph-02-images.dot ... etc.

# call graphviz on each .dot file.
# -O automatically names the files bw-graph-01-images.dot --> bw-graph-01-images.dot.svg
neato -O -Tsvg *-images.dot

