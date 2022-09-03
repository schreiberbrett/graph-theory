all:
	~/bin/lit --weave src/main.lit --out-dir weave
	~/bin/lit --tangle src/main.lit --out-dir tangle
	gcc -S tangle/main.c -o tangle/main.S
	gcc -O3 -S tangle/main.c -o tangle/main-opt.S
	gcc tangle/main.c -o tangle/main

