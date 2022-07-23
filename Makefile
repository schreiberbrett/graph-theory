all: weave tangle check

weave:
	~/bin/lit --weave src/main.lit --out-dir weave
	
tangle:
	~/bin/lit --tangle src/main.lit --out-dir tangle
	
check: tangle
	deno check tangle/*
	
