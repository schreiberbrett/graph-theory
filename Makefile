all: weave tangle

weave:
	~/bin/lit --weave src/main.lit --out-dir weave
	
tangle:
	~/bin/lit --tangle src/main.lit --out-dir tangle
