.PHONY: clean console test

build:
	dune build

clean:
	dune clean

console:
	dune utop ./src

test:
	dune runtest ./test

watch:
	dune build -w
