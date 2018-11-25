.PHONY: all hooks clean test

all:
	dune build

test:
	dune runtest -j1 --no-buffer

data:
	bench/gather_data.sh

hooks:
	rm -rf .git/hooks
	ln -sft .git ../hooks

clean:
	dune clean
	git clean -dfX
