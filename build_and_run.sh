#!/bin/bash
dune build
for f in $@; do
    echo "${f}: "
    ./_build/default/Compiler.exe mips $f /tmp/code.s &&
	spim -f /tmp/code.s &&
	echo ""
done

