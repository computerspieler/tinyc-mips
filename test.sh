#!/bin/bash
for f in $@; do
    echo "${f}: "
    /home/user/Documents/Devoirs/INF108/Compiler/_build/default/Compiler.exe mips $f /tmp/code.s &&
	spim -f /tmp/code.s &&
	echo ""
done

