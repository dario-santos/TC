#!/bin/bash

score_i=0
max_i=0
start=`date +%s000`
echo "TC - Problema E - (ficheiros em tests/)"

for f in tests/*.txt; do
    max_i=`expr $max_i + 1`;
    expected=tests/`basename $f .txt`.out
    rm -f out
    start=`date +%s000`
    if (./a.out < $f) > out; then
	if cmp --quiet out $expected; then
        end=`date +%s000`
        elapsed=$(($end-$start))
        echo "$f - Time elapsed: $elapsed"
	    score_i=`expr $score_i + 1`;
	else
	    echo "  FALHA : saida errada para $f"
	fi
    else
	echo "  FALHA da interpretação para $f"
    fi
done

echo
percent=`expr 100 \* $score_i / $max_i`;
echo "Score: $score_i / $max_i testes, seja $percent%"
