#!/bin/bash

# Write something to the variable
# to have DOT output
DOT=

# Percentage of simplification
per=0

for d in $@; do
	echo -e "\r\033[0KEntering directory $d"
	dsize=$(ls -l $d | wc -l)
	let "dsize -= 1"
	index=1
	for f in $d/*.smt2; do
		echo -ne "\r\033[0K[$index/$dsize] Testing $f ..."
		r0=$(z3 $f)
		if [ $DOT ]; then
		  p=$(../src/verif $f --verbose 2 -d graph.dot -s tmp.smt2)
			dot graph.dot -Tpng -o "$f.png"
		  rm graph.dot
		else
			p=$(../src/verif $f --verbose 2 -s tmp.smt2)
		fi
		r1=$(z3 tmp.smt2)
		per=$(echo "$per + $p" | bc)
		if [ "$r0" != "$r1" ]; then
			echo -e "\r\033[0K\e[1;31m$f  :  $r0 != $r1\e[0m"
			cp $f error/
		fi
		rm tmp.smt2
		let "index += 1"
	done
	per=$(echo "$per / $dsize" | bc)
	echo -e "\r\033[0KDone with directory $d; Average arg deletion: $per %"
done
echo -e "\r\033[0KDone."
