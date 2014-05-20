#!/bin/bash

# Write something to the variable
# to have DOT output
DOT=

# Copy the binary
cp ../src/_build/front.native verif
for d in $@; do
	echo -e "\r\033[0KEntering directory $d"
	dsize=$(ls -l $d | wc -l)
	let "dsize -= 1"
	index=1
	for f in $d/*.smt2; do
		echo -ne "\r\033[0K[$index/$dsize] Testing $f ..."
		r0=$(z3 $f)
		if [ $DOT ]; then
		  p=$(./verif $f -d graph.dot -s tmp.smt2)
			dot graph.dot -Tpng -o "$f.png"
		else
			p=$(./verif $f -s tmp.smt2)
		fi
		r1=$(z3 tmp.smt2)
		if [ "$r0" != "$r1" ]; then
			echo -e "\r\033[0K\e[1;31m$f  :  $r0 != $r1\e[0m"
			cp $f error/
		fi
		rm -f tmp.smt2 graph.dot
		let "index += 1"
	done
done
rm -f verif
echo -e "\r\033[0KDone."
