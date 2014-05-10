#!/bin/bash

# Write something to the variable
# to have DOT output
DOT=

for d in $@; do
	echo -e "\r\033[0KEntering directory $d"
	dsize=$(ls -l $d | wc -l)
	let "dsize -= 1"
	index=1
	for f in $d/*.smt2; do
		echo -ne "\r\033[0K[$index/$dsize] Testing $f ..."
		r0=$(z3 $f)
		../src/verif $f -d graph.dot > cpy.smt2
		if [ $DOT ]; then
		  ../src/verif $f -d graph.dot > cpy.smt2
			dot graph.dot -Tpng -o "$f.png"
		  rm graph.dot
		else
			../src/verif $f > cpy.smt2
		fi
		r1=$(z3 cpy.smt2)
		if [ "$r0" != "$r1" ]; then
			echo -e "\r\033[0K\e[1;31m$f  :  $r0 != $r1\e[0m"
			cp $f error/
		fi
		rm cpy.smt2
		let "index += 1"
	done
done
echo -e "\r\033[0KDone."
