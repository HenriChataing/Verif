#!/bin/bash

DIR='linear linear-tree-like general'

if [ -a test.errors ]; then
	dsize=$(wc -l test.errors)
	set -- $dsize
	dsize=$1
	index=1
	for f in $(cat test.errors); do
		echo -ne "\r\033[0K[$index/$dsize] Testing $f ..."
		r0=$(z3 $f)
		../src/verif $f > cpy.smt2
		r1=$(z3 cpy.smt2)
		if [ "$r0" != "$r1" ]; then
			echo -e "\r\033[0K\e[1;31m$f  :  $r0 != $r1\e[0m"
		fi
		rm cpy.smt2
		let "index += 1"
	done
else	
	for d in $DIR; do
		echo -e "\r\033[0KEntering directory $d"
		dsize=$(ls -l $d | wc -l)
		let "dsize -= 1"
		index=1
		for f in $d/*.smt2; do
			echo -ne "\r\033[0K[$index/$dsize] Testing $f ..."
			r0=$(z3 $f)
			../src/verif $f > cpy.smt2
			r1=$(z3 cpy.smt2)
			if [ "$r0" != "$r1" ]; then
				echo -e "\r\033[0K\e[1;31m$f  :  $r0 != $r1\e[0m"
    	  echo $f >> test.errors
			fi
			rm cpy.smt2
			let "index += 1"
		done
	done
fi
echo -e "\r\033[0KDone."
