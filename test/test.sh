#!/bin/bash

# Test script
# The script expects a list of directories as input.
# For each file in all directories, it will run the program verif
# and check each output with z3.

# All files for which the program is found to be erronous are copied to
# the directory ERROR
ERROR=error

# Selection of the abstract domain.
DOMAIN=box

# Write something to the variable to have DOT graph produced
DOT=

# Copy the binary
cp ../src/_build/front.native verif

# Loop on input directories
for d in $@; do
	echo -e "\r\033[0KEntering directory $d"
	DSIZE=$(ls -l $d | wc -l)
	SOLVED=0
	INDEX=1
	DIFF=0
	TIME=0
	let "DSIZE -= 1"
	for f in $d/*.smt2; do
		echo -ne "\r\033[0K[$INDEX/$DSIZE] Testing $f ..."
		# Execute z3 on untouched example.
		START=$(date +%s%N)
		R0=$(z3 $f)
		T0=$(date +%s%N)
		let "T0 -= START"
		# Run verif, and z3 if the result is unknown.
		START=$(date +%s%N)
		if [ $DOT ]; then
		  R1=$(./verif $f -d graph.dot -s tmp.smt2 --domain $DOMAIN)
			dot graph.dot -Tpng -o "$f.png"
		else
			R1=$(./verif $f -s tmp.smt2 --domain $DOMAIN)
		fi
		if [ "$R0" == "$R1" ]; then
			let "SOLVED += 1"
		else
			if [ "$R1" == "unknown" ]; then
				R1=$(z3 tmp.smt2)
			fi
		fi
		T1=$(date +%s%N)
		let "T1 -= START"
		if [ "$R0" != "$R1" ]; then
			echo -e "\r\033[0K\e[1;31m$f  :  $R0 != $R1\e[0m"
			cp $f $ERROR/
		fi
		rm -f tmp.smt2 graph.dot
		let "INDEX += 1"
		let "DIFF += T1 - T0"
		let "TIME += T0"
	done
	PSOLVED=$(echo "$SOLVED * 100 / $DSIZE" | bc)
	OVERHEAD=$(echo "$DIFF * 100 / $TIME" | bc)
	let "TIME /= 1000000"
	let "DIFF /= 1000000"
	echo -e "\r\033[0KEnded directory $d"
	echo -e "... solved with abstract: $SOLVED/$DSIZE ($PSOLVED%)"
	echo -e "......... time  overhead: $DIFF ms/$TIME ms ($OVERHEAD%)"
done
rm -f verif
echo -e "\r\033[0KDone."

