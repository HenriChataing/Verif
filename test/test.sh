#!/bin/bash

# Test script
# The script expects a list of directories as input.
# For each file in all directories, it will run the program verif
# and check each output with z3.

# All files for which the program is found to be erronous are copied to
# the directory ERROR
ERRDIR=error

# Selection of the abstract domain.
DOMAIN=box

# Write something to the variable to have DOT graph produced
DOT=

# Command timeout
DURATION=5m

# Copy the binary
cp ../src/_build/front.native verif

# Loop on input directories
for d in $@; do
	echo -e "\r\033[0KEntering directory $d"
	DSIZE=$(ls -l $d | wc -l)
	INDEX=1

	SOLVED=0
	TIMEDOUT=0
	ERROR=0

	DIFF=0
	TIME=0
	let "DSIZE -= 1"
	for f in $d/*.smt2; do
		echo -ne "\r\033[0K[$INDEX/$DSIZE] Testing $f ..."
		# Execute z3 on untouched example.
		START=$(date +%s%N)
		R0=$(timeout $DURATION z3 $f)
		S0=$?
		T0=$(date +%s%N)

		# Abandon if timeout.
		if [ $S0 -eq 124 ]; then
			let "TIMEDOUT += 1"
		else
			let "T0 -= START"

			# Run verif
			START=$(date +%s%N)
			if [ $DOT ]; then
			  R1=$(./verif $f -d graph.dot -s tmp.smt2 --domain $DOMAIN 2> error.log)
				S1=$?
				dot graph.dot -Tpng -o "$f.png"
			else
				R1=$(./verif $f -s tmp.smt2 --domain $DOMAIN 2> error.log)
				S1=$?
			fi

			# Check status
			if [ $S1 -eq 0 ]; then
				# Check result
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
					let "ERROR += 1"
					cp $f $ERRDIR/
				fi
				rm -f tmp.smt2 graph.dot
			else
				let "ERROR += 1"
			fi
			let "DIFF += T1 - T0"
			let "TIME += T0"
		fi
		let "INDEX += 1"
	done
	PSOLVED=$(echo "$SOLVED * 100 / $DSIZE" | bc)
	PERROR=$(echo "$ERROR * 100 / $DSIZE" | bc)
	PTIMEDOUT=$(echo "$TIMEDOUT * 100 / $DSIZE" | bc)
	OVERHEAD=$(echo "$DIFF * 100 / $TIME" | bc)
	let "TIME /= 1000000"
	let "DIFF /= 1000000"
	echo -e "\r\033[0KEnded directory $d"
	echo -e "... solved with abstract: $SOLVED/$DSIZE ($PSOLVED%) ($DOMAIN)"
	echo -e "....... program failures: $ERROR/$DSIZE ($PERROR%)"
	echo -e ".............. timed out: $TIMEDOUT/$DSIZE ($PTIMEDOUT%) (after $DURATION)"
	echo -e "......... time  overhead: $DIFF ms/$TIME ms ($OVERHEAD%)"
done
rm -f verif
echo -e "\r\033[0KDone."

