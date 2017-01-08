#!/bin/bash

inputLocation=compi3
toolsLocation=testing-tools
testLocation=do-not-touch

if [ ! -d $testLocation ]; then 
	mkdir $testLocation
fi
cp $toolsLocation/* $testLocation
cp $inputLocation/* $testLocation
cd $testLocation


echo "_____________________________________________________________________________________________________________________________"

# --HW1-------------------------------------------------------

echo HW1 compare tests:
echo commented out...
#scheme --script compare-tests-hw1.scm | grep TESTS


# --HW2-------------------------------------------------------

#<<COMMENT

# .::part1::.
echo HW2 part1 compare tests:
scheme --script compare-tests-hw2.scm | grep TESTS

echo HW2 part1 my compare tests:
scheme --script my-compare-tests-hw2.scm | grep TESTS

# .::part2::.
echo HW2 part2 compare tests:
scheme --script compare-tests-hw2-CSE.scm | grep TESTS

echo HW2 part2 my compare tests:
scheme --script my-compare-tests-hw2-CSE.scm | grep TESTS

#COMMENT

# --HW3-------------------------------------------------------

echo HW3 tdd:
scheme --script test-hw3.scm 

echo HW3 compare tests:
#echo commented out...
scheme --script compare-tests-hw3.scm #| grep TESTS

#echo HW3 my compare tests:
#echo commented out...
#scheme --script my-compare-tests-hw3.scm





