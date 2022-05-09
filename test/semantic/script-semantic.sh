#!/bin/bash

echo
PWD=$(pwd)
echo "Running Semantic Tests in: $PWD"
#echo "Will run all files using -ts dune exec flag and compare .out files with .ref"

# semantic tests to run
# -s is a dune flag to print the semantic AST, we will pipe them to an out file

# pipe all of the filenames into a txt file, for looping
ls test-*.viz > viz_test_files.txt 
ls fail-*.err > viz_err_files.txt

num_tests=0
num_passed=0
counter=1

# loop through the viz_test_files.txt file
while IFS= read -r line; do

    # split by '.' into array choose base
    BASE=$(echo "$line" | cut -d'.' -f 1)
    
    #echo $BASE
    FILENAME="$BASE.viz"
    REFFILE="$BASE.ref"
    OUTFILE="$BASE.out"

    # execute the dune test
    dune exec -- vc $FILENAME -s > $OUTFILE 2>> "log.txt" 

    # get the diff
    run_test=$(diff $OUTFILE $REFFILE)
    if [$run_test = ""]
    then
        echo "Semantic Test $counter: $FILENAME passed"
        ((num_passed++))
    else
        echo "Semantic Test $counter: $FILENAME failed"
        echo "--------------------------------"
        echo $run_test
        echo "--------------------------------"
    fi

    ((counter++))
    ((num_tests++))
    
done < ./viz_test_files.txt

# loop through the viz_err_files.txt file
while IFS= read -r line; do

    # split by '.' into array choose base
    BASE=$(echo "$line" | cut -d'.' -f 1)
    
    FILENAME="$BASE.err"
    REFFILE="$BASE.ref"
    OUTFILE="$BASE.out"
    TMPFILE="$BASE.tmp"
    
    touch $OUTFILE
    SUB="Entering directory"
    # execute the dune test
    dune exec -- vc $FILENAME -s 2> $TMPFILE
    
    # need to pipe the stdout into temp file
    # trying to remove this pesky "Entering directory '/path/to/directory' 
    # which is ruining my tests adding an additional line to the .out file

    # get rid of this annoying "Entering directory '/path/' line ruining tests"
    while read line; do 
        # check if line contains entering directory 
        if grep -q "$SUB" <<< "$line"
        then 
            #echo "PRESENT"
            #echo ">>>>>>>"
            echo $line >> "log.txt"
        else
            echo $line >> $OUTFILE
        fi
    done < $TMPFILE

    # get the diff
    run_test=$(diff $OUTFILE $REFFILE)
    if [$run_test = ""]
    then
        echo "Semantic Test $counter: $FILENAME passed"
        ((num_passed++))
    else
        echo "Semantic Test $counter: $FILENAME failed"
        echo "--------------------------------"
        echo $run_test
        echo "--------------------------------"
    fi

    ((counter++))
    ((num_tests++))
    
done < ./viz_err_files.txt

# print results back out to the console
echo "($num_passed / $num_tests) Semantic Tests Passed"

# remove .out files
echo
echo "removing all the intermediate files in $PWD"
echo

# comment this out to see the intermediary files
rm *.out *.tmp log.txt viz_err_files.txt viz_test_files.txt