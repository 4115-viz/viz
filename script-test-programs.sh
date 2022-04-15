#!/bin/bash

# log into the the Viz Docker Container
#docker run -it -v $(pwd):/home/viz -w=/home/viz viz /bin/bash

# first gotta change to test/programs/ to get list of files
chmod 766 test
chmod 766 test/programs/
cd test/programs/
echo
PWD=$(pwd)
echo "Running Test Programs in: $PWD"

# Need to use the Docker Container to run our programs since LLVM is installed in it

# pipe all of the filenames into a txt file, for looping
ls *.viz > viz_test_files.txt 
#ls *.err > viz_err_files.txt # does not handle .err files yet

# change back to the root to run the ./viz script correctly
cd ../..

# set variables
num_tests=0
num_passed=0
counter=1

# loop through the viz_test_files.txt file to build the .exe files 
# in the _build folder

echo "Building all of the Executables be patient..."

while IFS= read -r line; do

    BASE=$(echo "$line" | cut -d'.' -f 1) 
    
    FILENAME="test/programs/$BASE.viz"
    
    # build inside the container
    ./viz $FILENAME
    clear # clear the terminal

done < test/programs/viz_test_files.txt

echo "Now lets run the executables and tests..."

# loop through the viz_test_files.txt file
while IFS= read -r line; do

    BASE=$(echo "$line" | cut -d'.' -f 1) 
    
    FILENAME="test/programs/$BASE.viz"
    REFFILE="test/programs/$BASE.ref"
    OUTFILE="test/programs/$BASE.out"
    EXECUTABLE="$BASE.exe"
    
    # build inside the container
    #./viz $FILENAME
    #clear # clear the terminal

    # run the executable and pipe into .out file
    ./_build/$EXECUTABLE > $OUTFILE 2>> "test/programs/log.txt"

    # get the diff
    run_test=$(diff $OUTFILE $REFFILE)
    if [$run_test = ""]
    then
        echo "Test $counter: $FILENAME passed"
        ((num_passed++))
    else
        echo "Test $counter: $FILENAME failed"
        echo "--------------------------------"
        echo $run_test
        echo "--------------------------------"
    fi

    ((counter++))
    ((num_tests++))
    
done < test/programs/viz_test_files.txt






# run the error cases
#for entry in $fail_cases
#do
    # split by '.' into array choose base
#    BASE=$(echo "$entry" | cut -d'.' -f 1)
    
#    FILENAME="$BASE.err"
#    REFFILE="$BASE.ref"
#    OUTFILE="$BASE.out"
#    TMPFILE="$BASE.tmp"
    
#    touch $OUTFILE
#    SUB="Entering directory"
#    # execute the dune test
#    dune exec -- vc $FILENAME -ts 2> $TMPFILE
    
    # need to pipe the stdout into temp file
    # trying to remove this pesky "Entering directory '/path/to/directory' 
    # which is ruining my tests adding an additional line to the .out file

    # get rid of this annoying "Entering directory '/path/' line ruining tests"
#    while read line; do 
#        # check if line contains entering directory 
#        if grep -q "$SUB" <<< "$line"
#        then 
            #echo "PRESENT"
            #echo ">>>>>>>"
#            echo $line >> "log.txt"
#        else
#            echo $line >> $OUTFILE
#        fi
#    done < $TMPFILE

    # get the diff
#    run_test=$(diff $OUTFILE $REFFILE)
#    if [$run_test = ""]
#    then
#        echo "Test $counter: $FILENAME passed"
#        ((num_passed++))
#    else
#        echo "Test $counter: $FILENAME failed"
#        echo "--------------------------------"
#        echo $run_test
#        echo "--------------------------------"
#    fi

#    ((counter++))
#    ((num_tests++))
    
#done

# print results back out to the console
echo "($num_passed / $num_tests) tests passed"

# remove .out files
echo
echo "removing all the intermediate files in $PWD"
echo

# comment this out to see the intermediary files
#rm test/programs/*.out test/programs/*.tmp test/programs/log.txt test/programs/viz_test_files.txt
rm test/programs/*.out test/programs/log.txt test/programs/viz_test_files.txt