#!/bin/bash

path=$1
files=$(ls $path)
for filename in $files
do
    if [ ${filename##*.} = "mhs" ]
    then
        number=${filename%%.*} 
        echo "Test ${number}:"
        result=$(stack exec minhs-1 -- $filename)
        echo $result
        echo $(cat ${number}.out)
    fi
done