#!/bin/sh
# script adapted from test.sh
# usage: 
#    singleTest.sh [gt file] [oz option] 

file="$1"
option="$2"
Goat=./Goat
Oz=./oz/oz
Temp=./temp
# compile goat and oz 
make && make -C oz

#file naming information
outdir=$(dirname $file)
barename=$(basename $file .gt)
ozfile=$outdir/$barename

# run Goat on the gt file and put oz file in same place and run oz on the oz file
$Goat $file &> $ozfile.oz
echo "\n-----Oz Output-------"
$Oz $option "$ozfile.oz"
echo "\n"

