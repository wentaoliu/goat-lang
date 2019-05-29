#!/bin/sh
Goat=./Goat
Oz=./oz/oz
Temp=./temp
# compile goat and oz 
make && make -C oz
# output directory
rm -rf $Temp && mkdir -p $Temp 
# compile all goat test files
for gt in $(find tests -name "*.gt"); do
  outdir=$Temp/$(dirname $gt)
  barename=$(basename $gt .gt)
  ozfile=$outdir/$barename
  mkdir -p $outdir
  $Goat $gt &> $ozfile.oz
done
