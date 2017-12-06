#!/bin/bash

# Show differences between measure and calculated proctor density 

# files to be compared
file_1="Veg_BD_all_norm.dat"
file_2="proctor-SERDP.dat"

# columns to be extracted
col_1="2 4 10"
col_2="2 3 9 "

# extract the multiple values from file_1, sort and discard duplicates
cat ${file_1} | colex ${col_1} | sort -n | uniq > temp_1
# extract multiple values from file_2, and sort (no duplicates there)
cat ${file_2} | colex ${col_2} | sort -n | uniq > temp_2

# sorted so now can abut columns to be compared
abut temp_1 temp_2 > temp_3

echo "set term wxt size 1800,1000" > temp.plt
echo "set key bottom" >> temp.plt
echo "pauseflg = -1" >> temp.plt
echo "set xlabel 'Measured'" >> temp.plt
echo "set ylabel 'Functional'" >> temp.plt
#echo "set xrange [0:]" >> temp.plt
#echo "set yrange [0:]" >> temp.plt
echo "plot x, 'temp_3' using 6:3:1 with labels title 'Proctor Density (Mg/m^3)'" >> temp.plt
echo "pause pauseflg" >> temp.plt
gnuplot temp.plt

# remove temporary files
rm temp_1 temp_2 temp_3 temp.plt

