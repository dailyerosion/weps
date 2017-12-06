#!/bin/bash

# calculate differences between output values in two files 

# files to be compared
file_1="Veg_BD_proc_only.dat"
file_2="Veg_BD_proc_loosen.dat"

# range of columns to be compared
col_1=7
col_2=9

# abut columns to be compared to end of file

for col_n in $( seq ${col_1} ${col_2} )
do
  cat ${file_1} | colex ${col_n} > temp1
  cat ${file_2} | colex ${col_n} > temp2
  abut temp1 temp2 | dm 'if INLINE > 1 then INPUT else SKIP' > temp3
  cat temp3 | dm x2-x1 | stats min mean max
  echo "set term wxt size 1800,1000" > temp.plt
  echo "set key bottom" >> temp.plt
  echo "pauseflg = -1" >> temp.plt
  echo "plot x, 'temp3'" >> temp.plt
  echo "pause pauseflg" >> temp.plt
  gnuplot temp.plt
done

# remove temporary files
rm temp1 temp2 temp3 temp.plt

