#!/bin/bash

# file name
file_name="proctor-SERDP.dat"

# set up gnuplot file
echo "# graphing bulk density data by soil type" > temp.plt
echo "set term pdf" >> temp.plt
echo "set output 'textures.pdf'" >> temp.plt
echo "set xlabel 'Sand (%)'" >> temp.plt
echo "set ylabel 'Clay (%)'" >> temp.plt
echo "set xrange [0:100]" >> temp.plt
echo "set yrange [0:100]" >> temp.plt

echo "set size square" >> temp.plt

echo "tri_x(x,y)=x+y*cos(pi/3)" >> temp.plt
echo "tri_y(x,y)=y*sin(pi/3)" >> temp.plt

# create gnuplot commands for graphing this soil data against plot numbers
echo "set title 'Soil Textures'" >> temp.plt
echo "plot '${file_name}' using (tri_x(\$4,\$6)):(tri_y(\$4,\$6)):2 with labels title '' \\" >> temp.plt
# blank line to terminate plot command
echo "" >> temp.plt

gnuplot temp.plt

