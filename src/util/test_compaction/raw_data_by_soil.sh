#!/bin/bash

# file name
file_name="Veg_BD_all_norm.dat"

# set up gnuplot file
echo "# graphing bulk density data by soil type" > temp.plt
echo "set term pdf" >> temp.plt
echo "set output 'raw_data_by_soil.pdf'" >> temp.plt
echo "set xlabel 'Plot'" >> temp.plt
echo "set ylabel 'Bulk Density (Mg/m^3)'" >> temp.plt

# enumerate Soils
soils=$(cat ${file_name} | dm 'if INLINE>1 then INPUT else SKIP' | colex 2 | sort -n | uniq | tr '\n' ' ')

echo ${soils[@]}

# create graph for each soil
for soil in ${soils[@]}
do

  # create soil data file
  cat ${file_name} | dm "if s2='${soil}' then INPUT else SKIP" > ${soil}.dat

  for depth in 0_5cm 5_10cm 10_15cm
  do
    if [ "${depth}" = "0_5cm" ]
    then 
      col_wc=12
      col_meas=15
    elif [ "${depth}" = "5_10cm" ]
    then 
      col_wc=13
      col_meas=16
    elif [ "${depth}" = "10_15cm" ]
    then 
      col_wc=14
      col_meas=17
    fi

    # create gnuplot commands for graphing this soil data against plot numbers
    echo "set title 'Soil: ${soil} Depth: ${depth}'" >> temp.plt
    echo "plot '${soil}.dat' using 4:${col_meas}:7 with labels font 'Times,2' textcolor lt 1 title 'Measured' \\" >> temp.plt
    echo "    ,'${soil}.dat' using 4:9 with points title 'Settled' \\" >> temp.plt
    echo "    ,'${soil}.dat' using 4:10 with points title 'Proctor' \\" >> temp.plt
    echo "    ,'${soil}.dat' using 4:${col_wc}:7 with labels font 'Times,2' textcolor lt 4 title 'Proctor_wc' \\" >> temp.plt
    # blank line to terminate plot command
    echo "" >> temp.plt
  done

done

# extract soil type, proctor and settled density
cat ${file_name} | dm 'if INLINE>1 then INPUT else SKIP' | colex 2 9 10 | sort | uniq | dm s1 s1 s2 s3 | sed -e 's/1-SiCL/1/' -e 's/2-SiL/2/' -e 's/3-LS/3/' -e 's/4-SL/4/' -e 's/5-L/5/' -e 's/6-SL/6/' > all_soils.dat

# create graph showing proctor and settled density by soil
echo "set title 'All Soils'" >> temp.plt
echo "set xlabel 'Soil'" >> temp.plt
echo "plot 'all_soils.dat' using 1:3:xtic(2) with points title 'Settled' \\" >> temp.plt
echo "    ,'all_soils.dat' using 1:4:xtic(2) with points title 'Proctor' \\" >> temp.plt
echo "" >> temp.plt

gnuplot temp.plt

