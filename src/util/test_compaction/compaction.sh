#!/bin/bash

# file name
file_name="Veg_BD_norm.dat"

# set up gnuplot file
echo "# graphing soil compaction by vehicle" > temp.plt
echo "set term pdf" >> temp.plt
echo "set output 'compaction.pdf'" >> temp.plt
echo "set xlabel 'Passes'" >> temp.plt
echo "set ylabel 'Normalized Compaction'" >> temp.plt

# enumerate vehicles
vehicles=$(cat ${file_name} | dm 'if INLINE>1 then INPUT else SKIP' | colex 2 | sort -n | uniq | tr '\n' ' ')

# select and average results for each vehicle
for vehicle in ${vehicles[@]}
do
  # create vehicle file - select values based on column 2
  cat ${file_name} | dm "if s2='${vehicle}' then INPUT else SKIP" > ${vehicle}_sel.dat

  # enumerate Soils
  soils=$(cat ${vehicle}_sel.dat | colex 1 | sort -n | uniq | tr '\n' ' ')
  # enumerate pass levels
  levels=$(cat ${vehicle}_sel.dat | colex 5 | sort -n | uniq | tr '\n' ' ')

  echo ${vehicle}
  echo ${soils[@]}
  echo ${levels[@]}

  # for each measured soil depth
  # create mean values for each pass level

  # Initialize output data files with header
  #rm -f ${vehicle}_all_depths.dat
  echo "pass mean 0_5cm 5_10cm 10_15cm" > ${vehicle}_all_depths.dat
  for depth in 0_5cm 5_10cm 10_15cm
  do
    # Clear output data file
    rm -f ${vehicle}_${depth}.dat
  done

  # create data line for each pass level
  for level in ${levels[@]}
  do

    all_mean=$(cat ${vehicle}_sel.dat | dm "if x5=${level} then INPUT else SKIP" | colex 7-9 | stats mean)
    depth_line=$( echo "${level} ${all_mean}" )

    for depth in 0_5cm 5_10cm 10_15cm
    do
      if [ "${depth}" = "0_5cm" ]
      then 
        colnum=7
      elif [ "${depth}" = "5_10cm" ]
      then 
        colnum=8
      elif [ "${depth}" = "10_15cm" ]
      then 
        colnum=9
      fi

      depth_mean=$(cat ${vehicle}_sel.dat | dm "if x5=${level} then x${colnum} else SKIP" | stats mean)
      depth_line=$( echo "${depth_line} ${depth_mean}" )

      # create mean value for each soil at each pass level
      soil_line=$( echo "${level} ${depth_mean}" )
      for soil in ${soils[@]}
      do
        soil_mean=$(cat ${vehicle}_sel.dat | dm "if s1='${soil}' then INPUT else SKIP" | dm "if x5=${level} then x${colnum} else SKIP" | stats mean)
        soil_line=$( echo "${soil_line} ${soil_mean}" )
      done

      # write pass level data values to file for graphing
      echo "${soil_line}" >> ${vehicle}_${depth}.dat
    done

    # write pass level data values to file for graphing
    echo "${depth_line}" >> ${vehicle}_all_depths.dat

  done

  # create gnuplot commands for graphing this vehicle
  for depth in 0_5cm 5_10cm 10_15cm
  do
    echo "set title 'Normalized Compaction for ${vehicle} Vehicle at ${depth} Depth'" >> temp.plt
    echo "plot '${vehicle}_${depth}.dat' using 1:2 with linespoints title 'mean' \\" >> temp.plt
    colnum=2
    for soil in ${soils[@]}
    do
      let "colnum = colnum + 1"
      echo "    ,'${vehicle}_${depth}.dat' using 1:${colnum} with linespoints title '${soil}' \\" >> temp.plt
    done
    # blank line to terminate plot command
    echo "" >> temp.plt
  done

  echo "set title 'Normalized Compaction for ${vehicle} Vehicle Soils Averaged'" >> temp.plt
  echo "plot '${vehicle}_all_depths.dat' using 1:2 with linespoints title 'mean' \\" >> temp.plt
  colnum=2
  for depth in 0_5cm 5_10cm 10_15cm
  do
    let "colnum = colnum + 1"
    echo "    ,'${vehicle}_all_depths.dat' using 1:${colnum} with linespoints title '${depth}' \\" >> temp.plt
  done
  # blank line to terminate plot command
  echo "" >> temp.plt

done

gnuplot temp.plt

