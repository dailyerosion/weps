# graphing soil compaction by vehicle
set term pdf
set output 'compaction.pdf'
set xlabel 'Passes'
set ylabel 'Normalized Compaction'
set title 'Normalized Compaction for FW Vehicle at 0_5cm Depth'
plot 'FW_0_5cm.dat' using 1:2 with linespoints title 'mean' \
    ,'FW_0_5cm.dat' using 1:3 with linespoints title '4-SL' \

set title 'Normalized Compaction for FW Vehicle at 5_10cm Depth'
plot 'FW_5_10cm.dat' using 1:2 with linespoints title 'mean' \
    ,'FW_5_10cm.dat' using 1:3 with linespoints title '4-SL' \

set title 'Normalized Compaction for FW Vehicle at 10_15cm Depth'
plot 'FW_10_15cm.dat' using 1:2 with linespoints title 'mean' \
    ,'FW_10_15cm.dat' using 1:3 with linespoints title '4-SL' \

set title 'Normalized Compaction for FW Vehicle Soils Averaged'
plot 'FW_all_depths.dat' using 1:2 with linespoints title 'mean' \
    ,'FW_all_depths.dat' using 1:3 with linespoints title '0_5cm' \
    ,'FW_all_depths.dat' using 1:4 with linespoints title '5_10cm' \
    ,'FW_all_depths.dat' using 1:5 with linespoints title '10_15cm' \

set title 'Normalized Compaction for TR Vehicle at 0_5cm Depth'
plot 'TR_0_5cm.dat' using 1:2 with linespoints title 'mean' \
    ,'TR_0_5cm.dat' using 1:3 with linespoints title '1-SiCL' \
    ,'TR_0_5cm.dat' using 1:4 with linespoints title '2-SiL' \
    ,'TR_0_5cm.dat' using 1:5 with linespoints title '3-LS' \
    ,'TR_0_5cm.dat' using 1:6 with linespoints title '5-L' \
    ,'TR_0_5cm.dat' using 1:7 with linespoints title '6-SL' \

set title 'Normalized Compaction for TR Vehicle at 5_10cm Depth'
plot 'TR_5_10cm.dat' using 1:2 with linespoints title 'mean' \
    ,'TR_5_10cm.dat' using 1:3 with linespoints title '1-SiCL' \
    ,'TR_5_10cm.dat' using 1:4 with linespoints title '2-SiL' \
    ,'TR_5_10cm.dat' using 1:5 with linespoints title '3-LS' \
    ,'TR_5_10cm.dat' using 1:6 with linespoints title '5-L' \
    ,'TR_5_10cm.dat' using 1:7 with linespoints title '6-SL' \

set title 'Normalized Compaction for TR Vehicle at 10_15cm Depth'
plot 'TR_10_15cm.dat' using 1:2 with linespoints title 'mean' \
    ,'TR_10_15cm.dat' using 1:3 with linespoints title '1-SiCL' \
    ,'TR_10_15cm.dat' using 1:4 with linespoints title '2-SiL' \
    ,'TR_10_15cm.dat' using 1:5 with linespoints title '3-LS' \
    ,'TR_10_15cm.dat' using 1:6 with linespoints title '5-L' \
    ,'TR_10_15cm.dat' using 1:7 with linespoints title '6-SL' \

set title 'Normalized Compaction for TR Vehicle Soils Averaged'
plot 'TR_all_depths.dat' using 1:2 with linespoints title 'mean' \
    ,'TR_all_depths.dat' using 1:3 with linespoints title '0_5cm' \
    ,'TR_all_depths.dat' using 1:4 with linespoints title '5_10cm' \
    ,'TR_all_depths.dat' using 1:5 with linespoints title '10_15cm' \

set title 'Normalized Compaction for WH Vehicle at 0_5cm Depth'
plot 'WH_0_5cm.dat' using 1:2 with linespoints title 'mean' \
    ,'WH_0_5cm.dat' using 1:3 with linespoints title '1-SiCL' \
    ,'WH_0_5cm.dat' using 1:4 with linespoints title '2-SiL' \
    ,'WH_0_5cm.dat' using 1:5 with linespoints title '3-LS' \
    ,'WH_0_5cm.dat' using 1:6 with linespoints title '4-SL' \
    ,'WH_0_5cm.dat' using 1:7 with linespoints title '5-L' \
    ,'WH_0_5cm.dat' using 1:8 with linespoints title '6-SL' \

set title 'Normalized Compaction for WH Vehicle at 5_10cm Depth'
plot 'WH_5_10cm.dat' using 1:2 with linespoints title 'mean' \
    ,'WH_5_10cm.dat' using 1:3 with linespoints title '1-SiCL' \
    ,'WH_5_10cm.dat' using 1:4 with linespoints title '2-SiL' \
    ,'WH_5_10cm.dat' using 1:5 with linespoints title '3-LS' \
    ,'WH_5_10cm.dat' using 1:6 with linespoints title '4-SL' \
    ,'WH_5_10cm.dat' using 1:7 with linespoints title '5-L' \
    ,'WH_5_10cm.dat' using 1:8 with linespoints title '6-SL' \

set title 'Normalized Compaction for WH Vehicle at 10_15cm Depth'
plot 'WH_10_15cm.dat' using 1:2 with linespoints title 'mean' \
    ,'WH_10_15cm.dat' using 1:3 with linespoints title '1-SiCL' \
    ,'WH_10_15cm.dat' using 1:4 with linespoints title '2-SiL' \
    ,'WH_10_15cm.dat' using 1:5 with linespoints title '3-LS' \
    ,'WH_10_15cm.dat' using 1:6 with linespoints title '4-SL' \
    ,'WH_10_15cm.dat' using 1:7 with linespoints title '5-L' \
    ,'WH_10_15cm.dat' using 1:8 with linespoints title '6-SL' \

set title 'Normalized Compaction for WH Vehicle Soils Averaged'
plot 'WH_all_depths.dat' using 1:2 with linespoints title 'mean' \
    ,'WH_all_depths.dat' using 1:3 with linespoints title '0_5cm' \
    ,'WH_all_depths.dat' using 1:4 with linespoints title '5_10cm' \
    ,'WH_all_depths.dat' using 1:5 with linespoints title '10_15cm' \

