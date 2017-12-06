set term wxt size 1800,1000
set key bottom
pauseflg = -1

set ylabel 'Data BD (Mg/m^3)'

set term wxt 1
set xlabel 'Settled Bulk Density (Mg/m^3)'
plot x, 'Veg_BD_all_norm.dat' using 9:15 title '0-5', '' using 9:16 title '5-10', '' using 9:17 title '10-15'

set term wxt 2
set xlabel 'Proctor Bulk Density (Mg/m^3)'
plot x, 'Veg_BD_all_norm.dat' using 10:15 title '0-5', '' using 10:16 title '5-10', '' using 10:17 title '10-15'

set term wxt 3
set xlabel 'Settled Bulk Density (Mg/m^3)'
plot x, 'Veg_BD_all_norm.dat' using 9:($6==0?$15:NaN) title '0-5', '' using 9:($6==0?$16:NaN) title '5-10', '' using 9:($6==0?$17:NaN) title '10-15'

set term wxt 4
set xlabel 'Degree of Loosening:Compaction (1:-1)'
plot 'Veg_BD_all_norm.dat' using 9:($6==0?$18:NaN) title '0-5', '' using 9:($6==0?$19:NaN) title '5-10', '' using 9:($6==0?$20:NaN) title '10-15'

pause pauseflg
