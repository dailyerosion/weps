#!/bin/bash

# extracts columns for Bulk Density analysis from spreadsheet. (colex did not work)

input="Veg_BD_plot_texture_2015-05-06.csv"
output="Veg_BD_data.dat"

rm -f ${output}

while read -r line
do
	IFS=, read -r f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28 f29 f30 f31 f32 f33 f34 f35 f36<<<"${line}"
	# If this value is missing, then all BD and WC data for that line are missing
	if [ "${f35}" != "" ]
        then
		# Columns extracted are:
		# Sample_# Site_Code Average%_Clay Average%_Silt Average%_Sand Average%_OM Vehicle_Code Pass Loc 5cm_BD_(0-5cm) 10cm_BD_(5-10cm) 15cm_BD_(10-15cm) 5cm_WC_(0-5cm) 10cm_WC_(5-10cm) 15cm_WC_(10-15cm)
	        #echo "${f1}	${f3}	${f9}	${f8}	${f7}	${f10}	${f14}	${f19}	${f20}	${f27}	${f28}	${f29}	${f33}	${f34}	${f35}" | grep -v "http:"| grep -v "Mallow" >> ${output}

		# Sample_# SoilCode Average%_Clay Average%_Silt Average%_Sand Average%_OM TrackCode Plot Rep Pass Loc 5cm_BD_(0-5cm) 10cm_BD_(5-10cm) 15cm_BD_(10-15cm) 5cm_WC_(0-5cm) 10cm_WC_(5-10cm) 15cm_WC_(10-15cm)
	        echo "${f1}	${f6}	${f9}	${f8}	${f7}	${f10}	${f15}	${f17}	${f18}	${f19}	${f20}	${f27}	${f28}	${f29}	${f33}	${f34}	${f35}" | grep -v "http:"| grep -v "Mallow" >> ${output}
	fi
done < "${input}"
