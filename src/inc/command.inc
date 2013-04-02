!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the variables set by command line switches
!     which are parsed in "cmdline.for"

!     + + + VARIABLE DECLARATIONS + + +

      integer wc_type
      integer ifc_format
      integer report_debug
      integer saeinp_daysim
      integer saeinp_jday
      integer saeinp_all
      integer init_cycle
      integer run_erosion
      integer calibrate_crops
      integer calibrate_rotcycles
      integer cook_yield
      integer growth_stress
      real    water_stress_max
      integer layer_scale
      integer layer_infla
      integer layer_weighting
      integer puddle_warm
      integer winter_ann_root
      real    wind_max_value
      integer wepp_hydro
      integer soil_cond
      integer resurf_roots
      integer calc_confidence
      real    frac_frst_mass_lost
      integer transpiration_depth

      common /command_args/ wc_type, ifc_format, report_debug,          &
     & saeinp_daysim, saeinp_jday, saeinp_all, init_cycle, run_erosion, &
     & calibrate_crops, cook_yield, growth_stress, water_stress_max,    &
     & layer_scale, layer_infla, layer_weighting, puddle_warm,          &
     & winter_ann_root, wind_max_value, wepp_hydro, soil_cond,          &
     & calibrate_rotcycles, resurf_roots, calc_confidence,              &
     & frac_frst_mass_lost, transpiration_depth

      save :: /command_args/

!     + + + VARIABLE DEFINITIONS + + +

!     wc_type    - Water content type (specifies units for soil wc variables)
!                  wc_type = 0      !1/3bar(vol) 15bar(vol)
!                  wc_type = 1      !1/3bar(vol) 15bar(grav)
!                  wc_type = 2      !1/3bar(grav) 15bar(grav)
!                  wc_type = 3      !ignore and compute internally
!                  wc_type = 4      !compute everything from texture, om, and cec

!     ifc_format - Specifies which soil ifc file format is being used
!                  ifc_format = 0   !new ifc file format (additional parms)
!                  ifc_format = 1   !old ifc file format

!     report_debug - specifies action of print statements in new reporting function
!                  report_debug = 0  !printing is off
!                  report_debug = 1  !printing is on
!                  report_debug = 2  !additional debug printing is on

!     saeinp_daysim - specifies the simulation day that a S_tand A_lone E_rosion INP_ut
!                     file will be created

!     saeinp_jday - specifies the julian day that a S_tand A_lone E_rosion INP_ut
!                   file will be created (used when date is input)

!     saeinp_all -  specifies that S_tand A_lone E_rosion INP_ut files will be created
!                   on every day erosion is entered. (in a subdirectory with daysim # in file name)

!     init_cycle - Specifies how many man rotation cycles are done
!                  init_cycle = 0   !no initialization cycle
!                  init_cycle = 1   !one initialization cycle (default)
!                  init_cycle = x   !x initialization cycles
     
!     run_erosion - Specifies whether the erosion submodel is run or not
!                  run_erosion = 0   !do not run erosion submodel
!                  run_erosion = 1   !run erosion submodel (default)
     
!     calibrate_crops - Specifies how many man rotation cycles are done
!                  calibrate_crops = 0   !do not run in crop calibration mode (dfault)
!                  calibrate_crops = 1   !run in crop calibration mode

!     cook_yield - flag setting which uses input from crop record to 
!                  guarantee a fixed yield/redsidue ratio at harvest
!                  (this is cooking the books :-(

!     growth_stress - flag setting which turns on water or temperature 
!                   stress (or both)
!                   growth_stress = 0  ! no stress values applied
!                   growth_stress = 1  ! turn on water stress
!                   growth_stress = 2  ! turn on temperature stress
!                   growth_stress = 3  ! turn on both

!     water_stress_max - Cap water stress at some maximum value
!                  (note maximum stress occurs at 0.0 and minimum stress at 1.0)
!                   water_stress_max = x.xx   ! specified stress limit

!     layer_scale - scale setting for thickness of soil layers used for 
!                   finite differencing in all areas of the model. This
!                   is used to set the minimum layer thickness for the 
!                   layer splitting. (Units are in millimeters, but no
!                   decimals are allowed)

!     layer_infla - setting for inflation of layer thickness with depth
!                   in percent of the previous layer

!     layer_weighting - specifies the layer weighting method to use
!                       layer_weighting = 0 (arithmetic mean, 0.5 method - default)
!                       layer_weighting = 1 (layer thickness porportional weighted)
!                       layer_weighting = 2 (internodal method, darcian mean) 

!     puddle_warm - Select soil puddling with saturation all above freezing
!                   puddle_warm = 0   ! disable
!                   puddle_warm = 1   ! enable

!     winter_ann_root - select root growth option for winter annuals
!                   winter_ann_root = 0  ! root depth grows at same rate as height
!                   winter_ann_root = 1  ! root depth grows with fall heat units

!     wind_max_value - Cap wind speeds to specified value
!                   wind_max_value = xx.x   ! specified wind speed
!                   wind_max_value > 0.0    ! sets wind_max_flag to 1

!     wepp_hydro - specifies hydrology calculation method used
!                  wepp_hydro = 0 ! darcian flow
!                  wepp_hydro = 1 ! Green-Ampt infiltration, simple drainage

!     soil_cond - specifies output of the soil conditioning index
!                 soil_cond = 0 ! no output
!                 soil_cond = 1 ! output file created

!     resurf_roots - specify whether buried roots are resurfaced via process 26
!                 resurf_roots = 0 ! no resurfacing of buried roots
!                 resurf_roots = 1 ! resurface buried roots

!     calc_confidence - flag to determine if confidence intervals for the
!                       rotation mean annual erosion are calculated
!                 calc_confidence = 0 ! no confidence interval calculation
!                 calc_confidence = 1 ! confidence interval calculated and reported
!                 calc_confidence = 2 ! confidence interval calculated and
!                                       used for early termination (not implemented)

!     transpiration_depth - flag to determine if deep furrows will change
!                 layer in the soil to which the roots can reach for water.
!                 Transpiration depth will be deeper than the crop root depth.
                
!     frac_frst_mass_lost - fraction of leaf mass that is frozen that disappears


!     Default values are set in "cmdline.for"
!     ------------------------------------------------------------------
