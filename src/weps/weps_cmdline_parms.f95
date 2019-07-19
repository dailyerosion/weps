!$Author: wagner $
!$Date: $
!$Revision: $
!$HeadURL:
!https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion/src/weps/weps_cmdline_parms.f95 $

module weps_cmdline_parms

! WEPS command line options
    integer :: wc_type        ! Water content type (specifies units for soil wc variables)
                              ! 0      !1/3bar(vol) 15bar(vol)
                              ! 1      !1/3bar(vol) 15bar(grav)
                              ! 2      !1/3bar(grav) 15bar(grav)
                              ! 3      !ignore and compute internally
                              ! 4      !compute everything from texture, om, and cec
    integer :: ifc_format     ! Specifies which soil ifc file format is being used
                              ! 0   !new ifc file format (additional parms)
                              ! 1   !old ifc file format
    integer :: hb_freq        ! specifies heartbeat frequency (yearly reporting interval of simulatio progress to GUI
                              ! 0  !heartbeat report is disabled
                              ! 1  !yearly (default)
                              ! 50 !every 50 years, etc.
    integer :: report_info    ! specifies action of print statements in new reporting function
                              ! 0  !printing is off
                              ! 1  !printing is on (default)
                              ! 2  !additional debug printing is on
    integer :: report_debug   ! specifies action of print statements in new reporting function
                              ! 0  !printing is off
                              ! 1  !printing is on
                              ! 2  !additional debug printing is on
    integer :: saeinp_daysim  ! specifies the simulation day that a S_tand A_lone E_rosion INP_ut
                              ! file will be created
    integer :: saeinp_jday    ! specifies the julian day that a S_tand A_lone E_rosion INP_ut
                              ! file will be created (used when date is input)
    integer :: saeinp_all     ! specifies that S_tand A_lone E_rosion INP_ut files will be created
                              ! on every day erosion is entered. (in a subdirectory with daysim # in file name)
    integer :: init_cycle     ! Specifies how many man rotation cycles are done
                              ! 0   !no initialization cycle
                              ! 1   !one initialization cycle (default)
                              ! x   !x initialization cycles
    integer :: run_erosion    ! Specifies whether the erosion submodel is run or not
                              ! 0   !do not run erosion submodel
                              ! 1   !run erosion submodel (default)
    integer :: calibrate_crops ! Specifies whether to do crop calibration or not
                               ! 0   !do not run in crop calibration mode (default)
                               ! 1   !run in crop calibration mode
    integer :: calibrate_rotcycles ! Specifies maximum number of cycles to run while calibrating
    integer :: cook_yield     ! flag setting which uses input from crop record to 
                              ! guarantee a fixed yield/redsidue ratio at harvest
                              ! (this is cooking the books :-(
    integer :: growth_stress  ! flag setting which turns on water or temperature stress (or both)
                              ! 0  ! no stress values applied
                              ! 1  ! turn on water stress
                              ! 2  ! turn on temperature stress
                              ! 3  ! turn on both
    real :: water_stress_max  ! Cap water stress at some maximum value
                              ! (note maximum stress occurs at 0.0 and minimum stress at 1.0)
                              ! water_stress_max = x.xx   ! specified stress limit
    integer :: layer_scale    ! scale setting for thickness of soil layers used for 
                              ! finite differencing in all areas of the model. This
                              ! is used to set the minimum layer thickness for the 
                              ! layer splitting. (Units are in millimeters, but no
                              ! decimals are allowed)
    integer :: layer_infla    ! setting for inflation of layer thickness with depth
                              ! in percent of the previous layer
    integer :: layer_weighting ! specifies the layer weighting method to use
                               ! 0 (arithmetic mean, 0.5 method - default)
                               ! 1 (layer thickness porportional weighted)
                               ! 2 (internodal method, darcian mean) 
    integer :: puddle_warm    ! Select soil puddling with saturation all above freezing
                              ! 0   ! disable
                              ! 1   ! enable
    integer :: winter_ann_root ! select root growth option for winter annuals
                               ! 0  ! root depth grows at same rate as height
                               ! 1  ! root depth grows with fall heat units
    integer :: wepp_hydro     ! specifies hydrology calculation method used
                              ! 0 ! darcian flow
                              ! 1 ! Green-Ampt infiltration, simple drainage
    integer :: soil_cond      ! specifies output of the soil conditioning index
                              ! 0 ! no output
                              ! 1 ! output file created
    integer :: resurf_roots   ! specify whether buried roots are resurfaced via process 26
                              ! 0 ! no resurfacing of buried roots
                              ! 1 ! resurface buried roots
    integer :: upgm_growth    ! grow WEPS crops using UPGM growth routines
                              ! 0 ! no UPGM gowth of WEPS crops (default)
                              ! 1 ! use UPGM routines
    integer :: calc_confidence ! flag to determine if confidence intervals for the
                               ! rotation mean annual erosion are calculated
                              ! 0 ! no confidence interval calculation
                              ! 1 ! confidence interval calculated and reported
                              ! 2 ! confidence interval calculated and used for early termination (not implemented)
    real :: frac_frst_mass_lost ! fraction of leaf mass that is frozen that disappears
    integer :: transpiration_depth ! flag to determine if deep furrows will change
                                   ! layer in the soil to which the roots can reach for water.
                                   ! Transpiration depth will be deeper than the crop root depth.
end module weps_cmdline_parms
