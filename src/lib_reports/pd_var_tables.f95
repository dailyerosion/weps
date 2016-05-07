!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
MODULE pd_var_tables

    IMPLICIT NONE

    ! Table of half month variables to be computed as period sums
    ! for half months that are averaged (on a per day basis)
    ! (climatic variables)
    INTEGER, PARAMETER :: Precipi         = 1
    INTEGER, PARAMETER :: Wind_energy     = 2
    INTEGER, PARAMETER :: Dryness_ratio   = 3
    INTEGER, PARAMETER :: Snow_cover      = 4
    INTEGER, PARAMETER :: Heat_units      = 5   ! not used yet
    INTEGER, PARAMETER :: Irrigation      = 6

    INTEGER, PARAMETER :: N_cli_vars      = 6
    !Index values for "cli"mate variables
    INTEGER, PARAMETER :: Min_cli_vars    = 1
    INTEGER, PARAMETER :: Max_cli_vars    = Min_cli_vars + N_cli_vars - 1


    ! Table of period variables to be computed as sums for periods
    ! that are averaged
    INTEGER, PARAMETER :: Eros_loss       =  1 + Max_cli_vars
    INTEGER, PARAMETER :: Salt_loss       =  2 + Max_cli_vars
    INTEGER, PARAMETER :: Susp_loss       =  3 + Max_cli_vars
    INTEGER, PARAMETER :: PM10_loss       =  4 + Max_cli_vars
    INTEGER, PARAMETER :: PM2_5_loss      =  5 + Max_cli_vars
    INTEGER, PARAMETER :: Salt_1          =  6 + Max_cli_vars
    INTEGER, PARAMETER :: Salt_2          =  7 + Max_cli_vars
    INTEGER, PARAMETER :: Salt_3          =  8 + Max_cli_vars
    INTEGER, PARAMETER :: Salt_4          =  9 + Max_cli_vars
    INTEGER, PARAMETER :: Susp_1          = 10 + Max_cli_vars
    INTEGER, PARAMETER :: Susp_2          = 11 + Max_cli_vars
    INTEGER, PARAMETER :: Susp_3          = 12 + Max_cli_vars
    INTEGER, PARAMETER :: Susp_4          = 13 + Max_cli_vars
    INTEGER, PARAMETER :: PM10_1          = 14 + Max_cli_vars
    INTEGER, PARAMETER :: PM10_2          = 15 + Max_cli_vars
    INTEGER, PARAMETER :: PM10_3          = 16 + Max_cli_vars
    INTEGER, PARAMETER :: PM10_4          = 17 + Max_cli_vars
    INTEGER, PARAMETER :: PM2_5_1         = 18 + Max_cli_vars
    INTEGER, PARAMETER :: PM2_5_2         = 19 + Max_cli_vars
    INTEGER, PARAMETER :: PM2_5_3         = 20 + Max_cli_vars
    INTEGER, PARAMETER :: PM2_5_4         = 21 + Max_cli_vars
    INTEGER, PARAMETER :: N_eros_events   = 22 + Max_cli_vars
    INTEGER, PARAMETER :: Crop_Transp     = 23 + Max_cli_vars
    INTEGER, PARAMETER :: Evaporation     = 24 + Max_cli_vars
    INTEGER, PARAMETER :: Runoff          = 25 + Max_cli_vars
    INTEGER, PARAMETER :: Drainage        = 26 + Max_cli_vars

    INTEGER, PARAMETER :: N_eave_vars     = 26
    !Index values for field "e"rosion variables
    INTEGER, PARAMETER :: Min_eave_vars   = Max_cli_vars + 1
    INTEGER, PARAMETER :: Max_eave_vars   = Min_eave_vars + N_eave_vars - 1


    INTEGER, PARAMETER :: Salt_loss2      =  1 + Max_eave_vars  !rate = mass/total field area
    INTEGER, PARAMETER :: Salt_loss2_mass =  2 + Max_eave_vars
    INTEGER, PARAMETER :: Salt_loss2_area =  3 + Max_eave_vars
    INTEGER, PARAMETER :: Salt_loss2_frac =  4 + Max_eave_vars

    INTEGER, PARAMETER :: N_lave_vars     =  4

    !Index values for within field "l"oss area variables
    INTEGER, PARAMETER :: Min_lave_vars    = Max_eave_vars + 1
    INTEGER, PARAMETER :: Max_lave_vars    = Min_lave_vars + N_lave_vars - 1


    INTEGER, PARAMETER :: Salt_dep2       =  1 + Max_lave_vars  !rate = mass/total field area
    INTEGER, PARAMETER :: Salt_dep2_mass  =  2 + Max_lave_vars
    INTEGER, PARAMETER :: Salt_dep2_area  =  3 + Max_lave_vars
    INTEGER, PARAMETER :: Salt_dep2_frac  =  4 + Max_lave_vars

    INTEGER, PARAMETER :: N_dave_vars     =  4

    !Index values for within field "d"eposition area variables
    INTEGER, PARAMETER :: Min_dave_vars    = Max_lave_vars + 1
    INTEGER, PARAMETER :: Max_dave_vars    = Min_dave_vars + N_dave_vars - 1


    INTEGER, PARAMETER :: Trans_cap_area  =  1 + Max_dave_vars
    INTEGER, PARAMETER :: Trans_cap_frac  =  2 + Max_dave_vars
    INTEGER, PARAMETER :: Sheltered_area  =  3 + Max_dave_vars
    INTEGER, PARAMETER :: Sheltered_frac  =  4 + Max_dave_vars

    INTEGER, PARAMETER :: N_tave_vars     =  4

    !Index values for within field "t"ranport capacity sheltered area variables
    INTEGER, PARAMETER :: Min_tave_vars    = Max_dave_vars + 1
    INTEGER, PARAMETER :: Max_tave_vars    = Min_tave_vars + N_tave_vars - 1

    INTEGER, PARAMETER :: Salt_loss2_rate =  1 + Max_tave_vars  !rate = mass/high saltation loss area
    INTEGER, PARAMETER :: Salt_dep2_rate  =  2 + Max_tave_vars  !rate = mass/high deposition area
    INTEGER, PARAMETER :: Trans_cap       =  3 + Max_tave_vars  !Not used yet

    INTEGER, PARAMETER :: N_mave_vars     =  3

    !Index values for within field loss/deposition variables ("m"anual average)
    INTEGER, PARAMETER :: Min_mave_vars    = Max_tave_vars + 1
    INTEGER, PARAMETER :: Max_mave_vars    = Min_mave_vars + N_mave_vars - 1


    ! Table of period variables in which the end-of-period values
    ! are to be averaged
    INTEGER, PARAMETER :: Random_rough      =  1 + Max_mave_vars
    INTEGER, PARAMETER :: Ridge_ht          =  2 + Max_mave_vars
    INTEGER, PARAMETER :: Ridge_sp          =  3 + Max_mave_vars
    INTEGER, PARAMETER :: Ridge_dir         =  4 + Max_mave_vars

    INTEGER, PARAMETER :: Crop_canopy_cov   =  5 + Max_mave_vars
    INTEGER, PARAMETER :: Crop_stand_sil    =  6 + Max_mave_vars
    INTEGER, PARAMETER :: Crop_stand_mass   =  7 + Max_mave_vars
    INTEGER, PARAMETER :: Crop_root_mass    =  8 + Max_mave_vars
    INTEGER, PARAMETER :: Crop_stand_height =  9 + Max_mave_vars
    INTEGER, PARAMETER :: Crop_number_stems = 10 + Max_mave_vars


    INTEGER, PARAMETER :: Res_flat_cov      = 11 + Max_mave_vars
    INTEGER, PARAMETER :: Res_stand_sil     = 12 + Max_mave_vars
    INTEGER, PARAMETER :: Res_flat_mass     = 13 + Max_mave_vars
    INTEGER, PARAMETER :: Res_stand_mass    = 14 + Max_mave_vars
    INTEGER, PARAMETER :: Res_buried_mass   = 15 + Max_mave_vars
    INTEGER, PARAMETER :: Res_root_mass     = 16 + Max_mave_vars
    INTEGER, PARAMETER :: Res_stand_height  = 17 + Max_mave_vars
    INTEGER, PARAMETER :: Res_number_stems  = 18 + Max_mave_vars

    INTEGER, PARAMETER :: All_flat_cov      = 19 + Max_mave_vars
    INTEGER, PARAMETER :: All_stand_sil     = 20 + Max_mave_vars
    INTEGER, PARAMETER :: All_flat_mass     = 21 + Max_mave_vars
    INTEGER, PARAMETER :: All_stand_mass    = 22 + Max_mave_vars
    INTEGER, PARAMETER :: All_buried_mass   = 23 + Max_mave_vars

    INTEGER, PARAMETER :: Surface_Ag_84     = 24 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Ag_AS     = 25 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Ag_DN     = 26 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Ag_CA     = 27 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr        = 28 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr_AS     = 20 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr_LM     = 30 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr_TH     = 31 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr_DN     = 32 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr_LF     = 33 + Max_mave_vars
    INTEGER, PARAMETER :: Surface_Cr_CA     = 34 + Max_mave_vars
    INTEGER, PARAMETER :: Soil_Water        = 35 + Max_mave_vars

    INTEGER, PARAMETER :: N_eop_vars        = 35

    !"e"nd of "o"f "p"eriod variables (mostly crop, residue, and surface)
    INTEGER, PARAMETER :: Min_eop_vars     = Max_mave_vars + 1
    INTEGER, PARAMETER :: Max_eop_vars     = Min_eop_vars + N_eop_vars - 1


! Index values for these parameters when used in DO loops

    INTEGER, PARAMETER :: Min_period_vars  = Min_eave_vars
    INTEGER, PARAMETER :: Max_period_vars  = Max_eop_vars

    INTEGER, PARAMETER :: Min_yrly_vars    = Min_cli_vars
    INTEGER, PARAMETER :: Max_yrly_vars    = Max_mave_vars

    INTEGER, PARAMETER :: Min_monthly_vars = Min_cli_vars
    INTEGER, PARAMETER :: Max_monthly_vars = Max_mave_vars

    INTEGER, PARAMETER :: Min_hmonth_vars  = Min_cli_vars
    INTEGER, PARAMETER :: Max_hmonth_vars  = Max_cli_vars


! Note that the cli and ave variables are needed for yrly, monthly,
! and monthly/rot_yr periods.  The cli variables alone are needed for
! half month/rot_yr periods.  The ave and eop (end-of-period) variables
! are needed for the "period" periods.  Hopefully, we can allocate
! the dynamic arrays to start at: Max_cli_vars+1 and go to:
! Max_cli_ave_vars+Max_eop_vars  We will see.

END MODULE pd_var_tables
