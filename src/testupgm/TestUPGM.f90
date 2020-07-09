!$Author$
!$Date$
!$Revision$
!$HeadURL$

    !****************************************************************************
    !
    !  PROGRAM: TestUPGM
    !
    !  PURPOSE:  Entry point for the console application.
    !
    !****************************************************************************

    program TestUPGM
    use upgm_mod
    use Preprocess_mod
    use gddmethod1_mod
    use gddmethodWEPS_mod
    use test_phenol_mod, only : test_phenol
    use constants, only : dp, int32
    use environment_state_mod
    implicit none
    type(upgm) :: theModel
    type(environment_state) :: env

    real(dp) :: r_setter
    real(dp), dimension(12) :: ra_setter
    integer(int32) :: i_setter
    logical :: l_setter
    real(dp) :: remgdd, daygdd
    logical :: success = .false.
    CHARACTER(len=255) :: cd

    real(dp), dimension(5) :: soil_moisture
    real(dp), dimension(4) :: gdd_resp, swc_curve
    integer(int32) :: nextstage

    integer(int32) :: dayap

    call GETCWD(cd)
    print *, trim(cd)

    theModel =  UPGM()
    call theModel%plant%plantstate%init()
    ! prepare to add processes
    theModel%plant%processCurrent%ptr => theModel%plant%processes%ptr
    ! prepare to add phases
    theModel%plant%phaseCurrent%ptr => theModel%plant%phases%ptr

    ! iniitalize environmental conditions
    env = environment_state()
    call env%init()

    r_setter = 10.0_dp
    call env%state%put("tmin", r_setter, success)
    r_setter = 25.0_dp
    call env%state%put("tmax", r_setter, success)

    ! add process
    ! create gddWEPS method
    call theModel%plant%add_process("gddweps_method", "WEPS GDD", 0)
    ! create input names
    r_setter = 10.0_dp
    call theModel%plant%plantstate%pars%put("tbas", r_setter, success)
    r_setter = 30.0_dp
    call theModel%plant%plantstate%pars%put("topt", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%plantstate%state%put("daygdd", r_setter, success)

!    ! add process
!    ! create ritchieVernalization method
!    call theModel%plant%add_process("ritchie_vernalization", "Vernalization", 0)
!    ! create input names
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("chill_unit_cum", r_setter, success)

!    ! add process
!    ! create ritchieHardening method
!    call theModel%plant%add_process("ritchie_winterhardening", "Winter Hardening", 0)
!    ! create input names
!    r_setter = 2.0_dp
!    call env%state%put("tsmn1", r_setter, success)
!    r_setter = 13.0_dp
!    call env%state%put("tsmx1", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("harden_index", r_setter, success)

!    ! add process
!    ! create WEPSwarmdays method
!    call theModel%plant%add_process("weps_warmdays", "Warm Days", 0)
!    ! create input names
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("warmdays", r_setter, success)

    ! add undefined process
    ! call theModel%plant%add_process("apple_pie", "Apple Pie", 0)

    ! add phase
    call theModel%plant%add_phase("pmms_germination", "Germination", 0)
    ! phase state parameter
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    ! phase specific parameters for this instance
    swc_curve = [0.45, 0.35, 0.25, -0.1]    !"ratio" response
    call theModel%plant%phaseCurrent%ptr%phasePars%put("swc_curve", swc_curve, success)
    gdd_resp = [25, 30, 35, 600]    ! corresponding gdd value
    call theModel%plant%phaseCurrent%ptr%phasePars%put("gdd_resp", gdd_resp, success)
    ! create all input names
    soil_moisture = [0.45, 0.35, 0.30, 0.35, 0.32]
    call env%state%put("swc", soil_moisture, success)
    i_setter = 2   ! planting in layer 2
    call theModel%plant%plantstate%state%put("p_layer", i_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Emergence", 0)
    ! create all input names

    ! plant state
    ! use from above
    ! daygdd
    r_setter = 1.0_dp
    call theModel%plant%plantstate%state%put("stress", r_setter, success)
    call theModel%plant%plantstate%state%put("fliveleaf", r_setter, success)
    r_setter = 2.0_dp
    call theModel%plant%plantstate%state%put("thu_shoot_beg", r_setter, success)
    call theModel%plant%plantstate%state%put("thu_shoot_end", r_setter, success)

    r_setter = 0.0_dp
    call theModel%plant%plantstate%state%put("ffa", r_setter, success)
    call theModel%plant%plantstate%state%put("ffw", r_setter, success)
    call theModel%plant%plantstate%state%put("ffr", r_setter, success)
    call theModel%plant%plantstate%state%put("gif", r_setter, success)
    call theModel%plant%plantstate%state%put("shoot_hui", r_setter, success)
    call theModel%plant%plantstate%state%put("shoot_huiy", r_setter, success)
    call theModel%plant%plantstate%state%put("p_rw", r_setter, success)
    call theModel%plant%plantstate%state%put("p_st", r_setter, success)
    call theModel%plant%plantstate%state%put("p_lf", r_setter, success)
    call theModel%plant%plantstate%state%put("p_rp", r_setter, success)
    call theModel%plant%plantstate%state%put("pdht", r_setter, success)
    call theModel%plant%plantstate%state%put("pdrd", r_setter, success)
    call theModel%plant%plantstate%state%put("hu_delay", r_setter, success)
    l_setter = .true.
    call theModel%plant%plantstate%state%put("growing", l_setter, success)

    ! phase state
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)

    ! phase parameters
    r_setter = 25.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 55.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "V4", 0)
    ! create all input names
    ! use from above
    ! daygdd
    ! stress
    ! fliveleaf
    ! thu_shoot_beg
    ! thu_shoot_end

    ! output names
    ! use from above
    ! ffa
    ! ffw
    ! ffr
    ! gif
    ! shoot_hui
    ! shoot_huiy
    ! p_rw
    ! p_st
    ! p_lf
    ! p_rp
    ! pdht
    ! pdrd
    ! hu_delay
    ! growing

    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Begin Internode Elongation", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 75.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 75.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "V8", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 65.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 65.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "V8", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "V12", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Last Leaf, Tassel", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 210.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 260.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Silk", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 50.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Blister", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 165.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 149.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Milk", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 110.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 99.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Dough", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 80.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 72.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Dent", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 165.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 140.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Mature", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 200.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 150.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!--------------
    ! add phase
    call theModel%plant%add_phase("pmms_basephenol", "Harvest Ready", 0)
    ! create all input names
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
    r_setter = 100.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
    r_setter = 0.2_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
    r_setter = 80.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
    r_setter = 0.1_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("root_depth_inc", r_setter, success)
    r_setter = 1.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_live_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_weath_leaf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_senes_root", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_grain_index", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_grain_index", r_setter, success)
    r_setter = 0.4_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rw", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rw", r_setter, success)
    r_setter = 0.8_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_lf", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_lf", r_setter, success)
    r_setter = 0.0_dp
    call theModel%plant%phaseCurrent%ptr%phasePars%put("beg_p_rp", r_setter, success)
    call theModel%plant%phaseCurrent%ptr%phasePars%put("end_p_rp", r_setter, success)

!    ! add phase
!    call theModel%plant%add_phase("pmms_fallphenol", "Tiller_Initiation", 0)
!    ! create all input names
!    r_setter = 0.0_dp
!    call theModel%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)
!    call theModel%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
!          r_setter = 0.05_dp
!    call theModel%plant%phaseCurrent%ptr%phasePars%put("height_inc", r_setter, success)
!          r_setter = 200.0_dp
!    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_trans_gdd", r_setter, success)
!          r_setter = 0.1_dp
!    call theModel%plant%phaseCurrent%ptr%phasePars%put("GN_stress", r_setter, success)
!          r_setter = 200.0_dp
!    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_trans_gdd", r_setter, success)
!          r_setter = 0.9_dp
!    call theModel%plant%phaseCurrent%ptr%phasePars%put("GS_stress", r_setter, success)
!          r_setter = 0.035_dp
!    call theModel%plant%phaseCurrent%ptr%phasePars%put("tverndel", r_setter, success)

    ! add phase
!    call theModel%plant%add_phase("weps_shootgrow", "Shoot Grow", 0)
!    ! create all input names
!    ra_setter = [5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp, 5.0_dp]
!    call env%state%put("htsmn", ra_setter, success)
!    r_setter = 30.0_dp
!    call env%state%put("eirr", r_setter, success)
!    r_setter = 1.0_dp
!    call env%state%put("fwsf", r_setter, success)
!    r_setter = 12.0_dp
!    call env%state%put("hrlty", r_setter, success)
!    r_setter = 12.0_dp
!    call env%state%put("hrlt", r_setter, success)

!    r_setter = 1.2_dp
!    call theModel%plant%plantstate%pars%put("hmx", r_setter, success)
!    r_setter = 1.4_dp
!    call theModel%plant%plantstate%pars%put("leafstem", r_setter, success)
!    r_setter = 0.012_dp
!    call theModel%plant%plantstate%pars%put("fshoot", r_setter, success)
!    r_setter = 2.87_dp
!    call theModel%plant%plantstate%pars%put("ssaa", r_setter, success)
!    r_setter = 1.0_dp
!    call theModel%plant%plantstate%pars%put("ssab", r_setter, success)
!    r_setter = 0.3_dp
!    call theModel%plant%plantstate%pars%put("diammax", r_setter, success)
!    i_setter = 2
!    call theModel%plant%plantstate%pars%put("hyldflag", i_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%pars%put("yield_coefficient", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%pars%put("residue_intercept", r_setter, success)
!    r_setter = 1.0_dp
!    call theModel%plant%plantstate%pars%put("grf", r_setter, success)
!    r_setter = 0.6_dp
!    call theModel%plant%plantstate%pars%put("ck", r_setter, success)
!    r_setter = 0.8_dp
!    call theModel%plant%plantstate%pars%put("hui0", r_setter, success)
!    i_setter = 1
!    call theModel%plant%plantstate%pars%put("idc", i_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%pars%put("rowspac", r_setter, success)
!    r_setter = 1.2192148_dp
!    call theModel%plant%plantstate%pars%put("rdmx", r_setter, success)
!    r_setter = 30.0_dp
!    call theModel%plant%plantstate%pars%put("bceff", r_setter, success)
!    r_setter = 0.013_dp
!    call theModel%plant%plantstate%pars%put("a_lf", r_setter, success)
!    r_setter = 0.8013_dp
!    call theModel%plant%plantstate%pars%put("b_lf", r_setter, success)
!    r_setter = 0.4293_dp
!    call theModel%plant%plantstate%pars%put("c_lf", r_setter, success)
!    r_setter = -0.086_dp
!    call theModel%plant%plantstate%pars%put("d_lf", r_setter, success)
!    r_setter = -0.018_dp
!    call theModel%plant%plantstate%pars%put("a_rp", r_setter, success)
!    r_setter = 0.932_dp
!    call theModel%plant%plantstate%pars%put("b_rp", r_setter, success)
!    r_setter = 0.556_dp
!    call theModel%plant%plantstate%pars%put("c_rp", r_setter, success)
!    r_setter = 0.0736_dp
!    call theModel%plant%plantstate%pars%put("d_rp", r_setter, success)
!    r_setter = 0.3293_dp
!    call theModel%plant%plantstate%pars%put("a_ht", r_setter, success)
!    r_setter = -0.086_dp
!    call theModel%plant%plantstate%pars%put("b_ht", r_setter, success)
!    r_setter = 20.0_dp
!    call theModel%plant%plantstate%pars%put("sla", r_setter, success)
!    r_setter = 0.005_dp
!    call theModel%plant%plantstate%pars%put("stemdia", r_setter, success)
!    r_setter = 1.0_dp
!    call theModel%plant%plantstate%pars%put("cbafact", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%pars%put("leaf2stor", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%pars%put("stem2stor", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%pars%put("stor2stor", r_setter, success)
!    r_setter = 1.0_dp
!    call theModel%plant%plantstate%pars%put("regrmshoot", r_setter, success)
!    r_setter = -5.0_dp
!    call theModel%plant%plantstate%pars%put("frsx1", r_setter, success)
!    r_setter = 0.005_dp
!    call theModel%plant%plantstate%pars%put("frsy1", r_setter, success)
!    r_setter = -15.0_dp
!    call theModel%plant%plantstate%pars%put("frsx2", r_setter, success)
!    r_setter = 0.05_dp
!    call theModel%plant%plantstate%pars%put("frsy2", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("zloc_regrow", r_setter, success)
!    i_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("dayam", i_setter, success)

    ! plant states needed
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("stress", r_setter, success)
!    r_setter = 0.0_dp
!    call theModel%plant%plantstate%state%put("height", r_setter, success)


    ! loop day steps
    theModel%plant%phaseCurrent%ptr => theModel%plant%phases%ptr
    ! trigger initial data load to first phase
    nextstage = 1
    call theModel%plant%plantstate%state%put("nextstage", nextstage, success)
    r_setter = 0.0_dp
    call theModel%plant%plantstate%state%put("remgdd", r_setter, success)

    !write(*,*) 'BEFORE LOOP', associated(theModel%plant%phaseCurrent%ptr)

    dayap = 0

    do while( associated(theModel%plant%phaseCurrent%ptr) )

      dayap = dayap + 1

      ! run daily preprocesses
      theModel%plant%processCurrent%ptr => theModel%plant%processes%ptr
      call theModel%preproc(env)

      ! update daily inputs
      select case(theModel%plant%phaseCurrent%ptr%phaseName)
      case ("pmms_germination")
        soil_moisture = [0.45, 0.35, 0.30, 0.35, 0.32]
        call env%state%replace("swc", soil_moisture, success)

      case ("pmms_basephenol")
        r_setter = 0.7_dp
        call theModel%plant%plantstate%state%replace("stress", r_setter, success)

      end select

      ! check for residual gdd and add to phase input
      call theModel%plant%plantstate%state%get("remgdd", remgdd, success)
      if( remgdd .gt. 0.0_dp ) then
        call theModel%plant%plantstate%state%get("daygdd", daygdd, success)

        !write(*,*) 'DAY: ', dayap, 'REMGDD: ', remgdd, daygdd

        daygdd = daygdd + remgdd
        remgdd = 0.0_dp
        call theModel%plant%plantstate%state%replace("daygdd", daygdd, success)
        call theModel%plant%plantstate%state%replace("remgdd", remgdd, success)
      end if

      ! run current phase
      call theModel%grow(env)

    end do

    !call test_phenol()

    call UPGM_DELETE(theModel) ! cleanup the model

    end program TestUPGM
