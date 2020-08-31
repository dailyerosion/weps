!$Author$
!$Date$
!$Revision$
!$HeadURL$

module PhenologyMMSSpringphenol_mod
  use phases_mod
  use constants, only: dp, int32, check_return
  use WEPSCrop_util_mod, only: chilluv, shootnum, dev_floor, shoot_delay, shoot_flg, verndelmax, hard_spring
  use PhenologyMMS_mod, only: gdd_stressed_del
  implicit none

  type, extends(phase) :: PhenologyMMS_Springphenol
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doPhase => pmms_springphenol ! may not need to pass self
    procedure, pass(self) :: register => phase_register
  end type PhenologyMMS_Springphenol

  contains

    subroutine load_state(self, phaseState)
      implicit none
      class(PhenologyMMS_Springphenol), intent(inout) :: self
      type(hash_state), intent(inout) :: phaseState
      ! Body of loadState
      ! load phaseState into my state:
      self%phaseState = hash_state()
      call self%phaseState%init()
      call self%phaseState%clone(phaseState)
    end subroutine load_state

    subroutine phase_register(self, req_input, prod_output)
      ! Variables
      implicit none
      class(PhenologyMMS_Springphenol), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of stage_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine phase_register

    subroutine pmms_springphenol(self, plnt, env)
      implicit none
      class(PhenologyMMS_Springphenol), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: stagegdd       ! GDD accumulated for this phase
      real(dp) :: phase_rel_gdd  ! relative GDD accumulated for this phase (1 means phase completion)
      real(dp) :: daygdd         ! GDD total for this growth day
      real(dp) :: stress         ! level of stress for this day (0 = no stress, 1 = maximum stress)
      real(dp) :: GN_trans_gdd   ! Non-stressed transition GDD
      real(dp) :: GN_stress      ! level of stress corresponding to Non-stressed transition GDD
      real(dp) :: GS_trans_gdd   ! Stressed transition GDD
      real(dp) :: GS_stress      ! level of stress corresponding to stressed transition GDD
      real(dp) :: height_inc     ! potential height increase during this phase
      real(dp) :: root_depth_inc ! potential root depth increase during this phase
      real(dp) :: begin_phase_rel ! phase_rel_gdd at beginning of day step
      real(dp) :: beg_live_leaf  ! live leaf fraction at beginning of phase
      real(dp) :: end_live_leaf  ! live leaf fraction at end of phase
      real(dp) :: beg_weath_leaf ! standing leaf mass remaining after weathering of senesced leaf mass at beginning of phase
      real(dp) :: end_weath_leaf ! standing leaf mass remaining after weathering of senesced leaf mass at end of phase
      real(dp) :: beg_senes_root ! fibrous root mass remaining after senescence of mass at beginning of phase
      real(dp) :: end_senes_root ! fibrous root mass remaining after senescence of mass at end of phase
      real(dp) :: beg_grain_index ! grain fill fraction at beginning of phase
      real(dp) :: end_grain_index ! grain fill fraction at end of phase
      real(dp) :: beg_p_rw       ! fibrous root allocation fraction at beginning of phase
      real(dp) :: end_p_rw       ! fibrous root allocation fraction at end of phase
      real(dp) :: beg_p_lf       ! leaf allocation fraction at beginning of phase
      real(dp) :: end_p_lf       ! leaf allocation fraction at end of phase
      real(dp) :: beg_p_st       ! stem allocation fraction at beginning of phase
      real(dp) :: end_p_st       ! stem allocation fraction at end of phase
      real(dp) :: beg_p_rp       ! reproductive allocation fraction at beginning of phase
      real(dp) :: end_p_rp       ! reproductive allocation fraction at end of phase
      integer(int32) :: i_setter
      logical :: succ = .false.

      ! plant database
      real(dp) :: bcdpop ! Number of plants per unit area (#/m^2)
                     ! Note: bcdstm/bcdpop gives the number of stems per plant
      integer(int32) :: bc0idc ! crop type:annual,perennial,etc
      real(dp) :: bctverndel ! thermal delay coefficient pre-vernalization
      real(dp) :: bcfleaf2stor ! fraction of assimilate partitioned to leaf that is diverted to root store
      real(dp) :: bcfstem2stor ! fraction of assimilate partitioned to stem that is diverted to root store
      real(dp) :: bcfstor2stor ! fraction of assimilate partitioned to standing storage
                               ! (reproductive) that is diverted to root store
      real(dp) :: bc0shoot ! mass from root storage required for each shoot (mg/shoot)
      real(dp) :: bcdmaxshoot ! maximum number of shoots possible from each plant

      ! environment
      integer(int32) :: bnslay ! number of soil layers
      integer(int32) :: jd ! day of year

      ! plant state
      real(dp) :: bcmtotshoot ! total mass released from root storage biomass (kg/m^2)
                              ! in the period from beginning to completion of emegence heat units
      real(dp), dimension(:), allocatable :: bcmrootstorez ! crop root storage mass by soil layer (kg/m^2)
                                                       ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      real(dp) :: bcdstm ! Number of crop stems per unit area (#/m^2)
      real(dp) :: bcfliveleaf ! fraction of standing plant leaf which is living (transpiring)
      real(dp) :: bczgrowpt ! depth in the soil of the growing point (m)
      real(dp) :: bcthu_shoot_beg ! heat unit index (fraction) for beginning of shoot grow from root storage period
      real(dp) :: bcthu_shoot_end ! heat unit index (fraction) for end of shoot grow from root storage period
      real(dp) :: bcthardnx ! hardening index for winter annuals (range from 0 t0 2)
      real(dp) :: bctwarmdays       ! accumulated warm days
      real(dp) :: bctchillucum ! accumulated chilling units (deg C day)
      integer(int32) :: dayspring
      logical :: can_regrow

      ! locally computed values
      integer(int32) :: spring_flg
      real(dp) :: live_leaf  ! live leaf fraction at end of today (interpolated)
      real(dp) :: hu_delay ! fraction of heat units accummulated based on incomplete vernalization and day length
      real(dp) :: ffa  ! leaf senescence factor (ratio)
      real(dp) :: ffw ! standing leaf mass remaining after weathering of senesced leaf mass at end of today (interpolated)
      real(dp) :: ffr  ! root weight reduction factor (ratio)
      real(dp) :: gif  ! grain index accounting for development of chaff before grain fill
      real(dp) :: shoot_hui    ! today fraction of heat unit shoot growth index accumulation
      real(dp) :: shoot_huiy   ! previous day fraction of heat unit shoot growth index accumulation
      real(dp) :: p_rw ! fibrous root partitioning ratio
      real(dp) :: p_st ! stem partitioning ratio
      real(dp) :: p_lf ! leaf partitioning ratio
      real(dp) :: p_rp ! reproductive partitioning ratio
      real(dp) :: pdht ! increment in potential height (m)'
      real(dp) :: pdrd ! potential increment in root length (m)
      real(dp) :: p_lf_rp    ! sum of leaf and reproductive partitioning fractions

      ! Body of mms_springphenol

      ! plant database
      call plnt%pars%get("plantpop", bcdpop, succ)
      if( .not. check_return( "plantpop", succ ) ) return
      call plnt%pars%get("idc", bc0idc, succ)
      if( .not. check_return( "idc", succ ) ) return
      call plnt%pars%get("verndel", bctverndel, succ)
      if( .not. check_return( "verndel", succ ) ) return
      call plnt%pars%get("leaf2stor", bcfleaf2stor, succ)
      if( .not. check_return( "leaf2stor", succ ) ) return
      call plnt%pars%get("stem2stor", bcfstem2stor, succ)
      if( .not. check_return( "stem2stor", succ ) ) return
      call plnt%pars%get("stor2stor", bcfstor2stor, succ)
      if( .not. check_return( "stor2stor", succ ) ) return
      call plnt%pars%get("mshoot", bc0shoot, succ)
      if( .not. check_return( "mshoot", succ ) ) return
      call plnt%pars%get("dmaxshoot", bcdmaxshoot, succ)
      if( .not. check_return( "dmaxshoot", succ ) ) return

      ! plant state
      call plnt%state%get("daygdd", daygdd, succ)
      if( .not. check_return( "daygdd", succ ) ) return
      call plnt%state%get("stress", stress, succ)
      if( .not. check_return( "stress", succ ) ) return
      call plnt%state%get("mtotshoot", bcmtotshoot, succ)
      if( .not. check_return( "mtotshoot", succ ) ) return
      call plnt%state%get("mrootstorez", bcmrootstorez, succ)
      if( .not. check_return( "mrootstorez", succ ) ) return
      bnslay = size(bcmrootstorez)

      call plnt%state%get("dstm", bcdstm, succ)
      if( .not. check_return( "dstm", succ ) ) return
      call plnt%state%get("fliveleaf", bcfliveleaf, succ)
      if( .not. check_return( "fliveleaf", succ ) ) return
      call plnt%state%get("zgrowpt", bczgrowpt, succ)
      if( .not. check_return( "zgrowpt", succ ) ) return
      call plnt%state%get("thu_shoot_beg", bcthu_shoot_beg, succ)
      if( .not. check_return( "thu_shoot_beg", succ ) ) return
      call plnt%state%get("thu_shoot_end", bcthu_shoot_end, succ)
      if( .not. check_return( "thu_shoot_end", succ ) ) return
      call plnt%state%get("harden_index", bcthardnx, succ)
      if( .not. check_return( "harden_index", succ ) ) return
      call plnt%state%get("warmdays", bctwarmdays, succ)
      if( .not. check_return( "warmdays", succ ) ) return
      call plnt%state%get("chill_unit_cum", bctchillucum, succ)
      if( .not. check_return( "chill_unit_cum", succ ) ) return
      call plnt%state%get("dayspring", dayspring, succ)
      if( .not. check_return( "dayspring", succ ) ) return
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( "can_regrow", succ ) ) return

      ! phase state
      ! initialized to zero at phase beginning
      call self%phaseState%get("phase_rel_gdd", phase_rel_gdd, succ)
      if( .not. check_return( "phase_rel_gdd", succ ) ) return
      call self%phaseState%get("stagegdd", stagegdd, succ)
      if( .not. check_return( "stagegdd", succ ) ) return

      ! phase parameters
      call self%phasePars%get("GN_trans_gdd", GN_trans_gdd, succ)
      if( .not. check_return( "GN_trans_gdd", succ ) ) return
      call self%phasePars%get("GN_stress", GN_stress, succ)
      if( .not. check_return( "GN_stress", succ ) ) return
      call self%phasePars%get("GS_trans_gdd", GS_trans_gdd, succ)
      if( .not. check_return( "GS_trans_gdd", succ ) ) return
      call self%phasePars%get("GS_stress", GS_stress, succ)
      if( .not. check_return( "GS_stress", succ ) ) return

      call self%phasePars%get("height_inc", height_inc, succ)
      if( .not. check_return( "height_inc", succ ) ) return
      call self%phasePars%get("root_depth_inc", root_depth_inc, succ)
      if( .not. check_return( "root_depth_inc", succ ) ) return

      call self%phasePars%get("beg_live_leaf", beg_live_leaf, succ)
      if( .not. check_return( "beg_live_leaf", succ ) ) return
      call self%phasePars%get("end_live_leaf", end_live_leaf, succ)
      if( .not. check_return( "end_live_leaf", succ ) ) return

      call self%phasePars%get("beg_weath_leaf", beg_weath_leaf, succ)
      if( .not. check_return( "beg_weath_leaf", succ ) ) return
      call self%phasePars%get("end_weath_leaf", end_weath_leaf, succ)
      if( .not. check_return( "end_weath_leaf", succ ) ) return

      call self%phasePars%get("beg_senes_root", beg_senes_root, succ)
      if( .not. check_return( "beg_senes_root", succ ) ) return
      call self%phasePars%get("end_senes_root", end_senes_root, succ)
      if( .not. check_return( "end_senes_root", succ ) ) return

      call self%phasePars%get("beg_grain_index", beg_grain_index, succ)
      if( .not. check_return( "beg_grain_index", succ ) ) return
      call self%phasePars%get("end_grain_index", end_grain_index, succ)
      if( .not. check_return( "end_grain_index", succ ) ) return

      call self%phasePars%get("beg_p_rw", beg_p_rw, succ)
      if( .not. check_return( "beg_p_rw", succ ) ) return
      call self%phasePars%get("end_p_rw", end_p_rw, succ)
      if( .not. check_return( "end_p_rw", succ ) ) return

      call self%phasePars%get("beg_p_lf", beg_p_lf, succ)
      if( .not. check_return( "beg_p_lf", succ ) ) return
      call self%phasePars%get("end_p_lf", end_p_lf, succ)
      if( .not. check_return( "end_p_lf", succ ) ) return

      call self%phasePars%get("beg_p_rp", beg_p_rp, succ)
      if( .not. check_return( "beg_p_rp", succ ) ) return
      call self%phasePars%get("end_p_rp", end_p_rp, succ)
      if( .not. check_return( "end_p_rp", succ ) ) return

      ! release growth in spring if conditions met
      spring_flg = -1
      if( can_regrow ) then
        if( (bc0idc.eq.2) .or. (bc0idc.eq.5) ) then

          ! check winter annuals for completion of vernalization,
          ! warming and spring day length 
          if( bczgrowpt .le. 0.0_dp ) then
           ! remember, negative number means above ground
           spring_flg = 1
           if( bctchillucum .ge. chilluv ) then
            spring_flg = 2
            if( bctwarmdays .ge. shoot_delay*bctverndel/verndelmax) then
             spring_flg = 3
             !if( huiy .gt. spring_trig ) then
             !if( bcthardnx .le. 0.0 ) then
             if( bcthardnx .lt. hard_spring ) then
              spring_flg = 4
              ! vernalized and ready to grow in spring
              bcthu_shoot_beg = phase_rel_gdd
              bcthu_shoot_end = phase_rel_gdd + (1.0_dp-phase_rel_gdd)/2.0_dp
              call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot,&
     &             bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )
              ! eliminate diversion of biomass to crown storage
              bcfleaf2stor = 0.0_dp
              bcfstem2stor = 0.0_dp
              bcfstor2stor = 0.0_dp
              ! turn off freeze hardening
              bcthardnx = 0.0_dp
              dayspring = jd
             end if
            end if
           end if
          end if
        end if
      end if

      begin_phase_rel = phase_rel_gdd

      ! if phase_rel_gdd exceeds 1.0, remainder of daygdd is returned
      call gdd_stressed_del(phase_rel_gdd, stagegdd, daygdd, stress, GN_trans_gdd, GN_stress, GS_trans_gdd, GS_stress, 1.0_dp)

      ! senescence is done on a whole plant mass basis not incremental mass
      live_leaf = beg_live_leaf + (end_live_leaf - beg_live_leaf) * phase_rel_gdd
      ffw = beg_weath_leaf + (end_weath_leaf - beg_weath_leaf) * phase_rel_gdd
      if( ffw .lt. live_leaf ) then
        ! weathering of leaf cannot exceed dead leaf amount
        ffw = live_leaf
      end if
      ffa = (live_leaf / bcfliveleaf) * (1.0_dp + bcfliveleaf * (ffw - 1.0_dp))
      ffr = beg_senes_root + (end_senes_root - beg_senes_root) * phase_rel_gdd
      gif = beg_grain_index + (end_grain_index - beg_grain_index) * phase_rel_gdd

      pdht = height_inc * (phase_rel_gdd - begin_phase_rel)
      pdrd = root_depth_inc * (phase_rel_gdd - begin_phase_rel)

      ! calculate shoot_hui
      if( begin_phase_rel .lt. bcthu_shoot_end ) then
        if( phase_rel_gdd .gt. bcthu_shoot_beg ) then
          ! fraction of shoot growth from stored reserves (today and yesterday)
          shoot_hui = min( 1.0_dp, (phase_rel_gdd - bcthu_shoot_beg) / (bcthu_shoot_end - bcthu_shoot_beg) )
          shoot_huiy = max( 0.0_dp, (begin_phase_rel - bcthu_shoot_beg) / (bcthu_shoot_end - bcthu_shoot_beg) )
        else
          shoot_hui = 0.0_dp
          shoot_huiy = 0.0_dp
        end if
      else
        shoot_hui = 1.0_dp
        shoot_huiy = 1.0_dp
      end if

      p_rw = beg_p_rw + (end_p_rw - beg_p_rw) * phase_rel_gdd
      p_lf = beg_p_lf + (end_p_lf - beg_p_lf) * phase_rel_gdd
      p_rp = beg_p_rp + (end_p_rp - beg_p_rp) * phase_rel_gdd

      ! normalize leaf and reproductive fractions so sum never greater than 1.0
      p_lf_rp = p_lf + p_rp
      if( p_lf_rp .gt. 1.0_dp ) then
          p_lf = p_lf / p_lf_rp
          p_rp = p_rp / p_lf_rp
          ! set stem partitioning parameter.
          p_st = 0.0_dp
      else
          ! set stem partitioning parameter.
          p_st = 1.0_dp - p_lf_rp
      end if

      hu_delay = 1.0_dp

      if (phase_rel_gdd .ge. 1.0_dp) then
        ! update plant stage pointer to next stage.
        ! set control variables
        i_setter = 1
        call plnt%state%replace("nextstage", i_setter, succ)
        i_setter = 0
        call plnt%state%replace("specstage", i_setter, succ)
      end if

      ! return modified values
      call self%phaseState%replace("phase_rel_gdd", phase_rel_gdd, succ)
      if( .not. check_return( "phase_rel_gdd", succ ) ) return
      call self%phaseState%replace("stagegdd", stagegdd, succ)
      if( .not. check_return( "stagegdd", succ ) ) return
      call plnt%state%replace("remgdd", daygdd, succ)
      if( .not. check_return( "remgdd", succ ) ) return

      ! update plant par values
      call plnt%pars%replace("leaf2stor", bcfleaf2stor, succ)
      if( .not. check_return( "leaf2stor", succ ) ) return
      call plnt%pars%replace("stem2stor", bcfstem2stor, succ)
      if( .not. check_return( "stem2stor", succ ) ) return
      call plnt%pars%replace("stor2stor", bcfstor2stor, succ)
      if( .not. check_return( "stor2stor", succ ) ) return

      ! update plant state values

      call plnt%state%replace("mtotshoot", bcmtotshoot, succ)
      if( .not. check_return( "mtotshoot", succ ) ) return
      call plnt%state%replace("dstm", bcdstm, succ)
      if( .not. check_return( "dstm", succ ) ) return
      call plnt%state%replace("thu_shoot_beg", bcthu_shoot_beg, succ)
      if( .not. check_return( "thu_shoot_beg", succ ) ) return
      call plnt%state%replace("thu_shoot_end", bcthu_shoot_end, succ)
      if( .not. check_return( "thu_shoot_end", succ ) ) return
      call plnt%state%replace("harden_index", bcthardnx, succ)
      if( .not. check_return( "harden_index", succ ) ) return
      call plnt%state%replace("dayspring", dayspring, succ)
      if( .not. check_return( "dayspring", succ ) ) return
      call plnt%state%replace("ffa", ffa, succ)
      if( .not. check_return( "ffa", succ ) ) return
      call plnt%state%replace("ffw", ffw, succ)
      if( .not. check_return( "ffw", succ ) ) return
      call plnt%state%replace("ffr", ffr, succ)
      if( .not. check_return( "ffr", succ ) ) return
      call plnt%state%replace("gif", gif, succ)
      if( .not. check_return( "gif", succ ) ) return
      call plnt%state%replace("shoot_hui", shoot_hui, succ)
      if( .not. check_return( "shoot_hui", succ ) ) return
      call plnt%state%replace("shoot_huiy", shoot_huiy, succ)
      if( .not. check_return( "shoot_huiy", succ ) ) return
      call plnt%state%replace("p_rw", p_rw, succ)
      if( .not. check_return( "p_rw", succ ) ) return
      call plnt%state%replace("p_st", p_st, succ)
      if( .not. check_return( "p_st", succ ) ) return
      call plnt%state%replace("p_lf", p_lf, succ)
      if( .not. check_return( "p_lf", succ ) ) return
      call plnt%state%replace("p_rp", p_rp, succ)
      if( .not. check_return( "p_rp", succ ) ) return
      call plnt%state%replace("pdht", pdht, succ)
      if( .not. check_return( "pdht", succ ) ) return
      call plnt%state%replace("pdrd", pdrd, succ)
      if( .not. check_return( "pdrd", succ ) ) return
      call plnt%state%replace("hu_delay", hu_delay, succ)
      if( .not. check_return( "hu_delay", succ ) ) return
      call plnt%state%replace("spring_flg", spring_flg, succ)
      if( .not. check_return( "spring_flg", succ ) ) return

      return

    end subroutine pmms_springphenol

end module PhenologyMMSSpringphenol_mod
