!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPS_UPGM_mod

    implicit none

  contains

    subroutine init_WEPS_UPGM( soil, plant )
      use upgm_mod
      use constants, only : dp, int32, precision_init
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer
      use climate_input_mod, only: amalat, cli_today
      use solar_mod, only: civilrise, daylen
      use datetime_mod, only: get_simdate_doy, get_simdate
      use environment_state_mod
      use WEPSCrop_util_mod, only: scrv1

      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older 

      real(dp) :: r_setter
      real(dp), dimension(:), allocatable :: ra_setter
      integer(int32) :: i_setter
      logical :: l_setter
      logical :: success = .false.
      integer :: nelem
      integer :: alloc_stat
      integer(int32) :: nextstage
      integer(int32) :: jd ! day of year
      real(dp) :: a_fr ! parameter in the frost damage s-curve
      real(dp) :: b_fr ! parameter in the frost damage s-curve

      ! init precision values
      call precision_init()

      ! initialize upgm_grow model
      plant%upgm_grow = UPGM()
      call plant%upgm_grow%plant%plantstate%init()

      ! iniitalize environmental conditions
      plant%env = environment_state()
      call plant%env%init()

      ! add process
      ! create gddWEPS method
      call plant%upgm_grow%plant%add_process("gddweps_method", "WEPS GDD", 0)
      ! create input names
      r_setter = cli_today%tdmn
      call plant%env%state%put("tmin", r_setter, success)
      r_setter = cli_today%tdmx
      call plant%env%state%put("tmax", r_setter, success)
      r_setter = plant%database%tmin
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("tbas", r_setter, success)
      r_setter = plant%database%topt
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("topt", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("daygdd", r_setter, success)

      ! add process
      ! create ritchieVernalization method
      call plant%upgm_grow%plant%add_process("ritchie_vernalization", "Vernalization", 0)
      ! create input names
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("chill_unit_cum", r_setter, success)

      ! add process
      ! create WEPS temperature stress method
      call plant%upgm_grow%plant%add_process("weps_tempstress", "Temp Stress", 0)
      ! create input names
      ! uses tmin, tmax from above
      r_setter = plant%database%tmin
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("tbas", r_setter, success)
      r_setter = plant%database%topt
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("topt", r_setter, success)
      r_setter = 1.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("tstress", r_setter, success)

      ! add process
      ! create WEPS freeze damage method
      call plant%upgm_grow%plant%add_process("weps_freezedamage", "Freeze Damage", 0)
      ! create input names
      r_setter = 1.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("ffa", r_setter, success)
      r_setter = soil%tsmn(1)
      call plant%env%state%put("tsmn1", r_setter, success)
      ! calculates Frost damage s-curve coefficients
      call scrv1(plant%database%fd1(1),plant%database%fd1(2),plant%database%fd2(1),plant%database%fd2(2),a_fr,b_fr)
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("a_fr", a_fr, success)
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("b_fr", b_fr, success)

      r_setter = plant%mass%standleaf
      call plant%upgm_grow%plant%plantstate%state%put("mstandleaf", r_setter, success)
      r_setter = plant%growth%fliveleaf
      call plant%upgm_grow%plant%plantstate%state%put("fliveleaf", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("frst", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("lost_mass", r_setter, success)

      ! add process
      ! create ritchieHardening method
      call plant%upgm_grow%plant%add_process("ritchie_winterhardening", "Winter Hardening", 0)
      ! create input names
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("harden_index", r_setter, success)
      l_setter = .false.
      call plant%upgm_grow%plant%plantstate%state%put("can_harden", l_setter, success)
      r_setter = soil%tsmx(1)
      call plant%env%state%put("tsmx1", r_setter, success)
      ! use from above
      ! tsmn1

      ! add process
      ! create WEPS warmdays method
      call plant%upgm_grow%plant%add_process("weps_warmdays", "Warm Days", 0)
      ! create input names
      r_setter = plant%database%tmin
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("tbas", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("warmdays", r_setter, success)

      ! add process
      ! create WEPS_regrowth method
      call plant%upgm_grow%plant%add_process("weps_regrowth", "Check for Regrowth", 0)

      ! create input names
      ! plant database
      r_setter = plant%geometry%dpop
      call plant%upgm_grow%plant%plantstate%pars%put("plantpop", r_setter, success)
      r_setter = plant%database%fleafstem
      call plant%upgm_grow%plant%plantstate%pars%put("leafstem", r_setter, success)
      i_setter = plant%database%idc
      call plant%upgm_grow%plant%plantstate%pars%put("idc", i_setter, success)
      r_setter = plant%database%shoot
      call plant%upgm_grow%plant%plantstate%pars%put("regrmshoot", r_setter, success)
      r_setter = plant%database%dmaxshoot
      call plant%upgm_grow%plant%plantstate%pars%put("dmaxshoot", r_setter, success)
      r_setter = plant%database%storeinit
      call plant%upgm_grow%plant%plantstate%pars%put("storeinit", r_setter, success)
      r_setter = plant%database%hue
      call plant%upgm_grow%plant%plantstate%pars%put("huie", r_setter, success)
      r_setter = plant%database%zloc_regrow
      call plant%upgm_grow%plant%plantstate%pars%put("zloc_regrow", r_setter, success)

      ! environment variables
      r_setter = daylen(amalat, jd-1, civilrise)
      call plant%env%state%put("hrlty", r_setter, success)
      r_setter = daylen(amalat, jd, civilrise)
      call plant%env%state%put("hrlt", r_setter, success)

      ! plant state
      r_setter = plant%mass%standstem
      call plant%upgm_grow%plant%plantstate%state%put("mstandstem", r_setter, success)
      ! use from above
      ! mstandleaf
      r_setter = plant%mass%standstore
      call plant%upgm_grow%plant%plantstate%state%put("mstandstore", r_setter, success)
      r_setter = plant%mass%flatstem
      call plant%upgm_grow%plant%plantstate%state%put("mflatstem", r_setter, success)
      r_setter = plant%mass%flatleaf
      call plant%upgm_grow%plant%plantstate%state%put("mflatleaf", r_setter, success)
      r_setter = plant%mass%flatstore
      call plant%upgm_grow%plant%plantstate%state%put("mflatstore", r_setter, success)
      r_setter = plant%growth%mshoot
      call plant%upgm_grow%plant%plantstate%state%put("masshoot", r_setter, success)
      r_setter = plant%growth%mtotshoot
      call plant%upgm_grow%plant%plantstate%state%put("mtotshoot", r_setter, success)

      nelem = size(plant%mass%stemz)
      allocate(ra_setter(nelem), stat = alloc_stat)
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate memory for UPGM.'
      end if
      ra_setter = plant%mass%stemz
      call plant%upgm_grow%plant%plantstate%state%put("mbgstemz", ra_setter, success)
      deallocate(ra_setter, stat = alloc_stat)

      nelem = size(plant%mass%rootstorez)
      allocate(ra_setter(nelem), stat = alloc_stat)
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate memory for UPGM.'
      end if
      ra_setter = plant%mass%rootstorez
      call plant%upgm_grow%plant%plantstate%state%put("mrootstorez", ra_setter, success)
      deallocate(ra_setter, stat = alloc_stat)

      r_setter = plant%geometry%zht
      call plant%upgm_grow%plant%plantstate%state%put("height", r_setter, success)
      r_setter = plant%geometry%dstm
      call plant%upgm_grow%plant%plantstate%state%put("dstm", r_setter, success)
      i_setter = plant%growth%dayam
      call plant%upgm_grow%plant%plantstate%state%put("dayam", i_setter, success)
      ! use from above
      ! fliveleaf
      r_setter = plant%growth%thu_shoot_beg
      call plant%upgm_grow%plant%plantstate%state%put("thu_shoot_beg", r_setter, success)
      r_setter = plant%growth%thu_shoot_end
      call plant%upgm_grow%plant%plantstate%state%put("thu_shoot_end", r_setter, success)
      r_setter = plant%geometry%grainf
      call plant%upgm_grow%plant%plantstate%state%put("grainf", r_setter, success)
      r_setter = plant%growth%leafareatrend
      call plant%upgm_grow%plant%plantstate%state%put("leafareatrend", r_setter, success)
      r_setter = plant%growth%stemmasstrend
      call plant%upgm_grow%plant%plantstate%state%put("stemmasstrend", r_setter, success)
      ! use from above
      ! warmdays
      ! chill_unit_cum
      i_setter = -2
      call plant%upgm_grow%plant%plantstate%state%put("regrowth_flg", i_setter, success)
      r_setter = plant%prev%liveleaf
      call plant%upgm_grow%plant%plantstate%state%put("prevliveleaf", r_setter, success)
      r_setter = plant%prev%standleaf
      call plant%upgm_grow%plant%plantstate%state%put("prevstandleaf", r_setter, success)
      r_setter = plant%prev%standstem
      call plant%upgm_grow%plant%plantstate%state%put("prevstandstem", r_setter, success)
      r_setter = plant%prev%flatstem
      call plant%upgm_grow%plant%plantstate%state%put("prevflatstem", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("res_standstem", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_standleaf", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_standstore", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_flatstem", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_flatleaf", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_flatstore", r_setter, success)

      nelem = size(soil%aszlyd)
      allocate(ra_setter(nelem), stat = alloc_stat)
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate memory for UPGM.'
      end if
      ra_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("res_bgstemz", ra_setter, success)
      deallocate(ra_setter, stat = alloc_stat)

      call plant%upgm_grow%plant%plantstate%state%put("res_grainf", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_zht", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("res_dstm", r_setter, success)
      l_setter = .false.
      call plant%upgm_grow%plant%plantstate%state%put("shoot_growing", l_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("can_regrow", l_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("do_regrow", l_setter, success)

      ! add phase
      call plant%upgm_grow%plant%add_phase("weps_shootgrow", "Shoot Grow", 0)
      ! Associate regrowth phase
      plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow => plant%upgm_grow%plant%phaseCurrent%ptr

      ! create phase states
      call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
      call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)

      ! create all input names
      ! use from above
      ! plantpop
      ! idc
      r_setter = plant%database%tverndel
      call plant%upgm_grow%plant%plantstate%pars%put("tverndel", r_setter, success)
      r_setter = plant%database%fleaf2stor
      call plant%upgm_grow%plant%plantstate%pars%put("leaf2stor", r_setter, success)
      r_setter = plant%database%fstem2stor
      call plant%upgm_grow%plant%plantstate%pars%put("stem2stor", r_setter, success)
      r_setter = plant%database%fstor2stor
      call plant%upgm_grow%plant%plantstate%pars%put("stor2stor", r_setter, success)
      ! use from above
      ! regrmshoot
      ! dmaxshoot
      ! huie
      r_setter = plant%database%thum
      call plant%upgm_grow%plant%plantstate%pars%put("thum", r_setter, success)
      r_setter = plant%database%alf
      call plant%upgm_grow%plant%plantstate%pars%put("alf", r_setter, success)
      r_setter = plant%database%blf
      call plant%upgm_grow%plant%plantstate%pars%put("blf", r_setter, success)
      r_setter = plant%database%clf
      call plant%upgm_grow%plant%plantstate%pars%put("clf", r_setter, success)
      r_setter = plant%database%dlf
      call plant%upgm_grow%plant%plantstate%pars%put("dlf", r_setter, success)
      r_setter = plant%database%arp
      call plant%upgm_grow%plant%plantstate%pars%put("arp", r_setter, success)
      r_setter = plant%database%brp
      call plant%upgm_grow%plant%plantstate%pars%put("brp", r_setter, success)
      r_setter = plant%database%crp
      call plant%upgm_grow%plant%plantstate%pars%put("crp", r_setter, success)
      r_setter = plant%database%drp
      call plant%upgm_grow%plant%plantstate%pars%put("drp", r_setter, success)
      r_setter = plant%database%aht
      call plant%upgm_grow%plant%plantstate%pars%put("aht", r_setter, success)
      r_setter = plant%database%bht
      call plant%upgm_grow%plant%plantstate%pars%put("bht", r_setter, success)
      r_setter = plant%database%zmxc
      call plant%upgm_grow%plant%plantstate%pars%put("zmxc", r_setter, success)
      r_setter = plant%database%zmrt
      call plant%upgm_grow%plant%plantstate%pars%put("zmrt", r_setter, success)
      r_setter = plant%database%ehu0
      call plant%upgm_grow%plant%plantstate%pars%put("ehu0", r_setter, success)
      jd = get_simdate_doy()
      call plant%env%state%put("dayofyear", jd, success)
      ! use from above
      ! mtotshoot
      ! mrootstorez
      ! dstm
      r_setter = plant%growth%zgrowpt
      call plant%upgm_grow%plant%plantstate%state%put("zgrowpt", r_setter, success)
      r_setter = plant%growth%trthucum
      call plant%upgm_grow%plant%plantstate%state%put("trthucum", r_setter, success)
      ! use from above
      ! thu_shoot_beg
      ! thu_shoot_end
      ! harden_index
      ! warmdays
      ! chill_unit_cum
      r_setter = plant%growth%dayspring
      call plant%upgm_grow%plant%plantstate%state%put("dayspring", r_setter, success)
      ! use from above
      ! can_regrow
      ! ffa
      ! values only returned from ShootGrow
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("ffw", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("ffr", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("gif", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("shoot_hui", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("shoot_huiy", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("p_rw", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("p_st", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("p_lf", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("p_rp", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("pdht", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("pdrd", r_setter, success)
      ! use from above
      l_setter = .false.
      call plant%upgm_grow%plant%plantstate%state%put("lastday", l_setter, success)
      ! reporting only variables
      call plant%upgm_grow%plant%plantstate%state%put("hu_delay", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("spring_flg", i_setter, success)

      ! all phases added, set phases to beginning
      call set_start_UPGM( plant )

      return

    end subroutine init_WEPS_UPGM

    subroutine set_start_UPGM( plant )
      use biomaterial, only: plant_pointer
      use datetime_mod, only: difdat, get_simdate
      use constants, only : dp, int32

      ! + + + ARGUMENT DECLARATIONS + + +
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older 

      ! + + + LOCAL VARIABLES + + +
      integer :: simdd    ! current simulation day
      integer :: simmm    ! current simulation month
      integer :: simyr    ! current simulation year
      type(plant_pointer), pointer :: thisPlant     ! pointer for looping through plants
      integer(int32) :: nextstage
      real(dp) :: r_setter
      logical :: success = .false.

      ! get current simulation day, month, year
      call get_simdate( simdd, simmm, simyr )

      ! expecting pointer to the newest plant in existence
      thisPlant => plant
      do while( associated(thisPlant) )
        if (difdat (simdd,simmm,simyr,thisPlant%pday,thisPlant%pmon,thisPlant%psimyr).eq.0) then
          ! this crop was planted today
          if( associated(thisPlant%upgm_grow%plant) ) then
            ! This is an UPGM crop
            ! Set Current Phase pointer to initial phase
            thisPlant%upgm_grow%plant%phaseCurrent%ptr => thisPlant%upgm_grow%plant%phases%ptr
            ! trigger initial data load to initial phase
            nextstage = 1
            call thisPlant%upgm_grow%plant%plantstate%state%put("nextstage", nextstage, success)
            r_setter = 0.0_dp
            call thisPlant%upgm_grow%plant%plantstate%state%put("remgdd", r_setter, success)
          end if
        else
          ! this crop was not planted today, check no more, exit do loop
          exit
        end if

        ! check next plant
        thisPlant => thisPlant%olderPlant
      end do

      return

    end subroutine

    subroutine run_UPGM( isr, soil, plant )
      use upgm_mod
      use constants, only : dp, int32
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residueAdd
      use crop_data_struct_defs, only: crop_residue, am0cfl
      use crop_data_struct_defs, only: create_crop_residue, destroy_crop_residue
      use climate_input_mod, only: amalat, cli_today
      use weps_main_mod, only: daysim
      use datetime_mod, only: get_simdate_doy, get_simdate_year
      use file_io_mod, only: luocrop, luoshoot
      use WEPSCrop_mod, only: shoot_grow, growth
      use solar_mod, only: civilrise, daylen

      integer(int32) :: isr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older 

      real(dp) :: r_setter
      real(dp), dimension(:), allocatable :: ra_setter
      integer(int32) :: i_setter
      logical :: l_setter
      logical :: success = .false.
      integer :: nelem
      integer :: alloc_stat
      real(dp) :: remgdd, daygdd
      type(crop_residue) :: cropres
      real(dp) :: temp_store
      real(dp) :: temp_fiber
      real(dp) :: temp_stem
      real(dp) :: eff_lai ! single plant effective leaf area index (based on maximum single plant coverage area)
      real(dp) :: trad_lai ! leaf area index based on whole field area (traditional)
      real(dp) :: tstress       ! temperature stress factor
      real(dp) :: frst ! frost damage factor
      real(dp) :: ffa  ! leaf senescence factor (ratio)
      real(dp) :: ffw  ! leaf weight reduction factor (ratio)
      real(dp) :: ffr  ! root weight reduction factor (ratio)
      real(dp) :: gif  ! grain index accounting for development of chaff before grain fill
      real(dp) :: hui  ! fraction of growing season accumulation
      real(dp) :: shoot_hui    ! today fraction of heat unit shoot growth index accumulation
      real(dp) :: shoot_huiy   ! previous day fraction of heat unit shoot growth index accumulation
      real(dp) :: s_root_sum   ! storage root mass sum (total in all layers) (kg/m^2)
      real(dp) :: f_root_sum   ! fibrous root mass sum (total in all layers) (kg/m^2)
      real(dp) :: tot_mass_req ! mass required from root mass for one shoot (mg/shoot)
      real(dp) :: end_shoot_mass ! total shoot mass at end of shoot growth period (mg/shoot)
      real(dp) :: end_root_mass ! total root mass at end of shoot growth period (mg/shoot)
      real(dp) :: d_root_mass  ! mass increment added to roots for the present day (mg/shoot)
      real(dp) :: d_shoot_mass ! mass increment added to shoot for the present day (mg/shoot)
      real(dp) :: d_s_root_mass ! mass increment removed from storage roots for the present day (mg/shoot)
      real(dp) :: end_stem_mass ! total stem mass at end of shoot growth period (mg/shoot)
      real(dp) :: end_stem_area ! total stem area at end of shoot growth period (m^2/shoot)
      real(dp) :: end_shoot_len ! total shoot length at end of shoot growth period (m)
      real(dp) :: par  ! photosynthetically active radiation (MJ/m2)
      real(dp) :: apar ! intercepted photosynthetically active radiation (MJ/m2)
      real(dp) :: pddm ! increment in potential dry matter (kg)
      real(dp) :: p_rw ! fibrous root partitioning ratio
      real(dp) :: p_st ! stem partitioning ratio
      real(dp) :: p_lf ! leaf partitioning ratio
      real(dp) :: p_rp ! reproductive partitioning ratio
      real(dp) :: pdht ! increment in potential height (m)'
      real(dp) :: pdrd ! potential increment in root length (m)
      real(dp) :: stem_propor ! Fraction of stem mass increase allocated to standing stems (remainder goes flat)
      real(dp) :: pdiam ! Reach of growing plant (m)
      real(dp) :: parea ! areal extent occupied by plant leaf (m^2/plant)
      real(dp) :: hu_delay ! fraction of heat units accummulated based on incomplete vernalization and day length
      real(dp) :: temp_sai
      real(dp) :: temp_stmrep
      real(dp) :: lost_mass    ! biomass that decayed (disappeared) from scenescence and freeze damage
      integer(int32) :: regrowth_flg
      integer(int32) :: spring_flg
      integer(int32) :: regrowth_or_spring_flg
      integer(int32) :: idx
      integer(int32) :: jd ! day of year
      real(dp) :: trend ! test computation for trend direction of living leaf area
      character(len=80) :: PhaseLabel
      integer(int32) :: nextstage
      integer(int32) :: specificStage
      real(dp) :: stagegdd

      regrowth_flg = -2
      spring_flg = -2
      jd = get_simdate_doy()

      if(    (plant%database%fleaf2stor .gt. 0.0_dp) &
        .or. (plant%database%fstem2stor .gt. 0.0_dp) &
        .or. (plant%database%fstor2stor .gt. 0.0_dp) ) then
        plant%growth%can_regrow = .true.
        plant%growth%can_harden = .true.
      else
        plant%growth%can_regrow = .false.
        plant%growth%can_harden = .false.
      end if

      ! update daily inputs for preprocesses
      plant%upgm_grow%plant%processCurrent%ptr => plant%upgm_grow%plant%processes%ptr
      do while ( associated(plant%upgm_grow%plant%processCurrent%ptr) )

        select case(plant%upgm_grow%plant%processCurrent%ptr%processName)

        case ("gddmethod1")
          r_setter = cli_today%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_today%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("gddweps_method")
          r_setter = cli_today%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_today%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("ritchie_vernalization")
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)
          r_setter = cli_today%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_today%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("ritchie_winterhardening")
          r_setter = plant%growth%thardnx
          call plant%upgm_grow%plant%plantstate%state%replace("harden_index", r_setter, success)
          l_setter = plant%growth%can_harden
          call plant%upgm_grow%plant%plantstate%state%replace("can_harden", l_setter, success)
          r_setter = soil%tsmx(1)
          call plant%env%state%replace("tsmx1", r_setter, success)
          r_setter = soil%tsmn(1)
          call plant%env%state%replace("tsmn1", r_setter, success)
        case ("weps_warmdays")
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = cli_today%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_today%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("weps_tempstress")
          r_setter = cli_today%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_today%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("weps_freezedamage")
          r_setter = plant%mass%standleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaf", r_setter, success)
          r_setter = plant%growth%fliveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("fliveleaf", r_setter, success)
          r_setter = soil%tsmn(1)
          call plant%env%state%replace("tsmn1", r_setter, success)
        case ("weps_regrowth")
          ! calculate day length
          r_setter = daylen(amalat, jd-1, civilrise)
          call plant%env%state%replace("hrlty", r_setter, success)
          r_setter = daylen(amalat, jd, civilrise)
          call plant%env%state%replace("hrlt", r_setter, success)
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          r_setter = plant%mass%standleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaf", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%replace("masshoot", r_setter, success)
          r_setter = plant%growth%mtotshoot
          call plant%upgm_grow%plant%plantstate%state%replace("mtotshoot", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%replace("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          nelem = size(plant%mass%rootstorez)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
             write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%rootstorez
          call plant%upgm_grow%plant%plantstate%state%replace("mrootstorez", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%replace("height", r_setter, success)
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          i_setter = plant%growth%dayam
          call plant%upgm_grow%plant%plantstate%state%replace("dayam", i_setter, success)
          r_setter = plant%growth%fliveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("fliveleaf", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)
          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%replace("grainf", r_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%replace("leafareatrend", r_setter, success)
          r_setter = plant%growth%stemmasstrend
          call plant%upgm_grow%plant%plantstate%state%replace("stemmasstrend", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)
          r_setter = plant%prev%liveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("prevliveleaf", r_setter, success)
          r_setter = plant%prev%standleaf
          call plant%upgm_grow%plant%plantstate%state%replace("prevstandleaf", r_setter, success)
          r_setter = plant%prev%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("prevstandstem", r_setter, success)
          r_setter = plant%prev%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("prevflatstem", r_setter, success)

          l_setter = plant%growth%shoot_growing
          call plant%upgm_grow%plant%plantstate%state%replace("shoot_growing", l_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)

        end select

        plant%upgm_grow%plant%processCurrent%ptr => plant%upgm_grow%plant%processCurrent%ptr%processNext
      end do

      ! run daily preprocesses
      plant%upgm_grow%plant%processCurrent%ptr => plant%upgm_grow%plant%processes%ptr
      call plant%upgm_grow%preproc(plant%env)

      ! update local values changed by preprocesses
      plant%upgm_grow%plant%processCurrent%ptr => plant%upgm_grow%plant%processes%ptr
      do while ( associated(plant%upgm_grow%plant%processCurrent%ptr) )

        select case(plant%upgm_grow%plant%processCurrent%ptr%processName)

        case ("gddmethod1")
        case ("gddweps_method")
        case ("ritchie_vernalization")
          call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
          plant%growth%tchillucum = r_setter
        case ("ritchie_winterhardening")
          call plant%upgm_grow%plant%plantstate%state%get("harden_index", r_setter, success)
          plant%growth%thardnx = r_setter
        case ("weps_warmdays")
          call plant%upgm_grow%plant%plantstate%state%get("warmdays", r_setter, success)
          plant%growth%twarmdays = r_setter
        case ("weps_tempstress")
        case ("weps_freezedamage")
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaf", r_setter, success)
          plant%mass%standleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("fliveleaf", r_setter, success)
          plant%growth%fliveleaf = r_setter
        case ("weps_regrowth")
          call plant%upgm_grow%plant%plantstate%state%get("mstandstem", r_setter, success)
          plant%mass%standstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaf", r_setter, success)
          plant%mass%standleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandstore", r_setter, success)
          plant%mass%standstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstem", r_setter, success)
          plant%mass%flatstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatleaf", r_setter, success)
          plant%mass%flatleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstore", r_setter, success)
          plant%mass%flatstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("masshoot", r_setter, success)
          plant%growth%mshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mbgstemz", ra_setter, success)
          plant%mass%stemz = ra_setter
          deallocate(ra_setter, stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to deallocate memory for UPGM.'
          end if
          call plant%upgm_grow%plant%plantstate%state%get("height", r_setter, success)
          plant%geometry%zht = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayam", i_setter, success)
          plant%growth%dayam = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("grainf", r_setter, success)
          plant%geometry%grainf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("leafareatrend", r_setter, success)
          plant%growth%leafareatrend = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("stemmasstrend", r_setter, success)
          plant%growth%stemmasstrend = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
          plant%growth%tchillucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("regrowth_flg", regrowth_flg, success)

          cropres = create_crop_residue(soil%nslay)

          call plant%upgm_grow%plant%plantstate%state%get("res_standstem", r_setter, success)
          cropres%standstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_standleaf", r_setter, success)
          cropres%standleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_standstore", r_setter, success)
          cropres%standstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_flatstem", r_setter, success)
          cropres%flatstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_flatleaf", r_setter, success)
          cropres%flatleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_flatstore", r_setter, success)
          cropres%flatstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_bgstemz", ra_setter, success)
          cropres%stemz = ra_setter
          deallocate(ra_setter, stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to deallocate memory for UPGM.'
          end if
          call plant%upgm_grow%plant%plantstate%state%get("res_grainf", r_setter, success)
          cropres%grainf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_zht", r_setter, success)
          cropres%zht = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("res_dstm", r_setter, success)
          cropres%dstm = r_setter

          ! check for abandoned stems in crop regrowth
          if( ( cropres%standstem + cropres%standleaf + cropres%standstore &
              + cropres%flatstem + cropres%flatleaf + cropres%flatstore ) &
               .gt. 0.0 ) then
            ! create new residue pool and transfer cropres into it
            plant%residue => residueAdd( plant%residue, plant%residueIndex, soil%nslay ) 

            plant%residue%standstem = cropres%standstem
            plant%residue%standleaf = cropres%standleaf
            plant%residue%standstore = cropres%standstore
            plant%residue%flatstem = cropres%flatstem
            plant%residue%flatleaf = cropres%flatleaf
            plant%residue%flatstore = cropres%flatstore
            plant%residue%stemz = cropres%stemz
            plant%residue%zht = cropres%zht
            plant%residue%dstm = cropres%dstm
            plant%residue%xstmrep = plant%geometry%xstmrep
            plant%residue%grainf = cropres%grainf

            ! reset abandoned stem amounts to zero
            r_setter = 0.0_dp
            call plant%upgm_grow%plant%plantstate%state%replace("res_standstem", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_standleaf", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_standstore", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_flatstem", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_flatleaf", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_flatstore", r_setter, success)
            nelem = size(soil%aszlyd)
            allocate(ra_setter(nelem), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'Unable to allocate memory for UPGM.'
            end if
            ra_setter = 0.0_dp
            call plant%upgm_grow%plant%plantstate%state%replace("res_bgstemz", ra_setter, success)
            deallocate(ra_setter, stat = alloc_stat)

            call plant%upgm_grow%plant%plantstate%state%replace("res_grainf", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_zht", r_setter, success)
            call plant%upgm_grow%plant%plantstate%state%replace("res_dstm", r_setter, success)

          end if

          call destroy_crop_residue(cropres)

          call plant%upgm_grow%plant%plantstate%state%get("do_regrow", l_setter, success)
          plant%growth%do_regrow = l_setter
        end select

        plant%upgm_grow%plant%processCurrent%ptr => plant%upgm_grow%plant%processCurrent%ptr%processNext
      end do

        !if the next stage is ready, check the specific stage value.
        call plant%upgm_grow%plant%plantstate%state%get("nextstage", nextstage, success)

        if (nextstage == 1.and.success) then

        call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%get("stagegdd", stagegdd, success)

            success = .false.
            write(*,*) 'Degree Days: ', stagegdd, ' Phase Completed: ', trim(plant%upgm_grow%plant%phaseCurrent%ptr%phaseLabel)

            success = .false.
            call plant%upgm_grow%plant%plantstate%state%get("specstage", specificStage, success)
            if (success) then
                ! zero out stagegdd
                !stagegdd = 0.0_dp
                !call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%replace("phase_rel_gdd", stagegdd, success)
                !call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%replace("stagegdd", stagegdd, success)
                if (specificStage .eq. 1) then
                    print *, "regrow phase requested"
                    plant%upgm_grow%plant%phaseCurrent%ptr => plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow
                    !call plant%upgm_grow%plant%phaseCurrent%ptr%doPhase(plant%upgm_grow%plant%plantstate, env)
                else
                    ! print *, "next phase requested"
                    ! just go to next phase
                    plant%upgm_grow%plant%phaseCurrent%ptr => plant%upgm_grow%plant%phaseCurrent%ptr%phaseChild
                endif
            endif

!            if( associated(plant%upgm_grow%plant%phaseCurrent%ptr) ) then 
!              ! write info for start of phase
!              write(*,*) 'Phase, stagegdd: ', trim(plant%upgm_grow%plant%phaseCurrent%ptr%phaseLabel), stagegdd
!            end if

            ! reset controls
            nextstage = 0  ! pass this out to calling routine for use, and reset there
            specificStage = 0
            call plant%upgm_grow%plant%plantstate%state%replace("nextstage", nextstage, success)
            call plant%upgm_grow%plant%plantstate%state%replace("specstage", specificStage, success)
        endif

      if( associated(plant%upgm_grow%plant%phaseCurrent%ptr) ) then

        ! if regrowth is triggered, set current phase pointer to regrowth phase and set phase state inputs
        if( plant%growth%do_regrow ) then
          if( associated(plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow) ) then
            plant%upgm_grow%plant%phaseCurrent%ptr => plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow
            ! reset phase state to start again
            r_setter = 0.0_dp
            call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%replace("phase_rel_gdd", r_setter, success)
            call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%replace("stagegdd", r_setter, success)
            plant%growth%do_regrow = .false.
          end if
        end if

        ! update daily inputs
        select case(plant%upgm_grow%plant%phaseCurrent%ptr%phaseName)
        case ("pmms_germination")

          allocate(ra_setter(soil%nslay), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          do idx=1,soil%nslay
            ra_setter(idx) = soil%theta(idx)
          end do
          call plant%env%state%replace("swc", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

        case ("pmms_shootgrg")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)
          r_setter = plant%growth%fliveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("fliveleaf", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)

        case ("pmms_basephenol")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)
          r_setter = plant%growth%fliveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("fliveleaf", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg

        case ("pmms_springphenol")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)
          r_setter = plant%growth%mtotshoot
          call plant%upgm_grow%plant%plantstate%state%replace("mtotshoot", r_setter, success)

          nelem = size(plant%mass%rootstorez)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
             write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%rootstorez
          call plant%upgm_grow%plant%plantstate%state%replace("mrootstorez", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          r_setter = plant%growth%fliveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("fliveleaf", r_setter, success)
          r_setter = plant%growth%zgrowpt
          call plant%upgm_grow%plant%plantstate%state%replace("zgrowpt", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)
          r_setter = plant%growth%thardnx
          call plant%upgm_grow%plant%plantstate%state%replace("harden_index", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)
          r_setter = plant%growth%dayspring
          call plant%upgm_grow%plant%plantstate%state%replace("dayspring", r_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)

        case ("pmms_fallphenol")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)
          r_setter = plant%growth%fliveleaf
          call plant%upgm_grow%plant%plantstate%state%replace("fliveleaf", r_setter, success)
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)

        case ("weps_shootgrow")

          call plant%env%state%replace("dayofyear", jd, success)

          ! calculate day length
!          r_setter = daylen(amalat, jd, civilrise)
!          call plant%env%state%replace("hrlt", r_setter, success)

          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%replace("plantpop", r_setter, success)
          !i_setter = plant%database%idc
          !call plant%upgm_grow%plant%plantstate%pars%replace("idc", i_setter, success)
          !r_setter = plant%database%tverndel
          !call plant%upgm_grow%plant%plantstate%pars%replace("tverndel", r_setter, success)
          !r_setter = plant%database%fleaf2stor
          !call plant%upgm_grow%plant%plantstate%pars%replace("leaf2stor", r_setter, success)
          !r_setter = plant%database%fstem2stor
          !call plant%upgm_grow%plant%plantstate%pars%replace("stem2stor", r_setter, success)
          !r_setter = plant%database%fstor2stor
          !call plant%upgm_grow%plant%plantstate%pars%replace("stor2stor", r_setter, success)
          !r_setter = plant%database%shoot
          !call plant%upgm_grow%plant%plantstate%pars%replace("regrmshoot", r_setter, success)
          !r_setter = plant%database%dmaxshoot
          !call plant%upgm_grow%plant%plantstate%pars%replace("dmaxshoot", r_setter, success)
          !r_setter = plant%database%hue
          !call plant%upgm_grow%plant%plantstate%pars%replace("huie", r_setter, success)
          !r_setter = plant%database%thum
          !call plant%upgm_grow%plant%plantstate%pars%replace("thum", r_setter, success)

          !r_setter = plant%mass%standstem
          !call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          !r_setter = plant%mass%standstore
          !call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          !r_setter = plant%mass%flatstem
          !call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          !r_setter = plant%mass%flatleaf
          !call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
          !r_setter = plant%mass%flatstore
          !call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)
          !r_setter = plant%growth%mshoot
          !call plant%upgm_grow%plant%plantstate%state%replace("masshoot", r_setter, success)
          r_setter = plant%growth%mtotshoot
          call plant%upgm_grow%plant%plantstate%state%replace("mtotshoot", r_setter, success)

          !nelem = size(plant%mass%stemz)
          !allocate(ra_setter(nelem), stat = alloc_stat)
          !if( alloc_stat .gt. 0 ) then
          !  write(*,*) 'Unable to allocate memory for UPGM.'
          !end if
          !ra_setter = plant%mass%stemz
          !call plant%upgm_grow%plant%plantstate%state%replace("mbgstemz", ra_setter, success)
          !deallocate(ra_setter, stat = alloc_stat)
          !if( alloc_stat .gt. 0 ) then
          !  write(*,*) 'Unable to deallocate memory for UPGM.'
          !end if

          nelem = size(plant%mass%rootstorez)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%rootstorez
          call plant%upgm_grow%plant%plantstate%state%replace("mrootstorez", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to deallocate memory for UPGM.'
          end if

          !r_setter = plant%geometry%zht
          !call plant%upgm_grow%plant%plantstate%state%replace("height", r_setter, success)
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          r_setter = plant%growth%zgrowpt
          call plant%upgm_grow%plant%plantstate%state%replace("zgrowpt", r_setter, success)
          r_setter = plant%growth%trthucum
          call plant%upgm_grow%plant%plantstate%state%replace("trthucum", r_setter, success)
          !r_setter = plant%geometry%grainf
          !call plant%upgm_grow%plant%plantstate%state%replace("grainf", r_setter, success)
          !r_setter = plant%growth%leafareatrend
          !call plant%upgm_grow%plant%plantstate%state%replace("leafareatrend", r_setter, success)
          !r_setter = plant%growth%stemmasstrend
          !call plant%upgm_grow%plant%plantstate%state%replace("stemmasstrend", r_setter, success)
          r_setter = plant%growth%dayspring
          call plant%upgm_grow%plant%plantstate%state%replace("dayspring", r_setter, success)
          !r_setter = plant%prev%liveleaf
          !call plant%upgm_grow%plant%plantstate%state%replace("prevliveleaf", r_setter, success)
          !r_setter = plant%prev%standleaf
          !call plant%upgm_grow%plant%plantstate%state%replace("prevstandleaf", r_setter, success)
          !r_setter = plant%prev%standstem
          !call plant%upgm_grow%plant%plantstate%state%replace("prevstandstem", r_setter, success)
          !r_setter = plant%prev%flatstem
          !call plant%upgm_grow%plant%plantstate%state%replace("prevflatstem", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)
          r_setter = plant%growth%thardnx
          call plant%upgm_grow%plant%plantstate%state%replace("harden_index", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)
          r_setter = plant%growth%dayspring
          call plant%upgm_grow%plant%plantstate%state%replace("dayspring", r_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)

        end select

        ! check for residual gdd and add to phase input
        call plant%upgm_grow%plant%plantstate%state%get("remgdd", remgdd, success)
        if( remgdd .gt. 0.0_dp ) then
          call plant%upgm_grow%plant%plantstate%state%get("daygdd", daygdd, success)

          daygdd = daygdd + remgdd
          remgdd = 0.0_dp
          call plant%upgm_grow%plant%plantstate%state%replace("daygdd", daygdd, success)
          call plant%upgm_grow%plant%plantstate%state%replace("remgdd", remgdd, success)
        end if

        ! run current phase
        call plant%upgm_grow%grow(plant%env)

        ! assign changed growth values back to WEPS variables
        select case(plant%upgm_grow%plant%phaseCurrent%ptr%phaseName)
        case ("pmms_germination")
          ffa = 1.0_dp
          ffw = 1.0_dp
          ffr = 1.0_dp
          gif = 0.0_dp
          shoot_hui = 0.0_dp
          shoot_huiy = 0.0_dp
          p_rw = 0.0_dp
          p_st = 0.0_dp
          p_lf = 0.0_dp
          p_rp = 0.0_dp
          pdht = 0.0_dp
          pdrd = 0.0_dp
          hu_delay = 0.0_dp

        case ("pmms_shootgrg")
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("pmms_basephenol")
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("pmms_springphenol")
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("harden_index", r_setter, success)
          plant%growth%thardnx = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayspring", r_setter, success)
          plant%growth%dayspring = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)
          call plant%upgm_grow%plant%plantstate%state%get("spring_flg", spring_flg, success)

        case ("pmms_fallphenol")
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("weps_shootgrow")

          call plant%upgm_grow%plant%plantstate%pars%get("leaf2stor", r_setter, success)
          plant%database%fleaf2stor = r_setter
          call plant%upgm_grow%plant%plantstate%pars%get("stem2stor", r_setter, success)
          plant%database%fstem2stor = r_setter
          call plant%upgm_grow%plant%plantstate%pars%get("stor2stor", r_setter, success)
          plant%database%fstor2stor = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("trthucum", r_setter, success)
          plant%growth%trthucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("harden_index", r_setter, success)
          plant%growth%thardnx = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayspring", r_setter, success)
          plant%growth%dayspring = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)
          call plant%upgm_grow%plant%plantstate%state%get("spring_flg", spring_flg, success)
          call plant%upgm_grow%plant%plantstate%state%get("lastday", l_setter, success)
          plant%growth%lastday = l_setter

        end select

        !call plant%upgm_grow%plant%plantstate%state%get("mstandstem", r_setter, success)
        !plant%mass%standstem = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("mstandleaf", r_setter, success)
        !plant%mass%standleaf = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("mstandstore", r_setter, success)
        !plant%mass%standstore = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("mflatstem", r_setter, success)
        !plant%mass%flatstem = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("mflatleaf", r_setter, success)
        !plant%mass%flatleaf = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("mflatstore", r_setter, success)
        !plant%mass%flatstore = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("masshoot", r_setter, success)
        !plant%growth%mshoot = r_setter

        !call plant%upgm_grow%plant%plantstate%state%get("mbgstemz", ra_setter, success)
        !plant%mass%stemz = ra_setter
        !deallocate(ra_setter, stat = alloc_stat)
        !if( alloc_stat .gt. 0 ) then
        !  write(*,*) 'Unable to deallocate memory for UPGM.'
        !end if

        !call plant%upgm_grow%plant%plantstate%state%get("mrootstorez", ra_setter, success)
        !plant%mass%rootstorez = ra_setter
        !deallocate(ra_setter, stat = alloc_stat)
        !if( alloc_stat .gt. 0 ) then
        !  write(*,*) 'Unable to deallocate memory for UPGM.'
        !end if

        !call plant%upgm_grow%plant%plantstate%state%get("height", r_setter, success)
        !plant%geometry%zht = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("dayam", i_setter, success)
        !plant%growth%dayam = i_setter
        !call plant%upgm_grow%plant%plantstate%state%get("zgrowpt", r_setter, success)
        !plant%growth%zgrowpt = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("fliveleaf", r_setter, success)
        !plant%growth%fliveleaf = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("grainf", r_setter, success)
        !plant%geometry%grainf = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("leafareatrend", r_setter, success)
        !plant%growth%leafareatrend = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("stemmasstrend", r_setter, success)
        !plant%growth%stemmasstrend = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
        !plant%growth%tchillucum = r_setter
        !call plant%upgm_grow%plant%plantstate%state%get("warmdays", r_setter, success)
        !plant%growth%twarmdays = r_setter

        if( associated(plant%upgm_grow%plant%phaseCurrent%ptr) ) then
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%get("stagegdd", r_setter, success)
          plant%growth%thucum = r_setter
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%get("phase_rel_gdd", hui, success)
          PhaseLabel = plant%upgm_grow%plant%phaseCurrent%ptr%PhaseLabel
        else
          call plant%upgm_grow%plant%plantstate%state%get("remgdd", remgdd, success)
          call plant%upgm_grow%plant%plantstate%state%get("daygdd", daygdd, success)
          plant%growth%thucum = plant%growth%thucum + daygdd - remgdd
        end if

        if(plant%growth%am0cif) then
          if (am0cfl(isr) .ge. 1) then
            ! put double blank lines in daily files to create growth blocks
            write(luocrop(isr),*)   ! crop.out
            write(luocrop(isr),*)   ! crop.out
            write(luoshoot(isr),*)   ! shoot.out
            write(luoshoot(isr),*)   ! shoot.out
          end if
          ! turn off initialization flag
          plant%growth%am0cif = .false.
        end if

        if( plant%growth%growing ) then
          ! crop growth not yet complete
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow

          if( shoot_huiy .lt. 1.0_dp ) then

            if( shoot_hui .gt. 0.0_dp ) then

              ! set shoot growth flag
              plant%growth%shoot_growing = .true.

              ! daily shoot growth
              call shoot_grow( soil, plant, shoot_hui, shoot_huiy, s_root_sum, f_root_sum, tot_mass_req, &
                end_shoot_mass, end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass, &
                end_stem_mass, end_stem_area, end_shoot_len )

            end if

            if( shoot_hui .ge. 1.0_dp ) then
              ! shoot growth completed on this day

              ! set flag indicating regrowth capability
              plant%growth%shoot_growing = .false.

              ! move growing point to regrowth depth after shoot growth complete
              ! remember, a negative number is above ground
              plant%growth%zgrowpt = ( - plant%database%zloc_regrow )
            end if

          end if

          ! used in growth
          call plant%upgm_grow%plant%plantstate%state%get("tstress", tstress, success)
          call plant%upgm_grow%plant%plantstate%state%get("lost_mass", lost_mass, success)

          call growth( soil, plant, &
                   dble(cli_today%eirr), &
                   eff_lai, trad_lai, tstress, p_rw, p_st, p_lf, p_rp, &
                   pdht, pdrd, &
                   ffa, ffw, ffr, gif, par, apar, pddm, &
                   stem_propor, pdiam, parea, &
                   temp_sai, temp_stmrep, lost_mass )

          if(    (plant%database%fleaf2stor .gt. 0.0_dp) &
            .or. (plant%database%fstem2stor .gt. 0.0_dp) &
            .or. (plant%database%fstor2stor .gt. 0.0_dp) ) then
            plant%growth%can_regrow = .true.
          else
            plant%growth%can_regrow = .false.
          end if

          ! set trend direction for living leaf area
          trend = (plant%growth%fliveleaf * plant%mass%standleaf) - (plant%prev%liveleaf * plant%prev%standleaf)
          if ((trend .ne. 0.0_dp) &
            .and. (.not. plant%growth%shoot_growing .or. (plant%database%idc.eq.8))) then
            ! trend non-zero and (heat units past emergence or staged crown release crop)
            plant%growth%leafareatrend = trend
          end if
          ! set trend direction for above ground stem mass from growth
          trend = plant%mass%standstem + plant%mass%flatstem - plant%prev%standstem - plant%prev%flatstem
          if ((trend .ne. 0.0_dp) &
            .and. (.not. plant%growth%shoot_growing .or. (plant%database%idc.eq.8))) then
            ! trend non-zero and (heat units past emergence or staged crown release crop)
            plant%growth%stemmasstrend = trend
          end if

          plant%prev%standstem = plant%mass%standstem
          plant%prev%standleaf = plant%mass%standleaf
          plant%prev%standstore = plant%mass%standstore
          plant%prev%flatstem = plant%mass%flatstem
          plant%prev%flatleaf = plant%mass%flatleaf
          plant%prev%flatstore = plant%mass%flatstore
          plant%prev%liveleaf = plant%growth%fliveleaf
          plant%prev%mshoot = plant%growth%mshoot
          do idx = 1, soil%nslay
            plant%prev%stemz(idx) = plant%mass%stemz(idx)
            plant%prev%rootstorez(idx) = plant%mass%rootstorez(idx)
            plant%prev%rootfiberz(idx) = plant%mass%rootfiberz(idx)
          end do
          plant%prev%ht = plant%geometry%zht
          plant%prev%zshoot = plant%geometry%zshoot
          plant%prev%stm = plant%geometry%dstm
          plant%prev%rtd = plant%geometry%zrtd
          plant%prev%dayap = plant%growth%dayap
          plant%prev%hucum = plant%growth%thucum
          plant%prev%rthucum = plant%growth%trthucum
          plant%prev%grainf = plant%geometry%grainf
          plant%prev%chillucum = plant%growth%tchillucum
          plant%prev%dayspring = plant%growth%dayspring

          ! update values not directly used in growth, but for reporting
          plant%growth%dayap = plant%growth%dayap + 1
          plant%prev%dayap = plant%growth%dayap

        else
          ! accumulate days after maturity
          plant%growth%dayam = plant%growth%dayam + 1

        end if

        if (am0cfl(isr) .ge. 1) then
          !  print crop submodel output into 'crop.out'
          ! temporary sum for output
          temp_store = 0.0
          temp_fiber = 0.0
          temp_stem = 0.0
          do idx = 1, soil%nslay
            temp_store = temp_store + plant%mass%rootstorez(idx)
            temp_fiber = temp_fiber + plant%mass%rootfiberz(idx)
            temp_stem = temp_stem + plant%mass%stemz(idx)
          end do

          if( shoot_huiy .lt. 1.0_dp ) then
            if( shoot_hui .gt. 0.0_dp ) then
              if (am0cfl(isr) .ge. 1) then
                write(luoshoot(isr), &
                  "(1x,i5,1x,i3,1x,i4,1x,i4,1x,f6.3,2(1x,f10.4),2(1x,f12.4),4(1x,f10.4),4(1x,f10.4),(1x,f8.4),(1x,f8.3),1x,a)") &
                  daysim, jd, get_simdate_year(), plant%growth%dayap, shoot_hui, &
                  s_root_sum, f_root_sum, tot_mass_req, end_shoot_mass, &
                  end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass, &
                  end_stem_mass, end_stem_area, end_shoot_len, plant%geometry%zshoot, &
                  plant%growth%mshoot, plant%geometry%dstm, trim(plant%bname)
              end if
            end if

            if( shoot_hui .ge. 1.0_dp ) then
              if (am0cfl(isr) .ge. 1) then
                  ! single blank line to separate shoot growth periods
                  write(luoshoot(isr),*)  ! shoot.out
              end if
              ! last day of shoot grow, set shoot_huiy so shoot grow stops after shoot grow phase is completed.
              shoot_huiy = 1.0_dp
              call plant%upgm_grow%plant%plantstate%state%replace("shoot_huiy", shoot_huiy, success)
            end if
          end if

          if( plant%growth%growing) then
            ! reporting only variables
            call plant%upgm_grow%plant%plantstate%state%get("frst", frst, success)

            regrowth_or_spring_flg = max(regrowth_flg, spring_flg)

            write(luocrop(isr), "(1x,i6,1x,i3,1x,i4,1x,i5,1x,f6.3,12(1x,f7.4),1x,f7.2, &
     &         3(1x,f7.4),8(1x,f6.3),1x,e12.3, 11(1x,f6.3),2(1x,f8.5),1x,i2,1x,f6.3,1x,a,1x,a)") &
            daysim, jd, get_simdate_year(), plant%growth%dayap, &
            hui, &
            plant%mass%standstem, plant%mass%standleaf, plant%mass%standstore, &
            plant%mass%flatstem, plant%mass%flatleaf, plant%mass%flatstore, &
            temp_store, temp_fiber, temp_stem, &
            plant%mass%standleaf + plant%mass%flatleaf, &
            plant%mass%standstem + plant%mass%flatstem + temp_stem, &
            plant%geometry%zht, plant%geometry%dstm, trad_lai, eff_lai, plant%geometry%zrtd, &
            plant%geometry%grainf, tstress, plant%growth%fwsf, frst, ffa, ffw, &
            par, apar, pddm, p_rw, p_st, p_lf, p_rp, &
            stem_propor, pdiam, parea, pdiam/plant%database%diammax, &
            parea*plant%geometry%dpop, hu_delay, plant%growth%thardnx, temp_sai,  &
            temp_stmrep, regrowth_or_spring_flg, plant%growth%fliveleaf, trim(plant%bname) !, trim(PhaseLabel)
          end if

        end if

        if( plant%growth%lastday ) then
          ! heat units completed, crop leaf mass is non transpiring
          plant%growth%fliveleaf = 0.0_dp
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%replace("lastday", l_setter, success)
          plant%growth%lastday = .false.
          plant%growth%growing = .false.
        end if

      end if

    end subroutine run_UPGM

end module WEPS_UPGM_mod
