!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPS_UPGM_mod

    implicit none

  contains

    subroutine init_WEPS_UPGM( isr, soil, plant )
      use upgm_mod
      use constants, only : dp, int32, precision_init
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer
      use climate_input_mod, only: cli_day
      use solar_mod, only: amalat, civilrise, daylen
      use datetime_mod, only: get_psim_doy, get_psim_juld
      use environment_state_mod
      use WEPSCrop_util_mod, only: hu_leaf_days

      integer, intent(in) :: isr  ! subregion index number
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older 

      real(dp) :: r_setter
      real(dp), dimension(:), allocatable :: ra_setter
      integer(int32) :: i_setter
      logical :: l_setter
      logical :: success = .false.
      integer :: nelem
      integer :: alloc_stat
      integer(int32) :: jd ! day of year
      integer :: pjuld  ! present simulation julian day

      ! present julian day
      pjuld = get_psim_juld(isr)

      ! init precision values
      call precision_init()

      ! initialize upgm_grow model
      plant%upgm_grow = UPGM()
      call plant%upgm_grow%plant%plantstate%init()

      ! iniitalize environmental conditions
      jd = get_psim_doy(isr)
      plant%env = environment_state()
      call plant%env%init()

      ! add process
      ! create gddWEPS method
      call plant%upgm_grow%plant%add_process("gddWEPS_method", "WEPS GDD", 0)
      ! create input names
      r_setter = cli_day(pjuld)%tdmn
      call plant%env%state%put("tmin", r_setter, success)
      r_setter = cli_day(pjuld)%tdmx
      call plant%env%state%put("tmax", r_setter, success)
      r_setter = plant%database%tmin
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("tbas", r_setter, success)
      r_setter = plant%database%topt
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("topt", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("daygdd", r_setter, success)


      if( .not. ( (plant%database%idc .eq. 9) &
        .or. (plant%database%idc .eq. 12) ) ) then
        ! add process
        ! create ritchieVernalization method
        call plant%upgm_grow%plant%add_process("ritchieVernalization", "Vernalization", 0)
        ! create input names
        r_setter = 0.0_dp
        call plant%upgm_grow%plant%plantstate%state%put("chill_unit_cum", r_setter, success)
      end if

      ! add process
      ! create WEPS temperature stress method
      call plant%upgm_grow%plant%add_process("WEPSTempStress", "Temp Stress", 0)
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
      call plant%upgm_grow%plant%add_process("WEPSFreezeDamage", "Freeze Damage", 0)
      ! create input names
      r_setter = 1.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("ffa", r_setter, success)
      r_setter = soil%tsmn(1)
      call plant%env%state%put("tsmn1", r_setter, success)
      ! create process parameters for frost damage s-curve database values
      r_setter = plant%database%fd1(1)
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("frsx1", r_setter, success)
      r_setter = plant%database%fd2(1)
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("frsx2", r_setter, success)
      r_setter = plant%database%fd1(2)
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("frsy1", r_setter, success)
      r_setter = plant%database%fd2(2)
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("frsy2", r_setter, success)


      r_setter = plant%mass%standleaflive
      call plant%upgm_grow%plant%plantstate%state%put("mstandleaflive", r_setter, success)
      r_setter = plant%mass%standleafdead
      call plant%upgm_grow%plant%plantstate%state%put("mstandleafdead", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("frst", r_setter, success)
      call plant%upgm_grow%plant%plantstate%state%put("lost_mass", r_setter, success)

      ! add process
      ! create ritchieHardening method
      call plant%upgm_grow%plant%add_process("ritchieHardening", "Winter Hardening", 0)
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
      call plant%upgm_grow%plant%add_process("WEPSwarmdays", "Warm Days", 0)
      ! create input names
      r_setter = plant%database%tmin
      call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("tbas", r_setter, success)
      r_setter = 0.0_dp
      call plant%upgm_grow%plant%plantstate%state%put("warmdays", r_setter, success)

      if( (plant%database%idc .ge. 1) .and. (plant%database%idc .le. 8) ) then
        ! Annuals, Winter Annuals, Perennnials, w/ Tuber dormancy and w/ Staged release

        if(    (plant%database%idc .eq. 1) &
          .or. (plant%database%idc .eq. 4) ) then
          ! Annuals

          ! add process
          ! create WEPStrendleafexternal method
          call plant%upgm_grow%plant%add_process("WEPStrendleafexternal", "Set Leaf Area Trend from Operations", 0)

          ! create input names
          ! plant state
          ! mstandleaflive (created above)
          r_setter = plant%prev%standleaflive
          call plant%upgm_grow%plant%plantstate%state%put("prevstandleaflive", r_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%put("leafareatrend", r_setter, success)

          ! add process
          ! create WEPSregrowthannual method
          call plant%upgm_grow%plant%add_process("WEPSregrowthannual", "Check for Annual Regrowth", 0)

          ! create input names
          ! plant database
          ! plantpop (created below)
          r_setter = plant%database%fleafstem
          call plant%upgm_grow%plant%plantstate%pars%put("leafstem", r_setter, success)
          ! regrmshoot (created below)
          ! dmaxshoot (created below)"growing
          r_setter = plant%database%storeinit
          call plant%upgm_grow%plant%plantstate%pars%put("storeinit", r_setter, success)
          r_setter = plant%database%zloc_regrow
          call plant%upgm_grow%plant%plantstate%pars%put("zloc_regrow", r_setter, success)
          ! huie (created below)

          ! plant state
          ! mstandleaflive (created above)
          ! mstandleafdead (created above)
          ! can_regrow (created below)
          ! leafareatrend (created above)
          ! warmdays (created above)
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%put("shoot_growing", l_setter, success)
          l_setter = plant%growth%growing
          call plant%upgm_grow%plant%plantstate%state%put("growing", l_setter, success)
          ! mtotshoot (created below)
          ! mrootstorez (created below)
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%put("masshoot", r_setter, success)
          i_setter = plant%growth%dayam
          call plant%upgm_grow%plant%plantstate%state%put("dayam", i_setter, success)
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%put("mstandstem", r_setter, success)
          ! mstandleaf (created above)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%put("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%put("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%put("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%put("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%put("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%put("height", r_setter, success)
          ! dstm (created below)
          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%put("grainf", r_setter, success)
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
          ! thu_shoot_beg (created below)
          ! thu_shoot_end (created below)
          ! chill_unit_cum (created above)
          i_setter = -2
          call plant%upgm_grow%plant%plantstate%state%put("regrowth_flg", i_setter, success)
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%put("do_regrow", l_setter, success)

        else if(    (plant%database%idc .eq. 2) &
               .or. (plant%database%idc .eq. 5) ) then
          ! Winter Annuals 

          ! add process
          ! create WEPS_winterAnnSpring method
          call plant%upgm_grow%plant%add_process("WEPSwinterAnnSpring", "Check for spring growth of winter annuals", 0)

          ! create input names
          i_setter = 0
          call plant%env%state%put("day_of_year", i_setter, success)

          ! plant database
          ! tverndel (created below)

          ! plant state
          ! can_regrow (created below)
          r_setter = plant%growth%zgrowpt
          call plant%upgm_grow%plant%plantstate%state%put("zgrowpt", r_setter, success)
          ! harden_index (created above)
          ! warmdays (created above)
          ! chill_unit_cum (created above)
          r_setter = plant%growth%dayspring
          call plant%upgm_grow%plant%plantstate%state%put("dayspring", r_setter, success)
          i_setter = 0
          call plant%upgm_grow%plant%plantstate%state%put("spring_flg", i_setter, success)
          ! do_spring (created by shootgrow below)

        else if( (plant%database%idc .eq. 3) &
            .or. (plant%database%idc .eq. 6) ) then
          ! Perennnials, and w/ Tuber dormancy

          ! add process
          ! create WEPStrendleafexternal method
          call plant%upgm_grow%plant%add_process("WEPStrendleafexternal", "Set Leaf Area Trend from Operations", 0)

          ! create input names
          ! plant state
          ! mstandleaflive (created above)
          r_setter = plant%prev%standleaflive
          call plant%upgm_grow%plant%plantstate%state%put("prevstandleaflive", r_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%put("leafareatrend", r_setter, success)

          ! add process
          ! create WEPSregrowthperen method
          call plant%upgm_grow%plant%add_process("WEPSregrowthperen", "Check for Perennial Regrowth", 0)

          ! create input names
          ! plant database
          ! plantpop (created below)
          r_setter = plant%database%storeinit
          call plant%upgm_grow%plant%plantstate%pars%put("storeinit", r_setter, success)
          r_setter = plant%database%fleafstem
          call plant%upgm_grow%plant%plantstate%pars%put("leafstem", r_setter, success)
          ! regrmshoot (created below)
          ! dmaxshoot (created below)
          r_setter = plant%database%zloc_regrow
          call plant%upgm_grow%plant%plantstate%pars%put("zloc_regrow", r_setter, success)
          ! huie (created below)

          ! plant state
          ! can_regrow (created below)
          ! leafareatrend (created above)
          ! mstandleaflive (created above)
          ! mstandleafdead (created above)
          ! warmdays (created above)
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%put("shoot_growing", l_setter, success)
          ! mtotshoot (created below)
          ! mrootstorez (created below)
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%put("mstandstem", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%put("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%put("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%put("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%put("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%put("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%put("grainf", r_setter, success)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%put("height", r_setter, success)
          ! dstm (created below)
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
          ! thu_shoot_beg (created below)
          ! thu_shoot_end (created below)
          i_setter = plant%growth%dayam
          call plant%upgm_grow%plant%plantstate%state%put("dayam", i_setter, success)
          ! chill_unit_cum (created above)
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%put("masshoot", r_setter, success)
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%put("do_regrow", l_setter, success)
          i_setter = -2
          call plant%upgm_grow%plant%plantstate%state%put("regrowth_flg", i_setter, success)
          r_setter = 0.0_dp
          call plant%upgm_grow%plant%plantstate%state%put("regrow_release", r_setter, success)

        else if( plant%database%idc .eq. 8 ) then
          ! Perennnials, w/ Staged release

          ! add process
          ! create WEPStrendleafexternal method
          call plant%upgm_grow%plant%add_process("WEPStrendleafexternal", "Set Leaf Area Trend from Operations", 0)

          ! create input names
          ! plant state
          ! mstandleaflive (created above)
          r_setter = plant%prev%standleaflive
          call plant%upgm_grow%plant%plantstate%state%put("prevstandleaflive", r_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%put("leafareatrend", r_setter, success)

          ! add process
          ! create WEPStrendstemexternal method
          call plant%upgm_grow%plant%add_process("WEPStrendstemexternal", "Set Stem Mass Trend from Operations", 0)

          ! create input names
          ! plant state
          ! mstandstem (created below)
          ! mflatstem (created below)
          r_setter = plant%prev%standstem
          call plant%upgm_grow%plant%plantstate%state%put("prevstandstem", r_setter, success)
          r_setter = plant%prev%flatstem
          call plant%upgm_grow%plant%plantstate%state%put("prevflatstem", r_setter, success)
          r_setter = plant%growth%stemmasstrend
          call plant%upgm_grow%plant%plantstate%state%put("stemmasstrend", r_setter, success)

          ! add process
          ! create WEPSregrowthstaged method
          call plant%upgm_grow%plant%add_process("WEPSregrowthstaged", "Check for Staged Regrowth", 0)

          ! create input names
          ! plant database
          ! plantpop (created below)
          r_setter = plant%database%storeinit
          call plant%upgm_grow%plant%plantstate%pars%put("storeinit", r_setter, success)
          r_setter = plant%database%fleafstem
          call plant%upgm_grow%plant%plantstate%pars%put("leafstem", r_setter, success)
          ! regrmshoot (created below)
          ! dmaxshoot (created below)
          r_setter = plant%database%zloc_regrow
          call plant%upgm_grow%plant%plantstate%pars%put("zloc_regrow", r_setter, success)
          ! huie (created below)

          ! plant state
          ! can_regrow (created below)
          ! leafareatrend (created above)
          ! mstandleaflive (created above)
          ! mstandleafdead (created above)
          ! warmdays (created above)
          ! stemmasstrend (created above)
          ! mtotshoot (created below)
          ! mrootstorez (created below)
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%put("mstandstem", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%put("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%put("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%put("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%put("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%put("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%put("grainf", r_setter, success)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%put("height", r_setter, success)
          ! dstm (created below)
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
          ! thu_shoot_beg (created below)
          ! thu_shoot_end (created below)
          i_setter = plant%growth%dayam
          call plant%upgm_grow%plant%plantstate%state%put("dayam", i_setter, success)
          ! chill_unit_cum (created above)
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%put("masshoot", r_setter, success)
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%put("do_regrow", l_setter, success)
          i_setter = -2
          call plant%upgm_grow%plant%plantstate%state%put("regrowth_flg", i_setter, success)

          ! environment variables
          ! hrlty (created below)
          ! hrlt (created below)
        end if

        ! Crop type 1-8 original WEPS ShootGrow

        ! create phase WEPS_ShootGrow
        call plant%upgm_grow%plant%add_phase("WEPS_ShootGrow", "Shoot Grow", 0)
        ! Associate regrowth phase
        plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow => plant%upgm_grow%plant%phaseCurrent%ptr

        ! create phase states
        r_setter = 0.0_dp
        call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
        call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)

        ! plant database
        r_setter = plant%database%hue
        call plant%upgm_grow%plant%plantstate%pars%put("huie", r_setter, success)
        ! thum (created below)
        ! alf (created below)
        ! blf (created below)
        ! clf (created below)
        ! dlf (created below)
        ! arp (created below)
        ! brp (created below)
        ! crp (created below)
        ! drp (created below)
        ! aht (created below)
        ! bht (created below)
        ! zmxc (created below)
        ! zmrt (created below)
        ! ehu0 (created below)
        r_setter = plant%geometry%dpop
        call plant%upgm_grow%plant%plantstate%pars%put("plantpop", r_setter, success)
        r_setter = plant%database%shoot
        call plant%upgm_grow%plant%plantstate%pars%put("regrmshoot", r_setter, success)
        r_setter = plant%database%dmaxshoot
        call plant%upgm_grow%plant%plantstate%pars%put("dmaxshoot", r_setter, success)
        r_setter = plant%database%fleaf2stor
        call plant%upgm_grow%plant%plantstate%pars%put("leaf2stor", r_setter, success)
        r_setter = plant%database%fstem2stor
        call plant%upgm_grow%plant%plantstate%pars%put("stem2stor", r_setter, success)
        r_setter = plant%database%fstor2stor
        call plant%upgm_grow%plant%plantstate%pars%put("stor2stor", r_setter, success)
        r_setter = plant%database%tverndel
        call plant%upgm_grow%plant%plantstate%pars%put("tverndel", r_setter, success)

        ! plant state
        ! dstm (created below)
        ! trthucum (created below)
        ! thu_shoot_beg (created below)
        ! thu_shoot_end (created below)
        ! can_regrow (created below)
        ! daygdd (created above)
        l_setter = .false.
        call plant%upgm_grow%plant%plantstate%state%put("do_spring", l_setter, success)
        r_setter = plant%growth%mtotshoot
        call plant%upgm_grow%plant%plantstate%state%put("mtotshoot", r_setter, success)
        ! mrootstorez (created below)
        ! harden_index (created above)
        ! chill_unit_cum (created above)
        ! nextstage (created below in set_start_UPGM)
        ! shoot_hui (created below)
        ! shoot_huiy (created below)
        ! lastday (created below)
        ! ffa (created above)
        ! ffw (created below)
        ! ffr (created below)
        ! gif (created below)
        ! p_rw (created below)
        ! p_st (created below)
        ! p_lf (created below)
        ! p_rp (created below)
        ! pdht (created below)
        ! pdrd (created below)
        r_setter = 1.0_dp
        call plant%upgm_grow%plant%plantstate%state%put("hu_delay", r_setter, success)

      else if(    (plant%database%idc .eq. 9) &
        .or. (plant%database%idc .eq. 10) &
        .or. (plant%database%idc .eq. 11) &
        .or. (plant%database%idc .eq. 12) ) then

        ! create WEPScolddays process
        call plant%upgm_grow%plant%add_process("WEPScolddays", "WEPS cold day accum", 0)
        ! create input names
        ! Process Parameters
        r_setter = plant%database%tmin
        call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("tbas", r_setter, success)
        ! plant state
          r_setter = plant%growth%tcolddays
        call plant%upgm_grow%plant%plantstate%state%put("colddays", r_setter, success)
        ! environment variables
        ! tmax (created above)
        ! tmin (created above)

        ! create WEPSleafoff process
        call plant%upgm_grow%plant%add_process("WEPSleafoff", "Fall leaf off check", 0)
        ! create input names
        ! Process Parameters
        if( (plant%database%idc .eq. 9) .or. (plant%database%idc .eq. 12) ) then
          r_setter = 1.0_dp
        else if( plant%database%idc .eq. 11 ) then
          r_setter = 0.5_dp
        else  if( plant%database%idc .eq. 10 ) then
          r_setter = 0.0_dp
        end if
        call plant%upgm_grow%plant%processCurrent%ptr%processPars%put("dropfrac", r_setter, success)
        ! plant state
        i_setter = 0
        call plant%env%state%put("day_of_year", i_setter, success)
        ! can_regrow (created below)
        i_setter = plant%growth%dayleafoff
        call plant%upgm_grow%plant%plantstate%state%put("dayleafoff", i_setter, success)
        ! cold_days (created above)
        ! mstandleaflive (created above)
        ! mstandleafdead (created above)
        r_setter = plant%mass%flatleaf
        call plant%upgm_grow%plant%plantstate%state%put("mflatleaf", r_setter, success)
        ! environment variables
        ! hrlty (created below)
        ! hrlt (created below)
        ! output variables (not input)
        i_setter = plant%growth%dayleafon
        call plant%upgm_grow%plant%plantstate%state%put("dayleafon", i_setter, success)
        r_setter = 0.0_dp
        call plant%upgm_grow%plant%plantstate%state%put("res_flatleaf", r_setter, success)
        l_setter = plant%growth%do_leafoff
        call plant%upgm_grow%plant%plantstate%state%put("do_leafoff", l_setter, success)

        ! create WEPSleafon process
        call plant%upgm_grow%plant%add_process("WEPSleafon", "Spring leaf on check", 0)

        ! create input names
        ! plant state
        ! day_of_year (created above)
        ! can_regrow (created below)
        ! shoot_growing (created below)
        ! dayleafon (created above)
        ! hrlty (created below)
        ! hrlt (created below)
        ! warmdays (created above)
        ! mstandstore (created below)
        r_setter = plant%mass%flatstore
        call plant%upgm_grow%plant%plantstate%state%put("mflatstore", r_setter, success)
        r_setter = 0.0_dp
        call plant%upgm_grow%plant%plantstate%state%put("res_flatstore", r_setter, success)
        ! dayleafoff (created above)
        l_setter = plant%growth%do_leafon
        call plant%upgm_grow%plant%plantstate%state%put("do_leafon", l_setter, success)

        if( plant%database%idc .eq. 12 ) then
          ! Deciduous Wood capable of regrowing from stump

          ! create WEPSregrowwood process
          call plant%upgm_grow%plant%add_process("WEPSregrowwood", "Check Regrowth from Stump", 0)
          ! create input names

          ! plant database
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%put("plantpop", r_setter, success)
          r_setter = plant%database%shoot
          call plant%upgm_grow%plant%plantstate%pars%put("regrmshoot", r_setter, success)
          r_setter = plant%database%dmaxshoot
          call plant%upgm_grow%plant%plantstate%pars%put("dmaxshoot", r_setter, success)
          r_setter = plant%database%hue
          call plant%upgm_grow%plant%plantstate%pars%put("huie", r_setter, success)
          r_setter = plant%database%zloc_regrow
          call plant%upgm_grow%plant%plantstate%pars%put("zloc_regrow", r_setter, success)

          ! plant state
          ! can_regrow (created below)
          r_setter = plant%prev%ht
          call plant%upgm_grow%plant%plantstate%state%put("prevheight", r_setter, success)
          ! warmdays (created above)
          ! shoot_growing (created below)
          ! mrootstorez (created below)
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%put("mstandstem", r_setter, success)
          ! mstandleaf (created above)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%put("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%put("mflatstem", r_setter, success)
          ! mflatleaf (created above)
          ! mflatstore (created below)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%put("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%put("grainf", r_setter, success)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%put("height", r_setter, success)
          ! dstm (created below)
          r_setter = 0.0_dp
          call plant%upgm_grow%plant%plantstate%state%put("res_standstem", r_setter, success)
          call plant%upgm_grow%plant%plantstate%state%put("res_standleaf", r_setter, success)
          call plant%upgm_grow%plant%plantstate%state%put("res_standstore", r_setter, success)
          ! res_flatleaf (created above)
          call plant%upgm_grow%plant%plantstate%state%put("res_flatstem", r_setter, success)
          ! res_flatstore (created above)

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
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%put("masshoot", r_setter, success)
          r_setter = plant%growth%mtotshoot
          call plant%upgm_grow%plant%plantstate%state%put("mtotshoot", r_setter, success)
          i_setter = -2
          call plant%upgm_grow%plant%plantstate%state%put("regrowth_flg", i_setter, success)
          l_setter = .false.
          call plant%upgm_grow%plant%plantstate%state%put("do_regrow", l_setter, success)

        end if

        if( (plant%database%idc .eq. 9) .or. (plant%database%idc .eq. 12) ) then
          ! Deciduous Wood

          ! create phase Deciduous Wood
          call plant%upgm_grow%plant%add_phase("WEPS_DeciduousWood", "Shoot Grow Deciduous Wood", 0)
          ! Associate regrowth phase
          plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow => plant%upgm_grow%plant%phaseCurrent%ptr

          ! create phase parameter
          r_setter = hu_leaf_days * (plant%database%topt-plant%database%tmin) / plant%database%thum
          call plant%upgm_grow%plant%phaseCurrent%ptr%phasePars%put("leafhuie", r_setter, success)

          ! create phase states
          r_setter = 0.0_dp
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)

          ! create inputs
          ! plant database
          ! thum (created below)
          ! alf (created below)
          ! blf (created below)
          ! clf (created below)
          ! dlf (created below)
          ! arp (created below)
          ! brp (created below)
          ! crp (created below)
          ! drp (created below)
          ! aht (created below)
          ! bht (created below)
          ! zmxc (created below)
          ! zmrt (created below)
          ! ehu0 (created below)

          ! plant state
          ! dstm (created below)
          ! trthucum (created below)
          ! thu_shoot_beg (created below)
          ! thu_shoot_end (created below)
          ! can_regrow (created below)
          ! daygdd (created above)
          ! do_leafon (created above)
          ! do_leafoff (created above)
          ! mrootstorez (created below)
          ! colddays (created above)
          ! warmdays (created above)
          ! mtotleaf (created below)
          ! thu_leaf_beg (created below)
          ! thu_leaf_end (created below)
          ! nextstage (created below in set_start_UPGM)
          ! ffa (created above)
          ! ffw (created below)
          ! ffr (created below)
          ! gif (created below)
          ! shoot_hui (created below)
          ! shoot_huiy (created below)
          ! p_rw (created below)
          ! p_st (created below)
          ! p_lf (created below)
          ! p_rp (created below)
          ! pdht (created below)
          ! pdrd (created below)
          ! lastday (created below)
          l_setter = plant%growth%growing
          call plant%upgm_grow%plant%plantstate%state%put("growing", l_setter, success)

        elseif( (plant%database%idc .eq. 10) .or. (plant%database%idc .eq. 11) ) then
          ! Evergreen Wood

          ! create phase Evergreen Wood
          call plant%upgm_grow%plant%add_phase("WEPS_EvergreenWood", "Shoot Grow Evergreen Wood", 0)
          ! Associate regrowth phase
          plant%upgm_grow%plant%phaseCurrent%ptr%phaseRegrow => plant%upgm_grow%plant%phaseCurrent%ptr

          ! create phase parameter
          r_setter = hu_leaf_days * (plant%database%topt-plant%database%tmin) / plant%database%thum
          call plant%upgm_grow%plant%phaseCurrent%ptr%phasePars%put("leafhuie", r_setter, success)

          ! create phase states
          r_setter = 0.0_dp
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("stagegdd", r_setter, success)
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%put("phase_rel_gdd", r_setter, success)

          ! plant database
          ! thum (created below)
          ! alf (created below)
          ! blf (created below)
          ! clf (created below)
          ! dlf (created below)
          ! arp (created below)
          ! brp (created below)
          ! crp (created below)
          ! drp (created below)
          ! aht (created below)
          ! bht (created below)
          ! zmxc (created below)
          ! zmrt (created below)
          ! ehu0 (created below)
          r_setter = plant%database%tverndel
          call plant%upgm_grow%plant%plantstate%pars%put("tverndel", r_setter, success)

          ! plant state
          ! chill_unit_cum (created above)
          ! shoot_growing (created below)
          ! dstm (created below)
          ! trthucum (created below)
          ! thu_shoot_beg (created below)
          ! thu_shoot_end (created below)
          ! can_regrow (created below)
          ! daygdd (created above)
          ! do_leafon (created above)
          ! do_leafoff (created above)
          ! mrootstorez (created below)
          ! colddays (created above)
          ! warmdays (created above)
          ! mtotleaf (created below)
          ! thu_leaf_beg (created below)
          ! thu_leaf_end (created below)
          ! nextstage (created below in set_start_UPGM)
          ! ffa (created above)
          ! ffw (created below)
          ! ffr (created below)
          ! gif (created below)
          ! shoot_hui (created below)
          ! shoot_huiy (created below)
          ! p_rw (created below)
          ! p_st (created below)
          ! p_lf (created below)
          ! p_rp (created below)
          ! pdht (created below)
          ! pdrd (created below)
          ! reporting only variables
          r_setter = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%put("hu_delay", r_setter, success)
          ! lastday (created below)

        end if

        ! create inputs unique to these two phases

        ! plant database
        r_setter = 0.0_dp
        call plant%upgm_grow%plant%plantstate%state%put("mtotleaf", r_setter, success)

        ! plant state
        r_setter = 0.0_dp
        call plant%upgm_grow%plant%plantstate%state%put("thu_leaf_beg", r_setter, success)
        call plant%upgm_grow%plant%plantstate%state%put("thu_leaf_end", r_setter, success)
        ! warmdays (created above)
        l_setter = .false.
        call plant%upgm_grow%plant%plantstate%state%put("shoot_growing", l_setter, success)

        call plant%upgm_grow%plant%plantstate%state%put("leaf_hui", r_setter, success)
        call plant%upgm_grow%plant%plantstate%state%put("leaf_huiy", r_setter, success)

      end if

      ! create all input names
      ! plant database
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

      ! environment variables
      r_setter = daylen(amalat, jd-1, civilrise)
      call plant%env%state%put("hrlty", r_setter, success)
      r_setter = daylen(amalat, jd, civilrise)
      call plant%env%state%put("hrlt", r_setter, success)

      ! plant state
      r_setter = plant%geometry%dstm
      call plant%upgm_grow%plant%plantstate%state%put("dstm", r_setter, success)
      r_setter = plant%growth%trthucum
      call plant%upgm_grow%plant%plantstate%state%put("trthucum", r_setter, success)
      r_setter = plant%growth%thu_shoot_beg
      call plant%upgm_grow%plant%plantstate%state%put("thu_shoot_beg", r_setter, success)
      r_setter = plant%growth%thu_shoot_end
      call plant%upgm_grow%plant%plantstate%state%put("thu_shoot_end", r_setter, success)
      l_setter = .false.
      call plant%upgm_grow%plant%plantstate%state%put("can_regrow", l_setter, success)

      nelem = size(plant%mass%rootstorez)
      allocate(ra_setter(nelem), stat = alloc_stat)
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate memory for UPGM.'
      end if
      ra_setter = plant%mass%rootstorez
      call plant%upgm_grow%plant%plantstate%state%put("mrootstorez", ra_setter, success)
      deallocate(ra_setter, stat = alloc_stat)

      call plant%upgm_grow%plant%plantstate%state%put("res_flatleaf", r_setter, success)
      l_setter = .false.
      call plant%upgm_grow%plant%plantstate%state%put("lastday", l_setter, success)
      r_setter = 0.0_dp
      ! ffa (created above)
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

      ! all phases added, set phases to beginning
      call set_start_UPGM( isr, plant )

      return

    end subroutine init_WEPS_UPGM

    subroutine set_start_UPGM( isr, plant )
      use biomaterial, only: plant_pointer
      use datetime_mod, only: difdat
      use datetime_mod, only: get_psim_day, get_psim_mon, get_psim_year
      use constants, only : dp, int32

      ! + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr  ! subregion index number
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older 

      ! + + + LOCAL VARIABLES + + +
      type(plant_pointer), pointer :: thisPlant     ! pointer for looping through plants
      integer(int32) :: nextstage
      real(dp) :: r_setter
      logical :: success = .false.

      ! expecting pointer to the newest plant in existence
      thisPlant => plant
      do while( associated(thisPlant) )
        if( difdat (get_psim_day(isr), get_psim_mon(isr), get_psim_year(isr), &
                    thisPlant%pday, thisPlant%pmon, thisPlant%psimyr) .eq. 0 ) then
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

    end subroutine set_start_UPGM

    subroutine run_UPGM( isr, soil, plant )
      use upgm_mod
      use constants, only : dp, int32, check_return
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer, residueAdd
      use crop_data_struct_defs, only: crop_residue, am0cfl
      use crop_data_struct_defs, only: create_crop_residue, destroy_crop_residue
      use climate_input_mod, only: cli_day
      use datetime_mod, only: get_psim_daysim, get_psim_doy, get_psim_year, get_psim_juld
      use file_io_mod, only: luocrop, luoshoot
      use WEPSCrop_mod, only: shoot_grow, leaf_emerge, growth
      use solar_mod, only: amalat, civilrise, daylen

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
      real(dp) :: leaf_hui     ! today fraction of heat unit leaf growth index accumulation
      real(dp) :: leaf_huiy    ! previous day fraction of heat unit leaf growth index accumulation
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
      real(dp) :: temp_fliveleaf
      real(dp) :: lost_mass    ! biomass that decayed (disappeared) from scenescence and freeze damage
      real(dp) :: regrow_release ! fraction of storage root released to support regrowth of plant
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
      integer :: pjuld  ! present simulation julian day

      ! present julian day
      pjuld = get_psim_juld(isr)

      regrowth_flg = -2
      spring_flg = -2
      jd = get_psim_doy(isr)

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

        case ("gdd1_method")
          r_setter = cli_day(pjuld)%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_day(pjuld)%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("gddWEPS_method")
          r_setter = cli_day(pjuld)%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_day(pjuld)%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("ritchieHardening")
          r_setter = plant%growth%thardnx
          call plant%upgm_grow%plant%plantstate%state%replace("harden_index", r_setter, success)
          l_setter = plant%growth%can_harden
          call plant%upgm_grow%plant%plantstate%state%replace("can_harden", l_setter, success)
          r_setter = soil%tsmx(1)
          call plant%env%state%replace("tsmx1", r_setter, success)
          r_setter = soil%tsmn(1)
          call plant%env%state%replace("tsmn1", r_setter, success)
        case ("ritchieVernalization")
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)
          r_setter = cli_day(pjuld)%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_day(pjuld)%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("WEPScolddays")
          r_setter = plant%growth%tcolddays
          call plant%upgm_grow%plant%plantstate%state%replace("colddays", r_setter, success)
          r_setter = cli_day(pjuld)%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_day(pjuld)%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("WEPSFreezeDamage")
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%mass%standleafdead
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleafdead", r_setter, success)
          r_setter = soil%tsmn(1)
          call plant%env%state%replace("tsmn1", r_setter, success)
        case ("WEPSleafoff")
          ! environmental variables
          r_setter = daylen(amalat, jd-1, civilrise)
          call plant%env%state%replace("hrlty", r_setter, success)
          r_setter = daylen(amalat, jd, civilrise)
          call plant%env%state%replace("hrlt", r_setter, success)
          i_setter = jd
          call plant%env%state%replace("day_of_year", i_setter, success)
          ! plant state
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          i_setter = plant%growth%dayleafoff
          call plant%upgm_grow%plant%plantstate%state%replace("dayleafoff", i_setter, success)
          r_setter = plant%growth%tcolddays
          call plant%upgm_grow%plant%plantstate%state%replace("colddays", r_setter, success)
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%mass%standleafdead
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleafdead", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
        case ("WEPSleafon")
          ! environmental variables
          r_setter = daylen(amalat, jd-1, civilrise)
          call plant%env%state%replace("hrlty", r_setter, success)
          r_setter = daylen(amalat, jd, civilrise)
          call plant%env%state%replace("hrlt", r_setter, success)
          i_setter = jd
          call plant%env%state%replace("day_of_year", i_setter, success)
          ! plant state
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          l_setter = plant%growth%shoot_growing
          call plant%upgm_grow%plant%plantstate%state%replace("shoot_growing", l_setter, success)
          i_setter = plant%growth%dayleafon
          call plant%upgm_grow%plant%plantstate%state%replace("dayleafon", i_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)
        case ("WEPSregrowthannual")
          ! plant database values
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%replace("plantpop", r_setter, success)
          ! environmental variables
          r_setter = daylen(amalat, jd-1, civilrise)
          call plant%env%state%replace("hrlty", r_setter, success)
          r_setter = daylen(amalat, jd, civilrise)
          call plant%env%state%replace("hrlt", r_setter, success)
          ! plant state
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%replace("leafareatrend", r_setter, success)
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%mass%standleafdead
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleafdead", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          l_setter = plant%growth%shoot_growing
          call plant%upgm_grow%plant%plantstate%state%replace("shoot_growing", l_setter, success)
          l_setter = plant%growth%growing
          call plant%upgm_grow%plant%plantstate%state%replace("growing", l_setter, success)
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%replace("masshoot", r_setter, success)
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

          i_setter = plant%growth%dayam
          call plant%upgm_grow%plant%plantstate%state%replace("dayam", i_setter, success)
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%replace("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%replace("height", r_setter, success)
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%replace("grainf", r_setter, success)
        case ("WEPSregrowthperen")
          ! plant database values
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%replace("plantpop", r_setter, success)
          ! plant state
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%replace("leafareatrend", r_setter, success)
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%mass%standleafdead
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleafdead", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          l_setter = plant%growth%shoot_growing
          call plant%upgm_grow%plant%plantstate%state%replace("shoot_growing", l_setter, success)
          r_setter = plant%growth%mshoot
          call plant%upgm_grow%plant%plantstate%state%replace("masshoot", r_setter, success)
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

          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%replace("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%replace("grainf", r_setter, success)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%replace("height", r_setter, success)
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
        case ("WEPSregrowthstaged")
          ! plant database values
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%replace("plantpop", r_setter, success)
          ! environmental variables
          r_setter = daylen(amalat, jd-1, civilrise)
          call plant%env%state%replace("hrlty", r_setter, success)
          r_setter = daylen(amalat, jd, civilrise)
          call plant%env%state%replace("hrlt", r_setter, success)
          ! plant state
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          r_setter = plant%growth%leafareatrend
          call plant%upgm_grow%plant%plantstate%state%replace("leafareatrend", r_setter, success)
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%mass%standleafdead
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleafdead", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = plant%growth%stemmasstrend
          call plant%upgm_grow%plant%plantstate%state%replace("stemmasstrend", r_setter, success)
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

          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%replace("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%replace("grainf", r_setter, success)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%replace("height", r_setter, success)
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
        case ("WEPSregrowwood")
          ! plant database values
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%replace("plantpop", r_setter, success)
          ! plant state
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          r_setter = plant%prev%ht
          call plant%upgm_grow%plant%plantstate%state%replace("prevheight", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          l_setter = plant%growth%shoot_growing
          call plant%upgm_grow%plant%plantstate%state%replace("shoot_growing", l_setter, success)

          nelem = size(plant%mass%rootstorez)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
             write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%rootstorez
          call plant%upgm_grow%plant%plantstate%state%replace("mrootstorez", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%mass%standleafdead
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleafdead", r_setter, success)
          r_setter = plant%mass%standstore
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstore", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          r_setter = plant%mass%flatleaf
          call plant%upgm_grow%plant%plantstate%state%replace("mflatleaf", r_setter, success)
          r_setter = plant%mass%flatstore
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstore", r_setter, success)

          nelem = size(plant%mass%stemz)
          allocate(ra_setter(nelem), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          ra_setter = plant%mass%stemz
          call plant%upgm_grow%plant%plantstate%state%replace("mbgstemz", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

          r_setter = plant%geometry%grainf
          call plant%upgm_grow%plant%plantstate%state%replace("grainf", r_setter, success)
          r_setter = plant%geometry%zht
          call plant%upgm_grow%plant%plantstate%state%replace("height", r_setter, success)
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
        case ("WEPSTempStress")
          ! environmental variables
          r_setter = cli_day(pjuld)%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_day(pjuld)%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("WEPStrendleafexternal")
          r_setter = plant%mass%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("mstandleaflive", r_setter, success)
          r_setter = plant%prev%standleaflive
          call plant%upgm_grow%plant%plantstate%state%replace("prevstandleaflive", r_setter, success)
        case ("WEPStrendstemexternal")
          r_setter = plant%mass%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("mstandstem", r_setter, success)
          r_setter = plant%mass%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("mflatstem", r_setter, success)
          r_setter = plant%prev%standstem
          call plant%upgm_grow%plant%plantstate%state%replace("prevstandstem", r_setter, success)
          r_setter = plant%prev%flatstem
          call plant%upgm_grow%plant%plantstate%state%replace("prevflatstem", r_setter, success)
        case ("WEPSwarmdays")
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = cli_day(pjuld)%tdmn
          call plant%env%state%replace("tmin", r_setter, success)
          r_setter = cli_day(pjuld)%tdmx
          call plant%env%state%replace("tmax", r_setter, success)
        case ("WEPSwinterAnnSpring")
          ! environmental variables
          r_setter = daylen(amalat, jd-1, civilrise)
          call plant%env%state%replace("hrlty", r_setter, success)
          r_setter = daylen(amalat, jd, civilrise)
          call plant%env%state%replace("hrlt", r_setter, success)
          ! plant database
          ! tverndel

          ! plant state
          i_setter = jd
          call plant%env%state%replace("day_of_year", i_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          r_setter = plant%growth%zgrowpt
          call plant%upgm_grow%plant%plantstate%state%replace("zgrowpt", r_setter, success)
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)
          r_setter = plant%growth%twarmdays
          call plant%upgm_grow%plant%plantstate%state%replace("warmdays", r_setter, success)
          r_setter = plant%growth%thardnx
          call plant%upgm_grow%plant%plantstate%state%replace("harden_index", r_setter, success)
          r_setter = plant%growth%dayspring
          call plant%upgm_grow%plant%plantstate%state%put("dayspring", r_setter, success)

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

        case ("gdd1_method")
        case ("gddWEPS_method")
        case ("ritchieHardening")
          call plant%upgm_grow%plant%plantstate%state%get("harden_index", r_setter, success)
          plant%growth%thardnx = r_setter
        case ("ritchieVernalization")
          call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
          plant%growth%tchillucum = r_setter
        case ("WEPScolddays")
          call plant%upgm_grow%plant%plantstate%state%get("colddays", r_setter, success)
          plant%growth%tcolddays = r_setter
        case ("WEPSFreezeDamage")
          ! ffa is updated in phases and persistent
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaflive", r_setter, success)
          plant%mass%standleaflive = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleafdead", r_setter, success)
          plant%mass%standleafdead = r_setter
        case ("WEPSleafoff")
          call plant%upgm_grow%plant%plantstate%state%get("dayleafoff", i_setter, success)
          plant%growth%dayleafoff = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayleafon", i_setter, success)
          plant%growth%dayleafon = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaflive", r_setter, success)
          plant%mass%standleaflive = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleafdead", r_setter, success)
          plant%mass%standleafdead = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatleaf", r_setter, success)
          plant%mass%flatleaf = r_setter

          cropres = create_crop_residue(soil%nslay)

          call plant%upgm_grow%plant%plantstate%state%get("res_flatleaf", r_setter, success)
          cropres%flatleaf = r_setter

          ! check for abandoned stems in crop regrowth
          if( cropres%flatleaf .gt. 0.0 ) then
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

            ! reset abandoned leaf amounts to zero
            r_setter = 0.0_dp
            call plant%upgm_grow%plant%plantstate%state%replace("res_flatleaf", r_setter, success)
          end if

          call destroy_crop_residue(cropres)

          call plant%upgm_grow%plant%plantstate%state%get("do_leafoff", l_setter, success)
          plant%growth%do_leafoff = l_setter
        case ("WEPSleafon")
          call plant%upgm_grow%plant%plantstate%state%get("mstandstore", r_setter, success)
          plant%mass%standstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstore", r_setter, success)
          plant%mass%flatstore = r_setter

          cropres = create_crop_residue(soil%nslay)

          call plant%upgm_grow%plant%plantstate%state%get("res_flatstore", r_setter, success)
          cropres%flatstore = r_setter

          ! check for abandoned stems in crop regrowth
          if( cropres%flatstore .gt. 0.0 ) then
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
            call plant%upgm_grow%plant%plantstate%state%replace("res_flatstore", r_setter, success)
          end if

          call destroy_crop_residue(cropres)

          call plant%upgm_grow%plant%plantstate%state%get("dayleafoff", i_setter, success)
          plant%growth%dayleafoff = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayleafon", i_setter, success)
          plant%growth%dayleafon = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("do_leafon", l_setter, success)
          plant%growth%do_leafon = l_setter

        case ("WEPSregrowthannual")
          call plant%upgm_grow%plant%plantstate%state%get("mstandstem", r_setter, success)
          plant%mass%standstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaflive", r_setter, success)
          plant%mass%standleaflive = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleafdead", r_setter, success)
          plant%mass%standleafdead = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandstore", r_setter, success)
          plant%mass%standstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstem", r_setter, success)
          plant%mass%flatstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatleaf", r_setter, success)
          plant%mass%flatleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstore", r_setter, success)
          plant%mass%flatstore = r_setter
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
          call plant%upgm_grow%plant%plantstate%state%get("grainf", r_setter, success)
          plant%geometry%grainf = r_setter

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

          call plant%upgm_grow%plant%plantstate%state%get("masshoot", r_setter, success)
          plant%growth%mshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayam", i_setter, success)
          plant%growth%dayam = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
          plant%growth%tchillucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("regrowth_flg", regrowth_flg, success)
          call plant%upgm_grow%plant%plantstate%state%get("do_regrow", l_setter, success)
          plant%growth%do_regrow = l_setter
        case ("WEPSregrowthperen","WEPSregrowthstaged")
          call plant%upgm_grow%plant%plantstate%state%get("mstandstem", r_setter, success)
          plant%mass%standstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaflive", r_setter, success)
          plant%mass%standleaflive = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleafdead", r_setter, success)
          plant%mass%standleafdead = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandstore", r_setter, success)
          plant%mass%standstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstem", r_setter, success)
          plant%mass%flatstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatleaf", r_setter, success)
          plant%mass%flatleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstore", r_setter, success)
          plant%mass%flatstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mbgstemz", ra_setter, success)
          plant%mass%stemz = ra_setter
          deallocate(ra_setter, stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to deallocate memory for UPGM.'
          end if
          call plant%upgm_grow%plant%plantstate%state%get("grainf", r_setter, success)
          plant%geometry%grainf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("height", r_setter, success)
          plant%geometry%zht = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter

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

          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dayam", i_setter, success)
          plant%growth%dayam = i_setter
          call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
          plant%growth%tchillucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("masshoot", r_setter, success)
          plant%growth%mshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("regrowth_flg", regrowth_flg, success)
          call plant%upgm_grow%plant%plantstate%state%get("do_regrow", l_setter, success)
          plant%growth%do_regrow = l_setter
        case ("WEPSregrowwood")
          call plant%upgm_grow%plant%plantstate%state%get("mstandstem", r_setter, success)
          plant%mass%standstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleaflive", r_setter, success)
          plant%mass%standleaflive = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandleafdead", r_setter, success)
          plant%mass%standleafdead = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mstandstore", r_setter, success)
          plant%mass%standstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstem", r_setter, success)
          plant%mass%flatstem = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatleaf", r_setter, success)
          plant%mass%flatleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mflatstore", r_setter, success)
          plant%mass%flatstore = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mbgstemz", ra_setter, success)
          plant%mass%stemz = ra_setter
          deallocate(ra_setter, stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to deallocate memory for UPGM.'
          end if
          call plant%upgm_grow%plant%plantstate%state%get("grainf", r_setter, success)
          plant%geometry%grainf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("height", r_setter, success)
          plant%geometry%zht = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter

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

          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("masshoot", r_setter, success)
          plant%growth%mshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("regrowth_flg", regrowth_flg, success)
          call plant%upgm_grow%plant%plantstate%state%get("do_regrow", l_setter, success)
          plant%growth%do_regrow = l_setter
        case ("WEPSTempStress")
        case ("WEPStrendleafexternal")
          call plant%upgm_grow%plant%plantstate%state%get("leafareatrend", r_setter, success)
          plant%growth%leafareatrend = r_setter
        case ("WEPStrendstemexternal")
          call plant%upgm_grow%plant%plantstate%state%get("stemmasstrend", r_setter, success)
          plant%growth%stemmasstrend = r_setter
        case ("WEPSwarmdays")
          call plant%upgm_grow%plant%plantstate%state%get("warmdays", r_setter, success)
          plant%growth%twarmdays = r_setter
        case ("WEPSwinterAnnSpring")
          call plant%upgm_grow%plant%plantstate%state%get("dayspring", r_setter, success)
          plant%growth%dayspring = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("spring_flg", spring_flg, success)
          ! do_spring

        end select

        plant%upgm_grow%plant%processCurrent%ptr => plant%upgm_grow%plant%processCurrent%ptr%processNext
      end do

      !if the next stage is ready, check the specific stage value.
      call plant%upgm_grow%plant%plantstate%state%get("nextstage", nextstage, success)

      if( (nextstage == 1) .and. success) then

        call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%get("stagegdd", stagegdd, success)

        success = .false.
        write(*,'(a,1x,i0,2x,a,F7.1,2a)') 'Day of Year', jd, 'Degree Days: ', stagegdd, ' Phase Completed: ', &
                   trim(plant%upgm_grow%plant%phaseCurrent%ptr%phaseLabel)

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

!      if( associated(plant%upgm_grow%plant%phaseCurrent%ptr) ) then 
!          ! write info for start of phase
!          write(*,*) 'Phase, stagegdd: ', trim(plant%upgm_grow%plant%phaseCurrent%ptr%phaseLabel), stagegdd
!       end if

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
            plant%growth%growing = .true.
          end if
        else
          ! get stage state at beginning of day
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%get("phase_rel_gdd", hui, success)
          if( hui .lt. 1.0_dp ) then
            ! still actively growing
            plant%growth%growing = .true.
          end if
        end if

        ! update daily inputs
        select case(plant%upgm_grow%plant%phaseCurrent%ptr%phaseName)

        case ("PhenologyMMS_Basephenol")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)

        case ("PhenologyMMS_Fallphenol")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)
          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)

        case ("PhenologyMMS_Germination")

          allocate(ra_setter(soil%nslay), stat = alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to allocate memory for UPGM.'
          end if
          do idx=1,soil%nslay
            ra_setter(idx) = soil%theta(idx)
          end do
          call plant%env%state%replace("swc", ra_setter, success)
          deallocate(ra_setter, stat = alloc_stat)

        case ("PhenologyMMS_ShootGRG")
          ! set below
          ! "daygdd"
          r_setter = 1.0_dp - plant%growth%fwsf
          call plant%upgm_grow%plant%plantstate%state%replace("stress", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)

        case ("PhenologyMMS_Springphenol")
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

        case ("WEPS_DeciduousWood")

          ! plant database values
          ! thum
          ! alf
          ! blf
          ! clf
          ! dlf
          ! arp
          ! brp
          ! crp
          ! drp
          ! aht
          ! bht
          ! zmxc
          ! zmrt
          ! ehu0

          ! plant state
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          r_setter = plant%growth%trthucum
          call plant%upgm_grow%plant%plantstate%state%replace("trthucum", r_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          l_setter = plant%growth%do_leafon
          call plant%upgm_grow%plant%plantstate%state%replace("do_leafon", l_setter, success)
          l_setter = plant%growth%do_leafoff
          call plant%upgm_grow%plant%plantstate%state%replace("do_leafoff", l_setter, success)

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

          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_leaf_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_leaf_end", r_setter, success)
          r_setter = plant%growth%thu_leaf_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_leaf_beg", r_setter, success)

        case ("WEPS_EvergreenWood")

          ! plant database values
          ! thum
          ! alf
          ! blf
          ! clf
          ! dlf
          ! arp
          ! brp
          ! crp
          ! drp
          ! aht
          ! bht
          ! zmxc
          ! zmrt
          ! ehu0
          ! tverndel

          ! plant state
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          r_setter = plant%growth%trthucum
          call plant%upgm_grow%plant%plantstate%state%replace("trthucum", r_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          l_setter = plant%growth%do_leafon
          call plant%upgm_grow%plant%plantstate%state%replace("do_leafon", l_setter, success)
          l_setter = plant%growth%do_leafoff
          call plant%upgm_grow%plant%plantstate%state%replace("do_leafoff", l_setter, success)

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

          l_setter = plant%growth%shoot_growing
          call plant%upgm_grow%plant%plantstate%state%replace("shoot_growing", l_setter, success)

          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_leaf_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_leaf_end", r_setter, success)
          r_setter = plant%growth%thu_leaf_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_leaf_beg", r_setter, success)

        case ("WEPS_ShootGrow")

          ! calculate day length
          ! r_setter = daylen(amalat, jd, civilrise)
          ! call plant%env%state%replace("hrlt", r_setter, success)

          ! plant database values
          ! huie
          ! thum
          ! alf
          ! blf
          ! clf
          ! dlf
          ! arp
          ! brp
          ! crp
          ! drp
          ! aht
          ! bht
          ! zmxc
          ! zmrt
          ! ehu0
          r_setter = plant%geometry%dpop
          call plant%upgm_grow%plant%plantstate%pars%replace("plantpop", r_setter, success)
          !r_setter = plant%database%shoot
          !call plant%upgm_grow%plant%plantstate%pars%replace("regrmshoot", r_setter, success)
          !r_setter = plant%database%dmaxshoot
          !call plant%upgm_grow%plant%plantstate%pars%replace("dmaxshoot", r_setter, success)
          !r_setter = plant%database%tverndel
          !call plant%upgm_grow%plant%plantstate%pars%replace("tverndel", r_setter, success)

          ! plant state
          r_setter = plant%geometry%dstm
          call plant%upgm_grow%plant%plantstate%state%replace("dstm", r_setter, success)
          r_setter = plant%growth%trthucum
          call plant%upgm_grow%plant%plantstate%state%replace("trthucum", r_setter, success)
          r_setter = plant%growth%thu_shoot_beg
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_beg", r_setter, success)
          r_setter = plant%growth%thu_shoot_end
          call plant%upgm_grow%plant%plantstate%state%replace("thu_shoot_end", r_setter, success)
          l_setter = plant%growth%can_regrow
          call plant%upgm_grow%plant%plantstate%state%replace("can_regrow", l_setter, success)
          ! do_spring
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
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'Unable to deallocate memory for UPGM.'
          end if

          r_setter = plant%growth%tchillucum
          call plant%upgm_grow%plant%plantstate%state%replace("chill_unit_cum", r_setter, success)


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

        case ("PhenologyMMS_Basephenol")
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          ffw = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          shoot_hui = 1.0_dp
          shoot_huiy = 1.0_dp
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("PhenologyMMS_Fallphenol")
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          ffw = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          shoot_hui = 1.0_dp
          shoot_huiy = 1.0_dp
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("PhenologyMMS_Germination")
          ffa = 1.0_dp
          ffw = 1.0_dp
          ffr = 1.0_dp
          gif = 0.0_dp
          shoot_hui = 0.0_dp
          shoot_huiy = 0.0_dp
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
          p_rw = 0.0_dp
          p_st = 0.0_dp
          p_lf = 0.0_dp
          p_rp = 0.0_dp
          pdht = 0.0_dp
          pdrd = 0.0_dp
          hu_delay = 0.0_dp

        case ("PhenologyMMS_ShootGRG")
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          ffw = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("PhenologyMMS_Springphenol")
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
          ffw = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)
          call plant%upgm_grow%plant%plantstate%state%get("spring_flg", spring_flg, success)

        case ("WEPS_DeciduousWood")
          call plant%upgm_grow%plant%plantstate%state%get("colddays", r_setter, success)
          plant%growth%tcolddays = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("lastday", l_setter, success)
          plant%growth%lastday = l_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("warmdays", r_setter, success)
          plant%growth%twarmdays = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotleaf", r_setter, success)
          plant%growth%mtotleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_leaf_beg", r_setter, success)
          plant%growth%thu_leaf_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_leaf_end", r_setter, success)
          plant%growth%thu_leaf_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("trthucum", r_setter, success)
          plant%growth%trthucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("leaf_hui", leaf_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("leaf_huiy", leaf_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          hu_delay = 1.0_dp
          call plant%upgm_grow%plant%phaseCurrent%ptr%phaseState%get("phase_rel_gdd", hui, success)
          ! plant state may have changed at leafon
          if( hui .lt. 1.0_dp ) then
            ! now actively growing
            plant%growth%growing = .true.
          end if

        case ("WEPS_EvergreenWood")
          call plant%upgm_grow%plant%plantstate%state%get("chill_unit_cum", r_setter, success)
          plant%growth%tchillucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("colddays", r_setter, success)
          plant%growth%tcolddays = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("warmdays", r_setter, success)
          plant%growth%twarmdays = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotleaf", r_setter, success)
          plant%growth%mtotleaf = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_leaf_beg", r_setter, success)
          plant%growth%thu_leaf_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_leaf_end", r_setter, success)
          plant%growth%thu_leaf_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("trthucum", r_setter, success)
          plant%growth%trthucum = r_setter

          call plant%upgm_grow%plant%plantstate%state%get("lastday", l_setter, success)
          plant%growth%lastday = l_setter
          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("leaf_hui", leaf_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("leaf_huiy", leaf_huiy, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

        case ("WEPS_ShootGrow")

          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_beg", r_setter, success)
          plant%growth%thu_shoot_beg = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("thu_shoot_end", r_setter, success)
          plant%growth%thu_shoot_end = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("mtotshoot", r_setter, success)
          plant%growth%mtotshoot = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("dstm", r_setter, success)
          plant%geometry%dstm = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("harden_index", r_setter, success)
          plant%growth%thardnx = r_setter
          call plant%upgm_grow%plant%plantstate%pars%get("leaf2stor", r_setter, success)
          plant%database%fleaf2stor = r_setter
          call plant%upgm_grow%plant%plantstate%pars%get("stem2stor", r_setter, success)
          plant%database%fstem2stor = r_setter
          call plant%upgm_grow%plant%plantstate%pars%get("stor2stor", r_setter, success)
          plant%database%fstor2stor = r_setter

          call plant%upgm_grow%plant%plantstate%state%get("shoot_hui", shoot_hui, success)
          call plant%upgm_grow%plant%plantstate%state%get("shoot_huiy", shoot_huiy, success)
          leaf_hui = 1.0_dp
          leaf_huiy = 1.0_dp
          call plant%upgm_grow%plant%plantstate%state%get("lastday", l_setter, success)
          plant%growth%lastday = l_setter
          call plant%upgm_grow%plant%plantstate%state%get("trthucum", r_setter, success)
          plant%growth%trthucum = r_setter
          call plant%upgm_grow%plant%plantstate%state%get("ffa", ffa, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffw", ffw, success)
          call plant%upgm_grow%plant%plantstate%state%get("ffr", ffr, success)
          call plant%upgm_grow%plant%plantstate%state%get("gif", gif, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rw", p_rw, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_st", p_st, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_lf", p_lf, success)
          call plant%upgm_grow%plant%plantstate%state%get("p_rp", p_rp, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdht", pdht, success)
          call plant%upgm_grow%plant%plantstate%state%get("pdrd", pdrd, success)
          call plant%upgm_grow%plant%plantstate%state%get("hu_delay", hu_delay, success)

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
          ! get stage state at end of day
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
            write(luocrop(isr),'(a)')   ! crop.out
            write(luocrop(isr),'(a)')   ! crop.out
            write(luoshoot(isr),'(a)')   ! shoot.out
            write(luoshoot(isr),'(a)')   ! shoot.out
          end if
          ! turn off initialization flag
          plant%growth%am0cif = .false.
        end if

        if( plant%growth%growing ) then
          ! crop growth not yet complete
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow

          if( shoot_huiy .lt. 1.0_dp ) then

            if( shoot_hui .ge. 0.0_dp ) then

              ! set shoot growth flag
              plant%growth%shoot_growing = .true.

              ! daily shoot growth
              call shoot_grow( soil, plant, shoot_hui, shoot_huiy, s_root_sum, f_root_sum, tot_mass_req, &
                end_shoot_mass, end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass, &
                end_stem_mass, end_stem_area, end_shoot_len )

              if (am0cfl(isr) .ge. 1) then
                ! note: dayap has not yet been updated. shoot.out write moved to before update to allow blank line to be printed
                ! before shoot_growing set to false
                write(luoshoot(isr), &
                  "(1x,i5,1x,i3,1x,i4,1x,i4,1x,f6.3,2(1x,f10.4),2(1x,f12.4),4(1x,f12.4),4(1x,f12.4),(1x,f8.4),(1x,f8.3),1x,a)") &
                  get_psim_daysim(isr), jd, get_psim_year(isr), plant%growth%dayap+1, shoot_hui, &
                  s_root_sum, f_root_sum, tot_mass_req, end_shoot_mass, &
                  end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass, &
                  end_stem_mass, end_stem_area, end_shoot_len, plant%geometry%zshoot, &
                  plant%growth%mshoot, plant%geometry%dstm, trim(plant%bname)
              end if

            end if

            if( shoot_hui .ge. 1.0_dp ) then
              ! shoot growth completed on this day

              ! set flag indicating regrowth capability
              plant%growth%shoot_growing = .false.

              ! move growing point to regrowth depth after shoot growth complete
              ! remember, a negative number is above ground
              plant%growth%zgrowpt = ( - plant%database%zloc_regrow )

              if (am0cfl(isr) .ge. 1) then
                  ! single blank line to separate shoot growth periods
                  write(luoshoot(isr),'(a)')  ! shoot.out
              end if
              ! last day of shoot grow, set shoot_huiy so shoot grow stops after shoot grow phase is completed.
              shoot_huiy = 1.0_dp
              call plant%upgm_grow%plant%plantstate%state%replace("shoot_huiy", shoot_huiy, success)

            end if

          end if

          if( leaf_huiy .lt. 1.0_dp ) then

            if( leaf_hui .gt. 0.0_dp ) then

              ! daily leaf emergence
              call leaf_emerge( plant, leaf_hui, leaf_huiy )

            end if

          end if

          ! used in growth
          call plant%upgm_grow%plant%plantstate%state%get("tstress", tstress, success)
          if( .not. success ) then
            tstress = 1.0_dp
          end if
          call plant%upgm_grow%plant%plantstate%state%get("lost_mass", lost_mass, success)
          if( .not. success ) then
            lost_mass = 0.0_dp
          end if
          call plant%upgm_grow%plant%plantstate%state%get("regrow_release", regrow_release, success)
          if( .not. success ) then
            regrow_release = 1.0_dp
          end if

          call growth( soil, plant, &
                   dble(cli_day(pjuld)%eirr), &
                   eff_lai, trad_lai, tstress, p_rw, p_st, p_lf, p_rp, &
                   pdht, pdrd, &
                   ffa, ffw, ffr, gif, par, apar, pddm, &
                   stem_propor, pdiam, parea, &
                   temp_sai, temp_stmrep, lost_mass, regrow_release )

          if(    (plant%database%fleaf2stor .gt. 0.0_dp) &
            .or. (plant%database%fstem2stor .gt. 0.0_dp) &
            .or. (plant%database%fstor2stor .gt. 0.0_dp) ) then
            plant%growth%can_regrow = .true.
          else
            plant%growth%can_regrow = .false.
          end if

          ! set trend direction for living leaf area
          trend = plant%mass%standleaflive - plant%prev%standleaflive
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
          plant%prev%standleaflive = plant%mass%standleaflive
          plant%prev%standleafdead = plant%mass%standleafdead
          plant%prev%standstore = plant%mass%standstore
          plant%prev%flatstem = plant%mass%flatstem
          plant%prev%flatleaf = plant%mass%flatleaf
          plant%prev%flatstore = plant%mass%flatstore
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
          plant%prev%dayleafon = plant%growth%dayleafon

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

          if( plant%growth%growing) then
            ! reporting only variables
            call plant%upgm_grow%plant%plantstate%state%get("frst", frst, success)
            if( .not. success ) then
              frst = 0.0_dp
            end if

            regrowth_or_spring_flg = max(regrowth_flg, spring_flg)

            if( (plant%mass%standleaflive + plant%mass%standleafdead) .gt. 0.0_dp ) then
              temp_fliveleaf = plant%mass%standleaflive / (plant%mass%standleaflive + plant%mass%standleafdead)
            else
              temp_fliveleaf = 1.0_dp
            end if

            write(luocrop(isr), "(1x,i6,1x,i3,1x,i4,1x,i5,1x,f6.3,12(1x,f7.4),1x,f7.2, &
     &         3(1x,f7.4),8(1x,f6.3),1x,e12.3, 11(1x,f6.3),2(1x,f8.5),1x,i2,1x,f6.3,1x,a,1x,a)") &
            get_psim_daysim(isr), jd, get_psim_year(isr), plant%growth%dayap, &
            hui, &
            plant%mass%standstem, plant%mass%standleaflive + plant%mass%standleafdead, plant%mass%standstore, &
            plant%mass%flatstem, plant%mass%flatleaf, plant%mass%flatstore, &
            temp_store, temp_fiber, temp_stem, &
            plant%mass%standleaflive + plant%mass%standleafdead + plant%mass%flatleaf, &
            plant%mass%standstem + plant%mass%flatstem + temp_stem, &
            plant%geometry%zht, plant%geometry%dstm, trad_lai, eff_lai, plant%geometry%zrtd, &
            plant%geometry%grainf, tstress, plant%growth%fwsf, frst, ffa, ffw, &
            par, apar, pddm, p_rw, p_st, p_lf, p_rp, &
            stem_propor, pdiam, parea, pdiam/plant%database%diammax, &
            parea*plant%geometry%dpop, hu_delay, plant%growth%thardnx, temp_sai,  &
            temp_stmrep, regrowth_or_spring_flg, temp_fliveleaf, &
            trim(plant%bname) !, trim(PhaseLabel)
          end if

        end if

        if( plant%growth%lastday ) then
          ! heat units completed, crop leaf mass is non transpiring
          plant%mass%standleafdead = plant%mass%standleafdead + plant%mass%standleaflive
          plant%mass%standleaflive = 0.0_dp
          plant%growth%lastday = .false.
          l_setter = plant%growth%lastday
          call plant%upgm_grow%plant%plantstate%state%replace("lastday", l_setter, success)

          ! at full maturity plant is dormant
          plant%growth%growing = .false.
          l_setter = plant%growth%growing
          call plant%upgm_grow%plant%plantstate%state%replace("growing", l_setter, success)

          !if( .not. plant%growth%can_regrow ) then
            ! fully mature, so whole plant is dead (no regrowth possible)
            ! WEPS crop growth does not set this false, so do not do here
            ! plant%growth%living = .false.

            ! setting this to false without moving to residue will cause update to destroy plant and lose all biomass

            ! future, should entire plant biomass be moved to residue here
            ! so it can decompose from moment of maturity?
            ! P31 kill_plant would be cleanest way to move residue here. Also sets living to false.
          !end if

        end if

      end if

    end subroutine run_UPGM

end module WEPS_UPGM_mod
