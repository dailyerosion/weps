module WEPSCrop_mod

    ! routines from WEPS supporting crop growth the WEPS way

    use constants, only: dp, int32, u_pi, u_mgtokg, u_hatom2, u_mmtom
    implicit none

  contains

    subroutine shoot_grow( soil, plant, &
                 shoot_hui, shoot_huiy, s_root_sum, f_root_sum, tot_mass_req, &
                 end_shoot_mass, end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass, &
                 end_stem_mass, end_stem_area, end_shoot_len )

      ! + + + KEYWORDS + + +
      ! shoot growth

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer
      use weps_main_mod, only: cook_yield

      ! + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older
      real(dp), intent(in) :: shoot_hui    ! today fraction of heat unit shoot growth index accumulation
      real(dp), intent(in) :: shoot_huiy   ! previous day fraction of heat unit shoot growth index accumulation
      real(dp), intent(out) :: s_root_sum   ! storage root mass sum (total in all layers) (kg/m^2)
      real(dp), intent(out) :: f_root_sum   ! fibrous root mass sum (total in all layers) (kg/m^2)
      real(dp), intent(out) :: tot_mass_req ! mass required from root mass for one shoot (mg/shoot)
      real(dp), intent(out) :: end_shoot_mass ! total shoot mass at end of shoot growth period (mg/shoot)
      real(dp), intent(out) :: end_root_mass ! total root mass at end of shoot growth period (mg/shoot)
      real(dp), intent(out) :: d_root_mass  ! mass increment added to roots for the present day (mg/shoot)
      real(dp), intent(out) :: d_shoot_mass ! mass increment added to shoot for the present day (mg/shoot)
      real(dp), intent(out) :: d_s_root_mass ! mass increment removed from storage roots for the present day (mg/shoot)
      real(dp), intent(out) :: end_stem_mass ! total stem mass at end of shoot growth period (mg/shoot)
      real(dp), intent(out) :: end_stem_area ! total stem area at end of shoot growth period (m^2/shoot)
      real(dp), intent(out) :: end_shoot_len ! total shoot length at end of shoot growth period (m)

      ! + + + LOCAL VARIABLES + + +
      integer(int32) :: bnslay ! number of soil layers
      integer(int32) :: lay    ! index into soil layers for looping
      real(dp) :: fexp_hui     ! exponential function evaluated at todays shoot heat unit index
      real(dp) :: fexp_huiy    ! exponential function evaluated at yesterdays shoot heat unit index
      real(dp) :: d_stem_mass  ! mass increment added to stem for the present day (mg/shoot)
      real(dp) :: d_leaf_mass  ! mass increment added to leaf for the present day (mg/shoot)
      real(dp) :: red_mass_rat ! ratio of reduced mass available for stem growth to expected mass available
      real(dp) :: yesterday_len ! length of shoot yesterday (m)
      real(dp) :: stem_propor  ! ratio of standing stems mass to flat stem mass
      real(dp) :: ag_stem      ! above ground stem mass (mg/shoot)
      real(dp) :: bg_stem      ! below ground stem mass (mg/shoot)
      real(dp) :: flat_stem    ! flat stem mass (mg/shoot)
      real(dp) :: stand_stem   ! standing stem mass (mg/shoot)
      real(dp) :: avail_mass   ! storage root mass sum in (mg/shoot)
      real(dp) :: lost_mass    ! passed into cook yield, is simply set to zero
      real(dp) :: dlfwt        ! increment in leaf dry weight (kg/m^2)
      real(dp) :: dstwt        ! increment in dry weight of stem (kg/m^2)
      real(dp) :: drpwt        ! increment in reproductive mass (kg/m^2)
      real(dp) :: drswt        ! biomass diverted from partitioning to root storage

      ! + + + LOCAL PARAMETERS + + +
      real(dp), parameter :: shoot_exp = 2.0_dp ! exponent for shape of exponential function
                                                ! small numbers  go toward straight line
                                                ! large numbers delay development to end of period
      real(dp), parameter :: be_stor = 0.7_dp   ! conversion efficiency of biomass from storage to growth
      real(dp), parameter :: rootf = 0.4_dp     ! fraction of biomass allocated to roots when growing from seed

      ! + + + END OF SPECIFICATIONS + + +

      bnslay = size(soil%aszlyd)

      ! total shoot mass is grown at an exponential rate
      fexp_hui = (exp(shoot_exp*shoot_hui)-1.0_dp) / (exp(shoot_exp)-1.0_dp)
      fexp_huiy = (exp(shoot_exp*shoot_huiy)-1.0_dp) / (exp(shoot_exp)-1.0_dp)

      ! sum present storage and fibrous root mass (kg/m^2)
      s_root_sum = 0.0_dp
      f_root_sum = 0.0_dp
      do lay = 1, bnslay
          s_root_sum = s_root_sum + plant%mass%rootstorez(lay)
          f_root_sum = f_root_sum + plant%mass%rootfiberz(lay)
      end do

      ! calculate storage mass required to grow a single shoot
      ! units: kg/m^2 / ( shoots/m^2 * kg/mg ) = mg/shoot
      tot_mass_req = plant%growth%mtotshoot / (plant%geometry%dstm * u_mgtokg)

      ! divide ending mass between shoot and root
      if( f_root_sum .le. plant%growth%mshoot ) then   ! this works as long as rootf <= 0.5
          !roots develop along with shoot from same mass
          end_shoot_mass = tot_mass_req * be_stor * (1.0_dp-rootf)
          end_root_mass = tot_mass_req * be_stor * rootf
      else
          !roots remain static, while shoot uses all mass from storage
          end_shoot_mass = tot_mass_req * be_stor
          end_root_mass = 0.0_dp
      end if

      ! this days incremental shoot mass for a single shoot (mg/shoot)
      d_shoot_mass = end_shoot_mass * (fexp_hui - fexp_huiy)
      d_root_mass = end_root_mass * (fexp_hui - fexp_huiy)

      ! this days mass removed from the storage root (mg/shoot)
      d_s_root_mass = (d_shoot_mass + d_root_mass) / be_stor

      ! check that sufficient storage root mass is available
      ! units: mg/shoot = kg/m^2 / (kg/mg * shoot/m^2)
      avail_mass = s_root_sum  / (plant%geometry%dstm * u_mgtokg)
      if( (d_s_root_mass .gt. avail_mass) &
         .and. (d_s_root_mass .gt. 0.0_dp) ) then
          ! reduce removal to match available storage
          red_mass_rat = avail_mass / d_s_root_mass
          ! adjust root increment to match
          d_root_mass = d_root_mass * red_mass_rat
          ! adjust shoot increment to match
          d_shoot_mass = d_shoot_mass * red_mass_rat
          ! adjust removal amount to match exactly
          d_s_root_mass =  d_s_root_mass * red_mass_rat
      end if

      ! if no additional mass, no need to go further
      if( d_shoot_mass .le. 0.0_dp) return
      !! +++++++++++++ RETURN FROM HERE IF ZERO +++++++++++++++++

      ! find stem mass when shoot completely developed
      ! (mg tot/shoot) / ((kg leaf/kg stem)+1) = mg stem/shoot
      end_stem_mass = end_shoot_mass / (plant%database%fleafstem+1.0)

      ! length of shoot when completely developed, use the mass of stem per plant
      ! (mg stem/shoot)*(kg/mg)*(#stem/m^2)/(#plants/m^2) = kg stem/plant
      ! inserted into stem area index equation to get stem area in m^2 per plant
      ! and then converted back to m^2 per stem
      end_stem_area = plant%database%ssa &
                    * (end_stem_mass*u_mgtokg*plant%geometry%dstm/plant%geometry%dpop)**plant%database%ssb &
                    * plant%geometry%dpop / plant%geometry%dstm
      ! use silhouette area and stem diameter to length ratio to find length
      ! since silhouette area = length * diameter
      ! *** the square root is included since straight ratios do not really
      ! fit, but grossly underestimate the shoot length. This is possibly
      ! due to the difference between mature stem density vs. new growth
      ! with new stems being much higher in water content ***
      ! note: diameter to length ratio is when shoot has fully grown from root reserves
      ! during it's extension, it is assumed to grow at full diameter
      end_shoot_len = sqrt( end_stem_area / plant%database%fshoot )

      ! screen shoot emergence parameters for validity
      if( end_shoot_len .le. plant%growth%zgrowpt ) then
             write(UNIT=6,FMT="(1x,3(a),f7.4,a,f7.4,a)") &
                 'Warning: ', &
                 ' growth halted. Shoot extension: ', end_shoot_len, &
                 ' Depth in soil: ', plant%growth%zgrowpt, ' meters.'
      end if

      ! today and yesterday shoot length and stem and leaf mass increments
      ! length increase scaled by mass increase
      ! stem and leaf mass allocated proportionally (prevents premature emergence)
      plant%geometry%zshoot = end_shoot_len &
               * ((plant%growth%mshoot /(u_mgtokg * plant%geometry%dstm))+d_shoot_mass) &
               / end_shoot_mass

      yesterday_len = end_shoot_len * (plant%growth%mshoot /(u_mgtokg * plant%geometry%dstm)) &
                    / end_shoot_mass
      d_stem_mass = d_shoot_mass  / (plant%database%fleafstem+1.0_dp)
      d_leaf_mass = d_shoot_mass * plant%database%fleafstem / (plant%database%fleafstem+1.0_dp)

      ! divide above ground and below ground mass
      if( plant%geometry%zshoot .le. plant%growth%zgrowpt ) then
          ! all shoot growth for today below ground
          ag_stem = 0.0_dp
          bg_stem = d_stem_mass
      else if( yesterday_len .ge. plant%growth%zgrowpt ) then
          ! all shoot growth for today above ground
          ag_stem = d_stem_mass
          bg_stem = 0.0_dp
      else
          ! shoot breaks ground surface today
          ag_stem = d_stem_mass &
                  * (plant%geometry%zshoot-plant%growth%zgrowpt) / (plant%geometry%zshoot-yesterday_len)
          bg_stem = d_stem_mass * (plant%growth%zgrowpt - yesterday_len) &
                  / (plant%geometry%zshoot - yesterday_len)
      end if

      !convert from mg/shoot to kg/m^2
      dlfwt = d_leaf_mass * u_mgtokg * plant%geometry%dstm
      dstwt = ag_stem * u_mgtokg * plant%geometry%dstm
      drpwt = 0.0_dp
      drswt = 0.0_dp
      lost_mass = 0.0_dp

      ! yield residue relationship adjustment
      ! since this is in shoot_grow, do not allow this with plant%geometry%hyfg=5 since
      ! it is illogical to store yield into the storage root while at the 
      ! same time using the storage root to grow the shoot
      if(     (cook_yield .eq. 1) &
        .and. (plant%database%yld_coef .gt. 1.0_dp) .and. (plant%database%resid_int .ge. 0.0_dp) &
        .and. ( (plant%geometry%hyfg.eq.0).or.(plant%geometry%hyfg.eq.1) ) ) then
          call cookyield(plant%geometry%hyfg, bnslay, dlfwt, dstwt, drpwt, drswt, &
                         dble(plant%mass%standstem), dble(plant%mass%standleaf), dble(plant%mass%standstore), &
                         dble(plant%mass%flatstem), dble(plant%mass%flatleaf), dble(plant%mass%flatstore), &
                         dble(plant%mass%rootstorez), lost_mass, &
                         dble(plant%database%yld_coef), dble(plant%database%resid_int), dble(plant%database%grf) )
      end if

      ! divide above ground stem between standing and flat
      stem_propor = min(1.0_dp, plant%database%zmxc/plant%database%diammax) 
      stand_stem = dstwt * stem_propor
      flat_stem = dstwt * (1.0_dp - stem_propor)

      ! distribute mass into mass pools
      ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
      ! shoot mass pool (breakout pool, not true accumulator)
      plant%growth%mshoot = plant%growth%mshoot + d_shoot_mass * u_mgtokg * plant%geometry%dstm

      ! reproductive mass is added to above ground pools
      plant%mass%standstore = plant%mass%standstore + drpwt * stem_propor
      plant%mass%flatstore = plant%mass%flatstore + drpwt * (1.0_dp - stem_propor)

      ! leaf mass is added even if below ground
      ! leaf has very low mass (small effect) and some light interaction
      ! does occur as emergence approaches (if problem can be changed easily)

      if( (plant%mass%standleaf + dlfwt) .gt. 0.0_dp ) then
          ! added leaf mass adjusts live leaf fraction, otherwise no change
          plant%growth%fliveleaf = (plant%growth%fliveleaf*plant%mass%standleaf+dlfwt) &
                  / (plant%mass%standleaf + dlfwt)
      end if
      plant%mass%standleaf = plant%mass%standleaf + dlfwt

      ! above ground stems
      plant%mass%standstem = plant%mass%standstem + stand_stem
      plant%mass%flatstem = plant%mass%flatstem + flat_stem

      ! below ground stems
      do lay = 1, bnslay
          if( lay .eq. 1 ) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              plant%mass%stemz(lay) = plant%mass%stemz(lay) + bg_stem & 
              * u_mgtokg * plant%geometry%dstm * frac_lay( dble(plant%growth%zgrowpt)-dble(plant%geometry%zshoot), &
              plant%growth%zgrowpt-yesterday_len, 0.0_dp, soil%aszlyd(lay) * u_mmtom )
          else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              plant%mass%stemz(lay) = plant%mass%stemz(lay) + bg_stem &
              * u_mgtokg * plant%geometry%dstm * frac_lay( dble(plant%growth%zgrowpt)-dble(plant%geometry%zshoot), &
              plant%growth%zgrowpt-yesterday_len, soil%aszlyd(lay-1) * u_mmtom, &
              soil%aszlyd(lay) * u_mmtom )
          end if
      end do

      ! check plant height, the the case of regrowth from stem
      ! do not allow reaching max height in single day
      ! use stem proportion to account for flat stems
      plant%geometry%zht = min( 0.5_dp * (plant%database%zmxc + plant%geometry%zht), max( plant%geometry%zht, max( 0.0_dp, &
                  (plant%geometry%zshoot-plant%growth%zgrowpt)*stem_propor ) ) )

      ! check root depth
      plant%geometry%zrtd = max( plant%geometry%zrtd, (plant%growth%zgrowpt + plant%geometry%zshoot) )

      ! add to fibrous root mass, remove from storage root mass
      do lay = 1, bnslay
          if( lay .eq. 1 ) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              plant%mass%rootfiberz(lay) = plant%mass%rootfiberz(lay) + d_root_mass &
              * u_mgtokg * plant%geometry%dstm * frac_lay( dble(plant%growth%zgrowpt), dble(plant%geometry%zrtd), &
              0.0_dp, soil%aszlyd(lay) * u_mmtom )
          else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              plant%mass%rootfiberz(lay) = plant%mass%rootfiberz(lay) + d_root_mass &
              * u_mgtokg * plant%geometry%dstm * frac_lay( dble(plant%growth%zgrowpt), dble(plant%geometry%zrtd), &
              soil%aszlyd(lay-1) * u_mmtom, soil%aszlyd(lay) * u_mmtom )
          end if
          ! check for sufficient storage in layer to meet demand
          if(       (plant%mass%rootstorez(lay) .gt. 0.0_dp) &
              .and. (d_s_root_mass .gt. 0.0_dp) ) then
              ! demand and storage to meet it
              ! units: mg/shoot * kg/mg * shoots/m^2 = kg/m^2
              plant%mass%rootstorez(lay) = plant%mass%rootstorez(lay) - d_s_root_mass &
                                 * u_mgtokg * plant%geometry%dstm
              if( plant%mass%rootstorez(lay) .lt. 0.0_dp ) then
                  ! not enough mass in this layer to meet need. Carry over
                  ! to next layer in d_s_root_mass
                  d_s_root_mass = - plant%mass%rootstorez(lay) / (u_mgtokg*plant%geometry%dstm)
                  plant%mass%rootstorez(lay) = 0.0_dp
              else
                  ! no more mass needed
                  d_s_root_mass = 0.0_dp
             end if
          end if
      end do

      ! check if shoot sucessfully reached above ground
      if( (d_s_root_mass .gt. 0.0_dp) .and. (plant%geometry%zht .le. 0.0_dp) ) then
          write(0,*) "shoot_grow: not enough root storage to grow shoot"
          call exit(1)
      end if

      return
    end subroutine shoot_grow

    real(dp) function frac_lay( top_loc, bot_loc, top_lay, bot_lay )

      ! this function determines the fraction of a location which
      ! is contained in a layer. It could also be viewed as the
      ! fraction of "overlap" of the linear location with a layer
      ! depth slice. It was written assuming that top values are 
      ! less than bottom values

      real(dp) top_loc, bot_loc, top_lay, bot_lay

      if( top_lay .le. top_loc .and. bot_lay .gt. top_loc ) then
          ! top location is in layer
          if( bot_lay .ge. bot_loc ) then
              ! bottom location is also in layer
              frac_lay = 1.0_dp
          else
              ! bottom location is below layer, proportion
              frac_lay = (bot_lay - top_loc)/(bot_loc - top_loc)
          end if
      else if( top_lay .lt. bot_loc .and. bot_lay .ge. bot_loc ) then
          ! bottom location is in layer
          ! if we are here, top location is not in layer so proportion
          frac_lay = (bot_loc - top_lay)/(bot_loc - top_loc)
      else if( top_lay .gt. top_loc .and. bot_lay .lt. bot_loc ) then
          ! location completely spans layer
          frac_lay = (bot_lay - top_lay)/(bot_loc - top_loc)
      else
          ! location is not in the layer at all
          frac_lay = 0.0_dp
      end if

      return
    end function frac_lay

    subroutine growth(soil, plant, &
                      eirr, &
                      eff_lai, trad_lai, ts, p_rw, p_st, p_lf, p_rp, &
                      pdht, pdrd, &
                      ffa, ffw, ffr, gif, par, apar, pddm, &
                      stem_propor, pdiam, parea, &
                      temp_sai, temp_stmrep, lost_mass )

      ! Author : Amare Retta
      ! + + + PURPOSE + + +
      ! This subroutine calculates plant height, biomass partitioning,
      ! rootmass distribution, rooting depth.

      ! + + + KEYWORDS + + +
      ! biomass

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: plant_pointer
      use WEPSCrop_util_mod, only: freeze_damage, shootnum
      use weps_main_mod, only: cook_yield

      integer(int32), parameter :: growth_stress = 3
      real(dp), parameter :: water_stress_max = 0.0_dp
      integer(int32), parameter :: winter_ann_root = 1

      ! + + + ARGUMENT DECLARATIONS + + +
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(plant_pointer), pointer :: plant     ! pointer to youngest plant data, which chains to older
      real(dp) :: eirr         ! daily total shortwave radiation MJ/m^2
      real(dp) :: eff_lai      ! single plant effective leaf area index (based on maximum single plant coverage area)
      real(dp) :: trad_lai     ! leaf area index based on whole field area (traditional)
      real(dp) :: ts           ! temperature stress factor
      real(dp) :: p_rw         ! fibrous root partitioning ratio
      real(dp) :: p_st         ! stem partitioning ratio
      real(dp) :: p_lf         ! leaf partitioning ratio
      real(dp) :: p_rp         ! reproductive partitioning ratio
      real(dp) :: pdht         ! increment in potential height (m)
      real(dp) :: pdrd         ! potential increment in root length (m)
      real(dp) :: ffa          ! leaf senescence factor (ratio)
      real(dp) :: ffw          ! leaf weight reduction factor (ratio)
      real(dp) :: ffr          ! fibrous root weight reduction factor (ratio)
      real(dp) :: gif          ! grain index accounting for development of chaff before grain fill
      real(dp) :: par          ! photosynthetically active radiation (MJ/m2)
      real(dp) :: apar         ! intercepted photosynthetically active radiation (MJ/m2)
      real(dp) :: pddm         ! increment in potential dry matter (kg)
      real(dp) :: stem_propor  ! Fraction of stem mass increase allocated to standing stems (remainder goes flat)
      real(dp) :: pdiam        ! Reach of growing plant (m)
      real(dp) :: parea        ! areal extent occupied by plant leaf (m^2/plant)
      real(dp) :: temp_sai
      real(dp) :: temp_stmrep
      real(dp) :: lost_mass    ! biomass that decayed (disappeared) from scenescence and freeze damage

      ! + + + LOCAL VARIABLES + + +
      integer(int32) :: bnslay ! number of soil layers
      real(dp) :: ddm        ! stress modified increment in dry matter (kg/m^2)
      real(dp) :: ddm_rem    ! increment in dry matter excluding fibrous roots(kg/m^2)
      real(dp) :: drfwt      ! increment in fibrous root weight (kg/m^2)
      real(dp) :: dlfwt      ! increment in leaf dry weight (kg/m^2)
      real(dp) :: dstwt      ! increment in dry weight of stem (kg/m^2)
      real(dp) :: drpwt      ! increment in reproductive mass (kg/m^2)
      real(dp) :: drswt      ! biomass diverted from partitioning to root storage
      real(dp) :: dstandstem
      real(dp) :: dht        ! daily height increment (m)
      real(dp) :: xw         ! absolute value of minimum temperature
      real(dp) :: clfwt      ! leaf dry weight (kg/plant)
      real(dp) :: clfarea    ! leaf area (m^2/plant)
      integer(int32) :: i    ! array index used in loops
      real(dp) :: wcg        ! root mass distribution function exponent (see reference at equation)
      real(dp) :: wmaxd      ! root mass distribution function depth limit parameter
      real(dp) :: wffiber    ! total of weight fractions for fibrous roots (normalization)
      real(dp) :: wfstore    ! total of weight fractions for storage roots (normalization)
      integer(int32) :: irfiber ! index of deepest soil layer for fibrous roots
      integer(int32) :: irstore ! index of deepest soil layer for storage roots
      real(dp) :: temp_fiber
      real(dp) :: temp_store
      real(dp) :: temp_stem
      real(dp), dimension(:), allocatable :: wfl ! weight fraction by layer used to distribute root mass into the soil layers
      real(dp), dimension(:), allocatable :: za  ! soil layer representative depth
      real(dp) :: bhfwsf_adj  ! water stress factor adjusted by biomass adjustment factor
      real(dp) :: adjleaf2stor ! adjusted value of leaf biomass diversion to root/crown storage.
      real(dp) :: adjstem2stor ! adjusted value of stem biomass diversion to root/crown storage.
      real(dp) :: adjstor2stor ! adjusted value of storage biomass diversion to root/crown storage.
                               ! Adjusted based on: plants freeze hardening index, fullness of storage root reservoir
      real(dp) :: tempdstm   ! number of stem possible from root stores
      real(dp) :: temptotshoot ! amount of storage required from each stem
      real(dp) :: froz_mass  ! mass of living tissue that died today
      real(dp) :: live_leaf
      real(dp) :: dead_leaf  ! mass in kg/m^2
      real(dp) :: strs       ! stress mass reduction factor

      integer :: alloc_stat, sum_stat

      ! + + + LOCAL PARAMETERS + + +
      integer(int32), parameter :: shoot_flg = 1 ! used to control the behavior of the shootnum subroutine

      ! + + + END OF SPECIFICATIONS + + +

      bnslay = size(soil%aszlyd)

      !!!!! START SINGLE PLANT CALCULATIONS !!!!!
      ! calculate single plant effective lai (standing living leaves only)
      clfwt = plant%mass%standleaf / plant%geometry%dpop            ! kg/m^2 / plants/m^2 = kg/plant
      clfarea = clfwt * plant%database%sla * plant%growth%fliveleaf   ! kg/plant * m^2/kg = m^2/plant

      ! limiting plant area to a feasible plant area results in a
      ! leaf area index covering the "plant's area"
      ! 1/(#/m^2) = m^2/plant. Plant diameter now used to limit leaf
      ! coverage to present plant diameter.
      ! find present plant diameter (proportional to diam/height ratio)
      !pdiam = min( 2.0*plant%geometry%zht * max(1.0, plant%database%diammax/plant%database%zmxc), plant%database%diammax )
      ! This expression above may not give correct effect since it is
      ! difficult to correctly model plant area expansion without additional
      ! plant parameters and process description. Presently using leaf area
      ! over total plant maximum area before trying this effect. Reducing
      ! effective plant area can only reduce early season growth.
      pdiam = plant%database%diammax
      ! account for row spacing effects
      if( plant%geometry%xrow .gt. 0.0_dp ) then
          ! use row spacing and plants maximum reach
          parea = min(plant%geometry%xrow,pdiam) * min(1.0_dp/(plant%geometry%dpop*plant%geometry%xrow),pdiam)
      else
          ! this is broadcast, so use uniform spacing
          parea = min( u_pi * pdiam * pdiam /4.0_dp, 1.0_dp/plant%geometry%dpop )
      end if

      ! check for valid plant area
      if( parea .gt. 0.0_dp ) then
          eff_lai = clfarea / parea
      else
          eff_lai = 1.0_dp
      end if

      !traditional lai calculation for reporting puposes
      trad_lai = clfarea * plant%geometry%dpop

      ! Start biomass calculations
      ! eirr is total shortwave radiation and a factor of .5 is assumed
      ! to get to the photosynthetically active radiation
      par=0.5_dp*eirr                    ! MJ/m^2                                    ! C-4

      ! calculate intercepted PAR, which is the good stuff less what hits the ground
      apar=par*(1.0_dp-exp(-plant%database%ck*eff_lai))                                             ! C-4

      ! calculate potential biomass conversion (kg/plant/day) using
      ! biomass conversion efficiency at ambient co2 levels
      ! units: ((m^2)/plant)*(kg/ha)/(MJ/m^2) * (MJ/m^2) / 10000 m^2/ha = kg/plant
      pddm = parea * plant%database%bceff * apar / u_hatom2                                          ! C-4

      ! biomass adjustment factor applied
      ! apply to both biomass conversion efficiency and water stress factor, see below
      pddm = pddm * plant%database%baf

      ! These were attempts at compensating for low yield as a result of
      ! water stress. (ie. this is the cause of unrealistically low yield)
      ! These methods had many side effects and were abandoned
      ! if( plant%database%baf .gt. 1.0 ) then
          ! first attempt. Reduces water stress in the middle stress region
          ! bhfwsf_adj = plant%growth%fwsf ** (1.0/(plant%database%baf*plant%database%baf))
          ! second attempt. Reduces extreme water stress (zero values).
          ! bhfwsf_adj = min( 1.0, max( plant%growth%fwsf, plant%database%baf-1.0 ) )
      ! else
          ! bhfwsf_adj = plant%growth%fwsf
      ! end if
      bhfwsf_adj = max( water_stress_max, plant%growth%fwsf )
      !bhfwsf_adj = 1 !no water stress

      ! select application of stress functions based on command line flag
      if( growth_stress .eq. 0 ) then
          strs = 1.0_dp
      else if( growth_stress .eq. 1 ) then
          strs = bhfwsf_adj
      else if( growth_stress .eq. 2 ) then
          strs = ts
      else if( growth_stress .eq. 3 ) then
          strs = min(ts,bhfwsf_adj)
      end if

      ! until shoot breaks surface, no solar driven growth
      ! call it lack of light stress
      if( plant%geometry%zht .le. 0.0_dp ) then
          strs = 0.0_dp
      end if

      ! left here to show some past incantations of stress factors 
      !  strs=min(sn,sp,ts,plant%growth%fwsf)
      !  if (hui.lt.0.25) strs=strs**2
      !  if (hui.gt.huilx) strs=sqrt(strs)

      ! apply stress factor to generated biomass
      ddm = pddm * strs
      ! end Stress factor section

      ! convert from mass per plant to mass per square meter
      ! + kg/plant * plant/m^2 = kg/m^2
      ddm = ddm * plant%geometry%dpop

      !!!!! END SINGLE PLANT CALCULATIONS !!!!!

      drfwt = ddm * p_rw
      ddm_rem = ddm - drfwt

      ! calculate assimate mass increments (kg/m^2)
      dlfwt = ddm_rem * p_lf
      dstwt = ddm_rem * p_st
      drpwt = ddm_rem * p_rp

      ! when a plant has freeze hardened halfway into stage 1, divert any growth to storage
      if( plant%growth%thardnx .gt. 0.0_dp ) then
          if( plant%growth%thardnx .lt. 0.5_dp ) then
              adjleaf2stor=plant%database%fleaf2stor+(1.0_dp-plant%database%fleaf2stor)*(plant%growth%thardnx)*2.0_dp
              adjstem2stor=plant%database%fstem2stor+(1.0_dp-plant%database%fstem2stor)*(plant%growth%thardnx)*2.0_dp
              adjstor2stor=plant%database%fstor2stor+(1.0_dp-plant%database%fstor2stor)*(plant%growth%thardnx)*2.0_dp
          else
              adjleaf2stor = 1.0_dp
              adjstem2stor = 1.0_dp
              adjstor2stor = 1.0_dp
          end if
      else
          adjleaf2stor = plant%database%fleaf2stor
          adjstem2stor = plant%database%fstem2stor
          adjstor2stor = plant%database%fstor2stor
      end if

       ! check for full regrowth reserve on all but tuber crops
      if( plant%database%idc .ne. 7 ) then
          ! check for regrowth shoot number possible from root store
          call shootnum(shoot_flg, bnslay, plant%database%idc, dble(plant%geometry%dpop), dble(plant%database%shoot), &
                   dble(plant%database%dmaxshoot), temptotshoot, dble(plant%mass%rootstorez), tempdstm )
          ! compare to maximum shoot number
          if( tempdstm .ge. 5.0_dp * plant%database%dmaxshoot * plant%geometry%dpop ) then
              ! one of these must be non-zero or regrowth will never occur
              adjleaf2stor = 0.0_dp
              adjstem2stor = 0.0_dp
              adjstor2stor = 0.0000001_dp
          end if
      end if

      ! use ratios to divert biomass to root storage
      drswt = dlfwt * adjleaf2stor + dstwt * adjstem2stor &
            + drpwt * adjstor2stor
      dlfwt = dlfwt * (1.0_dp-adjleaf2stor)
      dstwt = dstwt * (1.0_dp-adjstem2stor)
      drpwt = drpwt * (1.0_dp-adjstor2stor)

      ! senescence is done on a whole plant mass basis not incremental mass
      ! loss from weathering of leaf mass added to mass lost to freeze damage
      lost_mass = lost_mass + plant%mass%standleaf * (1.0_dp - ffw)
      ! adjust for senescence (done here, not below, so consistent with lost mass amount)
      plant%mass%standleaf = plant%mass%standleaf * ffw
      ! change in living mass fraction due scenescence
      ! and accounting for weathering mass loss of dead leaf

      plant%growth%fliveleaf = ffa * plant%growth%fliveleaf / (1.0_dp + plant%growth%fliveleaf * (ffw - 1.0_dp))

      ! yield residue relationship adjustment
      if(     (cook_yield .eq. 1) &
        .and. (plant%database%yld_coef .gt. 1.0_dp) .and. (plant%database%resid_int .ge. 0.0_dp) &
        .and. ( (plant%geometry%hyfg .eq. 0).or.(plant%geometry%hyfg .eq. 1).or.(plant%geometry%hyfg .eq. 5) ) ) then

          call cookyield(plant%geometry%hyfg, bnslay, dlfwt, dstwt, drpwt, drswt, &
                         dble(plant%mass%standstem), dble(plant%mass%standleaf), dble(plant%mass%standstore), &
                         dble(plant%mass%flatstem), dble(plant%mass%flatleaf), dble(plant%mass%flatstore), &
                         dble(plant%mass%rootstorez), lost_mass, &
                         dble(plant%database%yld_coef), dble(plant%database%resid_int), dble(plant%database%grf) )

      end if

      ! calculate stress adjusted height
      dht = pdht * strs

      ! add mass increment to accumulated biomass (kg/m^2)
      ! all leaf mass added to living leaf in standing pool
      if( dlfwt .gt. 0.0_dp ) then
          ! recalculate fraction of leaf which is living
          plant%growth%fliveleaf = (plant%growth%fliveleaf*plant%mass%standleaf + dlfwt) &
                      / (plant%mass%standleaf + dlfwt)
          ! next add in the additional mass
          plant%mass%standleaf = plant%mass%standleaf + dlfwt
      end if

      ! divide between standing and flat stem and storage in proportion
      ! to maximum height and maximum radius ratio
      stem_propor = min(1.0_dp, 2.0_dp * plant%database%zmxc / plant%database%diammax)
      dstandstem = dstwt * stem_propor
      plant%mass%standstem = plant%mass%standstem + dstandstem
      plant%mass%flatstem = plant%mass%flatstem + dstwt * (1.0_dp - stem_propor)

      ! for all but below ground place rp portion in standing storage
      plant%mass%standstore = plant%mass%standstore + drpwt * stem_propor
      plant%mass%flatstore = plant%mass%flatstore + drpwt * (1.0_dp-stem_propor)

      ! check for consistency of height, diameter and stem area index.
      ! adjust rate of height increase to keep diameter inside a range.
      call ht_dia_sai( dble(plant%geometry%dpop), dble(plant%mass%standstem), dstandstem, &
                       dble(plant%database%ssa), dble(plant%database%ssb), dble(plant%geometry%dstm), &
                       dble(plant%geometry%zht), dht, temp_stmrep, temp_sai )

      ! increment plant height
      plant%geometry%zht = min( plant%database%zmxc, plant%geometry%zht + dht)

      ! root mass distributed by layer below after root depth set

      ! calculate rooting depth (eq. 2.203) and check that it is not deeper
      ! than the maximum potential depth, and the depth of the root zone.
      plant%geometry%zrtd = min(plant%database%zmrt, plant%geometry%zrtd + pdrd)
      plant%geometry%zrtd = min(soil%aszlyd(bnslay)*u_mmtom, plant%geometry%zrtd)

      sum_stat = 0
      allocate( wfl(bnslay), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      allocate( za(bnslay), stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
        write(*,*) 'Memory allocation failed in growth.'
      end if

      ! determine bottom layer # where there are roots
      ! and calculate root distribution function
      ! the root distribution functions were taken from agron. monog. 31, equ. 26
      ! on page 99. wcg should be a crop parameter. (impact is probably small
      ! since this is only affecting mass distribution, not water uptake)
      ! wcg = 1.0 for sunflowers (deep uniform root distribution)
      ! wcg = 2.0 for corn and soybeans
      ! wcg = 3.0 for sorghum (alot of roots close to the surface)
      ! wmaxd could also be a parameter but there is insufficient info
      ! to indicate how the values would vary the shape of the distribution.
      ! The article indicates that it must be greater than maximum root depth.
      wcg = 2.0_dp
      wmaxd = max(3.0_dp,plant%database%zmrt)
      do i = 1,bnslay
          if (i.eq.1) then
              ! calculate depth to the middle of a layer
              za(i) = (soil%aszlyd(i)/2.0_dp) * u_mmtom
              ! calculate root distribution function
              if( za(i) .lt. wmaxd ) then
                  wfl(i) = (1.0_dp-za(i)/wmaxd)**wcg
              else
                  wfl(i) = 0.0_dp
              end if
              wfstore = wfl(i)
              irstore = i
              wffiber = wfl(i)
              irfiber = i
          else
              ! calculate depth to the middle of a layer
              za(i) = (soil%aszlyd(i-1)+(soil%aszlyd(i)-soil%aszlyd(i-1))/2.0_dp) * u_mmtom
              ! calculate root distribution function
              if( za(i) .lt. wmaxd ) then
                  wfl(i) = (1.0_dp-za(i)/wmaxd)**wcg
              else
                  wfl(i) = 0.0_dp
              end if
              if( plant%geometry%zrtd/3.0_dp .gt. za(i)) then
                  wfstore = wfstore + wfl(i)
                  irstore = i
              end if
              ! check if reached bottom of root zone
              if (plant%geometry%zrtd .gt. za(i)) then
                  wffiber = wffiber + wfl(i)
                  irfiber = i
              end if
          end if
      end do 

      ! distribute root weight into each layer
      do i = 1,irfiber
          if ( i.le.irstore ) then
              plant%mass%rootstorez(i) = plant%mass%rootstorez(i)+(drswt*wfl(i)/wfstore)
          end if
          plant%mass%rootfiberz(i) = plant%mass%rootfiberz(i) + (drfwt * wfl(i)/wffiber)

          ! root senescence : 02/16/2000 (A. Retta)
          plant%mass%rootfiberz(i) = plant%mass%rootfiberz(i) * ffr
      end do

      sum_stat = 0
      deallocate( wfl, stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      deallocate( za, stat=alloc_stat )
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
        write(*,*) 'Memory deallocation failed in growth.'
      end if

      if (plant%geometry%hyfg.eq.1) then
          plant%geometry%grainf = plant%database%grf * gif
      else
          plant%geometry%grainf = plant%database%grf
      endif

      return
    end subroutine growth

    subroutine cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt, &
                         bcmstandstem, bcmstandleaf, bcmstandstore, &
                         bcmflatstem, bcmflatleaf, bcmflatstore, &
                         bcmrootstorez, lost_mass, &
                         bcyld_coef, bcresid_int, bcgrf )

      ! + + + PURPOSE + + +
      ! adjust incremental biomass allocation to leaf stem and reproductive
      ! pools to match the input residue yield ratio and intercept value,
      ! if running the model in that mode

      ! + + + ARGUMENT DECLARATIONS + + +
      integer(int32), intent(in) :: bchyfg ! flag indicating the part of plant to which to apply the "grain fraction",
                                           ! GRF, when removing that plant part for yield
                                           !  0   GRF applied to above ground storage (seeds, reproductive)
                                           !  1   GRF times growth stage factor (see growth.for) applied to
                                           !          above ground storage (seeds, reproductive)
                                           !  2   GRF applied to all aboveground biomass (forage)
                                           !  3   GRF applied to leaf mass (tobacco)
                                           !  4   GRF applied to stem mass (sugarcane)
                                           !  5   GRF applied to below ground storage mass (potatoes, peanuts)
      integer(int32), intent(in) :: bnslay ! number of soil layers
      real(dp), intent(inout) :: dlfwt     ! increment in leaf dry weight (kg/m^2)
      real(dp), intent(inout) :: dstwt     ! increment in dry weight of stem (kg/m^2)
      real(dp), intent(inout) :: drpwt     ! increment in reproductive mass (kg/m^2)
      real(dp), intent(inout) :: drswt     ! biomass diverted from partitioning to root storage
      real(dp), intent(in) :: bcmstandstem ! crop standing stem mass (kg/m^2)
      real(dp), intent(in) :: bcmstandleaf ! crop standing leaf mass (kg/m^2)
      real(dp), intent(in) :: bcmstandstore ! crop standing storage mass (kg/m^2)
                                            ! (head with seed, or vegetative head (cabbage, pineapple))
      real(dp), intent(in) :: bcmflatstem  ! crop flat stem mass (kg/m^2)
      real(dp), intent(in) :: bcmflatleaf  ! crop flat leaf mass (kg/m^2)
      real(dp), intent(in) :: bcmflatstore ! crop flat storage mass (kg/m^2)
      real(dp), intent(in) :: bcmrootstorez(*) ! crop root storage mass by soil layer (kg/m^2)
                                               ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      real(dp), intent(in) :: lost_mass    ! biomass that decayed (disappeared) 
      real(dp), intent(in) :: bcyld_coef   ! yield coefficient (kg/kg)
      real(dp), intent(in) :: bcresid_int  ! residue intercept (kg/m^2)
                                           ! harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
      real(dp), intent(in) :: bcgrf        ! fraction of reproductive biomass that is yield

      ! + + + LOCAL VARIABLES + + +
      integer(int32) :: idx   ! array index used in loops
      real(dp) :: ddm_res_yld ! increment in aboveground dry matter (kg/m^2)
      real(dp) :: temp_tot    ! temporary total biomass
      real(dp) :: store_mass  ! intermediate storage mass value
      real(dp) :: ddm_adj     ! adjusted increment in aboveground dry matter (kg/m^2)

      ! + + + END OF SPECIFICATIONS + + +

      ! bchyfg = 0 - GRF times  reproductive mass
      ! bchyfg = 1 - GRF calculated in growth.FOR times reproductive mass (grain)
      ! bchyfg = 5 - GRF times below ground storage mass

      ! method based on yield residue relationship
      ! sum yield mass increments
      select case (bchyfg)
      case (0,1)
          ! 0 - GRF times  reproductive mass
          ! 1 - GRF calculated in growth.FOR times reproductive mass (grain)

          ! change in residue + yield biomass
          ! (new mass (abovegound + yield) - lost scenesced mass)
          ddm_res_yld = dlfwt + dstwt + drpwt - lost_mass
      case (5)
          ! 5 - GRF times below ground storage mass

          ! change in residue + yield biomass
          ! (new mass (abovegound + yield) - lost scenesced mass)
          ddm_res_yld = dlfwt + dstwt + drpwt + drswt - lost_mass
      case default
          ! no adjustment
          ! variable must be initialized
          ddm_res_yld = 0.0_dp
      end select

      ! find yield storage mass increment based on yield residue relationship
      ! sum present yield + residue biomass
      temp_tot = 0.0_dp
      if ( bchyfg .eq. 5) then
          ! 5 - GRF times below ground storage mass
          do idx = 1, bnslay
              temp_tot = temp_tot + bcmrootstorez(idx)
          end do
      end if
      ! add lost mass here to allow removing if mass was above threshold
      temp_tot = temp_tot + lost_mass &
               + bcmstandstem + bcmstandleaf + bcmstandstore &
               + bcmflatstem + bcmflatleaf + bcmflatstore
      if( temp_tot + ddm_res_yld .le. bcresid_int ) then
          store_mass = 0.0_dp
      else if( temp_tot .le. bcresid_int ) then
          store_mass = (ddm_res_yld - (bcresid_int-temp_tot)) &
                     / bcyld_coef / bcgrf
      else
          store_mass = ddm_res_yld / bcyld_coef / bcgrf
      end if
      select case (bchyfg)
      case (0,1)
          ! 0 - GRF times  reproductive mass
          ! 1 - GRF calculated in growth.FOR times reproductive mass (grain)

          ! (new mass (abovegound + yield) - lost scenesced mass)
          ddm_adj = dlfwt + dstwt + drpwt
          ! set reproductive mass increment
          drpwt = store_mass
          ! find remainder of mass increment
          ddm_adj = ddm_adj - drpwt
          ! distribute remainder of mass increment between stem and leaf
          ! leaf increment gets priority
          if( ddm_adj .gt. dlfwt ) then
              ! set stem increment
              dstwt = ddm_adj - dlfwt
          else
              ! not enough for both, leaf increment reduced
              dstwt = 0.0_dp
              dlfwt = ddm_adj
          end if
      case (5)
          ! 5 - GRF times below ground storage mass

          ddm_adj = dlfwt + dstwt + drpwt + drswt
          ! set reproductive mass increment
          drswt = store_mass
          ! find remainder of mass increment
          ddm_adj = ddm_adj - drswt
          ! distribute remainder of mass increment between stem and leaf
          ! leaf increment, then reproductive gets priority
          if( ddm_adj .gt. dlfwt + drpwt ) then
              ! set stem increment
              dstwt = ddm_adj - dlfwt - drpwt
          else if( ddm_adj .gt. dlfwt ) then
              ! set stem increment
              dstwt = 0.0_dp
              ! set reproductive increment
              drpwt = ddm_adj - dlfwt
          else
              ! not enough for both, leaf increment reduced
              dstwt = 0.0_dp
              drpwt = 0.0_dp
              dlfwt = ddm_adj
          end if
      case default
          ! no adjustment
      end select

      return
    end subroutine cookyield

    subroutine ht_dia_sai( bcdpop, bcmstandstem, dmstandstem, &
                           bc0ssa, bc0ssb, bcdstm, &
                           bczht, dht, bcxstmrep, bcrsai )

      ! this routine checks for consistency between plant height and biomass
      ! accumulation, using half and double the stem diameter (previously unused)
      ! as check points. The representative stem diameter is set to show where
      ! within the range the actual stem diameter is.

      ! + + + ARGUMENT DECLARATIONS + + +
      real(dp), intent(in) :: bcdpop, bcmstandstem, dmstandstem
      real(dp), intent(in) :: bc0ssa, bc0ssb
      real(dp), intent(in) :: bcdstm, bczht 
      real(dp), intent(inout) :: dht
      real(dp), intent(out) :: bcxstmrep, bcrsai

      ! + + + ARGUMENT DEFINITIONS + + +
      ! bcdpop - Crop seeding density (#/m^2)
      ! bcmstandstem - crop standing stem mass (kg/m^2)
      ! dmstandstem - daily crop standing stem mass increment (kg/m^2)
      ! bc0ssa - stem area to mass coefficient a, result is m^2 per plant
      ! bc0ssb - stem area to mass coefficient b, argument is kg per plant
      ! bcdstm - Number of crop stems per unit area (#/m^2)
      ! bczht  - Crop height (m)
      ! dht - daily height increment (m)
      ! bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
      ! bcrsai - Crop stem area index (m^2/m^2)

      ! + + + END OF SPECIFICATIONS + + +

      ! calculate crop stem area index
      ! when exponent is not 1, must use mass for single plant stem to get stem area
      ! bcmstandstem, convert (kg/m^2) / (plants/m^2) = kg/plant
      ! result of ((m^2 of stem)/plant) * (# plants/m^2 ground area) = (m^2 of stem)/(m^2 ground area)
      if( bcdpop .gt. 0.0_dp ) then
          bcrsai = bcdpop * bc0ssa * (bcmstandstem/bcdpop)**bc0ssb
      else
          bcrsai = 0.0_dp
      end if

!      if( dmstandstem .le. 0.0_dp ) then
!        ! stem mass is not increasing, therefore height is not increasing.
!        dht = 0.0_dp
!      end if

      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
      if( (bcdstm * (bczht + dht)) .gt. 0.0_dp ) then
          bcxstmrep = bcrsai / (bcdstm * (bczht + dht))
      else
          bcxstmrep = 0.0_dp
      end if

      return
    end subroutine ht_dia_sai

end module WEPSCrop_mod
