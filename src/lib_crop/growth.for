!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine growth(bnslay, bszlyd, bc0ck, bcgrf,                   &
     &                 bcehu0, bczmxc, bc0idc, bc0nam,                  &
     &                 a_fr, b_fr, bcxrow, bc0diammax,                  &
     &                 bczmrt, bctmin, bctopt, cc0be,                   &
     &                 bc0alf, bc0blf, bc0clf, bc0dlf,                  &
     &                 bc0arp, bc0brp, bc0crp, bc0drp,                  &
     &                 bc0aht, bc0bht, bc0ssa, bc0ssb,                  &
     &                 bc0sla, bcxstm, bhtsmn,                          &
     &                 bwtdmx, bwtdmn, bweirr, bhfwsf,                  &
     &                 hui, huiy, huirt, huirty, hu_delay, bcthardnx,   &
     &                 bcbaf, bchyfg,                                   &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int,                         &
     &                 bcmstandstem, bcmstandleaf, bcmstandstore,       &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bcmbgstemz,                                      &
     &                 bczht, bcdstm, bczrtd, bcfliveleaf,              &
     &                 bcdayap, bcgrainf, bcdpop, daysim, regrowth_flg, &
     &                 bc0shoot, bcdmaxshoot )

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     This subroutine calculates plant height, biomass partitioning,
!     rootmass distribution, rooting depth.

!     + + + KEYWORDS + + +
!     biomass

      use weps_interface_defs
      use file_io_mod, only: luocrop

!     + + + ARGUMENT DECLARATIONS + + +
      integer bnslay
      real bszlyd(*), bc0ck, bcgrf
      real bcehu0, bczmxc
      integer bc0idc
      character*(80) bc0nam
      real a_fr, b_fr, bcxrow, bc0diammax
      real bczmrt, bctmin, bctopt, cc0be
      real bc0alf, bc0blf, bc0clf, bc0dlf
      real bc0arp, bc0brp, bc0crp, bc0drp
      real bc0aht, bc0bht, bc0ssa, bc0ssb
      real bc0sla, bcxstm, bhtsmn(*)
      real bwtdmx, bwtdmn, bweirr, bhfwsf
      real hui, huiy, huirt, huirty, hu_delay, bcthardnx
      real bcbaf
      integer bchyfg
      real bcfleaf2stor, bcfstem2stor, bcfstor2stor
      real bcyld_coef, bcresid_int
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bcmbgstemz(*)
      real bczht, bcdstm, bczrtd, bcfliveleaf
      integer bcdayap
      real bcgrainf, bcdpop
      integer daysim, regrowth_flg
      real bc0shoot, bcdmaxshoot

!     + + + ARGUMENT DEFINITIONS + + +
!     bnslay - number of soil layers
!     bszlyd - depth from top of soil to botom of layer, m
!     bc0ck  - extinction coeffficient (fraction)
!     bcgrf  - fraction of reproductive biomass that is yield
!     bcehu0 - relative gdd at start of senescence
!     bczmxc - maximum potential plant height (m)
!     bc0idc - crop type:annual,perennial,etc
!     bc0nam - crop name
!     a_fr - parameter in the frost damage s-curve
!     b_fr - parameter in the frost damage s-curve
!     bcxrow - Crop row spacing (m)
!     bc0diammax - crop maximum plant diameter (m)
!     bczmrt - maximum root depth
!     bctmin - base temperature (deg. C)
!     bctopt - optimum temperature (deg. C)
!     cc0be - biomass conversion efficiency (kg/ha)/(Mj/m^2)
!     bc0alf - leaf partitioning parameter
!     bc0blf - leaf partitioning parameter
!     bc0clf - leaf partitioning parameter
!     bc0dlf - leaf partitioning parameter
!     bc0arp - rprd partitioning parameter
!     bc0brp - rprd partitioning parameter
!     bc0crp - rprd partitioning parameter
!     bc0drp - rprd partitioning parameter
!     bc0aht - height s-curve parameter
!     bc0bht - height s-curve parameter
!     bc0ssa - biomass to stem area conversion coefficient a
!     bc0ssb - biomass to stem area conversion coefficient b
!     bc0sla - specific leaf area (cm^2/g)
!     bcxstm - mature crop stem diameter (m)
!     bhtsmn - daily minimum soil temperature (deg C)
!     bwtdmx - daily maximum air temperature (deg C)
!     bwtdmn - daily minimum air temperature (C)
!     bweirr - Daily global radiation (MJ/m^2)
!     bhfwsf - water stress factor (ratio)
!     hui - heat unit index (ratio of acthucum to acthum)
!     huiy - heat unit index (ratio of acthucum to acthum) on day (i-1)
!     huirt - heat unit index for root expansion (ratio of actrthucum to acthum)
!     huirty - heat unit index for root expansion (ratio of actrthucum to acthum) on day (i-1)
!     hu_delay - fraction of heat units accummulated
!                based on incomplete vernalization and day length
!     bcthardnx - hardening index for winter annuals (range from 0 t0 2)
!     bcbaf  - biomass adjustment factor
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)
!     bcfleaf2stor - fraction of assimilate partitioned to leaf that is diverted to root store
!     bcfstem2stor - fraction of assimilate partitioned to stem that is diverted to root store
!     bcfstor2stor - fraction of assimilate partitioned to standing storage
!                   (reproductive) that is diverted to root store
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bcmbgstemz  - crop buried stem mass by layer (kg/m^2)
!     bczht  - Crop height (m)
!     bcdstm - Number of plant stems per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives the number of stems per plant
!     bczrtd - root depth (m)
!     bcfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     bcdayap - number of days of growth completed since crop planted
!     bcgrainf - internally computed grain fraction of reproductive mass
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives the number of stems per plant
!     daysim   - day of the simulation
!     regrowth_flg - used to record changes is regrowth conditions day by day
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bcdmaxshoot - maximum number of shoots possible from each plant

!     + + + COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'precision.inc'
      include 'p1unconv.inc'
      include 'p1const.inc'
      include 'command.inc'

!     + + + LOCAL VARIABLES + + +
      real frst, par, apar, arg_exp
      real pddm, ddm, ddm_rem
      real p_rw, p_st, p_lf, p_rp
      real drfwt, dlfwt, dstwt, drpwt, drswt
      real pdht, dht
      real hux, ff, ffa, ffw, ffr
      real hui0f, pdrd
      real xw,gif
      real clfwt, clfarea, pdiam, parea, p_lf_rp
      real huf, hufy, pchty, pcht, strs, ts
      real stem_propor, prdy, prd
      real eff_lai, trad_lai
      integer day, mo, yr, doy
      integer i   
      real    wcg, wmaxd
      real lost_mass
      real wffiber, wfstore
      integer irfiber, irstore
      real temp_fiber, temp_store, temp_stem
      real wfl(mnsz) !  and weight fraction by layer used to distribute root mass into the soil layers
      real za(mnsz)
!      real ppx,ppveg,pprpd ! used with plant population adjustment
      real bhfwsf_adj
      real temp_sai, temp_stmrep
      real adjleaf2stor, adjstem2stor, adjstor2stor
      real tempdstm, temptotshoot

      real froz_mass, live_leaf, dead_leaf ! mass in kg/m^2

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     frst - frost damage factor
!     par - photosynthetically active radiation (MJ/m2)
!     apar - intercepted photosynthetically active radiation (MJ/m2)
!     arg_exp - argument calculated for exponential function (to test for validity)
!     pddm - increment in potential dry matter (kg)
!     ddm - stress modified increment in dry matter (kg/m^2)
!     ddm_rem - increment in dry matter excluding fibrous roots(kg/m^2)
!     p_rw - fibrous root partitioning ratio
!     p_st - stem partitioning ratio
!     p_lf - leaf partitioning ratio
!     p_rp - reproductive partitioning ratio
!     drfwt - increment in fibrous root weight (kg/m^2)
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     dstwt - increment in dry weight of stem (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     pdht - increment in potential height (m)
!     dht - daily height increment (m)
!     hux - relative gdd offset to start at scenescence
!     ff - senescence factor (ratio)
!     ffa - leaf senescnce factor (ratio)
!     ffw - leaf weight reduction factor (ratio)
!     ffr - fibrous root weight reduction factor (ratio)
!     hui0f - relative gdd at start of scenescence
!     pdrd - potential increment in root length (m)
!     xw - absolute value of minimum temprature
!     gif  - grain index accounting for development of chaff before grain fill

      ! used with plant population adjustment
!     ppx
!     ppveg
!     pprpd

!     bhfwsf_adj - water stress factor adjusted by biomass adjustment factor

!     clfwt - leaf dry weight (kg/plant)
!     clfarea - leaf area (m^2/plant)
!     pdiam - Reach of growing plant (m)
!     parea - areal extent occupied by plant leaf (m^2/plant)
!     p_lf_rp - sum of leaf and reproductive partitioning fractions
!     huf - heat unit factor for driving root depth, plant height development
!     hufy - value of huf on day (i-1)
!     pchty - potential plant height from previous day
!     pcht - potential plant height for today
!     strs - stress factor (fraction of growth occuring accounting for stress)
!     ts - temperature stress factor
!     stem_propor - Fraction of stem mass increase allocated to standing stems (remainder goes flat)
!     prdy - potential root depth from previous day
!     prd - potential root depth today
!     eff_lai - single plant effective leaf area index (based on maximum single plant coverage area)
!     trad_lai - leaf area index based on whole field area (traditional)
!     day - day of month
!     mo - month of year
!     yr - year
!     doy - day of year
!     i - array index used in loops
!     wcg - root mass distribution function exponent (see reference at equation)
!     wmaxd - root mass distribution function depth limit parameter
!     froz_mass - mass of living tissue that died today
!     lost_mass - biomass that decayed (disappeared) from scenescence and freeze damage

!     drswt - biomass diverted from partitioning to root storage
!     wffiber - total of weight fractions for fibrous roots (normalization)
!     wfstore - total of weight fractions for storage roots (normalization)
!     irfiber - index of deepest soil layer for fibrous roots
!     irstore - index of deepest soil layer for storage roots
!     wfl(mnsz) - weight fraction by layer (distribute root mass into the soil layers)
!     za(mnsz) - soil layer representative depth

!     adjleaf2stor, adjstem2stor, adjstor2stor - adjusted value of bomass diversion
!         to root/crown storage. Factor considered are:
!         - plants freeze hardening index
!         - fullness of storage root reservoir
!     tempdstm - number of stem possible from root stores
!     temptotshoot - amount of storage required from each stem

! From command.inc
!     frac_frst_mass_lost - fraction of leaf mass that is frozen that disappears

!     + + + LOCAL PARAMETERS + + +
      integer shoot_flg
      parameter( shoot_flg = 1)

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     shoot_flg - used to control the behavior of the shootnum subroutine
!             1 - returns the shoot number unconstrained by bcdmaxshoot

!     + + + FUNCTIONS CALLED + + +
!      integer dayear
!      real temps

!     + + + SUBROUTINES CALLED + + +
!     caldatw
!     nuse       !disabled
!     najn       !disabled
!     najna      !disabled
!     nuts       !disabled
!     waters     !disabled

!     + + + END OF SPECIFICATIONS + + +

      call caldatw(day, mo, yr)
      doy = dayear (day, mo, yr)

      ! find the heat unit index that indicates the start of scenescence
      hui0f=bcehu0-bcehu0*.1

!     reduce green leaf mass in freezing weather
      if (bhtsmn(1).lt.-2.0) then
!          xw=abs(bwtdmn)
!         use daily minimum soil temperature of first layer to account for snow cover effects
          xw = abs(bhtsmn(1))
          ! this was obviously to prevent excessive leaf loss
          ! frst=sqrt((1.-xw/(xw+exp(a_fr-b_fr*xw)))+0.000001)
          ! frst=sqrt(frst)
          ! tested to match the values input in the database
          frst = xw / (xw + exp(a_fr-b_fr*xw))
          frst = min(1.0, max(0.0, frst))

          ! is it before or after scenescence?
          if (hui.lt.hui0f) then
              ! before scenescence, frost killed mass is fragile and a fraction disappears
              ffa = 1.0 - frst
              ffw = 1.0 - frst * frac_frst_mass_lost
              lost_mass  = bcmstandleaf * (1.0 - ffw)

              ! eliminate these in favor of dead to live ratio
              ! reduce green leaf area due to frost damage (10/1/99)
              live_leaf = bcmstandleaf * bcfliveleaf
              dead_leaf = bcmstandleaf * (1.0 - bcfliveleaf)

!        write(*,*) 'Freeze: ',frac_frst_mass_lost,frst,ffa,ffw,lost_mass
!        write(*,*) 'Freeze: before ', bcmstandleaf, bcfliveleaf,         &
!     &            live_leaf/(live_leaf + dead_leaf)

              froz_mass = bcmstandleaf * bcfliveleaf * frst
              live_leaf = live_leaf - froz_mass
              dead_leaf = dead_leaf+froz_mass*(1.0-frac_frst_mass_lost)

              ! adjust here for lost mass amount so consistent below)
              bcmstandleaf = bcmstandleaf * ffw
              ! change in living mass fraction due freezing
              ! and accounting for weathering mass loss of dead leaf
              bcfliveleaf = ffa*bcfliveleaf/(1.0+bcfliveleaf*(ffw-1.0))
!        write(*,*) 'Freeze: after  ', bcmstandleaf, bcfliveleaf,         &
!     &            live_leaf/(live_leaf + dead_leaf)

          else
              ! after scenescence, frost killed mass is tougher and is not lost immediately
              ! reduce green leaf area due to frost damage (9/22/2003)
              bcfliveleaf = bcfliveleaf * (1.0 - frst)
              lost_mass = 0.0
           end if

          ! these are set here to show up on the output as initialized
          p_rw = 0.0
          p_lf = 0.0
          p_st = 0.0
          p_rp = 0.0
      else
          frst = 0.0
          lost_mass = 0.0
      endif

      !!!!! START SINGLE PLANT CALCULATIONS !!!!!
      ! calculate single plant effective lai (standing living leaves only)
      clfwt = bcmstandleaf / bcdpop            ! kg/m^2 / plants/m^2 = kg/plant
      clfarea = clfwt * bc0sla * bcfliveleaf   ! kg/plant * m^2/kg = m^2/plant

      ! limiting plant area to a feasible plant area results in a
      ! leaf area index covering the "plant's area"
      ! 1/(#/m^2) = m^2/plant. Plant diameter now used to limit leaf
      ! coverage to present plant diameter.
      ! find present plant diameter (proportional to diam/height ratio)
      !pdiam = min( 2.0*bczht * max(1.0, bc0diammax/bczmxc), bc0diammax )
      ! This expression above may not give correct effect since it is
      ! difficult to correctly model plant area expansion without additional
      ! plant parameters and process description. Presently using leaf area
      ! over total plant maximum area before trying this effect. Reducing
      ! effective plant area can only reduce early season growth.
      pdiam = bc0diammax
      ! account for row spacing effects
      if( bcxrow .gt. 0.0 ) then
          ! use row spacing and plants maximum reach
          parea = min(bcxrow,pdiam) * min(1.0/(bcdpop*bcxrow),pdiam)
      else
          ! this is broadcast, so use uniform spacing
          parea = min( pi * pdiam * pdiam /4.0, 1.0/bcdpop )
      end if

      ! check for valid plant area
      if( parea .gt. 0.0 ) then
          eff_lai = clfarea / parea
      else
          eff_lai = 1.0
      end if

      !traditional lai calculation for reporting puposes
      trad_lai = clfarea * bcdpop

!     Start biomass calculations
!     bweirr is total shortwave radiation and a factor of .5 is assumed
!     to get to the photosynthetically active radiation
      par=0.5*bweirr                    ! MJ/m^2                                    ! C-4

!     calculate intercepted PAR, which is the good stuff less what hits the ground
      apar=par*(1.-exp(-bc0ck*eff_lai))                                             ! C-4

!     calculate potential biomass conversion (kg/plant/day) using
!     biomass conversion efficiency at ambient co2 levels
      ! units: ((m^2)/plant)*(kg/ha)/(MJ/m^2) * (MJ/m^2) / 10000 m^2/ha = kg/plant
      pddm = parea * cc0be * apar / hatom2                                          ! C-4

!     biomass adjustment factor applied
      ! apply to both biomass converstion efficiency and water stress factor, see below
      pddm = pddm * bcbaf

      ! These were attempts at compensating for low yield as a result of
      ! water stress. (ie. this is the cause of unrealistically low yield)
      ! These methods had many side effects and were abandoned
      ! if( bcbaf .gt. 1.0 ) then
          ! first attempt. Reduces water stress in the middle stress region
          ! bhfwsf_adj = bhfwsf ** (1.0/(bcbaf*bcbaf))
          ! second attempt. Reduces extreme water stress (zero values).
          ! bhfwsf_adj = min( 1.0, max( bhfwsf, bcbaf-1.0 ) )
      ! else
          ! bhfwsf_adj = bhfwsf
      ! end if
      bhfwsf_adj = max( water_stress_max, bhfwsf )
      !bhfwsf_adj = 1 !no water stress

!     begin stress factor section

!     calculate N & P demand and supply
!      call nuse
!     calculate N & P uptake with increase in supply if necessary
!      call najn
!      call najna
!     calculate N stress
!      call nuts (un1,un2,sn)
!      call nuts (sun,un2,sn)
!     calculate P stress
!      call nuts (up1,up2,sp)

!     calculate temperature stress
      ts = temps (bwtdmx, bwtdmn, bctopt, bctmin)

      ! select application of stress functions based on command line flag
      if( growth_stress .eq. 0 ) then
          strs = 1.0
      else if( growth_stress .eq. 1 ) then
          strs = bhfwsf_adj
      else if( growth_stress .eq. 2 ) then
          strs = ts
      else if( growth_stress .eq. 3 ) then
          strs = min(ts,bhfwsf_adj)
      end if

      ! until shoot breaks surface, no solar driven growth
      ! call it lack of light stress
      if( bczht .le. 0.0 ) then
          strs = 0.0
      end if

      ! left here to show some past incantations of stress factors 
!      strs=min(sn,sp,ts,bhfwsf)
!      if (hui.lt.0.25) strs=strs**2
!      if (hui.gt.huilx) strs=sqrt(strs)

      ! apply stress factor to generated biomass
      ddm = pddm * strs
!     end Stress factor section

      ! convert from mass per plant to mass per square meter
      ! + kg/plant * plant/m^2 = kg/m^2
      ddm = ddm * bcdpop

      !!!!! END SINGLE PLANT CALCULATIONS !!!!!

      ! find partitioning between fibrous roots and all other biomass
      ! root partition done using root heat unit index, which is not reset
      ! when a harvest removes all the leaves. This index also is not delayed
      ! in prevernalization winter annuals. Made to parallel winter annual
      ! rooting depth flag as well.
      if( winter_ann_root .eq. 0 ) then
          p_rw = (.4-.2*hui)                                            ! C-5
      else
          p_rw = max(0.05, (.4-.2*huirt) )                              ! C-5
      end if
      drfwt = ddm * p_rw
      ddm_rem = ddm - drfwt

!     find partitioning factors of the remaining biomass (not fibrous root)
!     calculate leaf partitioning.
      arg_exp = -(hui-bc0clf)/bc0dlf
      if( arg_exp .ge. max_arg_exp ) then
          p_lf = bc0alf+bc0blf/max_real
      else
          p_lf=bc0alf+bc0blf/(1.+exp(-(hui-bc0clf)/bc0dlf))
      end if
      p_lf = max( 0.0, min( 1.0, p_lf ))

!     calculate reproductive partitioning based on partioning curve
      arg_exp = -(hui-bc0crp)/bc0drp
      if( arg_exp .ge. max_arg_exp ) then
          p_rp = bc0arp+bc0brp/max_real
      else
          p_rp=bc0arp+bc0brp/(1.+exp(-(hui-bc0crp)/bc0drp))
      end if
      p_rp = max( 0.0, min( 1.0, p_rp ))

      ! normalize leaf and reproductive fractions so sum never greater than 1.0
      p_lf_rp = p_lf + p_rp
      if( p_lf_rp .gt. 1.0 ) then
          p_lf = p_lf / p_lf_rp
          p_rp = p_rp / p_lf_rp
          ! set stem partitioning parameter.
          p_st = 0.0
      else
          ! set stem partitioning parameter.
          p_st = 1.0 - p_lf_rp
      end if

      ! calculate assimate mass increments (kg/m^2)
      dlfwt = ddm_rem * p_lf
      dstwt = ddm_rem * p_st
      drpwt = ddm_rem * p_rp

      ! when a plant has freeze hardened halfway into stage 1, divert any growth to storage
      if( bcthardnx .gt. 0.0 ) then
          if( bcthardnx .lt. 0.5 ) then
              adjleaf2stor=bcfleaf2stor+(1.0-bcfleaf2stor)*(bcthardnx)*2
              adjstem2stor=bcfstem2stor+(1.0-bcfstem2stor)*(bcthardnx)*2
              adjstor2stor=bcfstor2stor+(1.0-bcfstor2stor)*(bcthardnx)*2
          else
              adjleaf2stor = 1.0
              adjstem2stor = 1.0
              adjstor2stor = 1.0
          end if
      else
          adjleaf2stor = bcfleaf2stor
          adjstem2stor = bcfstem2stor
          adjstor2stor = bcfstor2stor
      end if

       ! check for full regrowth reserve on all but tuber crops
      if( bc0idc .ne. 7 ) then
          ! check for regrowth shoot number possible from root store
          call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot,    &
     &             bcdmaxshoot, temptotshoot, bcmrootstorez, tempdstm )
          ! compare to maximum shoot number
          if( tempdstm .ge. 5.0 * bcdmaxshoot * bcdpop ) then
              adjleaf2stor = 0.0
              adjstem2stor = 0.0
              adjstor2stor = 0.0
          end if
      end if


      ! use ratios to divert biomass to root storage
      drswt = dlfwt * adjleaf2stor + dstwt * adjstem2stor               &
     &      + drpwt * bcfstor2stor
      dlfwt = dlfwt * (1.0-adjleaf2stor)
      dstwt = dstwt * (1.0-adjstem2stor)
      drpwt = drpwt * (1.0-bcfstor2stor)

      ! senescence is done on a whole plant mass basis not incremental mass
      ! This starts scencescence before the entered heat unit index for
      ! the start of scencscence. For most leaf partitioning functions
      ! the coefficients draw a curve that approaches 1 around -0.5 but
      ! the value at zero, raised to fractional powers is still very small
      hui0f=bcehu0-bcehu0*.1
      if (hui.ge.hui0f) then
          hux=hui-bcehu0
          ff = 1./(1.+exp(-(hux-bc0clf/2.)/bc0dlf))
          ffa = ff**0.125
          ffw = ff**0.0625
          ffr = 0.98
          ! loss from weathering of leaf mass added to mass lost to freeze damage
          lost_mass = lost_mass + bcmstandleaf * (1.0 - ffw)
          ! adjust for senescence (done here, not below, so consistent with lost mass amount)
          bcmstandleaf = bcmstandleaf * ffw
          ! change in living mass fraction due scenescence
          ! and accounting for weathering mass loss of dead leaf
          bcfliveleaf = ffa*bcfliveleaf / (1.0 + bcfliveleaf*(ffw-1.0))
      else
          ! set a value to be written out
          ffa = 1.0
          ffw = 1.0
          ffr = 1.0
      endif

      ! yield residue relationship adjustment
      if(     (cook_yield .eq. 1)                                       &
     &  .and. (bcyld_coef .gt. 1.0) .and. (bcresid_int .ge. 0.0)        &
     &  .and. ( (bchyfg.eq.0).or.(bchyfg.eq.1).or.(bchyfg.eq.5) ) ) then

          call cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt,    &
     &                   bcmstandstem, bcmstandleaf, bcmstandstore,     &
     &                   bcmflatstem, bcmflatleaf, bcmflatstore,        &
     &                   bcmrootstorez, lost_mass,                      &
     &                   bcyld_coef, bcresid_int, bcgrf )

      end if

!     added method (different from EPIC) of calculating plant height
!     pht=cummulated potential height,pdht=daily potential height
!     aczht(am0csr) = cummulated actual height
!     adht=daily actual height, bc0aht,bc0bht are
!     height-scurve parameters (formerly lai parameters)
      ! previous day
      hufy = .01+1./(1.+exp((huiy-bc0aht)/bc0bht))
      ! today
      huf = .01+1./(1.+exp((hui-bc0aht)/bc0bht))

      pchty = min(bczmxc, bczmxc * hufy)
      pcht = min(bczmxc, bczmxc * huf)
      pdht = pcht - pchty

      ! calculate stress adjusted height
      dht = pdht * strs

      ! add mass increment to accumulated biomass (kg/m^2)
      ! all leaf mass added to living leaf in standing pool
      if( dlfwt .gt. 0.0 ) then
          ! recalculate fraction of leaf which is living
          bcfliveleaf = (bcfliveleaf*bcmstandleaf + dlfwt)              &
     &                / (bcmstandleaf + dlfwt)
          ! next add in the additional mass
          bcmstandleaf = bcmstandleaf + dlfwt
      end if

      ! divide between standing and flat stem and storage in proportion
      ! to maximum height and maximum radius ratio
      stem_propor = min(1.0, 2.0 * bczmxc / bc0diammax)
      bcmstandstem = bcmstandstem + dstwt * stem_propor
      bcmflatstem = bcmflatstem + dstwt * (1.0 - stem_propor)

      ! for all but below ground place rp portion in standing storage
      bcmstandstore = bcmstandstore + drpwt * stem_propor
      bcmflatstore = bcmflatstore + drpwt * (1.0-stem_propor)

      ! check for consistency of height, diameter and stem area index.
      ! adjust rate of height increase to keep diameter inside a range.
      call ht_dia_sai( bcdpop, bcmstandstem, bc0ssa, bc0ssb,            &
     &                 bcdstm, bcxstm, bczmxc, bczht, dht,              &
     &                 temp_stmrep, temp_sai )

      ! increment plant height
      bczht = bczht + dht

      ! root mass distributed by layer below after root depth set

!     calculate rooting depth (eq. 2.203) and check that it is not deeper
!     than the maximum potential depth, and the depth of the root zone.
!     This change from the EPIC method is undocumented!! It says that root depth
!     starts at 10cm and increases from there at the rate determined by huf.
!     the 10 cm assumption was prevously removed from elsewhere in the code
!     and is subsequently removed here. The initial depth is now set in 
!     crop record seeding depth, and  the function just increases it.
!     This is now based on a no delay heat unit accumulation to allow
!     rapid root depth development by winter annuals.
      if( winter_ann_root .eq. 0 ) then
          prdy = min(bczmrt, bczmrt * hufy + 0.1)
          prd = min(bczmrt, bczmrt * huf + 0.1)
      else
          prdy = bczmrt *(.01 + 1.0/(1.0 + exp((huirty-bc0aht)/bc0bht)))
          prd = bczmrt * (.01 + 1.0/(1.0 + exp((huirt-bc0aht)/bc0bht)))
      end if
      pdrd = max(0.0, prd - prdy)
      bczrtd = min(bczmrt, bczrtd + pdrd)
      bczrtd = min(bszlyd(bnslay)*mmtom, bczrtd)

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
      wcg = 2.0
      wmaxd = max(3.0,bczmrt)
      do i = 1,bnslay
          if (i.eq.1) then
              ! calculate depth to the middle of a layer
              za(i) = (bszlyd(i)/2.0) * mmtom
              ! calculate root distribution function
              if( za(i) .lt. wmaxd ) then
                  wfl(i) = (1.0-za(i)/wmaxd)**wcg
              else
                  wfl(i) = 0.0
              end if
              wfstore = wfl(i)
              irstore = i
              wffiber = wfl(i)
              irfiber = i
          else
              ! calculate depth to the middle of a layer
              za(i) = (bszlyd(i-1)+(bszlyd(i)-bszlyd(i-1))/2.0) * mmtom
              ! calculate root distribution function
              if( za(i) .lt. wmaxd ) then
                  wfl(i) = (1.0-za(i)/wmaxd)**wcg
              else
                  wfl(i) = 0.0
              end if
              if( bczrtd/3.0 .gt. za(i)) then
                  wfstore = wfstore + wfl(i)
                  irstore = i
              end if
              ! check if reached bottom of root zone
              if (bczrtd .gt. za(i)) then
                  wffiber = wffiber + wfl(i)
                  irfiber = i
              end if
          end if
      end do 

      ! distribute root weight into each layer
      do i = 1,irfiber
          if ( i.le.irstore ) then
              bcmrootstorez(i) = bcmrootstorez(i)+(drswt*wfl(i)/wfstore)
          end if
          bcmrootfiberz(i) = bcmrootfiberz(i) + (drfwt * wfl(i)/wffiber)
          ! root senescence : 02/16/2000 (A. Retta)
          bcmrootfiberz(i) = bcmrootfiberz(i) * ffr
      end do

      ! this factor prorates the grain reproductive fraction (grf) defined
      ! in the database for crop type 1, grains. Compensates for the
      ! development of chaff before grain filling, ie., grain is not
      ! uniformly a fixed fraction of reproductive mass during the entire 
      ! reproductive development stage.
      gif=1./(1.0+exp(-(hui-0.64)/.05))
      if (bchyfg.eq.1) then
          bcgrainf = bcgrf * gif
      else
          bcgrainf = bcgrf
      endif

!     the following write statements are for 'crop.out'
!     am0cfl is flag to print crop submodel output
      if (am0cfl .ge. 1) then
          ! temporary sum for output
          temp_store = 0.0
          temp_fiber = 0.0
          temp_stem = 0.0
          do i = 1, bnslay
              temp_store = temp_store + bcmrootstorez(i)
              temp_fiber = temp_fiber + bcmrootfiberz(i)
              temp_stem = temp_stem + bcmbgstemz(i)
          end do

          write(luocrop, 2130) daysim, doy, yr, bcdayap, hui,           &
     &                    bcmstandstem, bcmstandleaf, bcmstandstore,    &
     &                    bcmflatstem, bcmflatleaf, bcmflatstore,       &
     &                    temp_store, temp_fiber, temp_stem,            &
     &                    bcmstandleaf + bcmflatleaf,                   &
     &                    bcmstandstem + bcmflatstem + temp_stem,       &
     &                    bczht, bcdstm, trad_lai, eff_lai, bczrtd,     &
     &                    bcgrainf, ts, bhfwsf, frst, ffa, ffw,         &
     &                    par, apar, pddm, p_rw, p_st, p_lf, p_rp,      &
     &                    stem_propor, pdiam, parea, pdiam/bc0diammax,  &
     &                    parea*bcdpop, hu_delay, bcthardnx, temp_sai,  &
     &                    temp_stmrep, regrowth_flg, bc0nam
      end if

 2130 format(1x,i5,1x,i3,1x,i4,1x,i5,1x,f6.3,12(1x,f7.4),1x,f7.2,       &
     & 3(1x,f7.4),8(1x,f6.3),1x,e12.3, 11(1x,f6.3),2(1x,f8.5),1x,i2,    &
     & 1x,a30)

      return
      end
