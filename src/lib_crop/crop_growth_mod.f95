!$Author$
!$Date$
!$Revision$
!$HeadURL$

module crop_growth_mod

  contains

    subroutine cropgrow (isr, bnslay, bszlyd,                         &
     &                 bc0ck, bcgrf, bcehu0, bczmxc,                    &
     &                 bc0nam, bc0idc, bcxrow,                          &
     &                 bczmrt, bctmin, bctopt,                  &
     &                 bc0fd1, bc0fd2, cc0fd1, cc0fd2,                  &
     &                 bc0bceff,                                        &
     &                 bc0alf, bc0blf, bc0clf,                          &
     &                 bc0dlf, bc0arp, bc0brp, bc0crp,                  &
     &                 bc0drp, bc0aht, bc0bht,                          &
     &                 bc0sla, bc0hue, bctverndel,                      &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bhfwsf,                                          &
     &                 bm0cif,                                          &
     &                 bcbaf,                                  &
     &                 bchyfg, bcthum, bcdpop, bcdmaxshoot,             &
     &                 bc0storeinit, bcfshoot,                          &
     &                 bcfleafstem, bc0shoot,             &
     &                 bc0diammax, bc0ssa, bc0ssb,                      &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int, bcxstm,                 &
     &                 bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore, &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bcdayap, bcdayam,                                &
     &                 bcthucum, bctrthucum,                            &
     &                 bcgrainf, bczgrowpt,                &
     &                 bcleafareatrend, bcstemmasstrend, &
     &                 bctwarmdays, bctcolddays,  &
     &                 bctchillucum, bcthardnx, bcthu_shoot_beg,        &
     &                 bcthu_shoot_end, bcmtotleaf, bcthu_leaf_beg, &
     &                 bcthu_leaf_end, bcxstmrep, &
     &                 bprevstandstem, bprevstandleaflive, bprevstandleafdead, bprevstandstore, &
     &                 bprevflatstem, bprevflatleaf, bprevflatstore,    &
     &                 bprevmshoot, bprevbgstemz,                       &
     &                 bprevrootstorez, bprevrootfiberz,                &
     &                 bprevht, bprevzshoot, bprevstm, bprevrtd,        &
     &                 bprevdayap, bprevhucum, bprevrthucum,            &
     &                 bprevgrainf, bprevchillucum,      &
     &                 bprevdayspring, bprevdayleafon, bprevdayleafoff, &
     &                 daysim, bcdayspring, bcdayleafon, bcdayleafoff, &
     &                 bczloc_regrow, &
     &                 bgmstandstem, bgmstandleaf, bgmstandstore,       &
     &                 bgmflatstem, bgmflatleaf, bgmflatstore,          &
     &                 bgmbgstemz,                                      &
     &                 bgzht, bgdstm, bgxstmrep, bggrainf )

!     + + + PURPOSE + + +
!     This is the main program for implementing the crop growth calculations.

      use weps_cmdline_parms, only: frac_frst_mass_lost
      use datetime_mod, only: get_psim_doy, get_psim_juld
      use file_io_mod, only: luocrop, luoshoot
      use constants, only: u_mgtokg
      use crop_data_struct_defs, only: am0cfl
      use crop_climate_mod, only: huc1, freezeharden, chillunit_cum, warmday_cum, coldday_cum
      use climate_input_mod, only: cli_day
      use solar_mod, only: amalat, civilrise, daylen
      use solar_mod, only: N_spring_eqx, N_summer_sol, N_fall_eqx, N_winter_sol
      use solar_mod, only: S_spring_eqx, S_summer_sol, S_fall_eqx, S_winter_sol

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer bnslay
      real bszlyd(*)
      real bc0ck, bcgrf, bcehu0, bczmxc
      character*(80) bc0nam
      integer bc0idc
      real bcxrow
      real bczmrt, bctmin, bctopt
      real bc0fd1, bc0fd2
      real cc0fd1, cc0fd2, bc0bceff
      real bc0alf, bc0blf, bc0clf, bc0dlf, bc0arp, bc0brp
      real bc0crp, bc0drp, bc0aht, bc0bht
      real bc0sla, bc0hue, bctverndel
      real bhtsmx(*), bhtsmn(*)
      real bhfwsf
      integer bchyfg
      real bcthum, bcdpop, bcdmaxshoot
      real bc0storeinit, bcfshoot
      real bcfleafstem, bc0shoot
      real bc0diammax, bc0ssa, bc0ssb
      real bcfleaf2stor, bcfstem2stor, bcfstor2stor
      real bcyld_coef, bcresid_int, bcxstm
      real bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmtotshoot, bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bczshoot, bcdstm, bczrtd
      integer bcdayap, bcdayam
      double precision bcthucum
      double precision bctrthucum
      real bcgrainf, bczgrowpt
      double precision bcleafareatrend, bcstemmasstrend
      double precision bctwarmdays
      double precision bctcolddays
      double precision bctchillucum
      double precision bcthardnx
      double precision bcthu_shoot_beg, bcthu_shoot_end
      double precision bcmtotleaf
      double precision bcthu_leaf_beg, bcthu_leaf_end
      double precision bcxstmrep
      real bprevstandstem, bprevstandleaflive, bprevstandleafdead, bprevstandstore
      real bprevflatstem, bprevflatleaf, bprevflatstore
      real bprevmshoot, bprevbgstemz(*)
      real bprevrootstorez(*), bprevrootfiberz(*)
      real bprevht, bprevzshoot, bprevstm, bprevrtd
      integer bprevdayap
      double precision bprevhucum, bprevrthucum
      real bprevgrainf
      double precision bprevchillucum
      integer bprevdayspring, bprevdayleafon, bprevdayleafoff
      logical bm0cif
      real    bcbaf
      integer daysim, bcdayspring, bcdayleafon, bcdayleafoff
      real    bczloc_regrow
      real    bgmstandstem, bgmstandleaf, bgmstandstore
      real    bgmflatstem, bgmflatleaf, bgmflatstore
      real    bgmbgstemz(*)
      real    bgzht, bgdstm
      double precision bgxstmrep
      real bggrainf

!     + + + ARGUMENT DEFINITIONS + + +

!     bnslay - number of soil layers
!     bc0alf - leaf partitioning parameter
!     bc0arp - rprd partitioning parameter
!     bc0aht - height s-curve parameter
!     bc0blf - leaf partitioning parameter
!     bc0brp - rprd partitioning parameter
!     bc0bht - height s-curve parameter
!     bc0clf - leaf partitioning parameter
!     bc0crp - rprd partitioning parameter
!     bsfcec     - cation exchange capacity (cmol/kg)
!     bc0ck  - light extinction coeffficient (fraction)
!     bc0dlf - leaf partitioning parameter
!     bc0drp - rprd partitioning parameter
!     bc0fd1 - minimum temperature below zero (C)
!     cc0fd1 - fraction of biomass lost each day due to frost
!     bc0fd2 - minimum temperature below zero (C)
!     cc0fd2 - fraction of biomass lost each day due to frost
!     bczmxc - maximum potential plant height (m)
!     bc0hue - relative heat unit for emergence (fraction)
!     bctverndel - thermal delay coefficient pre-vernalization
!     bc0idc - crop type:annual,perennial,etc
!     bc0nam - crop name
!     bcxrow - Crop row spacing (m)
!     bczmrt - maximum root depth
!     bc0sla - specific leaf area (cm^2/g)
!     bsfsmb     - sum of bases (cmol/kg)
!     bctmin - base temperature (deg. C)
!     bctopt - optimum temperature (deg. C)
!     bc0bceff - biomass conversion efficiency (kg/ha/MJ/m^2)
!     bszlyd - depth from top of soil to botom of layer, m
!     bcbaf  - biomass adjustment factor
!     bcyraf - yield to biomass ratio adjustment factor
!     bhtsmx - daily maximum soil temperature by layer (deg.C)
!     bhtsmn - daily minimum soil temperature by layer (deg.C)
!     bcthum - potential heat units for crop maturity (deg. C)
!     bcdpop - Crop seeding density (#/m^2)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bc0storeinit - db input, crop storage root mass initialzation (mg/plant)
!     bcfshoot - crop ratio of shoot diameter to length
!     bcfleafstem - crop leaf to stem mass ratio for shoots
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bc0diammax - crop maximum plant diameter (m)
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bcfleaf2stor - fraction of assimilate partitioned to leaf that is diverted to root store
!     bcfstem2stor - fraction of assimilate partitioned to stem that is diverted to root store
!     bcfstor2stor - fraction of assimilate partitioned to standing storage (reproductive)
!                    that is diverted to root store
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcxstm - Crop stem diameter (m)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaflive - crop live standing leaf mass (kg/m^2)
!     bcmstandleafdead - crop dead standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a unique pool
!                since this mass is destributed into below ground stem and
!                standing stem as each increment of the shoot is added
!     bcmtotshoot - total mass released from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of shoot emergence heat units
!     bcmbgstemz - crop stem mass below soil surface by soil layer (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bczht  - Crop height (m)
!     bczshoot - length of actively growing shoot from root biomass (m)
!     bcdstm - Number of crop stems per unit area (#/m^2)
!     bczrtd  - Crop root depth (m)
!     bcdayap - number of days of growth completed since crop planted
!     bcdayam - number of days since crop matured
!     bprevdayap - number of days of growth completed since crop planted
!     bcthucum - crop accumulated heat units
!     bctrthucum - accumulated root growth heat units (degree-days)
!     bcgrainf - internally computed reproductive grain fraction
!     bczgrowpt - depth in the soil of the gowing point (m)
!     bcleafareatrend - direction in which leaf area is trending.
!                       Saves trend even if leaf area is static for long periods.
!     bcstemmasstrend - direction in which stem mass is trending.
!                        Saves trend even if stem mass is static for long periods.
!     bctwarmdays - number of days that the daily average temperature
!                   has been above the minimum growth temperature with decay
!     bctcolddays - number of days that the daily average temperature
!                   has been below the minimum growth temperature with decay
!     bctchillucum - accumulated chilling units (days)
!     bcthardnx - hardening index for winter annuals (range from 0 t0 2)
!     bcthu_shoot_beg - heat unit index (fraction) for beginning of shoot grow from root storage period
!     bcthu_shoot_end - heat unit index (fraction) for end of shoot grow from root storage period
!     bcmtotleaf - total mass released from root storage biomass (kg/m^2)
!                  in the period from beginning to completion of leaf emergence heat units
!     bcthu_leaf_beg - heat unit index (fraction) for beginning of leaf emergence from root storage period
!     bcthu_leaf_end - heat unit index (fraction) for end of leaf emergence from root storage period
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai

!     daysim   - day of the simulation
!     bcdayspring - day of year in which a winter annual/perennial releases stored growth
!     bcdayleafon   ! day of year in which a perennial begins to grow new leaves/needles
!     bcdayleafoff  ! day of year in which a perennial lost it's leaves (Deciduous all/ Conifer dead needles)
!     bczloc_regrow - location of regrowth point (+ on stem, 0 or negative from crown at or below surface) (m)
!     bgmstandstem - crop standing stem mass (kg/m^2)
!     bgmstandleaf - crop standing leaf mass (kg/m^2)
!     bgmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bgmflatstem  - crop flat stem mass (kg/m^2)
!     bgmflatleaf  - crop flat leaf mass (kg/m^2)
!     bgmflatstore - crop flat storage mass (kg/m^2)
!     bgmbgstemz  - crop buried stem mass by layer (kg/m^2)
!     bgzht  - Crop height (m)
!     bgdstm - Number of crop stems per unit area (#/m^2)
!            - It is computed by taking the tillering factor
!              times the plant population density.
!     bgxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     bgzrtd  - Crop root depth (m)
!     bggrainf - internally computed grain fraction of reproductive mass

!     + + + LOCAL VARIABLES + + +
      integer :: jd     ! simulation day of year
      integer :: spring_eqx
      integer :: summer_sol
      integer :: fall_eqx
      integer :: winter_sol

      integer lay
      double precision root_store_rel, pot_stems, pot_leaf_mass
      double precision vern_delay, photo_delay
      double precision trend
      double precision hu_delay
      integer regrowth_flg
      double precision frst, ffa, ffw
      double precision lost_mass
      real :: hrlt, hrlty   ! length of day in hours for today and yesterday
      double precision :: hui           ! heat unit amalat, jdindex
      double precision :: huiy          ! heat unit index for yesterday
      double precision :: huirt         ! root growth heat unit index
      double precision :: huirty        ! root growth heat unit index yesterday
      double precision :: xw            ! absolute value of minimum temperature
      double precision :: froz_mass     ! mass of living tissue that died today
      double precision :: huc

      double precision :: u_bcmtotshoot
      double precision :: u_bcdstm

      integer :: pjuld   ! present julian day for subregion

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     root_store_rel - root storage which could be released for regrowth
!     pot_stems - potential number of stems which could be released for regrowth
!     pot_leaf_mass - potential leaf mass which could be released for regrowth.
!     chilluv - effective vernalization days required to complete vernalization
!     vern_delay - reduction in heat unit accumulation based on vernalization
!     photo_delay - reduction in heat unit accumulation based on photoperiod
!     hu_delay - combined reduction in heat unit accummulation
!     trend - test computation for trend direction of living leaf area
!     ffa - frost loss factor (ratio)
!     ffw - leaf weight reduction factor (ratio)

!     regrowth_flg - used to record changes is regrowth conditions day by day

!     + + + LOCAL PARAMETERS + + +
      double precision chilluv
      double precision shoot_delay
      double precision verndelmax
      double precision dev_floor
      !double precision max_photo_per
      double precision spring_trig
      double precision hard_spring
      parameter(chilluv = 50.0d0)
      parameter(shoot_delay = 7.0d0)
      parameter(verndelmax = 0.04d0)
      parameter(dev_floor = 0.01d0)
      !parameter(max_photo_per = 20d0)
      parameter(spring_trig = 0.29d0)
      parameter(hard_spring = 1.0d0)

      double precision hu_leaf_days
      parameter(hu_leaf_days = 7.0d0)

      !double precision bctphotodel
      !parameter( bctphotodel = 0.0055d0)

      integer shoot_flg
      parameter( shoot_flg = 0)

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     chilluv - total of chill units require for vernalization (deg C)
!     shoot_delay - number of days minimum temperature must be above base
!                   crop growth temperature for shoot growth to occur
!     verndelmax - maximum value of vernalization delay parameter
!                  (see actverndel definition in include file)
!     dev_floor - minimum development rate fraction allowed (1-full rate, 0-no development)
!     max_photo_per - photo period where maximum development rate occurs (hours)
!     spring_trig - heat units ratio to spring allowing release of winter annual crown storage
!     hard_spring - hardening index threshold for spring growth breakout.

!     shoot_flg - used to control the behavior of the shootnum subroutine
!             0 - returns the shoot number constrained by bcdmaxshoot
!     hu_leaf_days - number of days of optimum heat units to get full leaf emergence

!     + + + END OF SPECIFICATIONS + + +

      ! set present julian day for subregion to index into cli_day array
      pjuld = get_psim_juld(isr)

      ! day of year
      jd = get_psim_doy(isr)

      ! convert single to double
      u_bcmtotshoot = dble(bcmtotshoot)
      u_bcdstm = dble(bcdstm)

      ! initialize growth variables when crop is planted
      ! bm0cif is flag to initialize crop at start of planting
      if (bm0cif) then
          if (am0cfl(isr) .ge. 1) then
              ! put double blank lines in daily files to create growth blocks
              write(luocrop(isr),*)   ! crop.out
              write(luocrop(isr),*)   ! crop.out
              write(luoshoot(isr),*)  ! shoot.out
              write(luoshoot(isr),*)  ! shoot.out
          end if

          bm0cif = .false.  !turn off after initialization is complete
      end if

      ! calculate day length
      hrlty = daylen(amalat, jd-1, civilrise)
      hrlt = daylen(amalat, jd, civilrise)

      ! reduce green leaf mass in freezing weather

      ! set heat unit index at beginning of the day
      huiy = min(1.0d0, bcthucum / bcthum)

      if( ((bcmstandleaflive + bcmstandleafdead) .gt. 0.0) .and. (dble(bhtsmn(1)) .lt. -2.0d0) ) then
          ! use daily minimum soil temperature of first layer to account for snow cover effects
          xw = abs( dble(bhtsmn(1)) )
          ! calculates Frost damage
          frst = freeze_damage( bc0fd1, cc0fd1, bc0fd2, cc0fd2, xw )

          ! is it before or after scenescence?
          if ( huiy .lt. (bcehu0-bcehu0*.1)) then
              ! before scenescence, frost killed mass is fragile and a fraction disappears
              ffa = 1.0d0 - frst
              ffw = 1.0d0 - frst * frac_frst_mass_lost
              lost_mass = (bcmstandleaflive + bcmstandleafdead) * (1.0d0 - ffw)

              ! reduce green leaf area due to frost damage (10/1/99)
              froz_mass = bcmstandleaflive * frst
              bcmstandleaflive = bcmstandleaflive - froz_mass
              bcmstandleafdead = bcmstandleafdead + froz_mass*(1.0d0-frac_frst_mass_lost)
          else
              ! after scenescence, frost killed mass is tougher and is not lost immediately
              ! reduce green leaf area due to frost damage (9/22/2003)
              froz_mass = bcmstandleaflive * frst
              bcmstandleaflive = bcmstandleaflive - froz_mass
              bcmstandleafdead = bcmstandleafdead + froz_mass
              lost_mass = 0.0d0
          end if
      else
          frst = 0.0d0
          lost_mass = 0.0d0
      endif

      ! set trend direction for living leaf area from external forces
      trend = bcmstandleaflive - bprevstandleaflive
      if ((trend .ne. 0.0d0) &
          .and. ((huiy .gt. bc0hue) .or. (bc0idc.eq.8))) &
          then  ! trend non-zero and (heat units past emergence or staged crown release crop)
          bcleafareatrend = trend
      end if

      ! set trend direction for above ground stem mass from external forces
      trend = dble(bcmstandstem) + dble(bcmflatstem) - dble(bprevstandstem) - dble(bprevflatstem)
      if ((trend .ne. 0.0d0) &
          .and. ((huiy .gt. bc0hue) .or. (bc0idc.eq.8))) &
          then  ! trend non-zero and (heat units past emergence or staged crown release crop)
          bcstemmasstrend = trend
      end if

      ! check for consecutive "warm" days based on daily average temperature
      call warmday_cum( bctwarmdays, bctmin, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn )

      ! check for consecutive "cold" days based on daily average temperature
      call coldday_cum( bctcolddays, bctmin, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn )

      ! accumulate chill units
      call chillunit_cum(bctchillucum, cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn)

      ! calculate freeze hardening index 
      call freezeharden(bcthardnx, dble(bhtsmx(1)), dble(bhtsmn(1)))

      ! check crop type for shoot growth action
      regrowth_flg = -1
      if(    (bcfleaf2stor .gt. 0.0) &
        .or. (bcfstem2stor .gt. 0.0) &
        .or. (bcfstor2stor .gt. 0.0) ) then

        if( (bc0idc.eq.2) .or. (bc0idc.eq.5) ) then
          ! check winter annuals for completion of vernalization,
          ! warming and spring day length 
          if( bczgrowpt .le. 0.0 ) then
           ! remember, negative number means above ground
           regrowth_flg = 1
           if( bctchillucum .ge. chilluv ) then
            regrowth_flg = 2
            if( bctwarmdays .ge. shoot_delay*bctverndel/verndelmax) then
             regrowth_flg = 3
             if( bcthardnx .lt. hard_spring ) then
              regrowth_flg = 4
              ! vernalized and ready to grow in spring
              bcthu_shoot_beg = huiy
              bcthu_shoot_end = huiy + bc0hue
              call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot, &
                   bcdmaxshoot, u_bcmtotshoot, bcmrootstorez, u_bcdstm )
              ! eliminate diversion of biomass to crown storage
              bcfleaf2stor = 0.0
              bcfstem2stor = 0.0
              bcfstor2stor = 0.0
              ! turn off freeze hardening
              bcthardnx = 0.0d0
              ! set day of year on which transition took place
              bcdayspring = jd
             end if
            end if
           end if
          end if

        else if( bc0idc.eq.7 ) then
          ! bi-annuals and perennials with tuber dormancy don't need
          ! either of these checks. Doing nothing here prevents
          ! resprouting after defoliation

        else if( (bc0idc.eq.9) .or. (bc0idc.eq.10) .or. (bc0idc.eq.11) .or. (bc0idc.eq.12) ) then
          ! bush/tree crops with annual cycle without replanting
          ! 9 - Deciduous
          ! 10 - Conifer
          ! 11 - mixed Deciduous and Conifer
          ! 12 - Deciduous with stump regrowth

          ! set winter solstice based on latitude
          if( amalat .gt. 0.0d0 ) then
            spring_eqx = N_spring_eqx
            summer_sol = N_summer_sol
            fall_eqx = N_fall_eqx
            winter_sol = N_winter_sol
          else
            spring_eqx = S_spring_eqx
            summer_sol = S_summer_sol
            fall_eqx = S_fall_eqx
            winter_sol = S_winter_sol
          end if

          ! check for regrowth from a stump
          if( bc0idc.eq.12 ) then
            regrowth_flg = 0
            ! Stump regrowth is possible
            if( bczht .lt. (0.1*bprevht) ) then
              ! tree has been cut close to the ground
              regrowth_flg = 1
              if( bctwarmdays .ge. shoot_delay ) then
                ! enough warm days to start regrowth
                regrowth_flg = 2
                if( huiy .ge. bc0hue ) then
                  ! heat units past emergence
                  regrowth_flg = 3
                  ! find out how much root store can be released for regrowth
                  call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot, &
                     bcdmaxshoot, root_store_rel, bcmrootstorez, pot_stems)
                  ! reset growth clock 
                  bcthucum = 0.0d0
                  huiy = 0.0d0
                  bcthu_shoot_beg = 0.0d0
                  bcthu_shoot_end = dble(bc0hue)
                  ! reset shoot grow configuration
                  if ( bczloc_regrow .gt. 0.0 ) then
                      ! regrows from stem, stem does not become residue
                      ! note, flat leaves are dead leaves, no storage in shoot.

                      ! testing shows that this is not what is intended
                      !bcmshoot = bcmstandstem +bcmflatstem +bcmstandleaf
                      !do lay = 1, bnslay
                      !    bcmshoot = bcmshoot + bcmbgstemz(lay)
                      !end do
                      ! shoot grows from stem regrow location using root reserves
                      bcmshoot = 0.0
                  else
                      ! regrows from crown, stem becomes residue
                      bgmstandstem = bcmstandstem
                      bgmstandleaf = bcmstandleaflive + bcmstandleafdead
                      bgmstandstore = bcmstandstore
                      bgmflatstem = bcmflatstem
                      bgmflatleaf = bcmflatleaf
                      bgmflatstore = bcmflatstore
                      do lay = 1, bnslay
                          bgmbgstemz(lay) = bcmbgstemz(lay)
                      end do
                      bggrainf = bcgrainf
                      bgzht = bczht
                      bgdstm = u_bcdstm
                      bgxstmrep = bcxstmrep
                      ! reset crop values to indicate new growth cycle
                      bcmshoot = 0.0
                      bcmstandstem = 0.0
                      bcmstandleaflive = 0.0
                      bcmstandleafdead = 0.0
                      bcmstandstore = 0.0
                      bcmflatstem = 0.0
                      bcmflatleaf = 0.0
                      bcmflatstore = 0.0
                      do lay = 1, bnslay
                          bcmbgstemz(lay) = 0.0
                      end do
                      bcgrainf = 0.0
                      bczht = 0.0
                  end if
                  u_bcmtotshoot = root_store_rel
                  u_bcdstm = pot_stems
                end if
              end if
            end if
          end if

          ! check for spring and leaf appearance
          if( huiy .ge. bc0hue ) then
            ! heat units past emergence
            if(     (hrlty .lt. hrlt) &
              ! days lengthening (ie. spring)
              .and. (bcdayleafon .eq. 0) ) then
              ! spring not yet triggered
              if( bctwarmdays .ge. shoot_delay) then
                ! consecutive warm days meets threshold
                ! drop any remaining reproductive into flat residue pool
                bgmflatstore = bcmflatstore + bcmstandstore
                ! reset crop values
                bcmstandstore = 0.0
                bcmflatstore = 0.0
                ! new leaves start to appear
                bcmtotleaf = total_leaf( bnslay, bcmrootstorez )
                if( (bc0idc .eq. 9) .or. (bc0idc .eq. 11) .or. (bc0idc .eq. 12) ) then
                  ! reset heat units
                  bcthucum = 0.0d0
                  huiy = 0.0d0
                end if
                bcthu_leaf_beg = huiy
                bcthu_leaf_end = (bcthucum + (hu_leaf_days * (bctopt-bctmin))) / bcthum
                ! set day of year on which transition took place
                bcdayleafon = jd
                ! reset triggers
                bctcolddays = 0.0d0
                bcdayleafoff = 0
              end if
            end if
          end if

          ! check for fall conditions and leaf drop
          if( (hrlty .gt. hrlt) &
            ! days shortening (ie. fall)
            .and. (bcdayleafoff .eq. 0)  ) then
            ! fall not yet triggered
            if( jd .ge. fall_eqx ) then
              ! at least the first day of fall
              if(    (bctcolddays .ge. shoot_delay) &  ! enough cold to trigger leaf drop
                .or. (jd .eq. winter_sol) &         ! always drop leaves by winter solstice
                ) then
                ! cold days meet threshold
                if( (bc0idc .eq. 9) .or. (bc0idc .eq. 12) ) then
                  ! Deciduous, drop all leaves
                  ! drop leaves into flat residue pool
                  bgmflatleaf = bcmflatleaf + bcmstandleaflive + bcmstandleafdead
                  ! reset crop values
                  bcmstandleaflive = 0.0
                  bcmstandleafdead = 0.0
                  bcmflatleaf = 0.0
                  ! set heat units to mature
                  bcthucum = bcthum
                else if( bc0idc .eq. 10 ) then
                  ! Evergreen, drop dead leaves
                  ! drop leaves into flat residue pool
                  bgmflatleaf = bcmflatleaf + bcmstandleafdead
                  ! reset crop values
                  bcmstandleafdead = 0.0
                  bcmflatleaf = 0.0
                  ! reset heat units (use vernalization delay)
                  bcthucum = 0.0d0
                  huiy = 0.0d0
                else if( bc0idc .eq. 11 ) then
                  ! Mixed Deciduous and Evergreen, default 50% tree mix
                  ! drop 50% leaf mass to simulate Deciduous leaf drop change in cover
                  bgmflatleaf = bcmflatleaf + bcmstandleaflive * 0.5d0 + bcmstandleafdead * 0.5d0
                  ! reset crop values
                  bcmstandleaflive = bcmstandleaflive * 0.5d0
                  bcmstandleafdead = bcmstandleafdead * 0.5d0
                  ! drop dead evergreen leaves into flat residue pool
                  bgmflatleaf = bgmflatleaf + bcmstandleafdead
                  ! reset crop values
                  bcmstandleafdead = 0.0
                  bcmflatleaf = 0.0
                  ! reset heat units (use vernalization delay)
                  bcthucum = 0.0d0
                  huiy = 0.0d0
                end if
                ! reset spring trigger values
                bcmtotleaf = 0.0d0
                bcthu_leaf_beg = 0.0d0
                bcthu_leaf_end = 0.0d0
                ! set day of year on which transition took place
                bcdayleafoff = jd
                ! reset triggers
                bctwarmdays = 0.0d0
                bcdayleafon = 0
                ! no shoot grow
                bcthu_shoot_beg = 0.0d0
                bcthu_shoot_end = 0.0d0
              end if
            end if
          end if

        else
          ! check summer annuals and perennials for removal of all (most) leaf mass
          ! perennials with staged crown release also exhibit tuber dormancy
          ! so we really need to wait for spring and not regrow immediately
          ! after it matures, even if it is defoliated, or cut down, but
          ! also regrow in the spring even if not cut down (test 4 to 5 check below)
          regrowth_flg = 0
          if( bcleafareatrend .lt. 0.0) then
           ! last change in leaf area was a reduction
           regrowth_flg = 1
           if( bcmstandleaflive .lt. 0.84d0*bc0storeinit*bcdpop & ! 0.42 * 2 = 0.84
            * u_mgtokg * bcfleafstem / (bcfleafstem + 1.0d0) ) then
            ! below minimum leaf emergence period living leaf mass (which is twice seed leaf mass)
            regrowth_flg = 2
            if( bctwarmdays .ge. shoot_delay ) then
             ! enough warm days to start regrowth
             regrowth_flg = 3
             if( (huiy .ge. bc0hue) &
              ! heat units past emergence
              .or.((bc0idc.eq.8).and.(bcstemmasstrend.lt.0.0)) ) then
              ! staged crown release will regrow without full emergence, but only if stem removed ie harvest
              regrowth_flg = 4
              if( (huiy .lt. 1.0d0) &
               ! not yet mature
               .or. ((bc0idc.eq.3) .or. (bc0idc.eq.6)) &
               ! perennial
               .or. ((bc0idc.eq.8) .and. (hrlty .lt. hrlt)) ) then
               ! staged crown release and days lengthening (ie. spring)
               regrowth_flg = 5
               ! find out how much root store could be released for regrowth
               call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot, &
                     bcdmaxshoot, root_store_rel, bcmrootstorez, pot_stems)
               ! find the potential leaf mass to be achieved with regrowth
               if ( bczloc_regrow .gt. 0.0 ) then
                   pot_leaf_mass = dble(bcmstandleaflive + bcmstandleafdead) + 0.42d0 &
                                 * min(root_store_rel, u_bcmtotshoot) &
                                 * dble(bcfleafstem) / (dble(bcfleafstem) + 1.0d0)
               else
                   pot_leaf_mass = 0.42d0 * root_store_rel &
                                 * dble(bcfleafstem) / (dble(bcfleafstem) + 1.0d0)
               end if
               ! is present living leaf mass less than leaf mass from storage regrowth
               if( dble(bcmstandleaflive) .lt. pot_leaf_mass ) then
                  regrowth_flg = 6
                  ! regrow possible from shoot for perennials, annuals.
                  ! reset growth clock 
                  bcthucum = 0.0d0
                  huiy = 0.0d0
                  bcthu_shoot_beg = 0.0d0
                  bcthu_shoot_end = dble(bc0hue)
                  bcdayam = 0
                  ! allow vernalization to start over (bluegrass uses this)
                  bctchillucum = 0.0
                  ! reset shoot grow configuration
                  if ( bczloc_regrow .gt. 0.0 ) then
                      ! regrows from stem, stem does not become residue
                      ! note, flat leaves are dead leaves, no storage in shoot.

                      ! testing shows that this is not what is intended
                      !bcmshoot = bcmstandstem +bcmflatstem +bcmstandleaflive + bcmstandleafdead
                      !do lay = 1, bnslay
                      !    bcmshoot = bcmshoot + bcmbgstemz(lay)
                      !end do
                      ! shoot grows from stem regrow location using root reserves
                      bcmshoot = 0.0
                      u_bcmtotshoot = min(root_store_rel, u_bcmtotshoot)
                  else
                      ! regrows from crown, stem becomes residue
                      bgmstandstem = bcmstandstem
                      bgmstandleaf = bcmstandleaflive + bcmstandleafdead
                      bgmstandstore = bcmstandstore
                      bgmflatstem = bcmflatstem
                      bgmflatleaf = bcmflatleaf
                      bgmflatstore = bcmflatstore
                      do lay = 1, bnslay
                          bgmbgstemz(lay) = bcmbgstemz(lay)
                      end do
                      bggrainf = bcgrainf
                      bgzht = bczht
                      bgdstm = u_bcdstm
                      bgxstmrep = bcxstmrep
                      ! reset crop values to indicate new growth cycle
                      bcmshoot = 0.0
                      bcmstandstem = 0.0
                      bcmstandleaflive = 0.0
                      bcmstandleafdead = 0.0
                      bcmstandstore = 0.0
                      bcmflatstem = 0.0
                      bcmflatleaf = 0.0
                      bcmflatstore = 0.0
                      do lay = 1, bnslay
                          bcmbgstemz(lay) = 0.0
                      end do
                      bcgrainf = 0.0
                      bczht = 0.0
                      u_bcmtotshoot = root_store_rel
                      u_bcdstm = pot_stems
                  end if
               end if
              end if
             end if
            end if
           end if
          end if
        end if
      else
        bcthardnx = 0.0d0
      end if

      ! calculate growing degree days
      ! set default heat unit delay value
      hu_delay = 1.0
      if( (bcthum .le. 0.0) .or. (u_bcdstm .le. 0.0) ) then
          ! always keep this invalid plant in first stage growth
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow
          huiy = 0.0d0
          hui = 0.0d0
      else
          ! previous day heat unit index
          huirty = bctrthucum / bcthum
          ! check for growth completion
          if( huiy .lt. 1.0d0 ) then
              ! accumulate additional for today
              ! check for emergence status
              if( (huiy .ge. bc0hue).and. (huiy .lt. spring_trig) ) then
                  ! emergence completed, account for vernalization and
                  ! photo period by delaying development rate until chill
                  ! units completed and spring trigger reached
                  vern_delay = 1.0d0-bctverndel*(chilluv-bctchillucum)
                  !vern_delay = 1.0        ! delay disabled
                  !photo_delay = 1.0-bctphotodel*(max_photo_per-hrlt)**2
                  photo_delay = 1.0d0       ! delay disabled
                  hu_delay =  max(dev_floor,min(vern_delay,photo_delay))
              end if
              ! accumulate heat units using set heat unit delay
              huc = huc1(cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, bctopt,bctmin)
              bcthucum = bcthucum + huc * hu_delay

              ! root depth growth heat units
              bctrthucum = bctrthucum + huc
              ! do not cap this for annuals, to allow it to continue
              ! root mass partition is reduced to lower levels after the
              ! first full year. Out of range is capped in the function
              ! in growth.for
              ! bctrthucum = min(bctrthucum, bcthum)
              ! calculate heat unit index
              hui = min(1.0d0, bcthucum / bcthum)
              huirt = bctrthucum / bcthum
          else
              hui = huiy
              huirt = huirty
          end if
      endif

      bcmtotshoot = u_bcmtotshoot
      bcdstm = u_bcdstm

      if( (huiy .lt. 1.0d0) .and. (bcdstm .gt. 0.0)) then
          ! crop growth not yet complete
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow
          ! increment day after planting counter since growth happens same day
          bcdayap = bcdayap + 1

          ! seedling, transplant initialization and winter annual shoot growth
          ! calculations using root reserves
          if(       (huiy .lt. bcthu_shoot_end)                         &
     &        .and. (hui  .ge. bcthu_shoot_beg) ) then

              ! daily shoot growth
              call shoot_grow( isr, bnslay, bszlyd, bcdpop,             &
     &                 bczmxc, bcfleafstem,                             &
     &                 bcfshoot, bc0ssa, bc0ssb, bc0diammax,            &
     &                 hui, huiy, bcthu_shoot_beg, bcthu_shoot_end,     &
     &                 bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore, &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bczgrowpt, bc0nam,                  &
     &                 bchyfg, bcyld_coef, bcresid_int, bcgrf,          &
     &                 daysim, bcdayap )
          end if

          if(     (huiy .lt. bcthu_leaf_end) &
            .and. (hui  .gt. bcthu_leaf_beg) ) then
            ! daily leaf emergence
            call leaf_emerge( bnslay, bcdpop, hui, huiy, bcthu_leaf_beg, bcthu_leaf_end, &
                              bcmstandleaflive, bcmtotleaf, bcmrootstorez )
          end if

          if(       (huiy .lt. bcthu_shoot_end)                         &
     &        .and. (hui .ge. bcthu_shoot_end) ) then
              ! shoot growth completed on this day
              ! move growing point to regrowth depth after shoot growth complete
              ! remember, a negative number is above ground
              bczgrowpt = ( - bczloc_regrow )
              if (am0cfl(isr) .ge. 1) then
                  ! single blank line to separate shoot growth periods
                  write(luoshoot(isr),*)  ! shoot.out
              end if
          end if

          ! calculate plant growth state variables
          call growth( isr, bnslay, bszlyd, bc0ck, bcgrf,               &
     &                 bcehu0, bczmxc, bc0idc, bc0nam,                  &
     &                 frst, lost_mass, bcxrow, bc0diammax,             &
     &                 bczmrt, bctmin, bctopt, bc0bceff,                &
     &                 bc0alf, bc0blf, bc0clf, bc0dlf, bc0arp,          &
     &                 bc0brp, bc0crp, bc0drp,                          &
     &                 bc0aht, bc0bht, bc0ssa, bc0ssb,                  &
     &                 bc0sla,                            &
     &                 bhfwsf,                                          &
     &                 hui, huiy, huirt, huirty, hu_delay, bcthardnx, &
     &                 bcbaf, bchyfg,                                   &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int,                         &
     &                 bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore, &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bcmbgstemz,                                      &
     &                 bczht, bcdstm, bczrtd,              &
     &                 bcdayap, bcgrainf, bcdpop, daysim, regrowth_flg, &
     &                 bc0shoot, bcdmaxshoot, bctwarmdays )

          ! set trend direction for living leaf area
          trend = bcmstandleaflive - bprevstandleaflive
          if ((trend .ne. 0.0)                                          &
     &        .and. ((hui .gt. bc0hue) .or. (bc0idc.eq.8))) &
     &        then  ! trend non-zero and (heat units past emergence or staged crown release crop)
              bcleafareatrend = trend
          end if
          ! set trend direction for above ground stem mass from growth
          trend = bcmstandstem + bcmflatstem                            &
     &          - bprevstandstem - bprevflatstem
          if ((trend .ne. 0.0)                                          &
     &        .and. ((hui .gt. bc0hue) .or. (bc0idc.eq.8))) &
     &        then  ! trend non-zero and (heat units past emergence or staged crown release crop)
              bcstemmasstrend = trend
          end if

          ! set saved values of crop state variables for comparison next time
          bprevstandstem = bcmstandstem
          bprevstandleaflive = bcmstandleaflive
          bprevstandleafdead = bcmstandleafdead
          bprevstandstore = bcmstandstore
          bprevflatstem = bcmflatstem
          bprevflatleaf = bcmflatleaf
          bprevflatstore = bcmflatstore
          bprevmshoot = bcmshoot
          do lay = 1, bnslay
              bprevbgstemz(lay) = bcmbgstemz(lay)
              bprevrootstorez(lay) = bcmrootstorez(lay)
              bprevrootfiberz(lay) = bcmrootfiberz(lay)
          end do
          bprevht = bczht
          bprevzshoot = bczshoot
          bprevstm = bcdstm
          bprevrtd = bczrtd
          bprevdayap  = bcdayap
          bprevhucum = bcthucum
          bprevrthucum = bctrthucum
          bprevgrainf = bcgrainf
          bprevchillucum = bctchillucum
          bprevdayspring = bcdayspring
          bprevdayleafon = bcdayleafon
          bprevdayleafoff = bcdayleafoff
      else
          ! accumulate days after maturity
          bcdayam = bcdayam + 1

      end if

      if( (hui .ge. 1.0d0) .and. (bcdstm .gt. 0.0)) then

          if( (bc0idc.ge.9) .and. (bc0idc.le.12) ) then
            ! these crops continue until leaf drop or are evergreen
          else
            ! heat units completed, crop leaf mass is non transpiring
            bcmstandleafdead = bcmstandleafdead + bcmstandleaflive
            bcmstandleaflive = 0.0
          end if

          ! check for mature perennial that may re-sprout before fall (alfalfa, grasses)
          if( (bc0idc.eq.3) .or. (bc0idc.eq.6) ) then
              ! check for growing weather and regrowth ready state
                  ! transfer all mature biomass to residue pool
                  ! find number of stems to regrow
                  ! reset heat units to start shoot regrowth
          end if
      end if

      return
    end subroutine cropgrow

    subroutine growth(isr, bnslay, bszlyd, bc0ck, bcgrf,              &
     &                 bcehu0, bczmxc, bc0idc, bc0nam,                  &
     &                 frst, lost_mass, bcxrow, bc0diammax,             &
     &                 bczmrt, bctmin, bctopt, cc0be,                   &
     &                 bc0alf, bc0blf, bc0clf, bc0dlf,                  &
     &                 bc0arp, bc0brp, bc0crp, bc0drp,                  &
     &                 bc0aht, bc0bht, bc0ssa, bc0ssb,                  &
     &                 bc0sla,                           &
     &                 bhfwsf,                                          &
     &                 hui, huiy, huirt, huirty, hu_delay, bcthardnx,   &
     &                 bcbaf, bchyfg,                                   &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int,                         &
     &                 bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore, &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bcmbgstemz,                                      &
     &                 bczht, bcdstm, bczrtd,              &
     &                 bcdayap, bcgrainf, bcdpop, daysim, regrowth_flg, &
     &                 bc0shoot, bcdmaxshoot, bctwarmdays )

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     This subroutine calculates plant height, biomass partitioning,
!     rootmass distribution, rooting depth.

!     + + + KEYWORDS + + +
!     biomass

      use weps_cmdline_parms, only: growth_stress, water_stress_max, winter_ann_root, cook_yield
      use datetime_mod, only: get_psim_juld, get_psim_doy, get_psim_year
      use file_io_mod, only: luocrop
      use constants, only: u_hatom2, u_pi, u_mmtom
      use crop_data_struct_defs, only: am0cfl
      use climate_input_mod, only: cli_day
      use crop_climate_mod, only: temp_stress
      use precision_mod, only: max_real, max_arg_exp

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer bnslay
      real bszlyd(*), bc0ck, bcgrf
      real bcehu0, bczmxc
      integer bc0idc
      character*(80) bc0nam
      double precision frst
      double precision lost_mass
      real bcxrow, bc0diammax
      real bczmrt, bctmin, bctopt, cc0be
      real bc0alf, bc0blf, bc0clf, bc0dlf
      real bc0arp, bc0brp, bc0crp, bc0drp
      real bc0aht, bc0bht, bc0ssa, bc0ssb
      real bc0sla
      real bhfwsf
      double precision hui, huiy
      double precision huirt, huirty
      double precision hu_delay
      double precision bcthardnx
      real bcbaf
      integer bchyfg
      real bcfleaf2stor, bcfstem2stor, bcfstor2stor
      real bcyld_coef, bcresid_int
      real bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bcmbgstemz(*)
      real bczht, bcdstm, bczrtd
      integer bcdayap
      real bcgrainf, bcdpop
      integer daysim, regrowth_flg
      real bc0shoot, bcdmaxshoot
      double precision bctwarmdays


!     + + + ARGUMENT DEFINITIONS + + +
!     bnslay - number of soil layers
!     bszlyd - depth from top of soil to botom of layer, m
!     bc0ck  - extinction coeffficient (fraction)
!     bcgrf  - fraction of reproductive biomass that is yield
!     bcehu0 - relative gdd at start of senescence
!     bczmxc - maximum potential plant height (m)
!     bc0idc - crop type:annual,perennial,etc
!     bc0nam - crop name
!     frst - frost damage factor
!     lost_mass - biomass that decayed (disappeared) from scenescence and freeze damage
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
!     bcmstandleaflive - crop live standing leaf mass (kg/m^2)
!     bcmstandleafdead - crop dead standing leaf mass (kg/m^2)
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
!     bcdayap - number of days of growth completed since crop planted
!     bcgrainf - internally computed grain fraction of reproductive mass
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives the number of stems per plant
!     daysim   - day of the simulation
!     regrowth_flg - used to record changes is regrowth conditions day by day
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bctwarmdays - number of days that the daily average temperature
!                   has been above the minimum growth temperature with decay

!     + + + LOCAL VARIABLES + + +
      double precision par, apar, arg_exp
      double precision pddm, ddm, ddm_rem
      double precision p_rw, p_st, p_lf, p_rp
      double precision drfwt, dlfwt, dstwt, drpwt, drswt
      double precision dstandstem
      double precision pdht
      double precision dht
      double precision hux, ff, ffa, ffw, ffr
      double precision hui0f, pdrd
      double precision gif
      double precision clfwt, clfarea, pdiam, parea, p_lf_rp
      double precision huf, hufy, pchty, pcht
      double precision strs
      double precision ts
      double precision stem_propor, prdy, prd
      double precision eff_lai, trad_lai
      integer i   
      double precision wcg, wmaxd
      double precision wffiber, wfstore
      integer irfiber, irstore
      double precision temp_fiber, temp_store, temp_stem
      double precision wfl(bnslay) !  and weight fraction by layer used to distribute root mass into the soil layers
      double precision za(bnslay)
!      real ppx,ppveg,pprpd ! used with plant population adjustment
      double precision bhfwsf_adj
      double precision temp_sai, temp_stmrep
      double precision adjleaf2stor, adjstem2stor, adjstor2stor
      double precision tempdstm, temptotshoot
      double precision lost_mass_weath  ! lost mass from weathering of senesence material
      double precision senes_mass       ! mass of leaf moved from live to dead (senescence)
      double precision temp_fliveleaf
      integer :: pjuld   ! julian day

!     + + + LOCAL VARIABLE DEFINITIONS + + +
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
!     ffa - leaf senescence factor (ratio)
!     ffw - leaf weight reduction factor (ratio)
!     ffr - fibrous root weight reduction factor (ratio)
!     hui0f - relative gdd at start of scenescence
!     pdrd - potential increment in root length (m)
!     gif  - grain index accounting for development of chaff before grain fill

      ! used with plant population adjustment
!     ppx
!     ppveg
!     pprpd

!     bhfwsf_adj - water stress factor adjusted by biomass adjustment factor

!     clfwt - live leaf dry weight (kg/plant)
!     clfarea - live leaf area (m^2/plant)
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
!     i - array index used in loops
!     wcg - root mass distribution function exponent (see reference at equation)
!     wmaxd - root mass distribution function depth limit parameter

!     drswt - biomass diverted from partitioning to root storage
!     wffiber - total of weight fractions for fibrous roots (normalization)
!     wfstore - total of weight fractions for storage roots (normalization)
!     irfiber - index of deepest soil layer for fibrous roots
!     irstore - index of deepest soil layer for storage roots
!     wfl(bnslay) - weight fraction by layer (distribute root mass into the soil layers)
!     za(bnslay) - soil layer representative depth

!     adjleaf2stor, adjstem2stor, adjstor2stor - adjusted value of bomass diversion
!         to root/crown storage. Factor considered are:
!         - plants freeze hardening index
!         - fullness of storage root reservoir
!     tempdstm - number of stem possible from root stores
!     temptotshoot - amount of storage required from each stem

!     + + + LOCAL PARAMETERS + + +
      integer shoot_flg
      parameter( shoot_flg = 1)

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     shoot_flg - used to control the behavior of the shootnum subroutine
!             1 - returns the shoot number unconstrained by bcdmaxshoot

!     + + + END OF SPECIFICATIONS + + +

      !!!!! START SINGLE PLANT CALCULATIONS !!!!!
      ! calculate single plant effective lai (standing living leaves only)
      clfwt = bcmstandleaflive / bcdpop  ! kg/m^2 / plants/m^2 = kg/plant
      clfarea = clfwt * bc0sla           ! kg/plant * m^2/kg = m^2/plant

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
      pdiam = dble(bc0diammax)
      ! account for row spacing effects
      if( dble(bcxrow) .gt. 0.0d0 ) then
          ! use row spacing and plants maximum reach
          parea = min(bcxrow,pdiam) * min(1.0d0/(bcdpop*bcxrow),pdiam)
      else
          ! this is broadcast, so use uniform spacing
          parea = min( u_pi * pdiam * pdiam /4.0d0, 1.0d0/bcdpop )
      end if

      ! check for valid plant area
      if( parea .gt. 0.0d0 ) then
          eff_lai = clfarea / parea
      else
          eff_lai = 1.0d0
      end if

      !traditional lai calculation for reporting puposes
      trad_lai = clfarea * dble(bcdpop)

      ! simulation julian day index into cli_day
      pjuld = get_psim_juld(isr)
      
      ! Start biomass calculations
      ! cli_day(pjuld)%eirr is total shortwave radiation and a factor of .5 is assumed
      ! to get to the photosynthetically active radiation
      par=0.5d0 * dble(cli_day(pjuld)%eirr)                    ! MJ/m^2   ! C-4

!     calculate intercepted PAR, which is the good stuff less what hits the ground
      apar=par*(1.0d0-exp(-bc0ck*eff_lai))                                             ! C-4

!     calculate potential biomass conversion (kg/plant/day) using
!     biomass conversion efficiency at ambient co2 levels
      ! units: ((m^2)/plant)*(kg/ha)/(MJ/m^2) * (MJ/m^2) / 10000 m^2/ha = kg/plant
      pddm = parea * cc0be * apar / u_hatom2                                          ! C-4

!     biomass adjustment factor applied
      ! apply to both biomass conversion efficiency and water stress factor, see below
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
      bhfwsf_adj = max( dble(water_stress_max), bhfwsf )
      !bhfwsf_adj = 1 !no water stress

!     calculate temperature stress
      ts = temp_stress(cli_day(pjuld)%tdmx, cli_day(pjuld)%tdmn, bctopt, bctmin)

      ! select application of stress functions based on command line flag
      if( growth_stress .eq. 0 ) then
          strs = 1.0d0
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
          strs = 0.0d0
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
          p_rw = (0.4d0 - 0.2d0 * hui)                                            ! C-5
      else
          p_rw = max(0.05d0, (0.4d0 - 0.2d0 * huirt) )                              ! C-5
      end if
      drfwt = ddm * p_rw
      ddm_rem = ddm - drfwt

!     find partitioning factors of the remaining biomass (not fibrous root)
!     calculate leaf partitioning.
      arg_exp = -(hui-bc0clf)/bc0dlf
      if( arg_exp .ge. max_arg_exp ) then
          p_lf = bc0alf+bc0blf/max_real
      else
          p_lf=bc0alf+bc0blf/(1.0d0+exp(-(hui-bc0clf)/bc0dlf))
      end if
      p_lf = max( 0.0d0, min( 1.0d0, p_lf ))

!     calculate reproductive partitioning based on partioning curve
      arg_exp = -(hui-bc0crp)/bc0drp
      if( arg_exp .ge. max_arg_exp ) then
          p_rp = bc0arp+bc0brp/max_real
      else
          p_rp=bc0arp+bc0brp/(1.0d0+exp(-(hui-bc0crp)/bc0drp))
      end if
      p_rp = max( 0.0d0, min( 1.0d0, p_rp ))

      ! normalize leaf and reproductive fractions so sum never greater than 1.0
      p_lf_rp = p_lf + p_rp
      if( p_lf_rp .gt. 1.0d0 ) then
          p_lf = p_lf / p_lf_rp
          p_rp = p_rp / p_lf_rp
          ! set stem partitioning parameter.
          p_st = 0.0d0
      else
          ! set stem partitioning parameter.
          p_st = 1.0d0 - p_lf_rp
      end if

      ! calculate assimate mass increments (kg/m^2)
      dlfwt = ddm_rem * p_lf
      dstwt = ddm_rem * p_st
      drpwt = ddm_rem * p_rp

      ! when a plant has freeze hardened halfway into stage 1, divert any growth to storage
      if( bcthardnx .gt. 0.0d0 ) then
          if( bcthardnx .lt. 0.5d0 ) then
              adjleaf2stor=bcfleaf2stor+(1.0d0-bcfleaf2stor)*(bcthardnx)*2.0d0
              adjstem2stor=bcfstem2stor+(1.0d0-bcfstem2stor)*(bcthardnx)*2.0d0
              adjstor2stor=bcfstor2stor+(1.0d0-bcfstor2stor)*(bcthardnx)*2.0d0
          else
              adjleaf2stor = 1.0d0
              adjstem2stor = 1.0d0
              adjstor2stor = 1.0d0
          end if
      else
          adjleaf2stor = bcfleaf2stor
          adjstem2stor = bcfstem2stor
          adjstor2stor = bcfstor2stor
      end if

       ! check for full regrowth reserve on all but tuber crops
      if( bc0idc .ne. 7 ) then
          ! check for regrowth shoot number possible from root store
          call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot,  &
     &             bcdmaxshoot, temptotshoot, bcmrootstorez, tempdstm )
          ! compare to maximum shoot number
          if( tempdstm .ge. 5.0d0 * bcdmaxshoot * bcdpop ) then
              ! one of these must be non-zero or regrowth will never occur
              adjleaf2stor = 0.0d0
              adjstem2stor = 0.0d0
              adjstor2stor = 0.0000001d0
          end if
      end if

      ! use ratios to divert biomass to root storage
      drswt = dlfwt * adjleaf2stor + dstwt * adjstem2stor + drpwt * adjstor2stor
      dlfwt = dlfwt * (1.0d0-adjleaf2stor)
      dstwt = dstwt * (1.0d0-adjstem2stor)
      drpwt = drpwt * (1.0d0-adjstor2stor)

      ! senescence is done on a whole plant mass basis not incremental mass
      ! This starts senescence before the entered heat unit index for
      ! the start of senscence. For most leaf partitioning functions
      ! the coefficients draw a curve that approaches 1 around -0.5 but
      ! the value at zero, raised to fractional powers is still very small
      hui0f=bcehu0-bcehu0*.1
      if (hui.ge.hui0f) then
          hux=hui-bcehu0
          ff = 1.0d0/(1.0d0+exp(-(hux-bc0clf/2.0d0)/bc0dlf))
          ffa = ff**0.125d0
          ffw = ff**0.0625d0
          ffr = 0.98d0
          ! loss from weathering of leaf mass
          lost_mass_weath = (bcmstandleaflive + bcmstandleafdead) * (1.0 - ffw)
          ! adjust for senescence (done here, not below, so consistent with lost mass amount)
          senes_mass = bcmstandleaflive * (1.0d0 - ffa)
          bcmstandleaflive = bcmstandleaflive - senes_mass
          bcmstandleafdead = bcmstandleafdead + senes_mass
          if( bcmstandleafdead .lt. lost_mass_weath ) then
              lost_mass_weath = bcmstandleafdead
              bcmstandleafdead = 0.0
          else
              bcmstandleafdead = bcmstandleafdead - lost_mass_weath
          endif
          ! loss from weathering of leaf mass added to mass lost to freeze damage
          lost_mass = lost_mass + lost_mass_weath
      else
          ! set a value to be written out
          ffa = 1.0d0
          ffw = 1.0d0
          ffr = 1.0d0
      endif

      ! yield residue relationship adjustment
      if(     (cook_yield .eq. 1)                                       &
     &  .and. (bcyld_coef .gt. 1.0) .and. (bcresid_int .ge. 0.0)        &
     &  .and. ( (bchyfg.eq.0).or.(bchyfg.eq.1).or.(bchyfg.eq.5) ) ) then

          call cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt,    &
     &                   dble(bcmstandstem), dble(bcmstandleaflive + bcmstandleafdead), dble(bcmstandstore), &
     &                   dble(bcmflatstem), dble(bcmflatleaf), dble(bcmflatstore), &
     &                   bcmrootstorez, lost_mass,                      &
     &                   dble(bcyld_coef), dble(bcresid_int), dble(bcgrf) )

      end if

!     added method (different from EPIC) of calculating plant height
!     pht=cummulated potential height,pdht=daily potential height
!     aczht(am0csr) = cummulated actual height
!     adht=daily actual height, bc0aht,bc0bht are
!     height-scurve parameters (formerly lai parameters)
      ! previous day
      hufy = .01d0 + 1.0d0/(1.0d0+exp((huiy-bc0aht)/bc0bht))
      ! today
      huf = .01d0 + 1.0d0/(1.0d0+exp((hui-bc0aht)/bc0bht))

      pchty = min(dble(bczmxc), dble(bczmxc) * hufy)
      pcht = min(dble(bczmxc), dble(bczmxc) * huf)
      pdht = pcht - pchty

      ! calculate stress adjusted height
      if( pddm .gt. 0.0 ) then
        ! potential biomass increase so adjust
        dht = pdht * strs
      else
        dht = 0.0d0
      end if

      ! add mass increment to accumulated biomass (kg/m^2)
      ! all leaf mass added to living leaf in standing pool
      bcmstandleaflive = bcmstandleaflive + dlfwt

      ! divide between standing and flat stem and storage in proportion
      ! to maximum height and maximum radius ratio
      stem_propor = min(1.0d0, 2.0d0 * bczmxc / bc0diammax)
      dstandstem = dstwt * stem_propor
      bcmstandstem = bcmstandstem + dstandstem
      bcmflatstem = bcmflatstem + dstwt * (1.0d0 - stem_propor)

      ! for all but below ground place rp portion in standing storage
      bcmstandstore = bcmstandstore + drpwt * stem_propor
      bcmflatstore = bcmflatstore + drpwt * (1.0d0 - stem_propor)

      ! check for consistency of height, diameter and stem area index.
      ! adjust rate of height increase to keep diameter inside a range.
      call ht_dia_sai( dble(bcdpop), dble(bcmstandstem), dstandstem, &
                       dble(bc0ssa), dble(bc0ssb), dble(bcdstm), &
                       dble(bczht), dht, temp_stmrep, temp_sai )

      ! increment plant height
      bczht = min(bczmxc, bczht + dht)

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
          prdy = min(bczmrt, bczmrt * hufy)
          prd = min(bczmrt, bczmrt * huf)
      else
          prdy = bczmrt *(.01d0 + 1.0d0/(1.0d0 + exp((huirty-bc0aht)/bc0bht)))
          prd = bczmrt * (.01d0 + 1.0d0/(1.0d0 + exp((huirt-bc0aht)/bc0bht)))
      end if
      if( pddm .gt. 0.0d0 ) then
        ! potential biomass increase so adjust
        pdrd = max(0.0d0, prd - prdy)
      else
        pdrd = 0.0d0
      end if

      bczrtd = min(bczmrt, bczrtd + pdrd)
      bczrtd = min(bszlyd(bnslay)*u_mmtom, bczrtd)

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
      wcg = 2.0d0
      wmaxd = max(3.0d0,bczmrt)
      do i = 1,bnslay
          if (i.eq.1) then
              ! calculate depth to the middle of a layer
              za(i) = (bszlyd(i)/2.0d0) * u_mmtom
              ! calculate root distribution function
              if( za(i) .lt. wmaxd ) then
                  wfl(i) = (1.0d0-za(i)/wmaxd)**wcg
              else
                  wfl(i) = 0.0d0
              end if
              wfstore = wfl(i)
              irstore = i
              wffiber = wfl(i)
              irfiber = i
          else
              ! calculate depth to the middle of a layer
              za(i) = (bszlyd(i-1)+(bszlyd(i)-bszlyd(i-1))/2.0d0) * u_mmtom
              ! calculate root distribution function
              if( za(i) .lt. wmaxd ) then
                  wfl(i) = (1.0d0-za(i)/wmaxd)**wcg
              else
                  wfl(i) = 0.0d0
              end if
              if( bczrtd/3.0d0 .gt. za(i)) then
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
      gif=1.0d0 / (1.0d0 + exp(-(hui-0.64d0)/.05d0))
      if (bchyfg.eq.1) then
          bcgrainf = bcgrf * gif
      else
          bcgrainf = bcgrf
      endif

!     the following write statements are for 'crop.out'
!     am0cfl is flag to print crop submodel output
      if (am0cfl(isr) .ge. 1) then
          ! temporary sum for output
          temp_store = 0.0d0
          temp_fiber = 0.0d0
          temp_stem = 0.0d0
          do i = 1, bnslay
              temp_store = temp_store + bcmrootstorez(i)
              temp_fiber = temp_fiber + bcmrootfiberz(i)
              temp_stem = temp_stem + bcmbgstemz(i)
          end do

          if( (bcmstandleaflive + bcmstandleafdead) .gt. 0.0d0 ) then
            temp_fliveleaf = bcmstandleaflive / (bcmstandleaflive + bcmstandleafdead)
          else
            temp_fliveleaf = 1.0d0
          end if

          write(luocrop(isr), 2130) daysim, get_psim_doy(isr), get_psim_year(isr), bcdayap, hui, &
     &                    bcmstandstem, bcmstandleaflive + bcmstandleafdead, bcmstandstore, &
     &                    bcmflatstem, bcmflatleaf, bcmflatstore,       &
     &                    temp_store, temp_fiber, temp_stem,            &
     &                    bcmstandleaflive + bcmstandleafdead + bcmflatleaf, &
     &                    bcmstandstem + bcmflatstem + temp_stem,       &
     &                    bczht, bcdstm, trad_lai, eff_lai, bczrtd,     &
     &                    bcgrainf, ts, bhfwsf, frst, ffa, ffw,         &
     &                    par, apar, pddm, p_rw, p_st, p_lf, p_rp,      &
     &                    stem_propor, pdiam, parea, pdiam/bc0diammax,  &
     &                    1.0-exp(-bc0ck*trad_lai), hu_delay, bcthardnx, temp_sai,  &
     &                    temp_stmrep, regrowth_flg, temp_fliveleaf, &
     &                    bctwarmdays, trim(bc0nam)
      end if

 2130 format(1x,i6,1x,i3,1x,i4,1x,i5,1x,f6.3,12(1x,f7.4),1x,f7.2,       &
     & 3(1x,f7.4),8(1x,f6.3),1x,e12.3, 11(1x,f6.3),2(1x,f8.5),1x,i2,    &
     & 1x,f6.3,1x,f6.2,1x,a)

      return
    end subroutine growth

    subroutine shoot_grow( isr, bnslay, bszlyd, bcdpop,               &
     &                 bczmxc, bcfleafstem,                             &
     &                 bcfshoot, bc0ssa, bc0ssb, bc0diammax,            &
     &                 hui, huiy, bcthu_shoot_beg, bcthu_shoot_end,     &
     &                 bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore, &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bczgrowpt, bc0nam,                  &
     &                 bchyfg, bcyld_coef, bcresid_int, bcgrf,          &
     &                 daysim, bcdayap )


!     + + + PURPOSE + + +

!     + + + KEYWORDS + + +
!     shoot growth

      use weps_cmdline_parms, only: cook_yield
      use datetime_mod, only: get_psim_doy, get_psim_year 
      use file_io_mod, only: luoshoot
      use constants, only: u_mgtokg, u_mmtom
      use crop_data_struct_defs, only: am0cfl

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer bnslay
      real bszlyd(*), bcdpop
      real bczmxc, bcfleafstem
      real bcfshoot, bc0ssa, bc0ssb, bc0diammax
      double precision hui, huiy
      double precision bcthu_shoot_beg, bcthu_shoot_end
      real bcmstandstem, bcmstandleaflive, bcmstandleafdead, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmtotshoot, bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bczshoot, bcdstm, bczrtd
      real bczgrowpt
      character*(80) bc0nam
      integer bchyfg
      real bcyld_coef, bcresid_int, bcgrf
      integer daysim, bcdayap

!     + + + ARGUMENT DEFINITIONS + + +
!     bnslay - number of soil layers
!     bszlyd - depth from top of soil to botom of layer, m
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives the number of stems per plant
!     bczmxc - maximum potential plant height (m)
!     bcfleafstem - crop leaf to stem mass ratio for shoots
!     bcfshoot - crop ratio of shoot diameter to length
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bc0diammax - crop maximum plant diameter (m)
!     hui - heat unit index for today
!     huiy - heat unit index for yesterday
!     bcthu_shoot_beg - heat unit index (fraction) for beginning of shoot grow from root storage period
!     bcthu_shoot_end - heat unit index (fraction) for end of shoot grow from root storage period
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaflive - crop live standing leaf mass (kg/m^2)
!     bcmstandleafdead - crop dead standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)

!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a unique pool
!                since this mass is destributed into below ground stem, above ground
!                standing and flat stem and leaf as each increment of the shoot is added
!     bcmtotshoot - total mass released from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emergence heat units
!     bcmbgstemz - crop stem mass below soil surface by layer (kg/m^2)

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)

!     bczht  - Crop height (m)
!     bczshoot - length of actively growing shoot from root biomass (m)
!     bcdstm - Number of crop stems per unit area (#/m^2)
!     bczrtd - root depth (m)
!     bczgrowpt - depth in the soil of the growing point (m)
!     bc0nam - crop name
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcgrf  - fraction of reproductive biomass that is yield
!     daysim   - day of the simulation
!     bcdayap - number of days of growth completed since crop planted

!     + + + LOCAL VARIABLES + + +
      integer lay
      double precision shoot_hui, shoot_huiy
      double precision fexp_hui, fexp_huiy
      double precision d_shoot_mass, d_stem_mass, d_leaf_mass, d_root_mass
      double precision d_s_root_mass, tot_mass_req, red_mass_rat
      double precision end_root_mass, end_shoot_mass, end_stem_mass
      double precision end_stem_area, end_shoot_len
      double precision yesterday_len
      double precision stem_propor
      double precision ag_stem, bg_stem, flat_stem, stand_stem
      double precision f_root_sum, s_root_sum, avail_mass
      double precision lost_mass
      double precision dlfwt, dstwt, drpwt, drswt

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     lay - index into soil layers for looping
!     shoot_hui - today fraction of heat unit shoot growth index accumulation
!     shoot_huiy - previous day fraction of heat unit shoot growth index accumulation
!     fexp_hui - exponential function evaluated at todays shoot heat unit index
!     fexp_huiy - exponential function evaluated at yesterdays shoot heat unit index
!     d_shoot_mass - mass increment added to shoot for the present day (mg/shoot)
!     d_stem_mass - mass increment added to stem for the present day (mg/shoot)
!     d_leaf_mass - mass increment added to leaf for the present day (mg/shoot)
!     d_root_mass - mass increment added to roots for the present day (mg/shoot)
!     d_s_root_mass - mass increment removed from storage roots for the present day (mg/shoot)
!     tot_mass_req - mass required from root mass for one shoot (mg/shoot)
!     red_mass_rat - ratio of reduced mass available for stem growth to expected mass available
!     end_root_mass - total root mass at end of shoot growth period (mg/shoot)
!     end_shoot_mass - total shoot mass at end of shoot growth period (mg/shoot)
!     end_stem_mass - total stem mass at end of shoot growth period (mg/shoot)
!     end_stem_area - total stem area at end of shoot growth period (m^2/shoot)
!     end_shoot_len - total shoot length at end of shoot growth period (m)
!     yesterday_len - length of shoot yesterday (m)
!     stem_propor - ratio of standing stems mass to flat stem mass
!     ag_stem - above ground stem mass (mg/shoot)
!     bg_stem - below ground stem mass (mg/shoot)
!     flat_stem - flat stem mass (mg/shoot)
!     stand_stem - standing stem mass (mg/shoot)
!     f_root_sum - fibrous root mass sum (total in all layers) (kg/m^2)
!     s_root_sum - storage root mass sum (total in all layers) (kg/m^2)
!     avail_mass - storage root mass sum in (mg/shoot)
!     lost_mass - passed into cook yield, is simply set to zero
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     dstwt - increment in dry weight of stem (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage

!     + + + LOCAL PARAMETERS + + +
      double precision, parameter :: shoot_exp = 2.0D0
      double precision, parameter :: be_stor = 0.7D0
      double precision, parameter :: rootf = 0.4D0

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     shoot_exp - exponent for shape of exponential function
!                 small numbers  go toward straight line
!                 large numbers delay development to end of period
!     be_stor - conversion efficiency of biomass from storage to growth
!     rootf - fraction of biomass allocated to roots when growing from seed

!     + + + FUNCTIONS CALLED + + +
!      real frac_lay

!     + + + END OF SPECIFICATIONS + + +

      ! fraction of shoot growth from stored reserves (today and yesterday)
      shoot_hui = min( 1.0d0, (hui - bcthu_shoot_beg)                     &
     &          / (dble(bcthu_shoot_end) - bcthu_shoot_beg) )
      shoot_huiy = max( 0.0d0, (huiy - bcthu_shoot_beg)                   &
     &           / (bcthu_shoot_end - bcthu_shoot_beg) )

      ! total shoot mass is grown at an exponential rate
      fexp_hui = (exp(shoot_exp*shoot_hui)-1.0) / (exp(shoot_exp)-1)
      fexp_huiy = (exp(shoot_exp*shoot_huiy)-1.0) / (exp(shoot_exp)-1)

      ! sum present storage and fibrous root mass (kg/m^2)
      s_root_sum = 0.0d0
      f_root_sum = 0.0d0
      do lay = 1, bnslay
          s_root_sum = s_root_sum + bcmrootstorez(lay)
          f_root_sum = f_root_sum + bcmrootfiberz(lay)
      end do

      ! calculate storage mass required to grow a single shoot
      ! units: kg/m^2 / ( shoots/m^2 * kg/mg ) = mg/shoot
      tot_mass_req = dble(bcmtotshoot) / (dble(bcdstm) * u_mgtokg)

      ! divide ending mass between shoot and root
      if( f_root_sum .le. bcmshoot ) then   ! this works as long as rootf <= 0.5
          !roots develop along with shoot from same mass
          end_shoot_mass = tot_mass_req * be_stor * (1.0d0-rootf)
          end_root_mass = tot_mass_req * be_stor * rootf
      else
          !roots remain static, while shoot uses all mass from storage
          end_shoot_mass = tot_mass_req * be_stor
          end_root_mass = 0.0d0
      end if

      ! this days incremental shoot mass for a single shoot (mg/shoot)
      d_shoot_mass = end_shoot_mass * (fexp_hui - fexp_huiy)
      d_root_mass = end_root_mass * (fexp_hui - fexp_huiy)

      ! this days mass removed from the storage root (mg/shoot)
      d_s_root_mass = (d_shoot_mass + d_root_mass) / be_stor

      ! check that sufficient storage root mass is available
      ! units: mg/shoot = kg/m^2 / (kg/mg * shoot/m^2)
      avail_mass = s_root_sum  / (bcdstm * u_mgtokg)
      if( (d_s_root_mass .gt. avail_mass)                               &
     &   .and. (d_s_root_mass .gt. 0.0d0) ) then
          ! reduce removal to match available storage
          red_mass_rat = avail_mass / d_s_root_mass
          ! adjust root increment to match
          d_root_mass = d_root_mass * red_mass_rat
          ! adjust shoot increment to match
          d_shoot_mass = d_shoot_mass * red_mass_rat
          ! adjust removal amount to match exactly
          d_s_root_mass =  d_s_root_mass * red_mass_rat
      end if

      ! find stem mass when shoot completely developed
      ! (mg tot/shoot) / ((kg leaf/kg stem)+1) = mg stem/shoot
      end_stem_mass = end_shoot_mass / (bcfleafstem+1.0)

      ! length of shoot when completely developed, use the mass of stem per plant
      ! (mg stem/shoot)*(kg/mg)*(#stem/m^2)/(#plants/m^2) = kg stem/plant
      ! inserted into stem area index equation to get stem area in m^2 per plant
      ! and then converted back to m^2 per stem
      end_stem_area = dble(bc0ssa) &
     &              * (end_stem_mass*u_mgtokg*dble(bcdstm)/dble(bcdpop))**dble(bc0ssb) &
     &              * dble(bcdpop) / dble(bcdstm)
      ! use silhouette area and stem diameter to length ratio to find length
      ! since silhouette area = length * diameter
      ! *** the square root is included since straight ratios do not really
      ! fit, but grossly underestimate the shoot length. This is possibly
      ! due to the difference between mature stem density vs. new growth
      ! with new stems being much higher in water content ***
      ! note: diameter to length ratio is when shoot has fully grown from root reserves
      ! during it's extension, it is assumed to grow at full diameter
      end_shoot_len = sqrt( end_stem_area / dble(bcfshoot) )

      ! screen shoot emergence parameters for validity
      if( end_shoot_len .le. bczgrowpt ) then
             write(UNIT=6,FMT="(1x,3(a),f7.4,a,f7.4,a)")                &
     &           'Warning: ',                                           &
     &           bc0nam(1:len_trim(bc0nam)),                            &
     &           ' growth halted. Shoot extension: ', end_shoot_len,    &
     &           ' Depth in soil: ', bczgrowpt, ' meters.'
      end if

      ! today and yesterday shoot length and stem and leaf mass increments
      ! length increase scaled by mass increase
      ! stem and leaf mass allocated proportionally (prevents premature emergence)
      
      if( end_shoot_mass .le. 0.0d0 ) then
          bczshoot = 0.0d0
      else
          bczshoot = end_shoot_len                                    &
     &         * ((bcmshoot /(u_mgtokg * bcdstm))+d_shoot_mass)           &
     &         / end_shoot_mass
      end if

      ! if no additional mass, no need to go further
      if( d_shoot_mass .le. 0.0d0) goto 900
!! +++++++++++++ RETURN FROM HERE IF ZERO +++++++++++++++++

      yesterday_len = end_shoot_len * (bcmshoot /(u_mgtokg * bcdstm))     &
     &              / end_shoot_mass
      d_stem_mass = d_shoot_mass  / (bcfleafstem+1.0d0)
      d_leaf_mass = d_shoot_mass * bcfleafstem / (bcfleafstem+1.0d0)

      ! divide above ground and below ground mass
      if( bczshoot .le. bczgrowpt ) then
          ! all shoot growth for today below ground
          ag_stem = 0.0d0
          bg_stem = d_stem_mass
      else if( yesterday_len .ge. bczgrowpt ) then
          ! all shoot growth for today above ground
          ag_stem = d_stem_mass
          bg_stem = 0.0d0
      else
          ! shoot breaks ground surface today
          ag_stem = d_stem_mass                                         &
     &            * (bczshoot-bczgrowpt) / (bczshoot-yesterday_len)
          bg_stem = d_stem_mass * (bczgrowpt - yesterday_len)           &
     &            / (bczshoot - yesterday_len)
      end if

      !convert from mg/shoot to kg/m^2
      dlfwt = d_leaf_mass * u_mgtokg * bcdstm
      dstwt = ag_stem * u_mgtokg * bcdstm
      drpwt = 0.0d0
      drswt = 0.0d0
      lost_mass = 0.0d0

      ! yield residue relationship adjustment
      ! since this is in shoot_grow, do not allow this with bchyfg=5 since
      ! it is illogical to store yield into the storage root while at the 
      ! same time using the storage root to grow the shoot
      if(     (cook_yield .eq. 1)                                       &
     &  .and. (bcyld_coef .gt. 1.0) .and. (bcresid_int .ge. 0.0)        &
     &  .and. ( (bchyfg.eq.0).or.(bchyfg.eq.1) ) ) then

          call cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt,    &
     &                   dble(bcmstandstem), dble(bcmstandleaflive + bcmstandleafdead), dble(bcmstandstore), &
     &                   dble(bcmflatstem), dble(bcmflatleaf), dble(bcmflatstore),        &
     &                   bcmrootstorez, lost_mass,                      &
     &                   dble(bcyld_coef), dble(bcresid_int), dble(bcgrf) )

      end if

      ! divide above ground stem between standing and flat
      stem_propor = min(1.0, bczmxc/bc0diammax) 
      stand_stem = dstwt * stem_propor
      flat_stem = dstwt * (1.0 - stem_propor)

      ! distribute mass into mass pools
      ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
      ! shoot mass pool (breakout pool, not true accumulator)
      bcmshoot = bcmshoot + d_shoot_mass * u_mgtokg * bcdstm

      ! reproductive mass is added to above ground pools
      bcmstandstore = bcmstandstore + drpwt * stem_propor
      bcmflatstore = bcmflatstore + drpwt * (1.0 - stem_propor)

      ! leaf mass is added even if below ground
      ! leaf has very low mass (small effect) and some light interaction
      ! does occur as emergence approaches (if problem can be changed easily)
      bcmstandleaflive = bcmstandleaflive + dlfwt

      ! above ground stems
      bcmstandstem = bcmstandstem + stand_stem
      bcmflatstem = bcmflatstem + flat_stem

      ! below ground stems
      do lay = 1, bnslay
          if( lay .eq. 1 ) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmbgstemz(lay) = bcmbgstemz(lay) + bg_stem               & 
     &        * u_mgtokg * bcdstm * frac_lay( dble(bczgrowpt)-dble(bczshoot), &
     &        dble(bczgrowpt)-yesterday_len, 0.0d0, bszlyd(lay) * u_mmtom )
          else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmbgstemz(lay) = bcmbgstemz(lay) + bg_stem               &
     &        * u_mgtokg * bcdstm * frac_lay( dble(bczgrowpt)-dble(bczshoot), &
     &        dble(bczgrowpt)-yesterday_len, bszlyd(lay-1) * u_mmtom,           &
     &        bszlyd(lay) * u_mmtom )
          end if
      end do

      ! check plant height, the the case of regrowth from stem
      ! do not allow reaching max height in single day
      ! use stem proportion to account for flat stems
      bczht = min( 0.5 * (bczmxc + bczht), max( bczht, max( 0.0,        &
     &            (bczshoot-bczgrowpt)*stem_propor ) ) )

      ! check root depth
      bczrtd = max( bczrtd, (bczgrowpt + bczshoot) )

      ! add to fibrous root mass, remove from storage root mass
      do lay = 1, bnslay
          if( lay .eq. 1 ) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmrootfiberz(lay) = bcmrootfiberz(lay) + d_root_mass     &
     &        * u_mgtokg * bcdstm * frac_lay( dble(bczgrowpt), dble(bczrtd), &
     &        0.0d0, bszlyd(lay) * u_mmtom )
          else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmrootfiberz(lay) = bcmrootfiberz(lay) + d_root_mass     &
     &        * u_mgtokg * bcdstm * frac_lay( dble(bczgrowpt), dble(bczrtd), &
     &        bszlyd(lay-1) * u_mmtom, bszlyd(lay) * u_mmtom )
          end if
          ! check for sufficient storage in layer to meet demand
          if(       (bcmrootstorez(lay) .gt. 0.0d0)                       &
     &        .and. (d_s_root_mass .gt. 0.0d0) ) then
              ! demand and storage to meet it
              ! units: mg/shoot * kg/mg * shoots/m^2 = kg/m^2
              bcmrootstorez(lay) = bcmrootstorez(lay) - d_s_root_mass   &
     &                           * u_mgtokg * bcdstm
              if( bcmrootstorez(lay) .lt. 0.0d0 ) then
                  ! not enough mass in this layer to meet need. Carry over
                  ! to next layer in d_s_root_mass
                  d_s_root_mass = - bcmrootstorez(lay) / (u_mgtokg*bcdstm)
                  bcmrootstorez(lay) = 0.0d0
              else
                  ! no more mass needed
                  d_s_root_mass = 0.0d0
             end if
          end if
      end do

      ! check if shoot sucessfully reached above ground
      if( (d_s_root_mass .gt. 0.0) .and. (bczht .le. 0.0) ) then
          write(0,*) "shoot_grow: not enough root storage to grow shoot"
          call exit(1)
      end if

!     the following write statements are for 'shoot.out'
!     am0cfl is flag to print crop submodel output
 900  if (am0cfl(isr) .ge. 1) then
          write(luoshoot(isr), 1000) daysim, get_psim_doy(isr), get_psim_year(isr), bcdayap, shoot_hui, &
     &        s_root_sum, f_root_sum, tot_mass_req, end_shoot_mass,     &
     &        end_root_mass, d_root_mass, d_shoot_mass, d_s_root_mass,  &
     &        end_stem_mass, end_stem_area, end_shoot_len, bczshoot,    &
     &        bcmshoot, bcdstm, trim(bc0nam)
      end if

 1000 format(1x,i5,1x,i3,1x,i4,1x,i4,1x,f6.3,                           &
     &       2(1x,f10.4), 2(1x,f12.4),                                  &
     &       4(1x,f12.4),                                               &
     &       4(1x,f12.4),                                               &
     &       (1x,f8.4),(1x,f8.3),1x,a)

      return
    end subroutine shoot_grow

    pure subroutine leaf_emerge( bnslay, bcdpop, hui, huiy, bcthu_leaf_beg, bcthu_leaf_end, &
     &                      bcmstandleaflive, bcmtotleaf, bcmrootstorez )

!     + + + KEYWORDS + + +
!     spring leaf emergence

      use constants, only: u_mgtokg

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: bnslay
      real, intent(in) :: bcdpop
      double precision, intent(in) :: hui
      double precision, intent(in) :: huiy
      double precision, intent(in) :: bcthu_leaf_beg
      double precision, intent(in) :: bcthu_leaf_end
      real, intent(inout) :: bcmstandleaflive
      double precision, intent(in) :: bcmtotleaf
      real, intent(inout) :: bcmrootstorez(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     bnslay - number of soil layers
!     bcdpop - Number of plants per unit area (#/m^2)
!     hui - heat unit index for today
!     huiy - heat unit index for yesterday
!     bcthu_leaf_beg - heat unit index (fraction) for beginning of leaf emergence from root storage period
!     bcthu_leaf_end - heat unit index (fraction) for end of leaf emergence from root storage period
!     bcmstandleaflive - crop standing leaf mass (kg/m^2)

!     bcmtotleaf - total mass released from root storage biomass (kg/m^2)
!                  in the period from beginning to completion of leaf emergence heat units

!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))

!     + + + LOCAL VARIABLES + + +
      integer lay
      double precision leaf_hui, leaf_huiy
      double precision fexp_hui, fexp_huiy
      double precision d_leaf_mass
      double precision d_s_root_mass, tot_mass_req, red_mass_rat
      double precision end_leaf_mass
      double precision s_root_sum, avail_mass
      double precision dlfwt

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     lay - index into soil layers for looping
!     leaf_hui - today fraction of heat unit leaf growth index accumulation
!     leaf_huiy - previous day fraction of heat unit leaf growth index accumulation
!     fexp_hui - exponential function evaluated at todays leaf heat unit index
!     fexp_huiy - exponential function evaluated at yesterdays leaf heat unit index
!     d_leaf_mass - mass increment added to leaf for the present day (mg/plant)
!     d_s_root_mass - mass increment removed from storage roots for the present day (mg/plant)
!     tot_mass_req - mass required from root mass for one plant (mg/plant)
!     red_mass_rat - ratio of reduced mass available for stem growth to expected mass available
!     end_leaf_mass - total leaf mass at end of leaf emergence period (mg/plant)
!     s_root_sum - storage root mass sum (total in all layers) (kg/m^2)
!     avail_mass - storage root mass sum in (mg/plant)
!     dlfwt - increment in leaf dry weight (kg/m^2)

!     + + + LOCAL PARAMETERS + + +
      double precision, parameter :: leaf_exp = 2.0D0
      double precision, parameter :: be_stor = 0.7D0

!     + + + LOCAL PARAMETER DEFINITIONS + + +
!     leaf_exp - exponent for shape of exponential function
!                 small numbers  go toward straight line
!                 large numbers delay development to end of period
!     be_stor - conversion efficiency of biomass from storage to growth

!     + + + END OF SPECIFICATIONS + + +

      ! fraction of leaf growth from stored reserves (today and yesterday)
      leaf_hui = min( 1.0d0, (hui - bcthu_leaf_beg) / (dble(bcthu_leaf_end) - bcthu_leaf_beg) )
      leaf_huiy = max( 0.0d0, (huiy - bcthu_leaf_beg) / (bcthu_leaf_end - bcthu_leaf_beg) )

      ! total leaf emergence occurs at an exponential rate
      fexp_hui = (exp(leaf_exp*leaf_hui)-1.0) / (exp(leaf_exp)-1)
      fexp_huiy = (exp(leaf_exp*leaf_huiy)-1.0) / (exp(leaf_exp)-1)

      ! sum present storage root mass (kg/m^2)
      s_root_sum = 0.0d0
      do lay = 1, bnslay
          s_root_sum = s_root_sum + bcmrootstorez(lay)
      end do

      ! calculate storage mass required for leaves on a single plant
      ! units: kg/m^2 / ( plants/m^2 * kg/mg ) = mg/plant
      tot_mass_req = dble(bcmtotleaf) / (dble(bcdpop) * u_mgtokg)

      end_leaf_mass = tot_mass_req * be_stor

      ! this days incremental leaf mass for a single plant (mg/plant)
      d_leaf_mass = end_leaf_mass * (fexp_hui - fexp_huiy)

      ! this days mass removed from the storage root (mg/plant)
      d_s_root_mass = d_leaf_mass / be_stor

      ! check that sufficient storage root mass is available
      ! units: mg/plant = kg/m^2 / (kg/mg * plant/m^2)
      avail_mass = s_root_sum  / (bcdpop * u_mgtokg)
      if( (d_s_root_mass .gt. avail_mass) .and. (d_s_root_mass .gt. 0.0d0) ) then
          ! reduce removal to match available storage
          red_mass_rat = avail_mass / d_s_root_mass
          ! adjust leaf increment to match
          d_leaf_mass = d_leaf_mass * red_mass_rat
          ! adjust removal amount to match exactly
          d_s_root_mass =  d_s_root_mass * red_mass_rat
      end if

      ! if no additional mass, no need to go further
      if( d_leaf_mass .le. 0.0d0) return
      !! +++++++++++++ RETURN FROM HERE IF ZERO +++++++++++++++++

      !convert from mg/plant to kg/m^2
      dlfwt = d_leaf_mass * u_mgtokg * bcdpop

      ! distribute mass into mass pools
      bcmstandleaflive = bcmstandleaflive + dlfwt

      ! remove from storage root mass
      do lay = 1, bnslay
          ! check for sufficient storage in layer to meet demand
          if( (bcmrootstorez(lay) .gt. 0.0d0) .and. (d_s_root_mass .gt. 0.0d0) ) then
              ! demand and storage to meet it
              ! units: mg/plant * kg/mg * plants/m^2 = kg/m^2
              bcmrootstorez(lay) = bcmrootstorez(lay) - d_s_root_mass * u_mgtokg * bcdpop
              if( bcmrootstorez(lay) .lt. 0.0d0 ) then
                  ! not enough mass in this layer to meet need. Carry over
                  ! to next layer in d_s_root_mass
                  d_s_root_mass = - bcmrootstorez(lay) / (u_mgtokg*bcdpop)
                  bcmrootstorez(lay) = 0.0d0
              else
                  ! no more mass needed
                  d_s_root_mass = 0.0d0
             end if
          end if
      end do

      return
    end subroutine leaf_emerge

    pure function frac_lay( top_loc, bot_loc, top_lay, bot_lay ) result(frac_layer)

      ! this function determines the fraction of a location which
      ! is contained in a layer. It could also be viewed as the
      ! fraction of "overlap" of the linear location with a layer
      ! depth slice. It was written assuming that top values are 
      ! less than bottom values

      double precision, intent(in) :: top_loc
      double precision, intent(in) :: bot_loc
      double precision, intent(in) :: top_lay
      double precision, intent(in) :: bot_lay
      
      double precision frac_layer

      if( top_lay .le. top_loc .and. bot_lay .gt. top_loc ) then
          ! top location is in layer
          if( bot_lay .ge. bot_loc ) then
              ! bottom location is also in layer
              frac_layer = 1.0d0
          else
              ! bottom location is below layer, proportion
              frac_layer = (bot_lay - top_loc)/(bot_loc - top_loc)
          end if
      else if( top_lay .lt. bot_loc .and. bot_lay .ge. bot_loc ) then
          ! bottom location is in layer
          ! if we are here, top location is not in layer so proportion
          frac_layer = (bot_loc - top_lay)/(bot_loc - top_loc)
      else if( top_lay .gt. top_loc .and. bot_lay .lt. bot_loc ) then
          ! location completely spans layer
          frac_layer = (bot_lay - top_lay)/(bot_loc - top_loc)
      else
          ! location is not in the layer at all
          frac_layer = 0.0d0
      end if

      return
    end function frac_lay

    pure subroutine shootnum( shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot, &
                              bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )

!     + + + PURPOSE + + +
!     determine the number of shoots that root storage mass can support,
!     and set the total mass to be released from root storage.

!     + + + KEYWORDS + + +
!     stem number, shoot growth

      use constants, only: u_mgtokg

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: shoot_flg
      integer, intent(in) :: bnslay
      integer, intent(in) :: bc0idc
      real, intent(in) :: bcdpop
      real, intent(in) :: bc0shoot
      real, intent(in) :: bcdmaxshoot
      double precision, intent(out) :: bcmtotshoot
      real, intent(in) :: bcmrootstorez(*)
      double precision, intent(out) :: bcdstm

!     + + + ARGUMENT DEFINITIONS + + +
!     shoot_flg - used to control the behavior of the shootnum subroutine
!             0 - returns the shoot number constrained by bcdmaxshoot
!             1 - returns the shoot number unconstrained by bcdmaxshoot
!     bnslay - number of soil layers
!     bc0idc - crop type:annual,perennial,etc
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives number of stems per plant
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bcmtotshoot - total mass released from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emergence heat units
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcdstm - Number of crop stems per unit area (#/m^2)

!     + + + LOCAL VARIABLES + + +
      integer lay
      double precision root_store_sum

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     lay - layer index for summing root storage
!     root_store_sum - sum of root storage

!     + + + PARAMETERS + + +
      double precision per_release
      PARAMETER (per_release = 0.9d0)
      double precision stage_release
      PARAMETER (stage_release = 0.5d0)

!     + + + PARAMETER DEFINITIONS + + +
!     per_release - fraction of available root stoage mass released to
!                   grow new shoots. Default is set to 90% of available
!     stage_release - fraction of available root stoage mass released to
!                   grow new shoots for cropID type 8.

      ! Find number of shoots (stems) that can be supported from
      ! root storage mass up to the maximum
      root_store_sum = 0.0d0
      do lay = 1,bnslay
          root_store_sum = root_store_sum + dble(bcmrootstorez(lay))
      end do

      ! determine number of regrowth shoots
      ! units are kg/m^2 / kg/shoot = shoots/m^2
      if( (bc0idc.eq.3) .or. (bc0idc.eq.6) .or. (bc0idc.eq.12) ) then
          ! Perennials hold some mass in reserve
          bcdstm = max( dble(bcdpop),                                         &
     &             per_release * root_store_sum/(dble(bc0shoot)*u_mgtokg)  )
      else if( bc0idc.eq.8 ) then
          ! This Perennial stages it's bud release, putting out less after each cutting
          bcdstm = max( dble(bcdpop),                                         &
     &             stage_release * root_store_sum/(dble(bc0shoot)*u_mgtokg) )
      else
          ! all others go for broke
          bcdstm = max( dble(bcdpop),                                         &
     &             root_store_sum/(dble(bc0shoot)*u_mgtokg) )
      end if

      if( shoot_flg .eq. 0 ) then
          ! respect maximum limit
          bcdstm =  min( dble(bcdmaxshoot)*dble(bcdpop), dble(bcdstm) )
      end if

!      write(*,*) 'shootnum:bcdstm: ', bcdstm
      ! set the mass of root storage that is released (for use in shoot grow)
      ! units are shoots/m^2 * kg/shoot = kg/m^2
      bcmtotshoot = min( root_store_sum, dble(bcdstm) * dble(bc0shoot) * u_mgtokg )

      return
    end subroutine shootnum

    pure function total_leaf( bnslay, bcmrootstorez ) result(bcmtotleaf)

!     + + + PURPOSE + + +
!     determine the mass of leaf emergence that root storage mass can support,
!     and set the total mass to be released from root storage.

!     + + + KEYWORDS + + +
!     leaf emergence

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: bnslay
      real, intent(in) :: bcmrootstorez(*)
      
      double precision bcmtotleaf

!     + + + ARGUMENT DEFINITIONS + + +
!     bnslay - number of soil layers
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmtotleaf - total mass released from root storage biomass (kg/m^2)
!                  in the period from beginning to completion of leaf emergence heat units

!     + + + LOCAL VARIABLES + + +
      integer lay
      double precision root_store_sum

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     lay - layer index for summing root storage
!     root_store_sum - sum of root storage (kg/m^2)

!     + + + PARAMETERS + + +
      double precision leaf_release
      PARAMETER (leaf_release = 0.5d0)

!     + + + PARAMETER DEFINITIONS + + +
!     leaf_release - fraction of available root storage mass released to
!                    grow new leaves. Default is set to 50% of available

      ! find mass of leaf that can be supported from
      ! root storage mass up to the maximum
      root_store_sum = 0.0d0
      do lay = 1,bnslay
          root_store_sum = root_store_sum + dble(bcmrootstorez(lay))
      end do

      ! set the mass of root storage that is released (for use in leaf emergence)
      bcmtotleaf = root_store_sum * leaf_release

      return
    end function total_leaf

    pure subroutine cookyield(bchyfg, bnslay, dlfwt, dstwt, drpwt, drswt, &
                              bcmstandstem, bcmstandleaf, bcmstandstore, &
                              bcmflatstem, bcmflatleaf, bcmflatstore, &
                              bcmrootstorez, lost_mass, &
                              bcyld_coef, bcresid_int, bcgrf )

!     + + + PURPOSE + + +
!     adjust incremental biomass allocation to leaf stem and reproductive
!     pools to match the input residue yield ratio and intercept value,
!     if running the model in that mode

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: bchyfg
      integer, intent(in) :: bnslay
      double precision, intent(inout) :: dlfwt
      double precision, intent(inout) :: dstwt
      double precision, intent(inout) :: drpwt
      double precision, intent(inout) :: drswt
      double precision, intent(in) :: bcmstandstem
      double precision, intent(in) :: bcmstandleaf
      double precision, intent(in) :: bcmstandstore
      double precision, intent(in) :: bcmflatstem
      double precision, intent(in) :: bcmflatleaf
      double precision, intent(in) :: bcmflatstore
      real, intent(in) :: bcmrootstorez(*)
      double precision, intent(in) :: lost_mass
      double precision, intent(in) :: bcyld_coef
      double precision, intent(in) :: bcresid_int
      double precision, intent(in) :: bcgrf

!     + + + ARGUMENT DEFINITIONS + + +
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              GRF, to when removing that plant part for yield
!         0     GRF applied to above ground storage (seeds, reproductive)
!         1     GRF times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     GRF applied to all aboveground biomass (forage)
!         3     GRF applied to leaf mass (tobacco)
!         4     GRF applied to stem mass (sugarcane)
!         5     GRF applied to below ground storage mass (potatoes, peanuts)
!     bnslay - number of soil layers
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     dstwt - increment in dry weight of stem (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     lost_mass - biomass that decayed (disappeared) 
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * Yield + bcresid_int (kg/m^2)
!     bcgrf  - fraction of reproductive biomass that is yield

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL VARIABLES + + +
      integer idx
      double precision ddm_res_yld, temp_tot, store_mass, ddm_adj

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx - array index used in loops
!     ddm_res_yld - increment in aboveground dry matter (kg/m^2)
!     temp_tot - temporary total biomass
!     store_mass - intermediate storage mass value
!     ddm_adj - adjusted increment in aboveground dry matter (kg/m^2)

!     + + + FUNCTIONS CALLED + + +

!     + + + SUBROUTINES CALLED + + +

!     + + + END OF SPECIFICATIONS + + +

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
          ddm_res_yld = 0.0
      end select

      ! find yield storage mass increment based on yield residue relationship
      ! sum present yield + residue biomass
      temp_tot = 0.0
      if ( bchyfg .eq. 5) then
          ! 5 - GRF times below ground storage mass
          do idx = 1, bnslay
              temp_tot = temp_tot + dble(bcmrootstorez(idx))
          end do
      end if
      ! add lost mass here to allow removing if mass was above threshold
      temp_tot = temp_tot + lost_mass &
               + bcmstandstem + bcmstandleaf + bcmstandstore &
               + bcmflatstem + bcmflatleaf + bcmflatstore
      if( temp_tot + ddm_res_yld .le. bcresid_int ) then
          store_mass = 0.0
      else if( temp_tot .le. bcresid_int ) then
          store_mass = (ddm_res_yld - (bcresid_int-temp_tot)) / bcyld_coef / bcgrf
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
              dstwt = 0.0
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
              dstwt = 0.0
              ! set reproductive increment
              drpwt = ddm_adj - dlfwt
          else
              ! not enough for both, leaf increment reduced
              dstwt = 0.0
              drpwt = 0.0
              dlfwt = ddm_adj
          end if
      case default
          ! no adjustment
      end select

      return
    end subroutine cookyield

    pure subroutine ht_dia_sai( bcdpop, bcmstandstem, dmstandstem, &
                                bc0ssa, bc0ssb, bcdstm, &
                                bczht, dht, bcxstmrep, bcrsai )

      !     + + + PURPOSE + + +
      ! this routine checks for consistency between plant height and biomass
      ! accumulation, using half and double the stem diameter (previously unused)
      ! as check points. The representative stem diameter is set to show where
      ! within the range the actual stem diameter is.

      ! + + + ARGUMENT DECLARATIONS + + +
      double precision, intent(in) :: bcdpop, bcmstandstem, dmstandstem
      double precision, intent(in) :: bc0ssa, bc0ssb
      double precision, intent(in) :: bcdstm, bczht 
      double precision, intent(inout) :: dht
      double precision, intent(out) :: bcxstmrep, bcrsai

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
      if( bcdpop .gt. 0.0 ) then
          bcrsai = bcdpop * bc0ssa * (bcmstandstem/bcdpop)**bc0ssb
      else
          bcrsai = 0.0
      end if

      ! if( dmstandstem .le. 0.0 ) then
      !   ! stem mass is not increasing, therefore height is not increasing.
      !   dht = 0.0
      ! end if

      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
      if( (bcdstm * (bczht + dht)) .gt. 0.0 ) then
          bcxstmrep = bcrsai / (bcdstm * (bczht + dht))
      else
          bcxstmrep = 0.0
      end if

      return
    end subroutine ht_dia_sai

    pure function freeze_damage( x1, y1, x2, y2, xw ) result(frst)

      use precision_mod, only: max_arg_exp_dp

      ! fraction of leaf area killed by a single day freeze event 
      ! with a low temperature of xw

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: x1
      real, intent(in) :: x2
      real, intent(in) :: y1
      real, intent(in) :: y2
      double precision, intent(in) :: xw

      double precision frst

      ! + + + LOCAL VARIABLES + + +
      double precision xx1, xx2, fx1, fx2, xxw

      double precision arg_exp, delta_x

      xx1 = abs(x1)
      xx2 = abs(x2)
      xxw = abs(xw)

      delta_x = (xx2 - xx1)

      if( (delta_x .gt. 0.0d0) .or. (delta_x .lt. 0.0d0) ) then
        ! delta_x is non-zero
        fx1 = log(xx1/y1-xx1)
        fx2 = log(xx2/y2-xx2)
      else
        ! delta_x is zero. Make non-zero in correct direction for frost damage.
        xx1 = xx1 - spacing(xx1)
        xx2 = xx2 + spacing(xx2)
        delta_x = (xx2 - xx1)
        fx1 = log(xx1/y1-xx1)
        fx2 = log(xx2/y2-xx2)
      end if

      arg_exp = fx1 + (xx1 - xxw) * (fx1 - fx2) / delta_x

      if( abs(arg_exp) .gt. max_arg_exp_dp ) then
        ! cap the value to avoid floating point error on exponential function
        ! preserve sign
        arg_exp = sign( (max_arg_exp_dp - spacing(max_arg_exp_dp)), arg_exp)
      end if

      ! b_fr = (fx1 - fx2) / delta_x
      ! a_fr = fx1 + b_fr * xx1
      ! frst = xw / (xw + exp(a_fr-b_fr*xw))

      frst = xxw / (xxw + exp(arg_exp))

      return
    end function freeze_damage

end module crop_growth_mod
