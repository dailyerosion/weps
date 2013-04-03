!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine cropgrow (bnslay, bszlyt, bszlyd, bsdblk,              &
     &                 bsfcce, bsfom, bsfcec, bsfsmb,                   &
     &                 bsfcla, bs0ph, bsftan, bsftap,                   &
     &                 bsmno3,                                          &
     &                 bc0ck, bcgrf, bcehu0, bczmxc,                    &
     &                 bc0nam, bc0idc, bcxrow,                          &
     &                 bctdtm, bczmrt, bctmin, bctopt,                  &
     &                 bc0fd1, bc0fd2, cc0fd1, cc0fd2,                  &
     &                 bc0bceff,                                        &
     &                 bc0alf, bc0blf, bc0clf,                          &
     &                 bc0dlf, bc0arp, bc0brp, bc0crp,                  &
     &                 bc0drp, bc0aht, bc0bht,                          &
     &                 bc0sla, bc0hue, bctverndel,                      &
     &                 bweirr, bwtdmx, bwtdmn,                          &
     &                 bhtsmx, bhtsmn,                                  &
     &                 bhfwsf,                                          &
     &                 bm0cif,                                          &
     &                 bcthudf, bcbaf,                                  &
     &                 bchyfg, bcthum, bcdpop, bcdmaxshoot,             &
     &                 bc0storeinit, bcfshoot,                          &
     &                 bc0growdepth, bcfleafstem, bc0shoot,             &
     &                 bc0diammax, bc0ssa, bc0ssb,                      &
     &                 bcfleaf2stor, bcfstem2stor, bcfstor2stor,        &
     &                 bcyld_coef, bcresid_int, bcxstm,                 &
     &                 bcmstandstem, bcmstandleaf, bcmstandstore,       &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bcdayap, bcdayam, bcthucum, bctrthucum,          &
     &                 bcgrainf, bczgrowpt, bcfliveleaf,                &
     &                 bcleafareatrend, bcstemmasstrend, bctwarmdays,   &
     &                 bctchillucum, bcthardnx, bcthu_shoot_beg,        &
     &                 bcthu_shoot_end, bcxstmrep,                      &
     &                 bprevstandstem, bprevstandleaf, bprevstandstore, &
     &                 bprevflatstem, bprevflatleaf, bprevflatstore,    &
     &                 bprevmshoot, bprevbgstemz,                       &
     &                 bprevrootstorez, bprevrootfiberz,                &
     &                 bprevht, bprevzshoot, bprevstm, bprevrtd,        &
     &                 bprevdayap, bprevhucum, bprevrthucum,            &
     &                 bprevgrainf, bprevchillucum, bprevliveleaf,      &
     &               bprevdayspring, daysim, bcdayspring, bczloc_regrow,&
     &                 bgmstandstem, bgmstandleaf, bgmstandstore,       &
     &                 bgmflatstem, bgmflatleaf, bgmflatstore,          &
     &                 bgmbgstemz,                                      &
     &                 bgzht, bgdstm, bgxstmrep, bggrainf )

!     + + + PURPOSE + + +
!     This is the main program for implementing the crop growth
!     calculations in the various subroutines. For any questions refer
!     to Amare Retta at the USDA Wind Erosion Research Laboratory,
!     University, Manhattan KS 66506.

!     + + + KEYWORDS + + +
!     Wind erosion crop model

      use weps_interface_defs
      use file_io_mod, only: luocrop, luoshoot
      use p1unconv_mod, only: mgtokg

!     + + + ARGUMENT DECLARATIONS + + +
      integer bnslay, bctdtm, bcthudf
      real bszlyt(*)
      real bszlyd(*), bsdblk(*), bsfcec(*), bsfcce(*)
      real bsfom(*), bsfcla(*), bs0ph(*)
      real bsftan(*), bsftap(*)
      real bsfsmb(*), bsmno3
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
      real bweirr, bwtdmx, bwtdmn
      real bhtsmx(*), bhtsmn(*)
      real bhfwsf
      integer bchyfg
      real bcthum, bcdpop, bcdmaxshoot
      real bc0storeinit, bcfshoot
      real bc0growdepth, bcfleafstem, bc0shoot
      real bc0diammax, bc0ssa, bc0ssb
      real bcfleaf2stor, bcfstem2stor, bcfstor2stor
      real bcyld_coef, bcresid_int, bcxstm
      real bcmstandstem, bcmstandleaf, bcmstandstore
      real bcmflatstem, bcmflatleaf, bcmflatstore
      real bcmshoot, bcmtotshoot, bcmbgstemz(*)
      real bcmrootstorez(*), bcmrootfiberz(*)
      real bczht, bczshoot, bcdstm, bczrtd
      integer bcdayap, bcdayam
      real bcthucum, bctrthucum
      real bcgrainf, bczgrowpt, bcfliveleaf
      real bcleafareatrend, bcstemmasstrend
      integer bctwarmdays
      real bctchillucum, bcthardnx, bcthu_shoot_beg, bcthu_shoot_end
      real bcxstmrep
      real bprevstandstem, bprevstandleaf, bprevstandstore
      real bprevflatstem, bprevflatleaf, bprevflatstore
      real bprevmshoot, bprevbgstemz(*)
      real bprevrootstorez(*), bprevrootfiberz(*)
      real bprevht, bprevzshoot, bprevstm, bprevrtd
      integer bprevdayap
      real bprevhucum, bprevrthucum
      real bprevgrainf, bprevchillucum, bprevliveleaf
      integer bprevdayspring
      logical bm0cif
      real    bcbaf
      integer daysim, bcdayspring
      real    bczloc_regrow
      real    bgmstandstem, bgmstandleaf, bgmstandstore
      real    bgmflatstem, bgmflatleaf, bgmflatstore
      real    bgmbgstemz(*)
      real    bgzht, bgdstm, bgxstmrep, bggrainf

!     + + + ARGUMENT DEFINITIONS + + +

!     bnslay - number of soil layers
!     bc0alf - leaf partitioning parameter
!     bc0arp - rprd partitioning parameter
!     bc0aht - height s-curve parameter
!     bsmno3 - amount of applied N (t/ha)
!     bc0blf - leaf partitioning parameter
!     bc0brp - rprd partitioning parameter
!     bc0bht - height s-curve parameter
!     bsdblk      - bulk density of a layer (g/cm^3=t/m^3)
!     bsfcce  - calcium carbonate (%)
!     bsfcla  - % clay
!     bsfom   - percent organic matter
!     bsftan  - total available N in a layer from all sources (kg/ha)
!     bsftap  - total available P in a layer from all sources (kg/ha)
!     bc0clf - leaf partitioning parameter
!     bc0crp - rprd partitioning parameter
!     bsfcec     - cation exchange capacity (cmol/kg)
!     bc0ck  - light extinction coeffficient (fraction)
!     bc0dlf - leaf partitioning parameter
!     bc0drp - rprd partitioning parameter
!     dmag   - stress adjusted cummulated aboveground biomass (t/ha)
!     bctdtm - days to maturity (same as dtm)
!     bc0fd1 - minimum temperature below zero (C)
!     cc0fd1 - fraction of biomass lost each day due to frost
!     bc0fd2 - minimum temperature below zero (C)
!     cc0fd2 - fraction of biomass lost each day due to frost
!     bczmxc - maximum potential plant height (m)
!     bc0hue - relative heat unit for emergence (fraction)
!     bctverndel - thermal delay coefficient pre-vernalization
!     bc0idc - crop type:annual,perennial,etc
!     bc0nam - crop name
!     acxrow - Crop row spacing (m)
!     bs0ph  - soil pH
!     bczmrt - maximum root depth
!     bc0sla - specific leaf area (cm^2/g)
!     bsfsmb     - sum of bases (cmol/kg)
!     bctmin - base temperature (deg. C)
!     bctopt - optimum temperature (deg. C)
!     bc0bceff - biomass conversion efficiency (kg/ha/mj)
!     bszlyd - depth from top of soil to botom of layer, m
!     acbaflg - flag for biomass adjustment action
!         0     o normal crop growth
!         1     o find biomass adjustment factor for target yield
!         2     o Use given biomass adjustment factor
!     acbaf  - biomass adjustment factor
!     acyraf - yield to biomass ratio adjustment factor
!     bwtdmx - daily maximum air temperature (deg.C)
!     bwtdmn - daily minimum air temperature (deg.C)
!     bhtsmx - daily maximum soil temperature by layer (deg.C)
!     bhtsmn - daily minimum soil temperature by layer (deg.C)
!     bcthum - potential heat units for crop maturity (deg. C)
!     bcdpop - Crop seeding density (#/m^2)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bc0storeinit - db input, crop storage root mass initialzation (mg/plant)
!     bcfshoot - crop ratio of shoot diameter to length
!     bc0growdepth - depth of growing point at time of planting (m)
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
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a unique pool
!                since this mass is destributed into below ground stem and
!                standing stem as each increment of the shoot is added
!     bcmtotshoot - total mass of shoot growing from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emegence heat units
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
!     bcfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     bcleafareatrend - direction in which leaf area is trending.
!                       Saves trend even if leaf area is static for long periods.
!     bcstemmasstrend - direction in which stem mass is trending.
!                        Saves trend even if stem mass is static for long periods.
!     bctwarmdays - number of consecutive days that the daily average temperature
!                   has been above the minimum growth temperature
!     bctchillucum - accumulated chilling units (days)
!     bcthardnx - hardening index for winter annuals (range from 0 t0 2)
!     bcthu_shoot_beg - heat unit index (fraction) for beginning of shoot grow from root storage period
!     bcthu_shoot_end - heat unit index (fraction) for end of shoot grow from root storage period
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai

!     daysim   - day of the simulation
!     bcdayspring - day of year in which a winter annual releases stored growth
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

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'p1solar.inc'

!     + + + COMMON BLOCKS + + +
      include 'crop/cgrow.inc'
      include 'crop/cenvr.inc'
      include 'crop/cparm.inc'
      include 'crop/csoil.inc'
      include 'crop/chumus.inc'
      include 'crop/cfert.inc'

!     + + + LOCAL VARIABLES + + +
      integer lay, dd, mm, yy
      real root_store_rel, pot_stems, pot_leaf_mass
      real vern_delay, photo_delay, hu_delay, trend
      integer regrowth_flg

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     dd,mm,yy - the current day, month, and year
!     root_store_rel - root storage which could be released for regrowth
!     pot_stems - potential number of stems which could be released for regrowth
!     pot_leaf_mass - potential leaf mass which could be released for regrowth.
!     chilluv - effective vernalization days required to complete vernalization
!     vern_delay - reduction in heat unit accumulation based on vernalization
!     photo_delay - reduction in heat unit accumulation based on photoperiod
!     hu_delay - combined reduction in heat unit accummulation
!     trend - test computation for trend direction of living leaf area
!     regrowth_flg - used to record changes is regrowth conditions day by day

!     + + + LOCAL PARAMETERS + + +
      real chilluv
      real shoot_delay
      real verndelmax
      real dev_floor
      real max_photo_per
      real spring_trig
      real hard_spring
      parameter(chilluv = 50.0)
      parameter(shoot_delay = 7.0)
      parameter(verndelmax = 0.04)
      parameter(dev_floor = 0.01)
      parameter(max_photo_per = 20)
      parameter(spring_trig = 0.29)
      parameter(hard_spring = 1.0)

      real bctphotodel
      parameter( bctphotodel = 0.0055)

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

!     + + + SUBROUTINES CALLED + + +
!     caldat
!     cinit
!     huc1
!     growth
!     npcy
!
!     + + + FUNCTION DECLARATIONS + + +
!      integer dayear
!      real huc1
!      real daylen

!     + + + END OF SPECIFICATIONS + + +

!     day of year
      call caldatw(dd, mm, yy)
      jd = dayear(dd, mm, yy)

      do 5 lay = 1, bnslay
         bsfcce(lay) = bsfcce(lay) * 100.
         bsfom(lay) = bsfom(lay) * 100.
         bsfcla(lay) = bsfcla(lay) * 100.

         wn(lay) = 0.0
         wp(lay) = 0.0
         wno3(lay) = bsftan(lay)
         ap(lay) = bsftap(lay)
    5 continue

!     initialize growth and nutrient variables when crop is planted
!     bm0cif is flag to initialize crop at start of planting
      if (bm0cif) then
          call cinit (bnslay, bszlyt, bszlyd, bsdblk, bsfcce, bsfcec,   &
     &              bsfsmb, bsfom, bsfcla, bs0ph,                       &
     &              bsmno3,                                             &
     &              bc0fd1, bc0fd2, bctopt, bctmin,                     &
     &              cc0fd1, cc0fd2,                                     &
     &              dd, mm, yy,                                         &
     &              bcthudf, bctdtm, bcthum, bc0hue, bcdmaxshoot,       &
     &              bc0shoot, bc0growdepth, bc0storeinit,               &
     &              bcmstandstem, bcmstandleaf, bcmstandstore,          &
     &              bcmflatstem, bcmflatleaf, bcmflatstore,             &
     &              bcmshoot, bcmtotshoot, bcmbgstemz,                  &
     &              bcmrootstorez, bcmrootfiberz,                       &
     &              bczht, bczshoot, bcdstm, bczrtd,                    &
     &              bcdayap, bcdayam, bcthucum, bctrthucum,             &
     &              bcgrainf, bczgrowpt, bcfliveleaf,                   &
     &              bcleafareatrend, bcstemmasstrend, bctwarmdays,      &
     &              bctchillucum, bcthardnx, bcthu_shoot_beg,           &
     &              bcthu_shoot_end, bcdpop, bcdayspring)

          ! set previous values to initial values
          bprevstandstem = bcmstandstem
          bprevstandleaf = bcmstandleaf
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
          bprevdayap = bcdayap
          bprevhucum = bcthucum
          bprevrthucum = bctrthucum
          bprevgrainf = bcgrainf
          bprevchillucum = bctchillucum
          bprevliveleaf = bcfliveleaf
          bprevdayspring = bcdayspring

          if (am0cfl .ge. 1) then
              ! put double blank lines in daily files to create growth blocks
              write(luocrop,*)   ! crop.out
              write(luocrop,*)   ! crop.out
              write(luoshoot,*)  ! shoot.out
              write(luoshoot,*)  ! shoot.out
          end if

          bm0cif = .false.  !turn off after initialization is complete
      else
          ! calculate day length
          hrlty = hrlt
          hrlt = daylen(xlat, jd, civilrise)

          ! set trend direction for living leaf area from external forces
          trend = (bcfliveleaf*bcmstandleaf)                            &
     &          - (bprevliveleaf*bprevstandleaf)
          if ((trend .ne. 0.0)                                          &
     &        .and. ((bcthucum/bcthum .gt. bc0hue) .or. (bc0idc.eq.8))) &
     &        then  ! trend non-zero and (heat units past emergence or staged crown release crop)
              bcleafareatrend = trend
          end if
          ! set trend direction for above ground stem mass from external forces
          trend = bcmstandstem + bcmflatstem                            &
     &          - bprevstandstem - bprevflatstem
          if ((trend .ne. 0.0)                                          &
     &        .and. ((bcthucum/bcthum .gt. bc0hue) .or. (bc0idc.eq.8))) &
     &        then  ! trend non-zero and (heat units past emergence or staged crown release crop)
              bcstemmasstrend = trend
          end if
      endif

      ! check for consecutive "warm" days based on daily average temperature
      if( 0.5*(bwtdmx+bwtdmn).gt.bctmin ) then
          ! this is a warm day
          bctwarmdays = bctwarmdays + 1
      else
          ! reduce warm day total, but do not zero, for proper fall regrow of perennials
          bctwarmdays = bctwarmdays / 2
      end if

      ! accumulate chill units
      call chillu(bctchillucum, bwtdmx, bwtdmn)

      ! zero out temp pool variables used in testing for residue from regrowth in callcrop
      bgmstandstem = 0.0
      bgmstandleaf = 0.0
      bgmstandstore = 0.0
      bgmflatstem = 0.0
      bgmflatleaf = 0.0
      bgmflatstore = 0.0

      ! check crop type for shoot growth action
      regrowth_flg = -1
      if(    (bcfleaf2stor .gt. 0.0)                                    &
     &  .or. (bcfstem2stor .gt. 0.0)                                    &
     &  .or. (bcfstor2stor .gt. 0.0) ) then
        if( (bc0idc.eq.2) .or. (bc0idc.eq.5) ) then
           ! calculate freeze hardening index 
           call freezeharden(bcthardnx, bhtsmx(1), bhtsmn(1))

          ! check winter annuals for completion of vernalization,
          ! warming and spring day length 
          if( bczgrowpt .le. 0.0 ) then
           ! remember, negative number means above ground
           regrowth_flg = 1
           if( bctchillucum .ge. chilluv ) then
            regrowth_flg = 2
            if( bctwarmdays .ge. shoot_delay*bctverndel/verndelmax) then
             regrowth_flg = 3
             !if( huiy .gt. spring_trig ) then
             !if( bcthardnx .le. 0.0 ) then
             if( bcthardnx .lt. hard_spring ) then
              regrowth_flg = 4
              ! vernalized and ready to grow in spring
              bcthu_shoot_beg = bcthucum / bcthum
              bcthu_shoot_end = bcthucum / bcthum + bc0hue
              call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot,&
     &             bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )
              ! eliminate diversion of biomass to crown storage
              bcfleaf2stor = 0.0
              bcfstem2stor = 0.0
              bcfstor2stor = 0.0
              ! turn off freeze hardening
              bcthardnx = 0.0
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
        else
          ! check summer annuals and perennials for removal of all (most) leaf mass
          ! perennials with staged crown release also exhibit tuber dormancy
          ! so we really need to wait for spring and not regrow immediately
          ! after it matures, even if it is defoliated, or cut down, but
          ! also regrow in the spring even if not cut down (test 4 to 5 check below)
          regrowth_flg = 0
!      write(*,*) 'crop:bcleafareatrend: ', bcleafareatrend
          if( bcleafareatrend .lt. 0.0) then                             ! last change in leaf area was a reduction
           regrowth_flg = 1
           if( bcfliveleaf * bcmstandleaf .lt. 0.84*bc0storeinit*bcdpop & ! 0.42 * 2 = 0.84
     &      * mgtokg * bcfleafstem / (bcfleafstem + 1.0) ) then           ! below minimum living leaf mass (which is twice seed leaf mass)
            regrowth_flg = 2
            if( bctwarmdays .ge. shoot_delay ) then                       ! enough warm days to start regrowth
             regrowth_flg = 3
             if( (bcthucum  / bcthum .ge. bc0hue)                       & ! heat units past emergence
     &           .or.((bc0idc.eq.8).and.(bcstemmasstrend.lt.0.0)) ) then  ! staged crown release will regrow without full emergence, but only if stem removed ie harvest
              regrowth_flg = 4
              if( (bcthucum .lt. bcthum)                                & ! not yet mature
     &            .or. ((bc0idc.eq.3) .or. (bc0idc.eq.6))               & ! perennial
     &            .or. ((bc0idc.eq.8) .and. (hrlty .lt. hrlt)) ) then     ! staged crown release and days lengthening (ie. spring)
               regrowth_flg = 5
               ! find out how much root store could be released for regrowth
               call shootnum(shoot_flg,bnslay, bc0idc, bcdpop, bc0shoot,&
     &               bcdmaxshoot,root_store_rel,bcmrootstorez,pot_stems)
               ! find the potential leaf mass to be achieved with regrowth
               if ( bczloc_regrow .gt. 0.0 ) then
                   pot_leaf_mass = bcmstandleaf + 0.42                  &
     &                           * min(root_store_rel, bcmtotshoot)     &
     &                           * bcfleafstem / (bcfleafstem + 1.0)
               else
                   pot_leaf_mass = 0.42 * root_store_rel                &
     &                           * bcfleafstem / (bcfleafstem + 1.0)
               end if
               ! is present living leaf mass less than leaf mass from storage regrowth
               if( (bcfliveleaf*bcmstandleaf) .lt. pot_leaf_mass ) then
                  regrowth_flg = 6
                  ! regrow possible from shoot for perennials, annuals.
                  ! reset growth clock 
                  bcthucum = 0.0
                  bcthu_shoot_beg = 0.0
                  bcthu_shoot_end = bc0hue
                  ! allow vernalization to start over (bluegrass uses this)
                  bctchillucum = 0.0
                  ! reset shoot grow configuration
                  if ( bczloc_regrow .gt. 0.0 ) then
                      ! regrows from stem, stem does not become residue
                      ! note, flat leaves are dead leaves, no storage in shoot.
                      bcmshoot = bcmstandstem +bcmflatstem +bcmstandleaf
                      do lay = 1, bnslay
                          bcmshoot = bcmshoot + bcmbgstemz(lay)
                      end do
                      bcmtotshoot = min(root_store_rel, bcmtotshoot)
                  else
                      ! regrows from crown, stem becomes residue
                      bgmstandstem = bcmstandstem
                      bgmstandleaf = bcmstandleaf
                      bgmstandstore = bcmstandstore
                      bgmflatstem = bcmflatstem
                      bgmflatleaf = bcmflatleaf
                      bgmflatstore = bcmflatstore
                      do lay = 1, bnslay
                          bgmbgstemz(lay) = bcmbgstemz(lay)
                      end do
                      bggrainf = bcgrainf
                      bgzht = bczht
                      bgdstm = bcdstm
                      bgxstmrep = bcxstmrep
                      ! reset crop values to indicate new growth cycle
                      bcmshoot = 0.0
                      bcmstandstem = 0.0
                      bcmstandleaf = 0.0
                      bcmstandstore = 0.0
                      bcmflatstem = 0.0
                      bcmflatleaf = 0.0
                      bcmflatstore = 0.0
                      do lay = 1, bnslay
                          bcmbgstemz(lay) = 0.0
                      end do
                      bcgrainf = 0.0
                      bczht = 0.0
                      bcmtotshoot = root_store_rel
                      bcdstm = pot_stems
                  end if
               end if
              end if
             end if
            end if
           end if
          end if
        end if
      end if

      ! calculate growing degree days
      ! set default heat unit delay value
      hu_delay = 1.0
      if( (bcthum .le. 0.0) .or. (bcdstm .le. 0.0) ) then
          ! always keep this invalid plant in first stage growth
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow
          huiy = 0.0
          hui = 0.0
      else
          ! previous day heat unit index
          huiy = bcthucum / bcthum
          huirty = bctrthucum / bcthum
          ! check for growth completion
          if( huiy .lt. 1.0 ) then
              ! accumulate additional for today
              ! check for emergence status
              if( (huiy .ge. bc0hue).and. (huiy .lt. spring_trig) ) then
                  ! emergence completed, account for vernalization and
                  ! photo period by delaying development rate until chill
                  ! units completed and spring trigger reached
                  vern_delay = 1.0-bctverndel*(chilluv-bctchillucum)
                  !vern_delay = 1.0        ! delay disabled
                  !photo_delay = 1.0-bctphotodel*(max_photo_per-hrlt)**2
                  photo_delay = 1.0       ! delay disabled
                  hu_delay =  max(dev_floor,min(vern_delay,photo_delay))
              end if
              ! do not accumulate heat units if daily minimum is below freezing
!              if( bwtdmn .gt. 0.0 ) then
                  ! accumulate heat units using set heat unit delay
                  bcthucum = bcthucum +huc1(bwtdmx,bwtdmn,bctopt,bctmin)&
     &                     * hu_delay
!              end if
              ! root depth growth heat units
              bctrthucum = bctrthucum +huc1(bwtdmx,bwtdmn,bctopt,bctmin)
              ! do not cap this for annuals, to allow it to continue
              ! root mass partition is reduced to lower levels after the
              ! first full year. Out of range is capped in the function
              ! in growth.for
              ! bctrthucum = min(bctrthucum, bcthum)
              ! calculate heat unit index
              hui = bcthucum / bcthum
              huirt = bctrthucum / bcthum
          end if
      endif

!      write(*,*) 'crop:huiy: ', huiy
!      write(*,*) 'crop:regrowth_flg: ', regrowth_flg
!      write(*,*) 'crop:bctwarmdays: ', bctwarmdays
      if( (huiy .lt. 1.0) .and. (bcdstm .gt. 0.0)) then
          ! crop growth not yet complete
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow
          ! increment day after planting counter since growth happens same day
          bcdayap = bcdayap + 1

          ! seedling, transplant initialization and winter annual shoot growth
          ! calculations using root reserves
          if(       (huiy .lt. bcthu_shoot_end)                         &
     &        .and. (hui  .gt. bcthu_shoot_beg) ) then

              ! daily shoot growth
              call shoot_grow( bnslay, bszlyd, bcdpop,                  &
     &                 bczmxc, bcfleafstem,                             &
     &                 bcfshoot, bc0ssa, bc0ssb, bc0diammax,            &
     &                 hui, huiy, bcthu_shoot_beg, bcthu_shoot_end,     &
     &                 bcmstandstem, bcmstandleaf, bcmstandstore,       &
     &                 bcmflatstem, bcmflatleaf, bcmflatstore,          &
     &                 bcmshoot, bcmtotshoot, bcmbgstemz,               &
     &                 bcmrootstorez, bcmrootfiberz,                    &
     &                 bczht, bczshoot, bcdstm, bczrtd,                 &
     &                 bczgrowpt, bcfliveleaf, bc0nam,                  &
     &                 bchyfg, bcyld_coef, bcresid_int, bcgrf,          &
     &                 daysim, bcdayap )
          end if

          if(       (huiy .lt. bcthu_shoot_end)                         &
     &        .and. (hui .ge. bcthu_shoot_end) ) then
              ! shoot growth completed on this day
              ! move growing point to regrowth depth after shoot growth complete
              ! remember, a negative number is above ground
              bczgrowpt = ( - bczloc_regrow )
              if (am0cfl .ge. 1) then
                  ! single blank line to separate shoot growth periods
                  write(luoshoot,*)  ! shoot.out
              end if
          end if

          ! temporary location    ! calculates Frost damage s-curve coefficients
          call scrv1(bc0fd1,cc0fd1,bc0fd2,cc0fd2,a_fr,b_fr)

          ! calculate plant growth state variables
          call growth( bnslay, bszlyd, bc0ck, bcgrf,                    &
     &                 bcehu0, bczmxc, bc0idc, bc0nam,                  &
     &                 a_fr, b_fr, bcxrow, bc0diammax,                  &
     &                 bczmrt, bctmin, bctopt, bc0bceff,                &
     &                 bc0alf, bc0blf, bc0clf, bc0dlf, bc0arp,          &
     &                 bc0brp, bc0crp, bc0drp,                          &
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

          ! set trend direction for living leaf area
          trend = (bcfliveleaf*bcmstandleaf)                            &
     &          - (bprevliveleaf*bprevstandleaf)
          if ((trend .ne. 0.0)                                          &
     &        .and. ((bcthucum/bcthum .gt. bc0hue) .or. (bc0idc.eq.8))) &
     &        then  ! trend non-zero and (heat units past emergence or staged crown release crop)
              bcleafareatrend = trend
          end if
          ! set trend direction for above ground stem mass from growth
          trend = bcmstandstem + bcmflatstem                            &
     &          - bprevstandstem - bprevflatstem
          if ((trend .ne. 0.0)                                          &
     &        .and. ((bcthucum/bcthum .gt. bc0hue) .or. (bc0idc.eq.8))) &
     &        then  ! trend non-zero and (heat units past emergence or staged crown release crop)
              bcstemmasstrend = trend
          end if

          ! set saved values of crop state variables for comparison next time
          bprevstandstem = bcmstandstem
          bprevstandleaf = bcmstandleaf
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
          bprevliveleaf = bcfliveleaf
          bprevdayspring = bcdayspring
      else
          ! heat units completed, crop leaf mass is non transpiring
          bcfliveleaf = 0.0

          ! check for mature perennial that may re-sprout before fall (alfalfa, grasses)
          if( (bc0idc.eq.3) .or. (bc0idc.eq.6) ) then
              ! check for growing weather and regrowth ready state
                  ! transfer all mature biomass to residue pool
                  ! find number of stems to regrow
                  ! reset heat units to start shoot regrowth
          end if

          ! accumulate days after maturity
          bcdayam = bcdayam + 1

      end if

      do lay = 1, bnslay
         bsfcce(lay) = bsfcce(lay) / 100.
         bsfom(lay) = bsfom(lay) / 100.
         bsfcla(lay) = bsfcla(lay) / 100.
      end do

      return
      end
