!$Author$
!$Date$
!$Revision$
!$HeadURL$

! NOTE! depth to bottom of soil layers coming in are in "mm" and not "m" as
!       this routine thinks.  I believe I have "quick fixed" the problems here.
!       5/14/99 - LEW

      subroutine cinit(bnslay, bszlyt, bszlyd, bsdblk, bsfcce, bsfcec,  &
     &           bsfsmb, bsfom, bsfcla, bs0ph,                          &
     &           bsmno3,                                                &
     &           bc0fd1, bc0fd2, bctopt, bctmin,                        &
     &           cc0fd1, cc0fd2,                                        &
     &           dd, mm, yy,                                            &
     &           bcthudf, bctdtm, bcthum, bc0hue, bcdmaxshoot,          &
     &           bc0shoot, bc0growdepth, bc0storeinit,                  &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmshoot, bcmtotshoot, bcmbgstemz,                     &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bczht, bczshoot, bcdstm, bczrtd,                       & 
     &           bcdayap, bcdayam, bcthucum, bctrthucum,                &
     &           bcgrainf, bczgrowpt, bcfliveleaf,                      &
     &           bcleafareatrend, bcstemmasstrend, bctwarmdays,         &
     &           bctchillucum, bcthardnx, bcthu_shoot_beg,              &
     &           bcthu_shoot_end, bcdpop, bcdayspring)

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     This subroutine initializes parameters for a crop every time it is planted.

!     + + + KEYWORDS + + +
!     Initialization

      use weps_interface_defs
      use file_io_mod, only: luoinpt
      use p1unconv_mod, only: mgtokg, mmtom

!     + + + ARGUMENT DECLARATIONS + + +
      integer bnslay, dd, mm, yy, bcthudf, bctdtm
      real bszlyt(*)  ! added so a local variable would be set correctly - LEW
      real bszlyd(*), bsdblk(*), bsfcce(*), bsfcec(*), bsfsmb(*)
      real bsfom(*), bsfcla(*), bs0ph(*)
      real bc0fd1, bc0fd2, bctopt, bctmin
      real cc0fd1, cc0fd2
      real bsmno3
      real bcthum, bc0hue, bcdmaxshoot, bc0shoot
      real bc0growdepth, bc0storeinit
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
      real bcdpop
      integer bcdayspring

!     + + + ARGUMENT DEFINITIONS + + +
!     bsmno3  - fertilizer applied as NO3
!     bsdblk  - bulk density of a layer (g/cm^3=t/m^3)
!     bsfcce  - calcium carbonate (%)
!     bsfcla  - % clay
!     bsfom   - percent organic matter
!     bsfcec  - cation exchange capacity (cmol/kg)
!     dmag    - above ground biomass (t/ha)
!     bnslay  - number of soil layers
!     bs0ph   - soil pH
!     bsfsmb  - sum of bases (cmol/kg)
!     bszlyd  - depth from top of soil to botom of layer, m
!     bcthudf - flag 0-grow in days to maturity, 1-grow in heat units
!     bctdtm  - db input days to maturity
!     bcthum  - db input heat units to maturity
!     bc0hue - relative heat unit for emergence (fraction)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bc0shoot - mass from root storage required for each regrowth shoot (mg/shoot)
!                seed shoots are smaller and adjusted for available seed mass
!     bc0growdepth - db input, initial depth of growing point (m)
!     bc0storeinit - db input, crop storage root mass initialzation (mg/plant)
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
!     bcthucum - crop accumulated heat units
!     bctrthucum - accumulated root growth heat units (degree-days)
!     bcgrainf - internally computed reproductive grain fraction
!     bczgrowpt - depth in the soil of the gowing point (m)
!     bcfliveleaf - fraction of standing plant leaf which is living (transpiring)
!     bcleafareatrend - direction in which leaf area is trending.
!                       Saves trend even if leaf area is static for long periods.
!     bcstemmasstrend - direction in which stem mass is trending.
!                        Saves trend even if stem mass is static for long periods.
!     bctwarmdays - number of consecutive days that the temperature has been above the minimum growth temperature
!     bcthardnx - hardening index for winter annuals (range from 0 t0 2)
!     bctchillucum - accumulated chilling units (deg C day)
!     bcthu_shoot_beg - heat unit total for beginning of shoot grow from root storage period
!     bcthu_shoot_end - heat unit total for end of shoot grow from root storage period
!     bcdpop - Number of plants per unit area (#/m^2)
!            - Note: bcdstm/bcdpop gives the number of stems per plant
!     bcdayspring - day of year in which a winter annual releases stored growth

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'
      include 'p1solar.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 'm1sim.inc'
      include 'c1gen.inc'
      include 'm1subr.inc'
      include 'w1clig.inc'
      include 'c1info.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'crop/csoil.inc'
      include 'crop/chumus.inc'
      include 'crop/cfert.inc'
      include 'crop/cgrow.inc'
      include 'crop/cenvr.inc'
      include 'crop/cparm.inc'
      include 'crop/p1crop.inc'

!     + + + FUNCTION DECLARATIONS + + +
!      integer dayear
!      real daylen
!      real huc1

!     + + + LOCAL VARIABLES + +
!     character*20 cpnm
      integer i,n, pdate,hdate,j, m, sdmn, sdmx, dxx
      real bsa, dg, dg1, hlmn, wt1, xz, jreal
!      real x1, xx
      real hlmx
      real dy_mon(14),mx_air_temp(14),mn_air_temp(14)
      real mx_air_temp2(14), mn_air_temp2(14)
      real sphu, yp1, ypn, bphu, ephu
      real max_air, min_air, heat_unit !,d1(365)%cumheatunits,d2(730)%cumheatunits

      type day_heatunits
          integer day
          real heatunits
          real cumheatunits
      end type day_heatunits
      type(day_heatunits) d1(365), d2(730)

!     + + + LOCAL VARIABLE DEFINITIONS + + +

! bc0fd1  - minimum temperature below zero (c) (1st x-coordinate in the
!          frost damage s-curve)
! bc0fd2  - minimum temperature below zero (c) (2nd x-coordinate in the
!          frost damage s-curve)
! bctmin  - base temperature (deg. c)
! bn      - N uptake parametres at different stages of growth(ratio)
! bp      - P uptake parametres at different stages of growth(ratio)
! bsa     - base saturation (%?)
! cc0fd1  - fraction of biomass lost each day due to frost
!          (1st y-coordinate in the frost damge s-curve)
! cc0fd2  - fraction of biomass lost each day due to frost
!          (2nd y-coordinate in the frost damge s-curve)
! ch     - interim variable in calculations of day length
! ck     - extinction coeffficient (fraction)
! co2    - co2 concentration in the atmosphere (ppm)
! cpnm   - crop name
! dg     - soil layer thickness (mm)
! dg1    - previous value of dg
! dlax1  - fraction of grwing season (1st x-coordinate in lai s-curve)
! dlay1  - fraction of maximum lai (1st y-coordinate in the lai s-curve)
! dlax2  - fraction of grwing season (2nd x-coordinate in lai s-curve)
! dlay2  - fraction of maximum lai (2nd y-coordinate in the lai s-curve)
! dtm    - days to maturity
! frs1   - same as bc0fd1 (value needed in cfrost.for:11/20/96)
! frs2   - same as bc0fd2 (value needed in cfrost.for:11/20/96)
! h      - interim variable in calculations of day length
! hdate  - day of year harvest occurs
! hi     - harvest index of a crop
! hix1   - ratio of actual to potential et (1st x-coordinate in the
!          water stress - harvest index s-curve)
! hiy1   - fraction of reduction in harvest index (1st x-coordinate in
!          the water stress - harvest index s-curve)
! hix2   - ratio of actual to potential et (2nd x-coordinate in the
!          water stress - harvest index s-curve)
! hiy2   - fraction of reduction in harvest index (2nd x-coordinate
!          in the water stress - harvest index s-curve)
! hlmn   - minimum daylength for a site (hr)
! hlmx   - maximum daylength for a site (hr)
! hmx    - maximum potential plant height (m)
! hui0   - heat unit index when leaf senescence starts.
! irint -  flag for printing end-of-season values : 10/6/99
! nc     - crop number
! pdate  - day of year planting can occur
! phu    - potential heat units for crop maturity (deg. c)
! rbmd   - biomass-energy ratio decline factor
! rlad   - crop parameter that governs leaf area index decline rate
! s11x1  - soil labile p concentration (ppm) (1st x-coordinate in the
!          p uptake reduction s-curve)
! s11y1  - p uptake restriction factor (1st y-coordinate in the p
!          uptake reduction s-curve)
! s11x2  - soil labile p concentration (ppm) (2nd x-coordinate in the
!          p uptake reduction s-curve)
! s11y2  - p uptake restriction factor (2nd y-coordinate in the p
!          uptake reduction s-curve)
! s8x1   - scaled ratio of actaul to potential n or p (1st x-coordinate
!          in the n stress factor s-curve)
! s8y1   - n or p stress factor (1st ycoordinate in the n or p stress
!          s-curve)
! s8x2   - scaled ratio of actaul to potential n or p (2nd x-coordinate
!          in the n stress factor s-curve)
! s8y2   - n or p stress factor (2nd ycoordinate in the n or p stress
!          s-curve)
! sdmn   - day of the year when daylength is minimum
! sdmx   - day of the year when daylength is maximum
! sphu   - running sum of heat units
! bphu   - sphu at planting date
! ephu   - sphu at harvest date
! heat_unit - daily heat units
! max_air - dayly maximum air temperature (cubic spline)
! min_air - dayly minimum air temperature (cubic spline)
! topt   - optimum temperature (deg. c)
! wsyf   - minimum crop harvest index under drought
! wt1    - convert N, P, etc. conc. from g/t to kg/t
! x1     - temporary bulk density value
! xx     - depth of above layer
! xz     -

!     + + + SUBROUTINES CALLED + + +
!     scrv1
!     sdst
!     nconc

!     + + + INPUT FORMATS + + +
! print crop input data


!     + + + OUTPUT FORMATS + + +
! 2110 format (5x,' a_co2=',f6.3,' b_co2=',f6.3,' a_frst=',f6.3,         &
!     &' b_frst=',f6.3)
 2120 format (i5, i7, i9, i11, i10, 2x, 2f10.1)

!     Initialize
!     initialize variables needed for season heat unit computation: added on
!     3/16/1998 (A. Retta)
      data dy_mon /-15,15,45,74,105,135,166,196,227,258,288,319,349,380/
!     transfer average monthly temperatures from the global array to a local.
!     For the southern hemisphere, monthly average temperatures should start
!     in July.1?
      do i=1,12
          mx_air_temp(i+1) = awtmxav(i)
          mn_air_temp(i+1) = awtmnav(i)
      end do
      mx_air_temp(1) = mx_air_temp(13)
      mx_air_temp(14) = mx_air_temp(2)
      mn_air_temp(1) = mn_air_temp(13)
      mn_air_temp(14) = mn_air_temp(2)

!     added algorithm to compute yield adjustment factor; 10/18/99
!     tdmag=15.0
!     if (dmag.le.0.)yaf=1.0
!     if (dmag.gt.0.)yaf=log(tdmag/0.00001)/log(dmag/0.00001)
!     write (*,*)'yaf, dmag, tdmag  ' ,yaf,dmag,tdmag

      ! determine number of shoots (for seeds, bc0shoot should be much
      ! greater than bc0storeinit resulting in one shoot with a mass
      ! reduced below bc0shoot
      ! units are mg/plant * plant/m^2 / mg/shoot = shoots/m^2
      bcdstm = bc0storeinit * bcdpop / bc0shoot
      if( bcdstm .lt. bcdpop ) then
          ! adjust count to reflect limit
          bcdstm = bcdpop
          ! not enough mass to make a full shoot
          ! adjust shoot mass to reflect storage mass for one shoot per plant
          ! units are mg/plant * kg/mg * plant/m^2 = kg/m^2
          bcmtotshoot = bc0storeinit * mgtokg * bcdpop
      else if( bcdstm .gt. bcdmaxshoot*bcdpop ) then
          ! adjust count to reflect limit
          bcdstm = bcdmaxshoot * bcdpop
          ! more mass than maximum number of shoots
          ! adjust total shoot mass to reflect maximum number of shoots
          ! units are shoots/m^2 * mg/shoot * kg/mg = kg/m^2
          bcmtotshoot = bcdstm * bc0shoot * mgtokg
      else
          ! mass and shoot number correspond
          ! units are mg/plant * kg/mg * plant/m^2 = kg/m^2
          bcmtotshoot = bc0storeinit * mgtokg * bcdpop
      end if

      ! All types initialized with no stem, leaves or roots, just root storage mass
      ! transplants start with a very short time to "sprout"
      bcmstandleaf = 0.0
      bcmstandstem = 0.0
      bcmstandstore = 0.0
      bcmflatstem = 0.0
      bcmflatleaf = 0.0
      bcmflatstore = 0.0
      bcmshoot = 0.0

      do i=1,bnslay
          bcmbgstemz(i) = 0.0
          bcmrootfiberz(i) = 0.0
      end do

      bczht = 0.0
      bczshoot = 0.0

      bcdayap = 0
      bcdayam = 0
      bcthucum = 0.0
      bctrthucum = 0.0
      bcgrainf = 0.0
      bczgrowpt = bc0growdepth
      bcfliveleaf = 1.0
      bcleafareatrend = 0.0
      bcstemmasstrend = 0.0

      ! zero out day of year that spring growth is released
      bcdayspring = 0

      ! root depth
      bczrtd = bc0growdepth

      ! initialize the root storage mass into a single layer
      if( (bszlyd(1)*mmtom .gt. bczrtd) ) then
          ! mg/plant * #/m^2 * 1kg/1.0e6mg = kg/m^2
          bcmrootstorez(1) = bc0storeinit * bcdpop * mgtokg
!          write(*,*) "cinit: stor lay ", 1, bczrtd, bcmrootstorez(1)
      else
          bcmrootstorez(1) = 0.0
      end if
      do i=2,bnslay
          if( ( (bszlyd(i-1)*mmtom .lt. bczrtd)                         &
     &        .and. (bszlyd(i)*mmtom .ge. bczrtd) ) ) then
              ! mg/plant * #/m^2 * 1kg/1.0e6mg = kg/m^2
              bcmrootstorez(i) = bc0storeinit * bcdpop * mgtokg
!              write(*,*) "cinit: stor lay ", i, bczrtd, bcmrootstorez(i)
          else
              bcmrootstorez(i) = 0.0
          end if
      end do

      bctwarmdays = 0
      bctchillucum = 0.0
      ! hardening index (can be used for freeze kill calculations)
      bcthardnx = 0.0
      ! set initial emergence (shoot growth) values
      bcthu_shoot_beg = 0.0
      bcthu_shoot_end = bc0hue

      cta=0.
      ceta=0.
      prcp=0.
      ctp=0.
      daye=0
      frs1=bc0fd1
      frs2=bc0fd2
      slaix = 0.0
      ssaix = 0.0

!     set variable in local include file
      xlat = amalat

!     minimum and maximum daylength for a location
      if (amalat.gt.0.) then
          sdmn = 354
          sdmx = 173
      else
          sdmn = 173
          sdmx = 354
      end if
      hlmn = daylen(amalat, sdmn, civilrise)
      hlmx = daylen(amalat, sdmx, civilrise)

!     planting day of year
      call caldatw(dd, mm, yy)
      pdate = dayear(dd, mm, yy)

!     initial daylength calculations
      hrlt = daylen(amalat, pdate, civilrise)
      hrlty = daylen(amalat, pdate-1, civilrise)

!     start calculation of seasonal heat unit requirement
      sphu = 0.
      ephu = 0.
      bphu = 0.
      n = 14
      yp1 = 1.0e31    ! signals spline to use natural bound (2nd deriv = 0)
      ypn = 1.0e31    ! signals spline to use natural bound (2nd deriv = 0)

      ! call cubic spline interpolation routines for air temperature
      call spline (dy_mon, mx_air_temp, n, yp1, ypn, mx_air_temp2)
      call spline (dy_mon, mn_air_temp, n, yp1, ypn, mn_air_temp2)
      do i = 1, 365
          jreal = i
          ! calculate daily temps. and heat units
          call splint(dy_mon,mx_air_temp,mx_air_temp2,n,jreal,max_air)
          call splint(dy_mon,mn_air_temp,mn_air_temp2,n,jreal,min_air)
          heat_unit = huc1(max_air, min_air, bctopt, bctmin)
          d1(i)%day=i
          d1(i)%heatunits=heat_unit
          d2(i)%day=i
          d2(i)%heatunits=heat_unit
      end do
!     duplicate the first year into the second year
      do j=1,365
          m=j+365
          d2(m)%day=m
          d2(m)%heatunits=d1(j)%heatunits
      end do
!     running sum of heat units
      do j=1,730
          sphu=sphu+d2(j)%heatunits
          d2(j)%cumheatunits=sphu
!          if (am0cfl .gt. 0) then
!              print for debugging
!              write(luoinpt,*) d2(j)%day,d2(j)%heatunits,d2(j)%cumheatunits
!          end if
      end do
      sphu=0.

!     find dtm or phu depending on heat unit flag=1
      do j=1,730
            if (d2(j)%day.eq.pdate) bphu = d2(j)%cumheatunits
      end do
      if (bcthudf.eq.1) then
         ! use heat unit calculations to find dtm 
         phu = bcthum
         do j=1,730
            if (d2(j)%cumheatunits.le.bphu+phu) dtm = d2(j)%day - pdate
         end do
         hdate = pdate + dtm
      else
         ! calculate average seasonal heat units
         dtm=bctdtm
         hdate = pdate + dtm
         if( hdate.gt.d2(730)%day) then
            ! this crop grows longer than one year
            ephu = d2(730)%cumheatunits
            phu = ephu - bphu
            ! cap this at two years
            dxx = min(730,hdate - int(d2(730)%day))
            ehu = d2(dxx)%cumheatunits
            phu = phu + ehu
         else
            do j=1,730
               if (d2(j)%day.eq.hdate) ephu = d2(j)%cumheatunits
            end do
            phu = ephu - bphu
         end if
      end if

      ! print out heat average heat unit and days to maturity
      if (am0cfl .gt. 0) then
         write(luoinpt,2120) pdate,hdate,bcthudf,dtm,bctdtm, phu, bcthum
      end if

      ! after printing the value, set the global parameter for maximum
      ! heat units to the new calculated value (this database value is
      ! read from management file every time crop is planted, so changing
      ! it here does not corrupt it)
      bcthum = phu

!     Calculate s-curve parameters
      call scrv1(bc0fd1,cc0fd1,bc0fd2,cc0fd2,a_fr,b_fr)   ! Frost damage

      trsd = 0.0
      tfon = 0.0
      tfop = 0.0
      tmp = 0.0
      top = 0.0
      twn = 0.0
      twmn = 0.0
      tp = 0.0
      tap = 0.0
      tno3 = 0.0
      do i=1,bnslay
!          x1=bsdblk(i)
          ! dg=soil layer thickness in mm; xx=bszlyd(i-1); wt(i)=wt of soil(t/ha-mm)
          ! (convert to t/ha ??) wt1=convert N, P, etc. conc. from g/t to kg/t
          ! bsfcla(i)=100.-san(i)-sil(i)
          ! NOTE:  bszlyd is in "mm" and not "m" - 5/14/99 - LEW
          ! Since "dg" is just the layer thickness in "mm", we have set it
          ! to the now included WEPS global layer thickness variable here.
          ! dg=1000.*(bszlyd(i)-xx)
          dg = bszlyt(i)
          wt(i)=bsdblk(i)*dg*10.
          wt1=wt(i)/1000.
          ! estimate initial values of: rsd(residue:t/ha);ap(labile P conc.:g/t);
          ! wno3 (no3 conc.:g/t). dg1=previous value of dg.
          ! call sdst (rsd,dg,dg1,i)
          call sdst (ap,dg,dg1,i)
          call sdst (wno3,dg,dg1,i)
          trsd=trsd+rsd(i)
          dg1=dg
          ! calculate ratio (rtn) of active(wmn) to total(wn) N pools associated
          ! with humus. yc=years of cultivation.
          ! yc was set to 150. as suggested by A. Retta - jt 1/10/94
          yc = 150.
          if (i.eq.1) rtn(1)=0.4*exp(-0.0277*yc)+0.1
          if (i.gt.1) rtn(i)=rtn(1)*.4

          ! estimate bsfcec, bsa
          ! there are some very questionable things done here - jt  1/10/94
          ! xx=bszlyd(i)
          if (bsfcec(i).gt.0.) then
              if (bsfcce(i).gt.0.) bsfsmb(i)=bsfcec(i)
              if (bsfsmb(i).gt.bsfcec(i)) bsfsmb(i)=bsfcec(i)
              bsa=bsfsmb(i)*100./(bsfcec(i)+1.e-20)
              bsfcec(i)=bs0ph(i)
              bsfsmb(i)=bsfcec(i)
          endif

          ! calculate amounts of N & P (kg/ha) from fresh organic matter(rsd),
          ! assuming that N content of residue is 0.8%; fon & fop are in kg/ha.
          fon(i)=rsd(i)*8.
          fop(i)=rsd(i)*1.1
          tfon=tfon+fon(i)
          tfop=tfop+fop(i)

          ! initial organic(humus) N & P concentrations (g/t) in the soil
          if (wn(i).eq.0.) wn(i)=1000.*bsfom(i)
          if (wp(i).eq.0.) wp(i)=0.125*wn(i)

          ! Estimate psp(P sorption ratio), which is the fraction of fertilizer P that
          ! remains in the labile form after incubation for different soil conditions.
          ! The weathering status code ids is inputted.
          ! ids=1:esitmate psp for calcareous soils without weathering information
          ! ids=2:estimate psp for noncalcareous slightly weathered soils
          ! ids=3:estimate psp for noncalcareous moderately weathered soils
          ! ids=4:estimate psp for noncalcareous highly weathered soils
          ! ids=5: input value of psp
          ! finally estimate the flow coefficient bk between the active and stable
          ! P pools (1/d)
          ids = 1
          if (ids.eq.1) then
              psp(i)=0.5
              if (bsfcce(i).gt.0.) psp(i)=0.58-0.0061*bsfcce(i)
          endif
          if (ids.eq.2) psp(i)=0.02+0.0104*ap(i)
          if (ids.eq.3) psp(i)=0.0054*bsa+0.116*bs0ph(i)-0.73
          if (ids.eq.4) psp(i)=0.46-0.0916*alog(bsfcla(i))
          if (psp(i).lt.0.05) psp(i)=0.05
          if (psp(i).gt.0.75) psp(i)=0.75
          bk(i)=0.0076
          if (bsfcce(i).gt.0.) bk(i)=exp(-1.77*psp(i)-7.05)

          ! calculate initial amount of active(pmn) and stable(op) mineral P pools
          ! ap=initial amount of labile P(g/t);wt1=conversion factor to kg/ha
          pmn(i)=ap(i)*(1.-psp(i))/psp(i)*wt1
          tmp=tmp+pmn(i)
          op(i)=4.*pmn(i)
          top=top+op(i)

          ! calculate amount of active(readily mineralizable) humus N pool(wmn)
          ! and total(active+stable) humus N pool(wn)
          wmn(i)=rtn(i)*wn(i)
          wn(i)=wn(i)-wmn(i)
          wn(i)=wn(i)*wt1
          twn=twn+wn(i)
          wmn(i)=wmn(i)*wt1
          twmn=twmn+wmn(i)

          ! convert total(active+stable) humus P pool to kg/ha
          wp(i)=wp(i)*wt1
          tp=tp+wp(i)

          ! convert initial no3 & labile p in the soil to kg/ha
          ap(i) = ap(i) * wt1
          tap = tap + ap(i)
          wno3(i) = wno3(i) * wt1
          tno3 = tno3 + wno3(i)

          ! calculate amount of humus in a layer (t/ha)
          ! moved from original location
          xz = bsfom(i) * .0172
          hum(i) = xz * wt(i)
      enddo

      ! add applied fertilizer to the top layer
      wno3(1) = wno3(1) + bsmno3
      tno3 = tno3 + bsmno3

      ! write (*,*) 'past cinit slai=',slai
      return
      end
