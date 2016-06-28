!$Author$
!$Date$
!$Revision$
!$HeadURL$

! NOTE! depth to bottom of soil layers coming in are in "mm" and not "m" as
!       this routine thinks.  I believe I have "quick fixed" the problems here.
!       5/14/99 - LEW

      subroutine cinit(isr, bnslay, bszlyd,                             &
     &           bctopt, bctmin,                                        &
     &           bcthudf, bctdtm, bcthum, bc0hue, bcdmaxshoot,          &
     &           bc0shoot, bc0growdepth, bc0storeinit,                  &
     &           bcmstandstem, bcmstandleaf, bcmstandstore,             &
     &           bcmflatstem, bcmflatleaf, bcmflatstore,                &
     &           bcmshoot, bcmtotshoot, bcmbgstemz,                     &
     &           bcmrootstorez, bcmrootfiberz,                          &
     &           bczht, bczshoot, bcdstm, bczrtd,                       & 
     &           bcdayap, bcdayam,                                      &
     &           bcthucum, bctrthucum,                                  &
     &           bcgrainf, bczgrowpt, bcfliveleaf,                      &
     &           bcleafareatrend, bcstemmasstrend, bctwarmdays,         &
     &           bctchillucum, bcthardnx, bcthu_shoot_beg,              &
     &           bcthu_shoot_end, bcdpop, bcdayspring)

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     This subroutine initializes parameters for a crop every time it is planted.

!     + + + KEYWORDS + + +
!     Initialization

      use weps_interface_defs, ignore_me=>cinit
      use datetime_mod, only: get_simdate_doy
      use file_io_mod, only: luoinpt
      use p1unconv_mod, only: mgtokg, mmtom
      use crop_data_struct_defs, only: am0cfl
      use climate_input_mod, only: cli_mav

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer bnslay, bcthudf, bctdtm
      real bszlyd(*)
      real bctopt, bctmin
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
!     bnslay  - number of soil layers
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
!     bcmtotshoot - total mass released from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emergence heat units
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
      include 'm1sim.inc'
      include 'c1gen.inc'
      include 'm1subr.inc'
      include 'c1info.inc'

!     + + + FUNCTION DECLARATIONS + + +
!      real daylen
!      real huc1

!     + + + LOCAL VARIABLES + +
      integer i,n, pdate,hdate,j, m, sdmn, sdmx, dxx
      integer dtm
      real phu
      real hlmn, jreal
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

! bctmin  - base temperature (deg. c)
! dtm    - days to maturity
! hdate  - day of year harvest occurs
! hlmn   - minimum daylength for a site (hr)
! hlmx   - maximum daylength for a site (hr)
! hmx    - maximum potential plant height (m)
! hui0   - heat unit index when leaf senescence starts.
! pdate  - day of year planting can occur
! phu    - potential heat units for crop maturity (deg. c)
! sdmn   - day of the year when daylength is minimum
! sdmx   - day of the year when daylength is maximum
! sphu   - running sum of heat units
! bphu   - sphu at planting date
! ephu   - sphu at harvest date
! heat_unit - daily heat units
! max_air - dayly maximum air temperature (cubic spline)
! min_air - dayly minimum air temperature (cubic spline)
! topt   - optimum temperature (deg. c)

!     + + + SUBROUTINES CALLED + + +
!     scrv1
!     sdst

!     + + + OUTPUT FORMATS + + +
 2120 format (i5, i7, i9, i11, i10, 2x, 2f10.1)

!     Initialize
!     initialize variables needed for season heat unit computation: added on
!     3/16/1998 (A. Retta)
      data dy_mon /-15,15,45,74,105,135,166,196,227,258,288,319,349,380/
!     transfer average monthly temperatures from the global array to a local.
!     For the southern hemisphere, monthly average temperatures should start
!     in July.1?
      do i=1,12
          mx_air_temp(i+1) = cli_mav%tmx(i)
          mn_air_temp(i+1) = cli_mav%tmn(i)
      end do
      mx_air_temp(1) = mx_air_temp(13)
      mx_air_temp(14) = mx_air_temp(2)
      mn_air_temp(1) = mn_air_temp(13)
      mn_air_temp(14) = mn_air_temp(2)

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
      pdate = get_simdate_doy()

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
!          if (am0cfl(isr) .gt. 0) then
!              print for debugging
!              write(luoinpt(isr),*) d2(j)%day,d2(j)%heatunits,d2(j)%cumheatunits
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
            phu = phu + d2(dxx)%cumheatunits
         else
            do j=1,730
               if (d2(j)%day.eq.hdate) ephu = d2(j)%cumheatunits
            end do
            phu = ephu - bphu
         end if
      end if

      ! print out heat average heat unit and days to maturity
      if (am0cfl(isr) .gt. 0) then
         write(luoinpt(isr),2120)                                       &
     &                      pdate,hdate,bcthudf,dtm,bctdtm, phu, bcthum
      end if

      ! after printing the value, set the global parameter for maximum
      ! heat units to the new calculated value (this database value is
      ! read from management file every time crop is planted, so changing
      ! it here does not corrupt it)
      bcthum = phu

      return
      end
