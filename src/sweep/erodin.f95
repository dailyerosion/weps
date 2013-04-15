!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine erodin (i_unit, o_unit, cmdebugflag, already_read_inputs, subrsurf)

!     +++ PURPOSE +++
!     Utility to read initial conditions and variables from
!     input file (stdin or erod.in) for the standalone erosion submodel
!
!     If "o_unit" == stdout (6) then input not echo'd

!     + + + Modules Used + + +
      use Polygons_Mod
      use subregions_mod, only: subr_poly, acct_poly
      use erosion_data_struct_defs, only: subregionsurfacestate, create_subregionsurfacestate, awdair, anemht, awzzo, wzoflg, &
                                          ntstep, awadir, awudmx, subday, am0efl, am0eif
      use p1erode_def, only: SLRR_MIN, SLRR_MAX, WZZO_MIN, WZZO_MAX
      use barriers_mod
      use grid_geo_def, only: amasim, amxsim

!     +++ ARGUMENT DECLARATIONS +++
      integer i_unit, o_unit, cmdebugflag, already_read_inputs
      type(subregionsurfacestate), dimension(:), allocatable :: subrsurf

!     +++ ARGUMENT DEFINITIONS +++
!
!
!     +++ PARAMETERS +++
!
      integer mrcl
      parameter (mrcl = 512)
      integer xchl
      parameter (xchl = 12)

!     + + + LOCAL COMMON BLOCKS + + +
      integer debugflg
      common /flags/ debugflg
!
      integer xplot,xflag
      character*(xchl) xcharin(30)
      real xin(30)
      common /plot/ xplot, xcharin, xin
!
!     +++ LOCAL VARIABLES +++
      integer i,j,k
      integer sr,ibr,a,l,h
      integer wflg
      real :: f(ntstep), wu(ntstep)
      real wfcalm, wuc, w0k, step
      integer :: poly_np     ! number of points to be read in for polygon or polyline
      integer :: ipol        ! index counter for reading in polygon or polyline points
      integer :: alloc_stat  ! indicates status of memory allocation attempt
      integer :: sum_stat    ! accumulates for multiple allocations, one error statement.
      integer :: nsubr       ! number of subregions (read from input file)
      integer :: nacctr      ! number of accounting regions (read from input file)
      integer :: nbr         ! number of barriers (read from input file)

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     i, j, k = do-loop indices
!     x,y,sr,b,a,l,h = do-loop indices
!     wflg = flag to determine format of wind speed data (0 = Weibull, 1 = real)
!     debugflg = flag to output debug data (0 = none, 1 = input, 2 = more, etc.)
!     xplot    = flag to put plot data in arrays
!               (value>0 = no. indep input variable, 0= none)
!     f(mntime) = cumulative frequency of wind at speeds < subday(i)%awu
!     wfcalm    = wind fraction intercept (+calm, - no calm in period)
!     wuc       = Weibull wind speed distribution scale factor (m/s)
!     w0k       = Weibull wind speed distribution shape factor
!     step      = tmp real variable for ntstep
!     xcharin(i)= indep. variable name(s) used in plot
!     xin(i)    = indep. variable value(s) used in plot

!     +++ FUNCTIONS CALLED +++
!     getline
!
      character*(mrcl) getline
      character*(mrcl) line

!     +++ END SPECIFICATIONS +++

!     +++ INIT STUFF +++
      if (already_read_inputs .gt. 0) goto 999  !Only write output 

      debugflg = 0 !needs to be initialized when using full debugging compiles
!     Read the debug flag to specify level of debug support
!     (currently only input file level debug supported)

      line = getline(i_unit)
      if (cmdebugflag .lt. 0) then  !commandline option not set - use input file setting
          read (line,*) debugflg
!         read ((line=getline(i_unit)),*) debugflg
       else
          debugflg = cmdebugflag  !use commandline setting
      endif

!     EROSION initialization flag (logical)
      line = getline(i_unit)
      read (line,*) am0eif
!     read (getline(i_unit),*) am0eif

!     EROSION "print" flag (integer)
      line = getline(i_unit)
      read (line,*) am0efl
!     read (getline(i_unit),*) am0efl

!     +++ SIMULATION REGION +++

!     Simulation region diagonal corners (x1,y1) and (x2,y2)
      line = getline(i_unit)
      read (line,*) amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y

!     Simulation region orientation angle
      line = getline(i_unit)
      read (line,*) amasim
!     read (getline(i_unit),*) amasim

!     +++ ACCOUNTING REGIONS +++

!     Number of accounting regions
      line = getline(i_unit)
      read (line,*) nacctr

      ! create accounting region polygon array
      allocate(acct_poly(nacctr), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, acct_poly'
      end if

      do 20  a = 1, nacctr
        ! read accounting region polygon point count
        line = getline(i_unit)
        read (line,*) poly_np
        ! create polygon point storage
        acct_poly(a) = create_polygon(poly_np)
        ! read in points
        do ipol = 1, poly_np
            ! read point pair
            line = getline(i_unit)
            read (line,*) acct_poly(a)%points(ipol)%x, acct_poly(a)%points(ipol)%y
        end do
   20 continue

!     +++ BARRIERS +++

!     Number of barriers
      line = getline(i_unit)
      read (line,*) nbr

!     NOTE: Barrier data must not be in the input file if "nbr = 0"
      if( nbr .gt. 0 ) then
        ! allocate structure for barriers
        allocate(barrier(nbr), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
           Write(*,*) 'ERROR: memory alloc., barriers'
        end if
      end if

      do ibr = 1, nbr
        ! number of points in barrier polyline
        line = getline(i_unit)
        read (line,*) poly_np
        ! create storage for point and barrier data
        barrier(ibr) = create_barrier(poly_np)
        ! read in points and point data
        do ipol = 1, poly_np
           ! read point pair
           line = getline(i_unit)
           read (line,*) barrier(ibr)%points(ipol)%x, barrier(ibr)%points(ipol)%y
           ! barrier height, width, porosity
           line = getline(i_unit)
           read (line,*) barrier(ibr)%param(ipol)%amzbr, barrier(ibr)%param(ipol)%amxbrw, barrier(ibr)%param(ipol)%ampbr
        end do
      end do

!     +++ SUBREGION REGIONS +++

!     m1subr.inc

!     Number of subregions
      line = getline(i_unit)
      read (line,*) nsubr

      ! create data array to hold input and derived values for each subregion
      sum_stat = 0
      allocate(subrsurf(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      ! create subregion polygon array
      allocate(subr_poly(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, subrsurf, subr_poly'
      end if

!     Dimensions, Biomass, Soil, and Hydrology (by subregion)
      do 100  sr=1, nsubr
        ! read subregion polygon point count
        line = getline(i_unit)
        read (line,*) poly_np
        ! create polygon point storage
        subr_poly(sr) = create_polygon(poly_np)
        ! read in points
        do ipol = 1, poly_np
            ! read point pair
            line = getline(i_unit)
            read (line,*) subr_poly(sr)%points(ipol)%x, subr_poly(sr)%points(ipol)%y
        end do

!     +++ BIOMASS +++

!     b1geom.inc

!       Biomass height
!        line = getline(i_unit)
!        read (line,*)  abzht(sr)
!!       read (getline(i_unit),*) abzht(sr)
!       Now reads in average "residue height" instead of "biomass height'
!       LEW - 1/26/06
        line = getline(i_unit)
        read (line,*)  subrsurf(sr)%adzht_ave

!     c1glob.inc

!       Crop height
        line = getline(i_unit)
        read (line,*)  subrsurf(sr)%aczht

!       Crop stem area index and leaf area index
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%acrsai, subrsurf(sr)%acrlai

!       Residue stem area index and leaf area index
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%adrsaitot, subrsurf(sr)%adrlaitot

!       use crop and residue values to find the total value
!       sum the stem area index and leaf area index values
        subrsurf(sr)%abrsai = subrsurf(sr)%acrsai + subrsurf(sr)%adrsaitot
        subrsurf(sr)%abrlai = subrsurf(sr)%acrlai + subrsurf(sr)%adrlaitot

        ! Compute the weighted average "biomass height" (residues and crop)
        ! which is used internally by the erosion code - LEW 1/26/06
        if (subrsurf(sr)%abrsai .le. 0.0) then
            subrsurf(sr)%abzht = 0.0
        else
            subrsurf(sr)%abzht = ( subrsurf(sr)%adzht_ave*subrsurf(sr)%adrsaitot                   &
                               + subrsurf(sr)%aczht*subrsurf(sr)%acrsai ) / subrsurf(sr)%abrsai
        endif
!     c1gen.inc

!       addition to code for biodrag
!       crop row spacing and seed location
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%acxrow, subrsurf(sr)%ac0rg

!       These aren't used in EROSION yet
!       Biomass stem area index by height
!       read (getline(i_unit),*) (abrsaz(h,sr), h=1,mncz)
!       Biomass leaf area index by height
!       read (getline(i_unit),*) (abrlaz(h,sr), h=1,mncz)

!       Biomass flat fraction cover, standing cover, and fraction total cover
!       read (getline(i_unit),*) abffcv(sr), abfscv(sr), abftcv(sr)
!       Only flat fraction cover used yet
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%abffcv

!     +++ SOIL +++

!       s1layr.inc, s1phys.inc & s1agg.inc

!       Number of soil layers (in this subregion)
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%nslay

        ! allocate arrays for soil layer and surface wetness values
        call create_subregionsurfacestate(subrsurf(sr)%nslay, 24, subrsurf(sr))

!       Soil layer thickness
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%aszlyt,l=1,subrsurf(sr)%nslay)

!       Soil layer bulk density
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asdblk, l=1,subrsurf(sr)%nslay)

!       Sand, silt, and clay fractions
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asfsan, l=1,subrsurf(sr)%nslay)

!       read very fine sand content edit 6-9-01 LH
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asfvfs, l=1,subrsurf(sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asfsil, l=1,subrsurf(sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asfcla, l=1,subrsurf(sr)%nslay)

!       Volume of rock fraction
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asvroc, l=1,subrsurf(sr)%nslay)

!       s1agg.inc
!       Soil layer aggregate density
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%asdagd, l=1,subrsurf(sr)%nslay)

!       Soil layer aggregate stability
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%aseags, l=1,subrsurf(sr)%nslay)

! Check these variables with ASD inc files and Hagen's EROSION inc files - LEW
!       Soil layer ASD parms (gmd, min, max, gsd)
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%aslagm, l=1,subrsurf(sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%aslagn, l=1,subrsurf(sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%aslagx, l=1,subrsurf(sr)%nslay)

        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%as0ags, l=1,subrsurf(sr)%nslay)

!       s1surf.inc & s1sgeo.inc

!       Crust parms (fraction, thickness)
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%asfcr, subrsurf(sr)%aszcr, &
!       Crust parms (fraction cover of loose material, mass loose material)
             subrsurf(sr)%asflos, subrsurf(sr)%asmlos, &
!       Crust parms (crust density and stability)
             subrsurf(sr)%asdcr, subrsurf(sr)%asecr

!       Random Roughness
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%aslrr

        !Lower and upper limits of grid cell RR allowed by erosion submodel
        if (subrsurf(sr)%aslrr < SLRR_MIN) then
           write(0,*) 'slrr: ', subrsurf(sr)%aslrr,' < ', SLRR_MIN
        end if
        if (subrsurf(sr)%aslrr > SLRR_MAX) then
           write(0,*) 'slrr: ', subrsurf(sr)%aslrr,' < ', SLRR_MIN
        end if

        !Lower and upper limits of grid cell aerodynamic roughness allowed
        !by erosion submodel (currently determined by equation used here)
        if (subrsurf(sr)%aslrr < (WZZO_MIN/0.3)) then
           write(0,*) 'slrr: ', subrsurf(sr)%aslrr
           write(0,*) 'wzzo < WZZO_MIN: ', subrsurf(sr)%aslrr*0.3,' < ', WZZO_MIN
        else if(subrsurf(sr)%aslrr > (WZZO_MAX/0.3)) then
           write(0,*) 'slrr: ', subrsurf(sr)%aslrr
           write(0,*) 'wzzo > WZZO_MAX: ', subrsurf(sr)%aslrr*0.3,' > ', WZZO_MAX
        end if

!       Oriented Roughness (ridge ht, spacing, width, orientation)
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%aszrgh, subrsurf(sr)%asxrgs, subrsurf(sr)%asxrgw, subrsurf(sr)%asargo

!       Oriented Roughness ( spacing)
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%asxdks

!     +++ HYDROLOGY +++

!       h1db1.inc

!       Snow depth
        line = getline(i_unit)
        read (line,*) subrsurf(sr)%ahzsnd

!       Soil layer wilting point
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%ahrwcw, l=1,subrsurf(sr)%nslay)

!       Soil layer water content
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%bsl(l)%ahrwca, l=1,subrsurf(sr)%nslay)

!       Soil surface hourly water content
        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%ahrwc0(h), h=1,12)

        line = getline(i_unit)
        read (line,*) (subrsurf(sr)%ahrwc0(h), h=13,24)

  100 continue

!     +++ WEATHER +++

! We need to check on the units for air density - variable definition says (kg/m^3)
! Also, we need to see why it currently isn't being used - LJH said it was
!     Air density
      line = getline(i_unit)
      read (line,*) awdair

!     Wind Direction
      line = getline(i_unit)
      read (line,*) awadir

!     Number of "steps" during 24 hours (96 = 15 minute intervals)
      line = getline(i_unit)
      read (line,*) ntstep

      ! allocate wind direction and speed array
      allocate(subday(ntstep), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, erodin wind direction and speed'
      end if

!     anemometer height, zo at anemom, and location (station or field)
!     note if flag=1, at field, awwzo will be changed to field value
      line = getline(i_unit)
      read (line,*) anemht, awzzo, wzoflg

!     Weibull wind flag (0 - read Weibull parms, 1 - read wind speeds)
      line = getline(i_unit)
      read (line,*) wflg

!     wind data inputs as the Weibull paramters
!     (wfcalm, wuc, w0k) is indicated by code ntstep = 99
      if (wflg .eq. 0) then

!       Weibull parms (fraction calm, c, k)
        line = getline(i_unit)
        read (line,*) wfcalm, wuc, w0k

!       calculate daily max wind speed (99% speed)
!       awudmx = wuc*(-log((1.0-0.99)/(1-wfcalm)))**(1.0/w0k)

!       calculate period wind speeds
        step = ntstep
        do 198 i= 1, ntstep
!         find center of each step and add empirical last term from file ntstep.mcd
            f(i) = (1.0/(2.0*step)) + ((i-1)/step) +0.3/(step*w0k)
!         to prevent out-of-range
          if (f(i) .lt. wfcalm) then
            f(i) = wfcalm
          endif
          subday(i)%awu = wuc*(-log((1.0-f(i))/(1.0-wfcalm)))**(1.0/w0k)
  198   end do
!       Use greatest interval wind speed rather than 99% speed above
        awudmx = subday(ntstep)%awu
!
!       change weibull wind speed dist. to a symmetric shape similar
!       to the daily distribution from wind gen

!       insure that ntstep is an even no.
        ntstep = (ntstep/2)*2

!       store wind speed in temp array
        do 110 i = 1, ntstep
          wu(i) = subday(i)%awu
  110   end do
!
!       generate the symmetric distribution
        i = -1
        do 115 j = 1, ntstep/2
           i = i+2
           subday(j)%awu = wu(i)
  115   continue
        i = ntstep+2
        do 125 j = (ntstep/2+1),ntstep
           i = i-2
           subday(j)%awu = wu(i)
  125   continue

      else     ! when (wflg .eq. 1) input wind period data directly
        do 191 j = 1, ntstep/6
          line = getline(i_unit)
          read (line,*) (subday(i)%awu,i=(j-1)*6+1,(j-1)*6+6)
191     end do
!       If not divisible evenly by 6, then get the remaining values
        if (mod(ntstep,6) .ne. 0) then
          line = getline(i_unit)
          read (line,*) (subday(i)%awu,i=(j-1)*6+1,(j-1)*6+mod(ntstep,6))
        endif

!     Determine the maximum wind speed during the day
        awudmx = 0.0
        do 193 i = 1, ntstep
           if( awudmx .lt. subday(i)%awu ) then
              awudmx = subday(i)%awu
           endif
  193   end do

      endif

!     + + + PLOT SECTION + + +
!     selectively reads succesive indep variable names and values
!     if xplot is set to zero and the xflag is set to
!      1 for a given variable in the erodin file
!
!     test if plotout file to be created
      line = getline(i_unit)
      read (line,*) xplot
      if (xplot .eq. 0) then
!     intialize xin array
      do 194 i=1,30
         xin(i) = 0.0
  194 continue
!
!     field length (good for wind parallel x-axis)
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = amxsim(2)%y - amxsim(1)%y
        else
          line = getline(i_unit)
        endif

!     biomass height
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%abzht
        else
          line = getline(i_unit)
        endif

!     biomass stem area index
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%abrsai
        else
          line = getline(i_unit)
        endif

!     biomass leaf area index
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%abrlai
        else
          line = getline(i_unit)
        endif

!     biomass flat cover
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%abffcv
        else
          line = getline(i_unit)
        endif

!     very fine sand
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%bsl(1)%asfvfs
        else
          line = getline(i_unit)
        endif

!     sand
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%asfsan
        else
          line = getline(i_unit)
        endif

!     silt
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%bsl(1)%asfsil
        else
          line = getline(i_unit)
        endif

!     clay
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%asfcla
        else
          line = getline(i_unit)
        endif

!     rock vol.
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%asvroc
        else
          line = getline(i_unit)
        endif

!     aggregate density
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%bsl(1)%asdagd
        else
          line = getline(i_unit)
        endif

!     aggregate stability
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%aseags
        else
          line = getline(i_unit)
        endif

!     agregate geometric mean diameter
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%aslagm
        else
          line = getline(i_unit)
        endif

!     aggreate minimum diameter
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%aslagn
        else
          line = getline(i_unit)
        endif

!     aggregate maximum diameter
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%aslagx
        else
          line = getline(i_unit)
        endif

!     aggregate geometric std dev
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%bsl(1)%as0ags
        else
          line = getline(i_unit)
        endif

!     soil fraction crust cover
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%asfcr
        else
          line = getline(i_unit)
        endif
!
!     surface crust thickness
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%aszcr
        else
          line = getline(i_unit)
        endif

!     fraction loose material on crust
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%asflos
        else
          line = getline(i_unit)
        endif

!     mass of loose material on crust
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%asmlos
        else
          line = getline(i_unit)
        endif

!     soil crust stability
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%asecr
        else
          line = getline(i_unit)
        endif

!     random roughness
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) = subrsurf(1)%aslrr
        else
          line = getline(i_unit)
        endif

!     ridge height
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%aszrgh
        else
          line = getline(i_unit)
        endif

!     ridge spacing
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%asxrgs
        else
          line = getline(i_unit)
        endif

!     ridge width
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%asxrgw
        else
          line = getline(i_unit)
        endif

!     ridge orientation
        line = getline(i_unit)
        read (line,*) xflag
        if (xflag .eq. 1) then
          xplot = xplot + 1
          line = getline(i_unit)
          xcharin(xplot) = line(1:xchl)
          xin(xplot) =  subrsurf(1)%asargo
        else
          line = getline(i_unit)
        endif
      endif

!     + + + OUTPUT SECTION + + +
  220 format (1x, 2f8.2)
  230 format (1x, 3f8.2)
  250 format (1x, i0)
  251 format (1x, 2f8.2)
  260 format (1x, 6f8.2)
  270 format (1x, 7f8.2)
  350 format (1x, i3, 7f8.2)
  400 format (1x, i3, 1x, l6, 4x, i3, 4x, i3, 4x, i3, 4x, i3)

999   if (o_unit .ne. 6) then  !Only echo input if stdout not specified
      write (o_unit,*)                                                  &
     &  '      REPORT OF INPUTS (read by erodin.for) '
      write (o_unit,*)
      write (o_unit,*)  '+++ Control Flags, etc. +++'
      write (o_unit,*)
      write (o_unit,*) 'ntstep  am0eif  nsubr  nacctr  nbr am0efl'
      write (o_unit,400) ntstep, am0eif, nsubr, nacctr, nbr,am0efl

      write (o_unit,*)
      write (o_unit,*)  '+++ SIMULATION REGION +++'
      write (o_unit,*)
      write (o_unit,*) 'orientation and dimensions of sim region'
      write (o_unit,*) 'amasim(deg)  amxsim - (x1,y1) (x2,y2)'
      write (o_unit,260) amasim,  amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y

      write (o_unit,*)
      write (o_unit,*)  '+++ ACCOUNTING REGIONS +++'
      write (o_unit,*)
      write (o_unit,*) 'nacctr - number of accounting regions'
      write (o_unit,*) nacctr
      write (o_unit,*) 'accounting region polygon, count, xy pairs'
      do 1000 a = 1, nacctr
        write (o_unit,250) acct_poly(a)%np
        do ipol = 1, acct_poly(a)%np
            write (o_unit,251) acct_poly(a)%points(ipol)%x, acct_poly(a)%points(ipol)%y
        end do
 1000 continue

      write (o_unit,*)
      write (o_unit,*)  '+++ BARRIERS +++'
      write (o_unit,*)
      write (o_unit,*) 'nbr - number of barriers'
      write (o_unit,*) nbr
      if (nbr .gt. 0) then
        write (o_unit,*) 'barrier dim (x1,y1) (x2,y2) ',                &
     &                   'and height, porosity, and width'
        do 1010 ibr = 1, nbr
          write (o_unit,270) barrier(ibr)%points(1)%x, barrier(ibr)%points(1)%y, &
                             barrier(ibr)%points(size(barrier(ibr)%points))%x, barrier(ibr)%points(size(barrier(ibr)%points))%y, &
     &                       barrier(ibr)%param(1)%amzbr, barrier(ibr)%param(1)%ampbr, barrier(ibr)%param(1)%amxbrw
 1010   continue
      endif

      write (o_unit,*)
      write (o_unit,*)  '+++ SUBREGIONS +++'
      write (o_unit,*)
      write (o_unit,*) 'nsubr - number of subregions'
      write (o_unit,*) nsubr
      write (o_unit,*) 'subregion polygon, count, xy pairs'
      do 1020 sr = 1, nsubr
        write (o_unit,250) subr_poly(sr)%np
        do ipol = 1, subr_poly(sr)%np
            write (o_unit,251) subr_poly(sr)%points(ipol)%x, subr_poly(sr)%points(ipol)%y
        end do
 1020 continue

      do 1100 sr = 1, nsubr
        write (o_unit,*)
        write (o_unit,*)
        write(o_unit,*) '*********************** Subregion ', sr,       &
     &                 ' ***********************'

        write (o_unit,*)
        write(o_unit,*) '+++ BIOMASS +++'
        write (o_unit,*)
        write (o_unit,*) 'Biomass ht,  flat cover'
        write (o_unit,220) subrsurf(sr)%abzht, subrsurf(sr)%abffcv

        write (o_unit,*)
        write (o_unit,*) 'Crop height, SAI,    LAI'
        write (o_unit,230) subrsurf(sr)%aczht, subrsurf(sr)%acrsai, subrsurf(sr)%acrlai

        write (o_unit,*)
        write (o_unit,*) 'Residue height, SAI,    LAI'
        write (o_unit,230) subrsurf(sr)%adzht_ave, subrsurf(sr)%adrsaitot, subrsurf(sr)%adrlaitot

        write (o_unit,*)
        write (o_unit,*) '+++ SOIL +++ '
        write (o_unit,*)
        write (o_unit,*) 'nslay - number of soil layers'
        write (o_unit,*) subrsurf(sr)%nslay

        write (o_unit,*)
        write (o_unit,*) 'layer depth b.density ',                      &
     &                   'vfsand   sand   silt   clay    rock vol'
        do 1030 l = 1, subrsurf(sr)%nslay
          write (o_unit,350) l, subrsurf(sr)%bsl(l)%aszlyt, subrsurf(sr)%bsl(l)%asdblk, &
                 subrsurf(sr)%bsl(l)%asfvfs, subrsurf(sr)%bsl(l)%asfsan, subrsurf(sr)%bsl(l)%asfsil, &
                 subrsurf(sr)%bsl(l)%asfcla, subrsurf(sr)%bsl(l)%asvroc
 1030   continue

        write (o_unit,*)
          write (o_unit,*) 'layer    AgD     AgS ',                     &
     &                   ' GMD    GMDmn     GMDmx    GSD'
        do 1040 l = 1, subrsurf(sr)%nslay
          write (o_unit,350) l, subrsurf(sr)%bsl(l)%asdagd, subrsurf(sr)%bsl(l)%aseags, &
                 subrsurf(sr)%bsl(l)%aslagm, subrsurf(sr)%bsl(l)%aslagn,   &
                 subrsurf(sr)%bsl(l)%aslagx, subrsurf(sr)%bsl(l)%as0ags
 1040   continue

        write (o_unit,*)
        write (o_unit,*) 'Crust frac thick mass LOS frac.LOS, ',        &
     &                   'density stability'
        write (o_unit,260) subrsurf(sr)%asfcr, subrsurf(sr)%aszcr, subrsurf(sr)%asmlos, subrsurf(sr)%asflos,    &
                           subrsurf(sr)%asdcr, subrsurf(sr)%asecr

        write (o_unit,*)
        write (o_unit,*) '    RR,    Rg ht,  width, spacing, ',         &
     &                   'orient., dike spacing'
        write (o_unit,270) subrsurf(sr)%aslrr, subrsurf(sr)%aszrgh, subrsurf(sr)%asxrgw,           &
                           subrsurf(sr)%asxrgs, subrsurf(sr)%asargo, subrsurf(sr)%asxdks

        write (o_unit,*)
        write (o_unit,*) '+++ HYDROLOGY +++ '
        write (o_unit,*)

        write (o_unit,*) 'Snow depth (mm)'
        write (o_unit,*) subrsurf(sr)%ahzsnd

        write (o_unit,*)
        write (o_unit,*) 'layer  wilting and actual water contents'
        do 1050 l = 1, subrsurf(sr)%nslay
          write (o_unit,350) l, subrsurf(sr)%bsl(l)%ahrwcw , subrsurf(sr)%bsl(l)%ahrwca
 1050   continue
        write (o_unit,*) 'Hourly water contents - ahrwc0'
        write (o_unit,260) (subrsurf(sr)%ahrwc0(h), h=1,6)
        write (o_unit,260) (subrsurf(sr)%ahrwc0(h), h=7,12)
        write (o_unit,260) (subrsurf(sr)%ahrwc0(h), h=13,18)
        write (o_unit,260) (subrsurf(sr)%ahrwc0(h), h=19,24)

 1100 continue

      write (o_unit,*)
      write (o_unit,*) '+++ WEATHER +++'
      write (o_unit,*)
      write (o_unit,*) ' anemht    awwzo   wzoflg '
      write (o_unit,*)   anemht, awzzo, wzoflg
      write (o_unit,*) ' wind dir (deg) and max wind speed (m/s)'
      write (o_unit,220) awadir, awudmx

      write (o_unit,*)
      write (o_unit,*) 'Wind speeds (m/s) - ', ntstep,' intervals'
      k = 6
      j = 1
  860 if(k .lt. ntstep) then
        write (o_unit,260) (subday(i)%awu, i=j,k)
        j = k+1
        k = k+6
        go to 860
      else
        k = ntstep
        write (o_unit,260) (subday(i)%awu, i=j,k)
      endif
      write (o_unit,*)
      write (o_unit,*) 'END OF INPUTS'

      endif !(o_unit .ne. 6)

      !close (i_unit)
      return
      end

!**********************************************************************
      character*(*) function getline(i_unit)

      integer debugflg
      common /flags/ debugflg

      integer i_unit
      integer dataline, linecount

      save dataline, linecount


1     read (i_unit, '(A)') getline
      linecount = linecount + 1
      if (BTEST(debugflg,0)) then
        write (6, *) linecount, ': ', trim(getline)
      endif

      if (getline(1:1) .ne. '#') goto 2
      goto 1

2     dataline = dataline + 1
      if (BTEST(debugflg,1)) then
        write (6, *) linecount, ':', dataline, ': ', trim(getline)
      endif

      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
