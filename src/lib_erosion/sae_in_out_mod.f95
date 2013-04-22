!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sae_in_out_mod

!     This module is for the creation of single or multiple stand alone erosion input files,
!     depending on the command line switches given

  type make_sae_in_out
     integer :: jday      ! the present day in julian days for output
     integer :: simday    ! the present day in simulation days for creation of file
     integer :: maxday    ! maximum simday number
     character*256 :: fullpath  ! the root path plus subdirectory if indicated by multiple files
  end type make_sae_in_out

  type(make_sae_in_out) :: mksaeinp
  type(make_sae_in_out) :: mksaeout

  ! placed here for sharing back with hagen_plot_flag
  real :: aegt, aegtss, aegt10
  logical :: in_weps

  contains

      subroutine saeinp( luo_saeinp, subrsurf )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
      use weps_interface_defs
      use file_io_mod, only: fopenk, makenamnum
      use grid_mod, only: amxsim, amasim
      use subregions_mod
      use barriers_mod, only: barrier
      use erosion_data_struct_defs, only: subregionsurfacestate, awzypt, awdair, anemht, awzzo, wzoflg, awadir, subday, ntstep

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(inout) :: luo_saeinp      ! output unit number
      type(subregionsurfacestate), dimension(:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     +++ LOCAL VARIABLES +++
      integer k,l, sr, ip
      integer b
      integer day, mon, yr
      

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     sr - index used in subregion loop
!     ip - index to polygon coordinates

!     +++ END SPECIFICATIONS +++

      if( luo_saeinp .lt. 0 ) then
        call fopenk (luo_saeinp, trim(mksaeinp%fullpath) // makenamnum('saeros', mksaeinp%simday, mksaeinp%maxday, '.in'),'unknown')
        call caldat (mksaeinp%jday,day,mon,yr)
        write(*,'(4(a,i0))') 'Made SWEEP input file D/M/Y: ', day,'/', mon,'/', yr,' simulation day: ', mksaeinp%simday
        write(luo_saeinp,2101) day, mon, yr
 2101   format('# WEPS erosion day mon yr',2(1x,i2),2x,i4)
      else
        write(luo_saeinp,*) '      REPORT OF INPUTS (read by erodin.for) '
      end if


!     print header info
      write(luo_saeinp,1005)
 1005 format ( &
      '#',65('*'),/, &
      '#',/, &
      '# +++ PURPOSE +++',/, &
      '#',/, &
      '#     File for input to standalone erosion submodel program (sweep)',/, &
      '#',/, &
      '#     All lines beginning with a "#" character are assumed to',/, &
      '#     be comment lines and are skipped.',/, &
      '#',/, &
      '#     +++ DEFINITIONS +++',/, &
      '#',/, &
      '#     All comments prior to each line of data input',/, &
      '#     in this template input file have the following format:',/, &
      '#',/, &
      '#     Variable_Name, Var_type, Text Definition',/, &
      '#',/, &
      '#     where Var_type is: I = integer L = logical R = real',/, &
      '#',/, &
      '#',/, &
      '# +++ DEBUG FLAG +++',/, &
      '#',/, &
      '#     debugflg - debug flag for providing different levels of debug info',/, &
      '#                currently useful to debug/check input file data format',/, &
      '#',/, &
      '#                value of 0 will print no debug information',/, &
      '#                value of 1 will print out and number all input file lines',/, &
      '#                value of 2 will print out and number all data input lines',/, &
      '#                value of 3 will do both 1 and 2')
      write(luo_saeinp,*) '0'

      write(luo_saeinp,2000)
 2000 format( &
      '#',/, &
      '#',/, &
      '# +++ INITIALIZATION +++',/, &
      '#',/, &
      '#     am0eif, L, EROSION "initialization" flag',/, &
      '#                Must be set to .TRUE. for standalone erosion runs')
      write(luo_saeinp,*) '.TRUE.'

      write(luo_saeinp,2100)
 2100 format ( &
      '#',/, &
      '#',/, &
      '# +++ SIMULATION REGION +++',/, &
      '#',/, &
      '#     amxsim(x,y), R, Simulation Region diagonal coordinates (meters)',/, &
      '#                     Input (x,y) coordinates in this form: x1,y1 x2,y2',/, &
      '#                     If orientation in 0 degrees, then x axis is East-West',/, &
      '#                     and y axis is North-South.',/, &
      '#                      Typical Range: 10.0 to 1600.0',/, &
      '#',/, &
      '#                     NOTE:  Accounting region and Subregion coordinates',/, &
      '#                            must also be set to the same values',/, &
      '#')
      write(luo_saeinp,*) amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y

      write(luo_saeinp,2105)
 2105 format( &
      '#',/, &
      '#',/, &
      '#     amasim, R, Simulation Region orientation angle (degrees from North)')
      write(luo_saeinp,*) amasim

      write(luo_saeinp,2110)
 2110 format('#',/, &
      '#  +++ ACCOUNTING REGIONS +++',/, &
      '#',/, &
      '# nacctr, I, Number of accounting regions')
      write(luo_saeinp,*) size(acct_poly)

      ! loop through all accounting regions
      do sr = 1, size(acct_poly)

      write(luo_saeinp,2115)
 2115 format('#',/, &
      '# accounting region polygon, count, xy pairs (subregions_mod)',/,&
      '#         polygons can be:',/, &
      '#         - open (the last point connects to the first point)',/,&
      '#         - closed (last point is same as first point)',/, &
      '#         - complex (multiple closed polygons entered as one)')
      ! number of coordinate pairs in polygon
      write(luo_saeinp,*) acct_poly(sr)%np
      ! the coordinate pairs
      do ip = 1, acct_poly(sr)%np
        write(luo_saeinp,*) acct_poly(sr)%points(ip)%x, acct_poly(sr)%points(ip)%y
      end do

      end do

!       barriers
      write(luo_saeinp,2120)
 2120 format ('#',/, &
      '# +++ BARRIERS +++',/, &
      '#',/, &
      '#     nbr, I, Number of barriers (0-5) ')
      write(luo_saeinp,*) size(barrier)
      write(luo_saeinp,2122)
 2122 format ('#',/, &
      '#     NOTE: Remaining BARRIER inputs are repeated for each barrier specified',/, &
      '#     If no barriers specified (nbr=0), then no BARRIER inputs will be here',/, &
      '#',/, &
      '#     barrier(b)%np, I, number of points in barrier polyline',/, &
      '#     Inputs are repeated for each point specified',/, &
      '#     barrier(b)%points(n)%x, barrier(b)%points(n)%y, R, x,y coordinate pair',/, &
      '# Example: 0.0 500.0 ',/, &
      '#     barrier(b)%points(n)%amzbr, R, Barrier height (m)',/, &
      '#     barrier(b)%points(n)%amxbrw, R, Barrier width (m)',/, &
      '#     barrier(b)%points(n)%ampbr, R, Barrier porosity (m^2/m^2)',/, &
      '# Example: 1.2 2.0 0.50 ',/, &
      '#')

      do b = 1, size(barrier)
         write(luo_saeinp,2125)
 2125    format('# ',/, &
         '# barrier(b)%np, I, number of points in barrier polyline')
         write(luo_saeinp,*) barrier(b)%np

         do ip = 1, barrier(b)%np
            write(luo_saeinp,2127)
 2127       format('# barrier(b)%points(n)%x, barrier(b)%points(n)%y, R, x,y coordinate pair')
            write(luo_saeinp,*) barrier(b)%points(ip)%x, barrier(b)%points(ip)%y

            write (luo_saeinp,2130)
 2130       format('# amzbr(b), amxbrw(b), ampbr(b)')
            write (luo_saeinp,*) barrier(b)%param(ip)%amzbr, barrier(b)%param(ip)%amxbrw, barrier(b)%param(ip)%ampbr
         end do
      end do

!       subregions
      write(luo_saeinp,2135)
 2135 format( &
      '#',/, &
      '# +++ SUBREGIONS +++',/, &
      '#',/, &
      '#     nsubr, I, Number of subregions (at least 1)',/, &
      '#            NOTE: together all must cover simulation region')
      write(luo_saeinp,*) size(subr_poly)

      write(luo_saeinp,2137)
 2137 format( &
      '#',/, &
      '#     NOTE: Remaining SUBREGION inputs (BIOMASS, SOIL, and HYDROLOGY,',/, &
      '#     ie. variables defined by subregion) are repeated for nsubr',/, &
      '#     subregions specified')

      ! loop through all subregions
      do sr = 1, size(subr_poly)

      write(luo_saeinp,2138)
 2138 format( &
      '#',/, &
      '#     subregion polygon, count, xy pairs (subregions_mod)',/, &
      '#         polygons can be:',/, &
      '#         - open (the last point connects to the first point)',/,&
      '#         - closed (last point is same as first point)',/, &
      '#         - complex (multiple closed polygons entered as one)')
      ! number of coordinate pairs in polygon
      write(luo_saeinp,*) subr_poly(sr)%np
      ! the coordinate pairs
      do ip = 1, subr_poly(sr)%np
        write(luo_saeinp,*) subr_poly(sr)%points(ip)%x, subr_poly(sr)%points(ip)%y
      end do

      write(luo_saeinp,2140)
 2140 format('#',/, &
      '#     +++ BIOMASS +++',/, &
      '#',/, &
      '#      subrsurf(s)adzht_ave, R, Average residue height (m)')
      write(luo_saeinp,*) subrsurf(sr)%adzht_ave
! Changed above to use "average residue height" instead of "overall height" - LEW 1/26/06
!      '#       biotot(s)%zht_ave, R, Overall biomass height (m)')
!      write(luo_saeinp,*) biotot(s)%zht_ave

      write(luo_saeinp,2146)
 2146 format('#',/, &
      '#       aczht(s), R, (c1glob.inc) Crop height (m)')
      write(luo_saeinp,*) subrsurf(sr)%aczht

      write(luo_saeinp,2147)
 2147 format('#',/, &
      '#       acrsai(s), R, (c1glob.inc) Crop stem area index (m^2/m^2)',/, &
      '#       acrlai(s), R, (c1glob.inc) Crop leaf area index (m^2/m^2)')
      write(luo_saeinp,*) subrsurf(sr)%acrsai, subrsurf(sr)%acrlai

      write(luo_saeinp,2148)
 2148 format('#',/, &
      '#       subrsurf(s)%adrsaitot, R, Residue stem area index (m^2/m^2)',/, &
      '#       subrsurf(s)%adrlaitot, R, Residue leaf area index (m^2/m^2)')
      write(luo_saeinp,*) subrsurf(sr)%adrsaitot, subrsurf(sr)%adrlaitot

      write(luo_saeinp,2149)
 2149 format('#',/, &
      '#       acxrow(s) Crop row spacing (m)',/, &
      '#       ac0rg(s)  Crop seed placement (0 - furrow, 1 - ridge)')
      write(luo_saeinp,*) subrsurf(sr)%acxrow, subrsurf(sr)%ac0rg

      write(luo_saeinp,2150)
 2150 format('#',/, &
      '# These are not implemented within EROSION',/, &
      '#       abrsaz(h,s), R, (b1geom.inc) Biomass stem area index by ht (1/m)',/, &
      '#             (should be 5 values here when used)',/, &
      '#       abrlaz(h,s), R, (b1geom.inc) Biomass leaf area index by ht (1/m)',/, &
      '#             (should be 5 values here when used)',/, &
      '#',/, &
      '# Only abffcv(s) is currently implemented within EROSION',/, &
      '#       abffcv(s), R, (b1geom.inc) Flat biomass cover (m^2/m^2)',/, &
      '#       abfscv(s), R, (b1geom.inc) Standing biomass cover (m^2/m^2)',/, &
      '#       abftcv(s), R, (b1geom.inc) Total biomass cover (m^2/m^2)',/, &
      '#             (should be 3 values here when abffcv(s) and abfscv(s) are used)')
      write(luo_saeinp,*) subrsurf(sr)%abffcv

!      soil
      write(luo_saeinp,2160)
 2160 format('#',/, &
      '#     +++ SOIL +++',/, &
      '#',/, &
      '#     nslay(s), I, (s1layr.inc) Number of soil layers (3-10)')

      write(luo_saeinp,*) subrsurf(sr)%nslay

      write(luo_saeinp,2165)
 2165 format('#',/, &
      '#     NOTE: Remaining SOIL inputs are repeated for each layer specified',/, &
      '#',/, &
      '#     aszlyt(l,s), R, (s1layr.inc) Soil layer thickness (mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aszlyt, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2170)
 2170 format('#',/, &
      '#     asdblk(l,s), R, (s1phys.inc) Soil layer bulk density (Mg/m^3')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asdblk, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2175)
 2175 format('#',/, &
      '#     asfsan(l,s),R,(s1dbh.inc) Soil layer sand content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfsan, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2177)
 2177 format('#',/, &
      '#     asfvfs(l,s), R, (s1dbh.inc) Soil layer very fine sand (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfvfs, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2180)
 2180 format('#',/, &
      '#     asfsil(l,s),R,(s1dbh.inc) Soil layer silt content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfsil, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2185)
 2185 format('#',/, &
      '#     asfcla(l,s),R,(s1dbh.inc) Soil layer clay content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfcla, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2190)
 2190 format('#',/, &
      '#     asvroc(l,s), R, (s1dbh.inc) Soil layer rock volume (m^3/m^3)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asvroc, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2195)
 2195 format('#',/, &
      '#     asdagd(l,s),R,(s1agg.inc) Soil layer agg density (Mg/m^3)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asdagd, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2200)
 2200 format('#',/, &
      '#     aseags(l,s), R, (s1agg.inc) Soil layer agg stability ln(J/kg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aseags, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2205)
 2205 format('#',/, &
      '#     aslagm(l,s), R, (s1agg.inc) Soil layer GMD (mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aslagm, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2210)
 2210 format('#',/, &
      '#     aslagn(l,s), R, (s1agg.inc) Soil layer minimum agg size (mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aslagn, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2215)
 2215 format('#',/, &
      '#     aslagx(l,s), R, (s1agg.inc) Soil layer maximum agg size (mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aslagx, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2220)
 2220 format('#',/, &
      '#     as0ags(l,s), R, (s1agg.inc) Soil layer GSD (mm/mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%as0ags, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2225)
 2225 format('#',/, &
      '#     asfcr(s), R, (s1surf.inc) Surface crust fraction (m^2/m^2)',/, &
      '#     aszcr(s), R, (s1surf.inc) Surface crust thickness (mm)',/, &
      '#     asflos(s), R, (s1surf.inc) Fraction of loose material on surface (m^2/m^2)',/, &
      '#     asmlos(s), R, (s1surf.inc) Mass of loose material on crust (kg/m^2)',/, &
      '#     asdcr(s), R, (s1surf.inc) Soil crust density (Mg/m^3)',/, &
      '#     asecr(s), R, (s1surf.inc) Soil crust stability ln(J/kg)')
      write(luo_saeinp,*) subrsurf(sr)%asfcr, subrsurf(sr)%aszcr, subrsurf(sr)%asflos, subrsurf(sr)%asmlos, &
                          subrsurf(sr)%asdcr, subrsurf(sr)%asecr
      write(luo_saeinp,2230)
 2230 format('#',/, &
      '#     aslrr(s), R, (s1sgeo.inc) Allmaras random roughness (mm)')
      write(luo_saeinp,*) subrsurf(sr)%aslrr
      write(luo_saeinp,2235)
 2235 format('#',/, &
      '#     aszrgh(s), R, (s1sgeo.inc) Ridge height (mm)',/, &
      '#     asxrgs(s), R, (s1sgeo.inc) Ridge spacing (mm)',/, &
      '#     asxrgw(s), R, (s1sgeo.inc) Ridge width (mm)',/, &
      '#     asargo(s), R, (s1sgeo.inc) Ridge orientation (deg)')
      write(luo_saeinp,*) subrsurf(sr)%aszrgh, subrsurf(sr)%asxrgs, subrsurf(sr)%asxrgw, subrsurf(sr)%asargo
      write(luo_saeinp,2240)
 2240 format('#',/, &
      '#     asxdks(s), R, (s1sgeo.inc) Dike spacing (mm)')
      write(luo_saeinp,*) subrsurf(sr)%asxdks
      write(luo_saeinp,2245)
!      hydrology
 2245 format('#',/, &
      '#     +++ HYDROLOGY +++',/, &
      '#',/, &
      '#     ahzsnd(s), R, (s1sgeo.inc) Snow depth (mm)')
      write(luo_saeinp,*) subrsurf(sr)%ahzsnd
      write(luo_saeinp,2250)
 2250 format('#',/, &
      '#     ahrwcw(l,s), R, (h1db1.inc) Soil layer wilting point water content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%ahrwcw, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2255)
 2255 format('#',/, &
      '#     ahrwca(l,s), R, (h1db1.inc) Soil layer water content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%ahrwca, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2260)
 2260 format('#',/, &
      '#     ahrwc0(h,s), R, (h1db1.inc) Surface layer water content (Mg/Mg)',/, &
      '#                  NOTE: the near surface water content is specified on an',/, &
      '#                        hourly basis.  We read in the hrly water content',/, &
      '#                        on two lines, with 12 values in each line.')
      write(luo_saeinp,22) (subrsurf(sr)%ahrwc0(l), l=1,12)
 22   format(12(1x,f10.7))
      write(luo_saeinp,22) (subrsurf(sr)%ahrwc0(l), l=13,24)

      ! end of subregion loop
      end do

!      weather
      write(luo_saeinp,2270)
 2270 format('#',/, &
      '# NOTE: This is the end of the SUBREGION variables',/, &
      '#',/, &
      '#     +++ WEATHER +++',/, &
      '#',/, &
      '#     awzypt, R, Average annual precipitation (mm)')
      write(luo_saeinp,*) awzypt
      write(luo_saeinp,2273)
 2273 format('#',/, &
      '#     awdair, R, Air density (kg/m^3)')
      write(luo_saeinp,*) awdair
      write(luo_saeinp,2275)
 2275 format('#',/, &
      '#     awadir, R, Wind direction (deg)')
      write(luo_saeinp,*) awadir
      write(luo_saeinp,2280)
 2280 format('#',/, &
      '#     ntstep, I, (local variable) Number of intervals/day to run EROSION')
      write(luo_saeinp,*) ntstep
      write(luo_saeinp,2285)
 2285 format('#',/, &
      '#     anemht, R  anemometer height (m)',/, &
      '#     awzzo,  R  aerodynamic roughness at anemometer site (mm)', /, &
      '#     wzoflg, I (global variable) zo location flag',/, &
      '#               (flag =0 - zo fixed at wx sta. location)',/, &
      '#               (flag = 1 - zo variable at field location)')
      write(luo_saeinp,*) anemht, awzzo, wzoflg
      write(luo_saeinp,2290)
 2290 format('#',/, &
      '#     wflg, I, (local variable) Wind/Weibull flag',/, &
      '#              (0 - read in Weibull parameters, 1 - read in wind speeds)')
      write(luo_saeinp,*) '1'
      write(luo_saeinp,2295)
 2295 format('#',/, &
      '# NOTE: This is only present when the above (wflg=0)',/, &
      '#     wfcalm, R, (local variable) Fraction of time winds are calm (hr/hr)',/, &
      '#     wuc, R, (local variable) Weibull "c" factor (m/s)',/, &
      '#     w0k, R, (local variable) Weibull "k" factor (fraction)',/, &
      '#   0.263    5.856    1.720', &
      '#',/, &
      '# NOTE: The remaining data is only present when (wflg=1)',/, &
      '#       wflg=1 uses standard input from windgen in WEPS.',/, &
      '#',/, &
      '#       awu(i), R, (w1wind) Wind speed for (ntstep) intervals (m/s)',/, &
      '#',/, &
      '# I think I can read multiple lines with variable number of values',/, &
      '# We will try and see - LEW  Must use 6 values per line LH.',/, &
      '#')
      write(luo_saeinp,*) (subday(k)%awu, k=1,6)
      write(luo_saeinp,*) (subday(k)%awu, k=7,12)
      write(luo_saeinp,*) (subday(k)%awu, k=13,18)
      write(luo_saeinp,*) (subday(k)%awu, k=19,24)
      write(luo_saeinp,2300)
 2300 format( '#' )

      close(luo_saeinp)

   end subroutine saeinp

   subroutine daily_erodout( o_unit, o_E_unit, sgrd_u, input_filename, cellstate )

!     +++  PURPOSE +++
!     To print output desired from standalone EROSION submodel

      use weps_interface_defs
      use file_io_mod, only: fopenk, makenamnum
      use datetime_mod, only: get_systime_string
      use erosion_data_struct_defs, only: cellsurfacestate, am0efl
      use grid_mod, only: imax, jmax, amasim, amxsim

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(inout) :: o_unit, o_E_unit, sgrd_u
      character(len=*), intent(in) :: input_filename
      type(cellsurfacestate), dimension(0:,0:), intent(in) :: cellstate     ! initialized grid cell state values

!     ++++ LOCAL VARIABLES +++
      integer i, j
      real tt, lx, ly
      real topt,topss, top10, bott, botss, bot10, ritt, ritss, rit10
      real lftt, lftss, lft10, tot, totbnd

      integer yr, mon, day

!     +++ END SPECIFICATIONS +++

!     Calculate Averages Crossing Borders
!      top border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       j = jmax
       do 1 i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
    1  continue
!      calc. average at top border
       topt  = aegt/(imax-1)
       topss = aegtss/(imax-1)
       top10 = aegt10/(imax-1)

!      bottom border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       j = 0
       do 2 i = 1, imax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
    2  continue
!      calc. average at bottom border
        bott  = aegt/(imax-1)
        botss = aegtss/(imax-1)
        bot10 = aegt10/(imax-1)

!     right border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       i = imax
       do 3 j = 1, jmax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
    3  continue
!      calc. average at right border
        ritt  = aegt/(jmax-1)
        ritss = aegtss/(jmax-1)
        rit10 = aegt10/(jmax-1)
!
!     left border
       aegt   = 0.0
       aegtss = 0.0
       aegt10 = 0.0
       i = 0
       do 4 j = 1, jmax-1
         aegt    = aegt   + cellstate(i,j)%egt
         aegtss  = aegtss + cellstate(i,j)%egtss
         aegt10  = aegt10 + cellstate(i,j)%egt10
    4  continue
!      calc. average at left border
        lftt   = aegt/(jmax-1)
        lftss  = aegtss/(jmax-1)
        lft10  = aegt10/(jmax-1)

!     calculate averages of inner grid points
      aegt   = 0.0
      aegtss = 0.0
      aegt10 = 0.0
      do 5 j=1,jmax-1
       do 5 i= 1, imax-1
        aegt= aegt + cellstate(i,j)%egt
        aegtss = aegtss + cellstate(i,j)%egtss
        aegt10 = aegt10 + cellstate(i,j)%egt10
    5 continue
      tt     = (imax-1)*(jmax-1)
      aegt   = aegt/tt
      aegtss = aegtss/tt
      aegt10 = aegt10/tt

!    calculate comparision of boundary and interior losses
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y
      tot = aegt*lx*ly
      totbnd = (topt + bott + topss + botss)*lx +                       &
     &         (ritt + lftt + ritss + lftss)*ly


      if (btest(am0efl,1)) then

      if( o_unit .lt. 0 ) then
        call fopenk (o_unit, trim(mksaeout%fullpath) // makenamnum('saeros', mksaeout%simday, mksaeout%maxday, '.egrd'),'unknown')
        call caldat (mksaeout%jday,day,mon,yr)
        write(*,'(4(a,i0))') 'Made Daily Erosion grid file for: ', day,'/', mon,'/', yr,' simulation day: ', mksaeout%simday
        write(o_unit, "('# WEPS erosion day mon yr',2(1x,i2),2x,i4)") day, mon, yr
        write (o_unit,*)
        write (o_unit,*) 'Grid cell output from WEPS run'
        write (o_unit,*)
      else
        ! write header to files
        write (o_unit,*)
        write (o_unit,*)
        write (o_unit,*) 'Grid cell output from SWEEP run'
        write (o_unit,*)
      end if

      ! Print date of Run
      write(o_unit,"(1x,'Date of run: ',a21)") get_systime_string()
      write(o_unit,*)

      write(o_unit,fmt="(1x,a)") "<field dimensions>"
      write(o_unit,fmt="(1x,5f10.2)") amasim, amxsim(1)%x, amxsim(1)%y, amxsim(2)%x, amxsim(2)%y
      write(o_unit,fmt="(1x,a)") "</field dimensions>"
      write(o_unit,*)
      write (o_unit,*) 'Total grid size: (', imax+1,',', jmax+1, ')   ',&
     &                 'Inner grid size: (', imax-1,',', jmax-1, ')'

      write (o_unit,*)
      write (o_unit,6)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt+cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt+cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt+cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt+cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,7)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egt, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egt, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egt, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,8)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0, j=1,jmax-1) '
      write (o_unit,10)  (cellstate(i,jmax)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(i,0)%egtss, i = 1, imax-1)
      write (o_unit,10)  (cellstate(imax,j)%egtss, j = 1, jmax-1)
      write (o_unit,10)  (cellstate(0,j)%egtss, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,9)
      write (o_unit,*)                                                  &
     & '  top(i=1,imax-1,j=jmax) ',                                     &
     & 'bottom(i=1,imax-1,j=0) ',                                       &
     & 'right(i=imax,j=1,jmax-1) ',                                     &
     & 'left(i=0,j=1,jmax-1) '
      write (o_unit,11)  (cellstate(i,jmax)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(i,0)%egt10, i = 1, imax-1)
      write (o_unit,11)  (cellstate(imax,j)%egt10, j = 1, jmax-1)
      write (o_unit,11)  (cellstate(0,j)%egt10, j = 1, jmax-1)

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Total Soil Loss', 'soil loss', '(kg/m^2)'
      do 19  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egt, i = 1, imax-1)
   19 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Saltation/Creep Soil Loss', 'salt/creep soil loss', '(kg/m^2)'
      do 29  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egt-cellstate(i,j)%egtss, i = 1, imax-1)
   29 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &                              
     & 'Suspension Soil Loss', 'suspension soil loss', '(kg/m^2)'
      do 39  j = jmax-1, 1, -1
      write (o_unit,10)  (cellstate(i,j)%egtss, i = 1, imax-1)
   39 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,fmt="(' <grid data> | ',3('|',a))")                 &
     & 'PM10 Soil Loss', 'PM10 soil loss', '(kg/m^2)'
      do 49  j = jmax-1, 1, -1
      write (o_unit,11)  (cellstate(i,j)%egt10, i = 1, imax-1)
   49 continue
      write (o_unit,fmt="(' </grid data>')")

      write (o_unit,*)
      write (o_unit,*) '**Averages - Field'
      write (o_unit,*) '     Total    salt/creep      susp       PM10 '
      write (o_unit,*) '     egt                      egtss      egt10'
      write (o_unit,*) '   -----------------kg/m^2--------------------'
      write (o_unit,15)    aegt, aegt-aegtss, aegtss, aegt10
      write (o_unit,*)
      write (o_unit,*) '**Averages - Crossing Boundaries '
      write (o_unit,*) 'Location      Total  Salt/Creep   Susp    PM10'
      write (o_unit,*) '--------------------kg/m----------------------'
      write (o_unit,21) topt+topss, topt, topss, top10
      write (o_unit,22) bott+botss, bott, botss, bot10
      write (o_unit,23) ritt+ritss, ritt, ritss, rit10
      write (o_unit,24) lftt+lftss, lftt, lftss, lft10
      write (o_unit,*)
      write (o_unit,*) '   Comparision of interior & boundary loss'
      write (o_unit,*) '      interior       boundary    int/bnd ratio'
      if( totbnd.gt.1.0e-9 ) then
          write (o_unit,16) tot, totbnd, tot/totbnd
      else
          !Boundary loss near or equal to zero
          write (o_unit,16) tot, totbnd, 1.0e-9
      end if

!     additional output statements for easy shell script parsing
      write (o_unit,*)
!     write losses as positive numbers
      write (o_unit,17) -aegt, aegtss-aegt, -aegtss, -aegt10
   17 format (' repeat of total, salt/creep, susp, PM10:', 3f12.4,f12.6)

      close(o_unit)

!     output formats
    6 format (1x,'  Passing Border Grid Cells - Total  egt+egtss(kg/m)')
    7 format (1x,'  Passing Border Grid Cells - Salt/Creep   egt(kg/m)')
    8 format (1x,'  Passing Border Grid Cells - Suspension egtss(kg/m)')
    9 format (1x,'  Passing Border Grid Cells - PM10       egt10(kg/m)')
   10 format (1x, 500f12.4)
   11 format (1x, 500f12.6)
   15 format (1x, 3(f12.4,2x), f12.6)
   16 format (1x, 2(f13.4,2x),2x, f13.4)
   21 format (1x, 'top   ', 1x, 4(f9.2,1x))
   22 format (1x, 'bottom', 1x, 4(f9.2,1x))
   23 format (1x, 'right ', 1x, 4(f9.2,1x))
   24 format (1x, 'left  ', 1x, 4(f9.2,1x))

      end if !if (btest(am0efl,1)) then

      !Erosion summary - total, salt/creep, susp, pm10
      !(loss values are positive - deposition values are negative)
      if (btest(am0efl,0)) then

      if( in_weps ) then
         call caldat (mksaeout%jday,day,mon,yr)
         write(*,'(4(a,i0))') 'Made Daily Erosion summary file for: ', day,'/', mon,'/', yr,' simulation day: ', mksaeout%simday
         write (UNIT=o_E_unit,FMT="(4(f12.6),' ')",ADVANCE="NO") -aegt, -(aegt-aegtss), -aegtss, -aegt10
         write (UNIT=o_E_unit,FMT="('# WEPS erosion day mon yr',2(1x,i2),2x,i4)",ADVANCE="NO") day, mon, yr
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES") ' (loss values are positive - deposition values are negative)'
      else
         write (UNIT=o_E_unit,FMT="(4(f12.6),' ')",ADVANCE="NO") -aegt, -(aegt-aegtss), -aegtss, -aegt10
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="NO") trim(input_filename)
         write (UNIT=o_E_unit,FMT="(A)",ADVANCE="YES") ' (loss values are positive - deposition values are negative)'
      end if

      end if

      !Duplicate Erosion summary info for the *.sgrd file so "tsterode" interface
      ! can display this info on graphical report window
      if (btest(am0efl,3) .and. (sgrd_u .ge. 0) ) then
       write (sgrd_u,*)
       write (sgrd_u,*) '**Averages - Field'
       write (sgrd_u,*) '     Total    salt/creep      susp       PM10 '
       write (sgrd_u,*) '     egt                      egtss      egt10'
       write (sgrd_u,*) '   -----------------kg/m^2--------------------'
       write (sgrd_u,15)    aegt, aegt-aegtss, aegtss, aegt10
       write (sgrd_u,*)
       write (sgrd_u,*) '**Averages - Crossing Boundaries '
       write (sgrd_u,*) 'Location      Total  Salt/Creep   Susp    PM10'
       write (sgrd_u,*) '--------------------kg/m----------------------'
       write (sgrd_u,21) topt+topss, topt, topss, top10
       write (sgrd_u,22) bott+botss, bott, botss, bot10
       write (sgrd_u,23) ritt+ritss, ritt, ritss, rit10
       write (sgrd_u,24) lftt+lftss, lftt, lftss, lft10
       write (sgrd_u,*)
       write (sgrd_u,*) '   Comparison of interior & boundary loss'
       write (sgrd_u,*) '      interior       boundary    int/bnd ratio'
       if( totbnd.gt.1.0e-9 ) then
         write (sgrd_u,16) tot, totbnd, tot/totbnd
       else
         !Boundary loss near or equal to zero
         write (sgrd_u,16) tot, totbnd, 1.0e-9
       end if
       if( sgrd_u .ge. 0 ) then
            close(sgrd_u)
       end if

      end if

   end subroutine daily_erodout

end module sae_in_out_mod

