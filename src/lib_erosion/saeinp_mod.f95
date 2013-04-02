!$Author$
!$Date$
!$Revision$
!$HeadURL$

module saeinp_mod

!     This module is for the creation of sinfle or multiple stand alone erosion input files,
!     depending on the command line switches given

  type make_saeinp
     integer :: jday      ! the present day in julian days for output
     integer :: simday    ! the present day in simulation days for creation of file
     integer :: maxday    ! maximum simday number
     character*256 :: fullpath  ! the root path plus subdirectory if indicated by multiple files
  end type make_saeinp

  type(make_saeinp) :: mksaeinp

  contains

      subroutine saeinp( subrsurf )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
      use weps_interface_defs
      use file_io_mod, only: fopenk, makenamnum
      use subregions_mod
      use erosion_data_struct_defs, only: subregionsurfacestate

!     +++ ARGUMENT DECLARATIONS +++
      type(subregionsurfacestate), dimension(:) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'p1const.inc' ! anemht, awzzo, wzoflg
      include  'm1geo.inc'   ! amasim, nacctr, nbr, amxsim, amxar, amxbr, amzbr, ampbr, amxbrw
      include  'w1wind.inc'  ! awadir, awu
      include  'w1pavg.inc'  ! awdair
      include  'm1sim.inc'   ! ntstep

!     +++ LOCAL VARIABLES +++
      integer k,l, sr, ip
      integer b
      integer day, mon, yr
      integer :: luo_saeinp      ! output unit number
      

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     sr - index used in subregion loop
!     ip - index to polygon coordinates

!     +++ END SPECIFICATIONS +++

      call fopenk (luo_saeinp, trim(mksaeinp%fullpath) // makenamnum( 'saeros', mksaeinp%simday, mksaeinp%maxday, '.in'),'unknown')
      call caldat (mksaeinp%jday,day,mon,yr)

      write(*,'(4(a,i0))') 'Made stand alone erosion input file D/M/Y: ', day,'/', mon,'/', yr,' simulation day: ', mksaeinp%simday

      write(luo_saeinp,2101) day, mon, yr
 2101 format('# day mon yr',2(1x,i2),2x,i4)

!     print header info
      write(luo_saeinp,1005)
 1005 format ('#',65('*'),/,'#      file:  erod.in',/,'#',65('*'),/,    &
     & '#',/,                                                           &
     & '# +++ PURPOSE +++',/,                                           &
     & '#',/,                                                           &
     & '#     Input file which is read by stand alone erosion model',/, &
     & '#',/,                                                           &
     & '# +++ DEFINITIONS +++',/,                                       &
     & '#',/,                                                           &
     & '#     Lines beginning with a "#" character are comments',/,     &
     & '#',/,                                                           &
     & '#     * = inputs NOT presently used by erosion',/,              &
     & '#',/,                                                           &
     & '#     All other input values must be correctly specified.',/,   &
     & '#',/,                                                           &
     & '#     Comments prior to each line of data have the following for&
     &mat',/,                                                           &
     & '#',/,                                                           &
     & '#     Variable_Name, Var_type, (inc file) Text Definition',/,   &
     & '#',/,                                                           &
     & '#     where Var_type is: I = integer, L = logical, R = real',/, &
     & '#',/,                                                           &
     & '# +++ DEBUG FLAG +++',/,                                        &
     & '#',/,                                                           &
     & '#     debugflg - debug flag for providing different levels of de&
     &bug output',/,                                                    &
     & '#                value of 0 will print no debug output',/,      &
     & '#                value of 1 will print out and number all input &
     & lines',/,                                                        &
     & '#                value of 2 will print out and number all data i&
     &nput lines',/,                                                    &
     & '#                value of 3 will do both 1 and 2 input line debu&
     &g output')

      write(luo_saeinp,*) '0'
      write(luo_saeinp,2000)
 2000 format(                                                           &
     & '#',/,                                                           &
     & '# +++ INITIALIZATIONS +++',/,                                   &
     & '#',/,                                                           &
     & '#     am0eif, L, (m1flag.inc) EROSION initialization flag')
      write(luo_saeinp,*) '.TRUE.'
      write(luo_saeinp,2005)
 2005 format('#    am0efl, L, (m1flag.inc) EROSION "print" flag')
      write(luo_saeinp,*) '1'
      write(luo_saeinp,2100)
 2100 format ('#',/,                                                    &
     &'# +++ SIMULATION REGION +++',/,                                  &
     &'#',/,                                                            &
     &'#     amxsim(x,y), R, (m1geo.inc) Simulation Region coordinates (&
     &m)',/,                                                            &
     &'#              Input x,y coordinates in this form: x1,y1  x2,y2')
      write(luo_saeinp,*)                                               &
     &    amxsim(1,1), amxsim(2,1), amxsim(1,2), amxsim(2,2)
      write(luo_saeinp,2105)
 2105 format(                                                           &
     &'#',/,                                                            &
     &'#     amxsim(x,y), R, (m1geo.inc) Simulation Region orientation a&
     &ngle',/,                                                          &
     &'#     clockwise with 0=north')
      write(luo_saeinp,*) amasim
      write(luo_saeinp,2110)
 2110 format('#',/,                                                     &
     &'#  +++ ACCOUNTING REGIONS +++',/,                                &
     &'#',/,                                                            &
     &'# nacctr, I, (m1geo.inc) Number of accounting regions')
      write(luo_saeinp,*) nacctr
      write(luo_saeinp,2115)
 2115 format('#',/,                                                     &
     &'#     amxar(x,y,a), R, (m1geo.inc) Accounting Region coordinates &
     &(m)',/,                                                           &
     &'#                     Input x,y coordinates in this form: x1,y1  &
     &x2,y2',/,                                                         &
     &'#                     for each accounting region specified (nacct&
     &r)')
      do b = 1, nacctr
         write(luo_saeinp,*) amxar(1,1,b), amxar(2,1,b),                &
     &            amxar(1,2,b),amxar(2,2,b)
      end do
!       barriers
      write(luo_saeinp,2120)
 2120 format ('#',/,                                                    &
     &'# +++ BARRIERS +++',/,                                           &
     &'#',/,                                                            &
     &'#     nbr, I, (m1geo.inc) Number of barriers (0-5) ')
      write(luo_saeinp,*) nbr
      write(luo_saeinp,2122)
 2122 format ('#',/,                                                    &
     &'#     NOTE: Remaining BARRIER inputs are repeated for each barrie&
     &r specified',/,                                                   &
     &'#     If no barriers specified (nbr=0), then no BARRIER inputs wi&
     &ll be here',/,                                                    &
     &'#',/,                                                            &
     &'#     amxbr(x,y,b), R, (m1geo.inc) Accounting Region coordinates &
     &(m)',/,                                                           &
     &'#             Input x,y coordinates in this form: x1,y1 x2,y2',/,&
     &'#             for each barrier specified (nbr)',/,               &
     &'#    0.0, 0.0  0.0  100.0',/,                                    &
     &'#',/,                                                            &
     &'#       amzbr(b), R, (m1geo.inc) Barrier height (m)',/,          &
     &'#       ampbr(b), R, (m1geo.inc) Barrier porosity (m^2/m^2)',/,  &
     &'#       amxbrw(b), R, (m1geo.inc) Barrier width (m)',/,          &
     &'#  1.2 0.50 2.0',/,                                              &
     &'#')

      do 33 b = 1,nbr
         write(luo_saeinp,2125)
 2125    format('# amxar(1,1,b), amxar(2,1), amxar(1,2),amxar(2,2)')
         write(luo_saeinp,*)                                            &
     &   amxbr(1,1,b), amxbr(2,1,b), amxbr(1,2,b), amxbr(2,2,b)

         write (luo_saeinp,2130)
 2130    format('#, amzbr(b), ampbr(b), amxbrw(b)')
         write (luo_saeinp,*) amzbr(b), ampbr(b), amxbrw(b)
   33 continue

!       subregions
      write(luo_saeinp,2135)
 2135 format('#',/,                                                     &
     &'# +++ SUBREGIONS +++',/,                                         &
     &'#',/,                                                            &
     &'#     nsubr, I, (m1subr.inc) Number of subregions (1-5)')
      write(luo_saeinp,*) size(subrsurf)
      write(luo_saeinp,2137)
 2137 format('#',/,                                                     &
     &'#     NOTE: Remaining SUBREGION inputs (BIOMASS, SOIL, and HYDROL&
     &OGY,',/,                                                          &
     &'#     ie. variables defined by subregion) are repeated for nsubr &
     &',/,                                                              &
     &'#     subregions specified')

      ! loop through all subregions
      do sr = 1, size(subrsurf)

      write(luo_saeinp,2138)
 2138 format('#',/,                                                     &
     &'#     subregion polygon, count, xy pairs (subregions_mod)',/,    &
     &'#         polygons can be:',/,                                   &
     &'#         - open (the last point connects to the first point)',/,&
     &'#         - closed (last point is same as first point)',/,       &
     &'#         - complex (multiple closed polygons entered as one)')
      ! number of coordinate pairs in polygon
      write(luo_saeinp,*) subr_poly(sr)%np
      ! the coordinate pairs
      do ip = 1, subr_poly(sr)%np
        write(luo_saeinp,*)                                             &
     &  subr_poly(sr)%points(ip)%x,subr_poly(sr)%points(ip)%y
      end do

      write(luo_saeinp,2140)
 2140 format('#',/, &
     &'#     +++ BIOMASS +++',/,                                        &
     &'#',/,                                                            &
     &'#      subrsurf(s)adzht_ave, R, Average residue height (m)')
      write(luo_saeinp,*) subrsurf(sr)%adzht_ave
! Changed above to use "average residue height" instead of "overall height" - LEW 1/26/06
!     &'#       biotot(s)%zht_ave, R, Overall biomass height (m)')
!      write(luo_saeinp,*) biotot(s)%zht_ave

      write(luo_saeinp,2146)
 2146 format('#',/,                                                     &
     &'#       aczht(s), R, (c1glob.inc) Crop height (m)')
      write(luo_saeinp,*) subrsurf(sr)%aczht

      write(luo_saeinp,2147)
 2147 format('#',/,                                                     &
     &'#       acrsai(s), R, (c1glob.inc) Crop stem area index (m^2/m^2)&
     &',/,                                                              &
     &'#       acrlai(s), R, (c1glob.inc) Crop leaf area index (m^2/m^2)&
     &')
      write(luo_saeinp,*) subrsurf(sr)%acrsai, subrsurf(sr)%acrlai

      write(luo_saeinp,2148)
 2148 format('#',/,                                                     &
     &'#       subrsurf(s)%adrsaitot, R, Residue stem area index (m^2/m^2)',/, &
     &'#       subrsurf(s)%adrlaitot, R, Residue leaf area index (m^2/m^2)')
      write(luo_saeinp,*) subrsurf(sr)%adrsaitot, subrsurf(sr)%adrlaitot

      write(luo_saeinp,2149)
 2149 format('#',/,                                                     &
     &'#       acxrow(s) Crop row spacing (m)'                          &
     &,/,                                                               &
     &'#       ac0rg(s)  Crop seed placement (0 - furrow, 1 - ridge)'   &
     &)
      write(luo_saeinp,*) subrsurf(sr)%acxrow, subrsurf(sr)%ac0rg

      write(luo_saeinp,2150)
 2150 format('#',/,                                                     &
     &'# These are not implemented within EROSION',/,                   &
     &'#       abrsaz(h,s), R, (b1geom.inc) Biomass stem area index by h&
     &t (1/m)',/,                                                       &
     &'#             (should be 5 values here when used)',/,            &
     &'#       abrlaz(h,s), R, (b1geom.inc) Biomass leaf area index by h&
     &t (1/m)',/,                                                       &
     &'#             (should be 5 values here when used)',/,            &
     &'#',/,                                                            &
     &'# Only abffcv(s) is currently implemented within EROSION',/,     &
     &'#       abffcv(s), R, (b1geom.inc) Flat biomass cover (m^2/m^2)',&
     &/,                                                                &
     &'#       abfscv(s), R, (b1geom.inc) Standing biomass cover (m^2/m^&
     &2)',/,                                                            &
     &'#       abftcv(s), R, (b1geom.inc) Total biomass cover (m^2/m^2)'&
     &,/,                                                               &
     &'#             (should be 3 values here when abffcv(s) and abfscv(&
     &s) are used)')
      write(luo_saeinp,*) subrsurf(sr)%abffcv

!      soil
      write(luo_saeinp,2160)
 2160 format('#',/,                                                     &
     &'#     +++ SOIL +++',/,                                           &
     &'#',/,                                                            &
     &'#     nslay(s), I, (s1layr.inc) Number of soil layers (3-10)')

      write(luo_saeinp,*) subrsurf(sr)%nslay

      write(luo_saeinp,2165)
 2165 format('#',/,                                                     &
     &'#     NOTE: Remaining SOIL inputs are repeated for each layer spe&
     &cified',/,                                                        &
     &'#',/,                                                            &
     &'#     aszlyt(l,s), R, (s1layr.inc) Soil layer thickness (mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aszlyt, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2170)
 2170 format('#',/,                                                     &
     &'#     asdblk(l,s), R, (s1phys.inc) Soil layer bulk density (Mg/m^&
     &3')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asdblk, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2175)
 2175 format('#',/,                                                     &
     &'#     asfsan(l,s),R,(s1dbh.inc) Soil layer sand content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfsan, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2177)
 2177 format('#',/,                                                     &
     &'#     asfvfs(l,s), R, (s1dbh.inc) Soil layer very fine sand (Mg/M&
     &g)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfvfs, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2180)
 2180 format('#',/,                                                     &
     &'#     asfsil(l,s),R,(s1dbh.inc) Soil layer silt content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfsil, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2185)
 2185 format('#',/,                                                     &
     &'#     asfcla(l,s),R,(s1dbh.inc) Soil layer clay content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asfcla, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2190)
 2190 format('#',/,                                                     &
     &'#     asvroc(l,s), R, (s1dbh.inc) Soil layer rock volume (m^3/m^3&
     &)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asvroc, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2195)
 2195 format('#',/,                                                     &
     &'#     asdagd(l,s),R,(s1agg.inc) Soil layer agg density (Mg/m^3)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%asdagd, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2200)
 2200 format('#',/,                                                     &
     &'#     aseags(l,s), R, (s1agg.inc) Soil layer agg stability ln(J/k&
     &g)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aseags, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2205)
 2205 format('#',/,                                                     &
     &'#     aslagm(l,s), R, (s1agg.inc) Soil layer GMD (mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aslagm, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2210)
 2210 format('#',/,                                                     &
     &'#     aslagn(l,s), R, (s1agg.inc) Soil layer minimum agg size (mm&
     &)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aslagn, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2215)
 2215 format('#',/,                                                     &
     &'#     aslagx(l,s), R, (s1agg.inc) Soil layer maximum agg size (mm&
     &)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%aslagx, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2220)
 2220 format('#',/,                                                     &
     &'#     as0ags(l,s), R, (s1agg.inc) Soil layer GSD (mm/mm)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%as0ags, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2225)
 2225 format('#',/,                                                     &
     &'#     asfcr(s), R, (s1surf.inc) Surface crust fraction (m^2/m^2)'&
     &,/,                                                               &
     &'#     aszcr(s), R, (s1surf.inc) Surface crust thickness (mm)',/, &
     &'#     asflos(s), R, (s1surf.inc) Fraction of loose material on su&
     &rface (m^2/m^2)',/,                                               &
     &'#     asmlos(s), R, (s1surf.inc) Mass of loose material on crust &
     &(kg/m^2)',/,                                                      &
     &'#     asdcr(s), R, (s1surf.inc) Soil crust density (Mg/m^3)',/,  &
     &'#     asecr(s), R, (s1surf.inc) Soil crust stability ln(J/kg)')
      write(luo_saeinp,*) subrsurf(sr)%asfcr, subrsurf(sr)%aszcr, subrsurf(sr)%asflos, subrsurf(sr)%asmlos, &
     &            subrsurf(sr)%asdcr, subrsurf(sr)%asecr
      write(luo_saeinp,2230)
 2230 format('#',/,                                                     &
     &'#     aslrr(s), R, (s1sgeo.inc) Allmaras random roughness (mm)')
      write(luo_saeinp,*) subrsurf(sr)%aslrr
      write(luo_saeinp,2235)
 2235 format('#',/,                                                     &
     &'#     aszrgh(s), R, (s1sgeo.inc) Ridge height (mm)',/,           &
     &'#     asxrgs(s), R, (s1sgeo.inc) Ridge spacing (mm)',/,          &
     &'#     asxrgw(s), R, (s1sgeo.inc) Ridge width (mm)',/,            &
     &'#     asargo(s), R, (s1sgeo.inc) Ridge orientation (deg)')
      write(luo_saeinp,*) subrsurf(sr)%aszrgh, subrsurf(sr)%asxrgs, subrsurf(sr)%asxrgw, subrsurf(sr)%asargo
      write(luo_saeinp,2240)
 2240 format('#',/,                                                     &
     &'#     asxdks(s), R, (s1sgeo.inc) Dike spacing (mm)')
      write(luo_saeinp,*) subrsurf(sr)%asxdks
      write(luo_saeinp,2245)
!      hydrology
 2245 format('#',/,                                                     &
     &'#     +++ HYDROLOGY +++',/,                                      &
     &'#',/,                                                            &
     &'#     ahzsnd(s), R, (s1sgeo.inc) Snow depth (mm)')
      write(luo_saeinp,*) subrsurf(sr)%ahzsnd
      write(luo_saeinp,2250)
 2250 format('#',/,                                                     &
     &'#     ahrwcw(l,s), R, (h1db1.inc) Soil layer wilting point water &
     &content (Mg/Mg)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%ahrwcw, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2255)
 2255 format('#',/,                                                     &
     &'#     ahrwca(l,s), R, (h1db1.inc) Soil layer water content (Mg/Mg&
     &)')
      write(luo_saeinp,*) (subrsurf(sr)%bsl(l)%ahrwca, l=1,subrsurf(sr)%nslay)
      write(luo_saeinp,2260)
 2260 format('#',/,                                                     &
     &'#     ahrwc0(h,s), R, (h1db1.inc) Surface layer water content (Mg&
     &/Mg)',/,                                                          &
     &'#                  NOTE: the near surface water content is specif&
     &ied on an',/,                                                     &
     &'#                        hourly basis.  We read in the hrly water&
     & content',/,                                                      &
     &'#                        on two lines, with 12 values in each lin&
     &e.')
      write(luo_saeinp,22) (subrsurf(sr)%ahrwc0(l), l=1,12)
 22   format(12(1x,f10.7))
      write(luo_saeinp,22) (subrsurf(sr)%ahrwc0(l), l=13,24)

      ! end of subregion loop
      end do

!      weather
      write(luo_saeinp,2270)
 2270 format('#',/,                                                     &
     &'# NOTE: This is the end of the SUBREGION variables',/,           &
     &'#',/,                                                            &
     &'#     +++ WEATHER +++',/,                                        &
     &'#',/,                                                            &
     &'#     awdair, R, (w1pavg.inc) Air density (kg/m^3)')
      write(luo_saeinp,*) awdair
      write(luo_saeinp,2275)
 2275 format('#',/,                                                     &
     &'#     awadir, R, (w1wind.inc) Wind direction (deg)')
      write(luo_saeinp,*) awadir
      write(luo_saeinp,2280)
 2280 format('#',/,                                                     &
     &'#     ntstep, I, (local variable) Number of intervals/day to run &
     &EROSION')
      write(luo_saeinp,*) ntstep
      write(luo_saeinp,2285)
 2285 format('#',/,                                                     &
     &'#     anemht, R  anemometer height (m)',/,                       &
     &'#     awzzo,  R  aerodynamic roughness at anemometer site (mm)', &
     &/,'#     wzoflg, I (global variable) zo location flag',/,         &
     &'#               (flag =0 - zo fixed at wx sta. location)',/,     &
     &'#               (flag = 1 - zo variable at field location)')
      write(luo_saeinp,*) anemht, awzzo, wzoflg
      write(luo_saeinp,2290)
 2290 format('#',/,                                                     &
     &'#     wflg, I, (local variable) Wind/Weibull flag',/,            &
     &'#              (0 - read in Weibull parameters, 1 - read in wind &
     &speeds)')
      write(luo_saeinp,*) '1'
      write(luo_saeinp,2295)
 2295 format('#',/,                                                     &
     &'# NOTE: This is only present when the above (wflg=0)',/,         &
     &'#     wfcalm, R, (local variable) Fraction of time winds are calm&
     & (hr/hr)',/,                                                      &
     &'#     wuc, R, (local variable) Weibull "c" factor (m/s)',/,      &
     &'#     w0k, R, (local variable) Weibull "k" factor (fraction)',/, &
     &'#   0.263    5.856    1.720',                                    &
     &'#',/,                                                            &
     &'# NOTE: The remaining data is only present when (wflg=1)',/,     &
     &'#       wflg=1 uses standard input from windgen in WEPS.',/,     &
     &'#',/,                                                            &
     &'#       awu(i), R, (w1wind) Wind speed for (ntstep) intervals (m/&
     &s)',/,'#',/,                                                      &
     &'# I think I can read multiple lines with variable number of value&
     &s',/,                                                             &
     &'# We will try and see - LEW  Must use 6 values per line LH.',/,  &
     &'#')
      write(luo_saeinp,*) (awu(k), k=1,6)
      write(luo_saeinp,*) (awu(k), k=7,12)
      write(luo_saeinp,*) (awu(k), k=13,18)
      write(luo_saeinp,*) (awu(k), k=19,24)
      write(luo_saeinp,2300)
 2300 format('#',/,                                                     &
     &     '#    + + + DATA TO PLOT + + +',/,                           &
     &     '#',/,                                                       &
     &     '#   names and values to input for plot',/,                  &
     &     '#   place 1 flag in 1st line after #-name line for variables&
     & to include in plot',/,                                           &
     &     '#',/,                                                       &
     &     '#   initial xplot value,I, (-1=no plot, 0 = plot indep.varia&
     &bles with 1 flag)')
      write(luo_saeinp,*) '-1'

      close(luo_saeinp)

      return
      end

end module saeinp_mod

