!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine saeinp

!
!     +++ PURPOSE +++
!     print out input file for stand alone erosion
!
!     +++ ARGUMENT DECLARATIONS +++

!     +++ ARGUMENT DEFINITIONS +++

!
!     +++ PARAMETER +++
!
!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'p1const.inc'
      include  'b1glob.inc'
      include  'c1glob.inc'
      include  'd1glob.inc'
      include  'm1geo.inc'
      include  'w1wind.inc'
      include  'w1pavg.inc'
      include  's1dbh.inc'
      include  's1layr.inc'
      include  's1phys.inc'
      include  's1agg.inc'
      include  's1surf.inc'
      include  's1sgeo.inc'
      include  'h1db1.inc'
      include  'm1flag.inc'
      include  'm1sim.inc'
      include  'm1subr.inc'
      include  'wpath.inc'
      include  'c1gen.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
      include 'erosion/e2grid.inc'
      include 'erosion/e3grid.inc'
      include 'erosion/m2geo.inc'
      include 'erosion/s2sgeo.inc'
      include 'erosion/s2agg.inc'
      include 'erosion/s2surf.inc'
!      include  'erosion/w2wind.inc'
!
!     +++ LOCAL VARIABLES +++
      integer k,l
      integer b
      integer day, mon, yr
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     +++ SUBROUTINES CALLED +++
!
!     +++ FUNCTION DECLARATIONS +++
!
!     +++ END SPECIFICATIONS +++
!
      call fopenk (42,rootp(1:len_trim(rootp)) //'saeros.in','unknown')
      call caldat (am0jd,day,mon,yr)
      write(42,2101) day, mon, yr
 2101 format('# day mon yr',2(1x,i2),2x,i4)

!     print header info
      write(42,1005)
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

      write(42,*) '0'
      write(42,2000)
 2000 format(                                                           &
     & '#',/,                                                           &
     & '# +++ INITIALIZATIONS +++',/,                                   &
     & '#',/,                                                           &
     & '#     am0eif, L, (m1flag.inc) EROSION initialization flag')
      write(42,*) '.TRUE.'
      write(42,2005)
 2005 format('#    am0efl, L, (m1flag.inc) EROSION "print" flag')
      write(42,*) '1'
      write(42,2100)
 2100 format ('#',/,                                                    &
     &'# +++ SIMULATION REGION +++',/,                                  &
     &'#',/,                                                            &
     &'#     amxsim(x,y), R, (m1geo.inc) Simulation Region coordinates (&
     &m)',/,                                                            &
     &'#              Input x,y coordinates in this form: x1,y1  x2,y2')
      write(42,*) amxsim(1,1), amxsim(2,1), amxsim(1,2),amxsim(2,2)
      write(42,2105)
 2105 format(                                                           &
     &'#',/,                                                            &
     &'#     amxsim(x,y), R, (m1geo.inc) Simulation Region orientation a&
     &ngle',/,                                                          &
     &'#     clockwise with 0=north')
      write(42,*) amasim
      write(42,2110)
 2110 format('#',/,                                                     &
     &'#  +++ ACCOUNTING REGIONS +++',/,                                &
     &'#',/,                                                            &
     &'# nacctr, I, (m1geo.inc) Number of accounting regions (must be 1 &
     &for now)')
      write(42,*) nacctr
      write(42,2115)
 2115 format('#',/,                                                     &
     &'#     amxar(x,y,a), R, (m1geo.inc) Accounting Region coordinates &
     &(m)',/,                                                           &
     &'#                     Input x,y coordinates in this form: x1,y1  &
     &x2,y2',/,                                                         &
     &'#                     for each accounting region specified (nacct&
     &r)')
      write(42,*) amxar(1,1,nacctr), amxar(2,1,nacctr),                 &
     &            amxar(1,2,nacctr),amxar(2,2,nacctr)
!       barriers
      write(42,2120)
 2120 format ('#',/,                                                    &
     &'# +++ BARRIERS +++',/,                                           &
     &'#',/,                                                            &
     &'#     nbr, I, (m1geo.inc) Number of barriers (0-5) ')
      write(42,*) nbr
      write(42,2122)
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
         write(42,2125)
 2125    format('# amxar(1,1,b), amxar(2,1), amxar(1,2),amxar(2,2)')
         write(42,*) amxbr(1,1,b),amxbr(2,1,b),amxbr(1,2,b),amxbr(2,2,b)

         write (42,2130)
 2130    format('#, amzbr(b), ampbr(b), amxbrw(b)')
         write (42,*) amzbr(b), ampbr(b), amxbrw(b)
   33 continue

!       subregions
      write(42,2135)
 2135 format('#',/,                                                     &
     &'# +++ SUBREGIONS +++',/,                                         &
     &'#',/,                                                            &
     &'#     nsubr, I, (m1subr.inc) Number of subregions (1-5)')
      write(42,*) nsubr
      write(42,2137)
 2137 format('#',/,                                                     &
     &'#     NOTE: Remaining SUBREGION inputs (BIOMASS, SOIL, and HYDROL&
     &OGY,',/,                                                          &
     &'#     ie. variables defined by subregion) are repeated for nsubr &
     &',/,                                                              &
     &'#     subregions specified',/,                                   &
     &'#',/,                                                            &
     &'#     amxsr(x,y,s), R, (m1subr.inc) Subregion coordinates (m)',/,&
     &'#             Input x,y coordinates in this form: x1,y1 x2,y2',/,&
     &'#             for each subregion specified (subr)')
      write(42,*) amxsr(1,1,1),amxsr(2,1,1),amxsr(1,2,1),amxsr(2,2,1)
      write(42,2140)
 2140 format('#',/,                                                     &
     &'#     +++ BIOMASS +++',/,                                        &
     &'#',/,                                                            &
     &'#      adzht_ave(s), R, (d1glob.inc) Average residue height (m)')
      write(42,*) adzht_ave(1)
! Changed above to use "average residue height" instead of "overall height" - LEW 1/26/06
!     &'#       abzht(s), R, (b1glob.inc) Overall biomass height (m)')
!      write(42,*) abzht(1)

      write(42,2146)
 2146 format('#',/,                                                     &
     &'#       aczht(s), R, (c1glob.inc) Crop height (m)')
      write(42,*) aczht(1)

      write(42,2147)
 2147 format('#',/,                                                     &
     &'#       acrsai(s), R, (c1glob.inc) Crop stem area index (m^2/m^2)&
     &',/,                                                              &
     &'#       acrlai(s), R, (c1glob.inc) Crop leaf area index (m^2/m^2)&
     &')
      write(42,*) acrsai(1), acrlai(1)

      write(42,2148)
 2148 format('#',/,                                                     &
     &'#       adrsaitot(s), R, (d1glob.inc) Residue stem area index (m^&
     &2/m^2)',/,                                                        &
     &'#       adrlaitot(s), R, (d1glob.inc) Residue leaf area index (m^&
     &2/m^2)')
      write(42,*) adrsaitot(1), adrlaitot(1)

      write(42,2149)
 2149 format('#',/,                                                     &
     &'#       acxrow(s) Crop row spacing (m)'                          &
     &,/,                                                               &
     &'#       ac0rg(s)  Crop seed placement (0 - furrow, 1 - ridge)'   &
     &)
      write(42,*) acxrow(1), ac0rg(1)

      write(42,2150)
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
      write(42,*) abffcv(1)

!      soil
      write(42,2160)
 2160 format('#',/,                                                     &
     &'#     +++ SOIL +++',/,                                           &
     &'#',/,                                                            &
     &'#     nslay(s), I, (s1layr.inc) Number of soil layers (3-10)')

      write(42,*) nslay(1)

      write(42,2165)
 2165 format('#',/,                                                     &
     &'#     NOTE: Remaining SOIL inputs are repeated for each layer spe&
     &cified',/,                                                        &
     &'#',/,                                                            &
     &'#     aszlyt(l,s), R, (s1layr.inc) Soil layer thickness (mm)')
      write(42,*) (aszlyt(l,1), l=1,nslay(1))
      write(42,2170)
 2170 format('#',/,                                                     &
     &'#     asdblk(l,s), R, (s1phys.inc) Soil layer bulk density (Mg/m^&
     &3')
      write(42,*) (asdblk(l,1), l=1,nslay(1))
      write(42,2175)
 2175 format('#',/,                                                     &
     &'#     asfsan(l,s),R,(s1dbh.inc) Soil layer sand content (Mg/Mg)')
      write(42,*) (asfsan(l,1), l=1,nslay(1))
      write(42,2177)
 2177 format('#',/,                                                     &
     &'#     asfvfs(l,s), R, (s1dbh.inc) Soil layer very fine sand (Mg/M&
     &g)')
      write(42,*) (asfvfs(l,1), l=1,nslay(1))
      write(42,2180)
 2180 format('#',/,                                                     &
     &'#     asfsil(l,s),R,(s1dbh.inc) Soil layer silt content (Mg/Mg)')
      write(42,*) (asfsil(l,1), l=1,nslay(1))
      write(42,2185)
 2185 format('#',/,                                                     &
     &'#     asfcla(l,s),R,(s1dbh.inc) Soil layer clay content (Mg/Mg)')
      write(42,*) (asfcla(l,1), l=1,nslay(1))
      write(42,2190)
 2190 format('#',/,                                                     &
     &'#     asvroc(l,s), R, (s1dbh.inc) Soil layer rock volume (m^3/m^3&
     &)')
      write(42,*) (asvroc(l,1), l=1,nslay(1))
      write(42,2195)
 2195 format('#',/,                                                     &
     &'#     asdagd(l,s),R,(s1agg.inc) Soil layer agg density (Mg/m^3)')
      write(42,*) (asdagd(l,1), l=1,nslay(1))
      write(42,2200)
 2200 format('#',/,                                                     &
     &'#     aseags(l,s), R, (s1agg.inc) Soil layer agg stability ln(J/k&
     &g)')
      write(42,*) (aseags(l,1), l=1,nslay(1))
      write(42,2205)
 2205 format('#',/,                                                     &
     &'#     aslagm(l,s), R, (s1agg.inc) Soil layer GMD (mm)')
      write(42,*) (aslagm(l,1), l=1,nslay(1))
      write(42,2210)
 2210 format('#',/,                                                     &
     &'#     aslagn(l,s), R, (s1agg.inc) Soil layer minimum agg size (mm&
     &)')
      write(42,*) (aslagn(l,1), l=1,nslay(1))
      write(42,2215)
 2215 format('#',/,                                                     &
     &'#     aslagx(l,s), R, (s1agg.inc) Soil layer maximum agg size (mm&
     &)')
      write(42,*) (aslagx(l,1), l=1,nslay(1))
      write(42,2220)
 2220 format('#',/,                                                     &
     &'#     as0ags(l,s), R, (s1agg.inc) Soil layer GSD (mm/mm)')
      write(42,*) (as0ags(l,1), l=1,nslay(1))
      write(42,2225)
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
      write(42,*) asfcr(1), aszcr(1), asflos(1), asmlos(1), asdcr(1),   &
     &            asecr(1)
      write(42,2230)
 2230 format('#',/,                                                     &
     &'#     aslrr(s), R, (s1sgeo.inc) Allmaras random roughness (mm)')
      write(42,*) aslrr(1)
      write(42,2235)
 2235 format('#',/,                                                     &
     &'#     aszrgh(s), R, (s1sgeo.inc) Ridge height (mm)',/,           &
     &'#     asxrgs(s), R, (s1sgeo.inc) Ridge spacing (mm)',/,          &
     &'#     asxrgw(s), R, (s1sgeo.inc) Ridge width (mm)',/,            &
     &'#     asargo(s), R, (s1sgeo.inc) Ridge orientation (deg)')
      write(42,*) aszrgh(1), asxrgs(1), asxrgw(1), asargo(1)
      write(42,2240)
 2240 format('#',/,                                                     &
     &'#     asxdks(s), R, (s1sgeo.inc) Dike spacing (mm)')
      write(42,*) asxdks(1)
      write(42,2245)
!      hydrology
 2245 format('#',/,                                                     &
     &'#     +++ HYDROLOGY +++',/,                                      &
     &'#',/,                                                            &
     &'#     ahzsnd(s), R, (s1sgeo.inc) Snow depth (mm)')
      write(42,*) ahzsnd(1)
      write(42,2250)
 2250 format('#',/,                                                     &
     &'#     ahrwcw(l,s), R, (h1db1.inc) Soil layer wilting point water &
     &content (Mg/Mg)')
      write(42,*) (ahrwcw(l,1), l=1,nslay(1))
      write(42,2255)
 2255 format('#',/,                                                     &
     &'#     ahrwca(l,s), R, (h1db1.inc) Soil layer water content (Mg/Mg&
     &)')
      write(42,*) (ahrwca(l,1), l=1,nslay(1))
      write(42,2260)
 2260 format('#',/,                                                     &
     &'#     ahrwc0(h,s), R, (h1db1.inc) Surface layer water content (Mg&
     &/Mg)',/,                                                          &
     &'#                  NOTE: the near surface water content is specif&
     &ied on an',/,                                                     &
     &'#                        hourly basis.  We read in the hrly water&
     & content',/,                                                      &
     &'#                        on two lines, with 12 values in each lin&
     &e.')
      write(42,22) (ahrwc0(l,1), l=1,12)
 22   format(12(1x,f10.7))
      write(42,22) (ahrwc0(l,1), l=13,24)

!      weather
      write(42,2270)
 2270 format('#',/,                                                     &
     &'# NOTE: This is the end of the SUBREGION variables',/,           &
     &'#',/,                                                            &
     &'#     +++ WEATHER +++',/,                                        &
     &'#',/,                                                            &
     &'#     awdair, R, (w1pavg.inc) Air density (kg/m^3)')
      write(42,*) awdair
      write(42,2275)
 2275 format('#',/,                                                     &
     &'#     awadir, R, (w1wind.inc) Wind direction (deg)')
      write(42,*) awadir
      write(42,2280)
 2280 format('#',/,                                                     &
     &'#     ntstep, I, (local variable) Number of intervals/day to run &
     &EROSION')
      write(42,*) ntstep
      write(42,2285)
 2285 format('#',/,                                                     &
     &'#     anemht, R  anemometer height (m)',/,                       &
     &'#     awzzo,  R  aerodynamic roughness at anemometer site (mm)', &
     &/,'#     wzoflg, I (global variable) zo location flag',/,         &
     &'#               (flag =0 - zo fixed at wx sta. location)',/,     &
     &'#               (flag = 1 - zo variable at field location)')
      write(42,*) anemht, awzzo, wzoflg
      write(42,2290)
 2290 format('#',/,                                                     &
     &'#     wflg, I, (local variable) Wind/Weibull flag',/,            &
     &'#              (0 - read in Weibull parameters, 1 - read in wind &
     &speeds)')
      write(42,*) '1'
      write(42,2295)
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
      write(42,*) (awu(k), k=1,6)
      write(42,*) (awu(k), k=7,12)
      write(42,*) (awu(k), k=13,18)
      write(42,*) (awu(k), k=19,24)
      write(42,2300)
 2300 format('#',/,                                                     &
     &     '#    + + + DATA TO PLOT + + +',/,                           &
     &     '#',/,                                                       &
     &     '#   names and values to input for plot',/,                  &
     &     '#   place 1 flag in 1st line after #-name line for variables&
     & to include in plot',/,                                           &
     &     '#',/,                                                       &
     &     '#   initial xplot value,I, (-1=no plot, 0 = plot indep.varia&
     &bles with 1 flag)')
      write(42,*) '-1'
!
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
