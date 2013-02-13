!$Author$
!$Date$
!$Revision$
!$HeadURL$

       
       
!      pi      - The constant PI (radians)
!      SEC_PER_DAY - number of seconds per day
!      KG_per_M2_to_LBS_per_ACRE - unit conversion factor
       
       integer   SEC_PER_DAY
       parameter (SEC_PER_DAY = 3600*24)  !number of seconds per day
       
       real      pi
       parameter (pi      = 3.1415927)
       
       real      KG_per_M2_to_LBS_per_ACRE
       parameter (KG_per_M2_to_LBS_per_ACRE = 8921.791)
       
       !     parameters used to define sunrise and sunset (daylength) for different purposes
       
             real             beamrise
             real             civilrise
             real             nautrise
             real             astrorise
       
             parameter( beamrise = 90.833 )
             parameter( civilrise = 96.0 )
             parameter( nautrise = 102.0 )
             parameter( astrorise = 108.0 )
       
       !     beamrise   - solar altitude angle at which the upper edge of the sun is visible
       !     civilrise  - solar altitude angle defined as civil twilight
       !     nautrise   - solar altitude angle defined as nautical twilight
       !     astrorise   - solar altitude angle defined as astronomical twilight
       
!
!     These parameter variables are used to size certain global strings
!     at compile time for WERM.  They may also be consulted from within
!     sections of the WERM code if this file has been included.
!
      integer          mfname
      integer          mwsta
      integer          mloc
      integer          mdate
!
!     + + + VARIABLE DECLARATIONS + + +
!
      parameter (mfname = 30)
      parameter (mwsta = 30)
      parameter (mloc = 10)
      parameter (mdate = 10)
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     mfname - Maximum length of filenames
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          OS dependent
!              typical        15         15
!              ------------   ---------  ---------
!
!            - The length of character strings containing filenames
!              to be handled by any compiled version of WERM is
!              specified by this parameter variable.
!
!     mwsta  - Maximum length of weather station names
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          OS dependent
!              typical        30         30
!              ------------   ---------  ---------
!
!            - The length of character strings containing weather
!              station names to be handled by any compiled version
!              of WERM is specified by this parameter variable.
!
!     mloc   - Maximum length of weather station location strings
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          OS dependent
!              typical        10         10
!              ------------   ---------  ---------
!
!            - The length of character strings containing the
!              location of weather station names to be handled
!              by any compiled version of WERM is specified
!              by this parameter variable.
!
!     mdate  - Maximum length of date strings
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          8          OS dependent
!              typical        10         10
!              ------------   ---------  ---------
!
!            - The length of character strings containing dates
!              to be handled by any compiled version of WERM is
!              specified by this parameter variable.
!
!     ------------------------------------------------------------------

!     These parameter variables are used for unit conversion values
!     within WERM to make the coding more readable and maintainable.
!     Generally, only common unit conversions likely to be used
!     throughout the WERM coding should reside here.  Conversion units
!     used locally in the WERM coding should be defined in separate
!     parameter include files.

!     These parameter values may be consulted from within any sections
!     of the WERM code if this file has been included.

      real             mtomm
      real             mmtom
      integer          hrday
      real             hrtosec
      real             daytosec
      real             degtorad
      real             radtodeg
      real             hatom2
      real             mgtokg
      real             fractopercent
      real             percenttofrac
      real             hrtomin
      real             sectohr

!     + + + VARIABLE DECLARATIONS + + +

      parameter (mtomm  = 1000.0)
      parameter (mmtom  = 0.001)
      parameter (hrday  = 24)
      parameter (hrtosec = 3600.0)
      parameter (daytosec = 86400.0)
      parameter (degtorad = 0.017453293) !pi/180
      parameter (radtodeg = 57.2957795) !180/pi
      parameter (hatom2 = 10000.0) ! hectare to square meters
      parameter (mgtokg = 0.000001) ! milligram to kilogram
      parameter (fractopercent = 100.0)
      parameter (percenttofrac = 0.01)
      parameter (hrtomin = 60.0)
      parameter (sectohr =  1.0/3600.0)

!     + + + VARIABLE DEFINITIONS + + +
!     mtomm  - Unit conversion constant (mm/m)
!     mmtom  - Unit conversion constant (m/mm)
!     hrday  - Number of hours in a day (hrs/day)
!     hrtosec - Unit conversion constant (seconds/hour)
!     daytosec - Unit conversion constant (seconds/day)
!     degtorad - To convert Degrees to Radians, multiply by pi/180 (rad/deg)
!     radtodeg - To convert Radians to Degrees, multiply by 180/pi (deg/rad)
!     hatom2 - To convert hectares to square meters, multiply by 10,000
!     mgtokg - To convert milligrams to kilograms, multiply by 0.000001
!     fractopercent - convert fraction to percent
!     percenttofrac - convert percent to fraction
!     hrtomin - Unit conversion constant (min/hr)

!     These parameter variables are used to size certain global arrays
!     at compile time for WERM.  They may also be consulted from within
!     sections of the WERM code if this file has been included.

      integer          mnar
      integer          mnarpt
      integer          mnbr
      integer          mnbpt
      integer          mncz
      integer          mnspt
      integer          mnsrpt
      integer          mnsub
      integer          mnsz
      integer          mntime
      integer          mnbpls
      integer          mndk
      integer          mngdpt
      integer          mnrbc

      integer          mnhhrs

      integer          mnryr
      integer          mnmon
      integer          mnopdat

      integer          mndayr

!     + + + VARIABLE DECLARATIONS + + +

      parameter (mnar = 4)
      parameter (mnarpt = 2)
      parameter (mnbr = 4)
      parameter (mnbpt = 2)
      parameter (mncz = 5)
      parameter (mnspt = 2)
      parameter (mnsrpt = 2)
      parameter (mnsub = 4)
      parameter (mnsz = 100)
      parameter (mntime = 96)
      parameter (mnbpls = 3)
      parameter (mndk = 5)
      parameter (mngdpt = 500) !increased from 60 for standalone erosion testing
      parameter (mnrbc = 5)

      parameter (mnhhrs = 24)

      parameter (mnryr = 16)
      parameter (mnmon = 13)
      parameter (mnopdat = 32)

      parameter (mndayr = 366)

!     + + + VARIABLE DEFINITIONS + + +

!     mnar   - Maximum number of accounting regions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          10
!              ------------   ---------  ---------
!
!            - The maximum number of accounting regions to be handled
!              by any compiled version of WERM is specified by this
!              parameter variable.

!     mnarpt - Maximum number of accounting region points
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of accounting region points to be
!              handled by any compiled version of WERM is specified
!              by this parameter variable.

!     mnbr  - Maximum number of barriers within subregions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          10
!              ------------   ---------  ---------

!            - The maximum number of barriers within subregions to be
!              handled by any compiled version of WERM is specified
!              by this parameter variable.

!     mnbpt  - Maximum number of barrier points

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of points which designate a barrier
!              shape and location within a subregion.

!     mncz   - Maximum number of crop height segments

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        5          5
!              ------------   ---------  ---------

!            - The number of crop height segments to be handled by any
!              compiled version of WERM is specified by this parameter
!              variable.  (Probably should not be modified since some
!              crop parameters assume there will be 5 crop height
!              segments).

!     mnspt  - Maximum number of simulation region points

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of simulation region points to be
!              handled by any compiled version of WERM is specified
!              by this parameter variable.

!     mnsrpt - Maximum number of subregion points

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of subregion points to be handled
!              by any compiled version of WERM is specified
!              by this parameter variable.

!     mnsub  - Maximum number of subregions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          10
!              ------------   ---------  ---------

!            - The maximum number of subregions to be handled by any
!              compiled version of WERM is specified by this parameter
!              variable.

!     mnsz   - Maximum number of soil layers

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          100
!              ------------   ---------  ---------

!            - The maximum number of soil layers to be handled by any
!              compiled version of WERM is specified by this parameter
!              variable.

!     mntime - Maximum number of timesteps per day

!              range values       min        max
!              ------------   ---------  ---------
!              fixed             1          96
!              typical           24         24
!              ------------   ---------  ---------

!            - The number of time steps to be handled by
!              any compiled version of WERM is specified
!              by this parameter variable.

!     mnbpls - Maximum number of biomass pools

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1              3
!              typical        1              3
!              ------------   ---------  ---------

!            - The maximum number of biomass pools for each subregion

!     mndk   - maximum number of decay coefficients (st,fl,bg,rt,stem no)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1              3
!              typical        1              3
!              ------------   ---------  ---------

!            - The number of biomass decay coefficients for each subregion
!              (standing mass, flat mass, buried mass, root mass, & stem_no)

!     mngdpt - Maximum number of grid points in a given direction.

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              1          30
!              typical            ?          30
!              ------------   ---------  ---------

!            - The maximum number of grid points in a given direction

!     mnrbc  - The number of residue burial classes

!     mnryr  - Maximum number of rotation years in a management file.

!              range values       min        max

!              ------------   ---------  ---------
!              fixed              1          16
!              typical            2           5
!              ------------   ---------  ---------

!            - The maximum number of rotation years in a management file.
!              The sixteenth year is used to store the monthly output
!              regardless of rotation year.

!     mnmon  - Maximum number of months in an output file.

!              range values       min        max

!              ------------   ---------  ---------
!              fixed              1          13
!              typical            1          13
!              ------------   ---------  ---------

!            - The maximum number of months in an output file.
!              The thirteenth month is used to store the annual output
!              for the rotation year.

!     mnopdat- Maximum number of operation dates in an output file.

!              range values       min        max

!              ------------   ---------  ---------
!              fixed              1          32
!              typical            1          32
!              ------------   ---------  ---------

!            - The maximum number of operation dates per month in an
!              output file.
!              The thirtysecond day is used to store the annual output
!              for the current rotation year.

!     mndayr- Maximum number of days in a year, 366.

       
!     nclass         - number of size classes
      integer    nclass
      parameter (nclass = 30)
       
      integer   lui1
      parameter (lui1=141)
!
      integer   lui2
      parameter (lui2=102)
!
      integer   lui3
      parameter (lui3=103)
!
      integer   lui4
      parameter (lui4=104)
!
      integer   lui5
      parameter (lui5=105)
!
      integer   luicli
      parameter (luicli=146)
!
      integer   luiwin
      parameter (luiwin=147)
!            
      integer   luiwsd
      parameter (luiwsd=148)

      !Logical Unit number for "mandate.man" (MCREW management) file
      integer   luimandate
      parameter (luimandate=149)
!      
      integer   luo1
      parameter (luo1=111)
!
      integer   luo2
      parameter (luo2=112)
!
      integer   luo3
      parameter (luo3=113)
!
      integer   luo4
      parameter (luo4=114)
!
      integer   luo5
      parameter (luo5=115)
!
      integer   luolog
      parameter (luolog=116)
!
      integer   luodbg
      parameter (luodbg=117)
!
      integer   luoplt
      parameter (luoplt=118)
!
! these are for debugging the crop and decomp biomass variables - LEW
!
      integer   luod_above        !Above ground decomp output
      parameter (luod_above=18)
!
      integer   luod_below        !Below ground decomp output
      parameter (luod_below=19)
!
      integer   luocrp1
      parameter (luocrp1=121)
!
      integer   luodec1
      parameter (luodec1=122)
!
      integer   luodec2
      parameter (luodec2=123)
!
      integer   luodec3
      parameter (luodec3=124)
!
      integer   luobio1
      parameter (luobio1=125)

!     these are for debugging the hydrology variables

      integer   luohydro
      parameter (luohydro=151)

      integer   luohlayers
      parameter (luohlayers=152)

      integer   luowater
      parameter (luowater=12)

!     new output units for general information

      integer   luoharvest_si
      parameter (luoharvest_si=126)
      integer   luoharvest_en
      parameter (luoharvest_en=127)

      integer   luohydrobal
      parameter (luohydrobal=129)

!     new output unit for calibration information
      integer   luoharvest_calib
      parameter (luoharvest_calib=130)

!     new output unit for GUI required calibration information
      integer   luoharvest_calib_parm
      parameter (luoharvest_calib_parm=131)

      !Logical Unit number for "gui1_data.out" file
      integer   luogui1
      parameter (luogui1=52)

      !Logical Unit number for "mandate.out" file
      integer   luomandate
      parameter (luomandate=81)

!     New output units for erosion submodel

      integer   luo_egrd         ! For daily grid output
      parameter (luo_egrd=201)
      integer   luo_erod         ! For daily erosion summary output
      parameter (luo_erod=202)
      integer   luo_emit         ! For subdaily erosion summary output
      parameter (luo_emit=203)
      integer   luo_sgrd         ! For subdaily grid output
      parameter (luo_sgrd=204)

      ! Logical Unit number for "soilsurf.out" file
      integer   luosoilsurf
      parameter (luosoilsurf=160)

      ! Logical Unit number for "soilsurf.out" file
      integer   luosoillay
      parameter (luosoillay=161)

!     new output for wepp flow routing, runoff events
      integer luowepphdrive
      parameter (luowepphdrive=180)

!     output for wepp erosion events
      integer luowepperod
      parameter (luowepperod=181)

!     output for wepp erosion plot
      integer luoweppplot
      parameter (luoweppplot=182)

!     output for wepp erosion summary
      integer luoweppsum
      parameter (luoweppsum=183)

! Note:  One must check main/openfils.for to ensure unit numbers are unique.
!        Not all files are using logical unit numbers yet.

      integer   TIMSTART
      parameter (TIMSTART = 1)
!
      integer   TIMSTOP
      parameter (TIMSTOP = 2)
!
      integer   TIMPRINT
      parameter (TIMPRINT =	3)
!
      integer   TIMRESET
      parameter (TIMRESET = 4)
!
      integer   TIMCROP
      parameter (TIMCROP = 1)
!
      integer   TIMHYDR
      parameter (TIMHYDR = 2)
!
      integer   TIMSOIL
      parameter (TIMSOIL = 3)
!
      integer   TIMDECO
      parameter (TIMDECO = 4)
!
      integer   TIMMANG
      parameter (TIMMANG = 5)
!
      integer   TIMDARC
      parameter (TIMDARC = 6)
!
      integer   TIMEROS
      parameter (TIMEROS = 7)
!
      integer   TIMSBWIND
      parameter (TIMSBWIND = 8)
!
      integer   TIMSBEROD
      parameter (TIMSBEROD = 9)
!
      integer   TIMSBQOUT
      parameter (TIMSBQOUT = 10)
!
      integer   TIMWEPS
      parameter (TIMWEPS = 11)
      
      real  s11x1,s11y1,s11x2,s11y2,s8x1,s8y1,s8x2,s8y2

      parameter (s11x1   =  5.00)
      parameter (s11x2   = 20.00)
      parameter (s11y1   =  0.01)
      parameter (s11y2   =  0.90)
      parameter (s8x1    = 20.00)
      parameter (s8x2    = 80.00)
      parameter (s8y1    =  0.50)
      parameter (s8y2    =  0.95)


!     + + + VARIABLE DEFINITIONS + + +
! s11x1 - soil labile p concentration (ppm) (1st x-coordinate in the p uptake reduction s-curve)
! s11y1 - p uptake restriction factor (1st y-coordinate in the p uptake reduction s-curve)
! s11x2 - soil labile p concentration (ppm) (2nd x-coordinate in the p uptake reduction s-curve)
! s11y2 - p uptake restriction factor (2nd y-coordinate in the p uptake reduction s-curve)
! s8x1 - scaled ratio of actaul to potential n or p (1st x-coordinate in the n stress factor s-curve)
! s8y1 - n or p stress factor (1st ycoordinate in the n or p stress s-curve)
! s8x2 - scaled ratio of actaul to potential n or p (2nd x-coordinate in the n stress factor s-curve)
! s8y2 - n or p stress factor (2nd ycoordinate in the n or p stress s-curve)  

      integer          N_G_DPT  !# of grid data points under no barrier cond.
      integer          B_G_DPT  !# of grid data points to use if barrier exists
      real             MIN_GRID_SP !minimum targeted grid spacing (m)


      parameter (N_G_DPT = 30)
      parameter (B_G_DPT = 60)
      parameter (MIN_GRID_SP = 7.0)
      
      !     These parameter variables are used to specify the scope 
      !     of specific variable values within the erosion submodel
      
      real  SLRR_MIN
      real  SLRR_MAX
      real  WZZO_MIN
      real  WZZO_MAX
      
            parameter (SLRR_MIN = 1.5)  !minimum RR value (mm)
            parameter (SLRR_MAX = 100.0)!maximum RR value (mm)
            parameter (WZZO_MIN = 0.5)  !minimum wwzo value (mm)
            parameter (WZZO_MAX = 30.0) !maximum wwzo value (mm)
      
      
      !     + + + VARIABLE DEFINITIONS + + +
      
      !     SLRR_MIN - Minimum RR value allowed for grid cell
      !                random roughness (mm)
      !     SLRR_MAX - Maximum RR value allowed for grid cell
      !                random roughness (mm)
      !     WZZO_MIN - Minimum wwzo value allowed for grid cell
      !                bare, non-ridged, surface aerodynamic roughness (mm)
      !                (currently this corresponds to ~1.67mm RR)
      !     WZZO_MAX - Maximum wwzo value allowed for grid cell
      !                bare, non-ridged, surface aerodynamic roughness (mm)
!                (currently this corresponds to ~100mm RR)

      real claygrav80rh, orggrav80rh

!     claygrav80rh - gravimetric water content of soil clay at 80% rh
!     orggrav80rh - gravimetric water content of soil organics at 80% rh

      parameter( claygrav80rh = 0.3 )
      parameter( orggrav80rh = 0.3 )
      
      integer  lrw, liw
      parameter (liw = 126)
      parameter (lrw = 1824)
      
!     lrw             - lsoda: length of real work array
!     liw             - lsoda: length of integer work array

      real sandheatcap, siltheatcap, clayheatcap
      real waterheatcap, iceheatcap, organheatcap

!     sandheatcap - heat capacity of sand (J/kg C)
!     siltheatcap - heat capacity of silt (J/kg C)
!     clayheatcap - heat capacity of clay (J/kg C)
!     waterheatcap - heat capacity of water (J/kg C)
!     iceheatcap - heat capacity of ice (J/kg C)
!     organheatcap - heat capacity of organic matter (J/kg C)

      ! values from Kluitenberg
      parameter( sandheatcap = 730.0 )
      parameter( siltheatcap = 730.0 )
      parameter( clayheatcap = 730.0 )
      parameter( waterheatcap = 4180.0 )

      ! value from deVries and from Incropera and DeWit
      parameter( iceheatcap = 2040.0 )

      ! values from Kluitenberg
      parameter( organheatcap = 1900.0 )
      
      real quartzheatcond, mineralheatcond
      real iceheatcond, organicheatcond
      
      !     sandheatcond - thermal conductivity of sand (J/s m C) or (W/m C)
      !     siltheatcond - thermal conductivity of silt (J/s m C) or (W/m C)
      !     clayheatcond - thermal conductivity of clay (J/s m C) or (W/m C)
      !     iceheatcond - thermal conductivity of ice (J/s m C) or (W/m C)
      !     organicheatcond - thermal conductivity of organic matter (J/s m C) or (W/m C)
      
            ! values from Bristow in Dane and Topp (2002)
            parameter( quartzheatcond = 8.8)
            parameter( mineralheatcond = 2.9)
            parameter( iceheatcond = 2.18)
      parameter( organicheatcond = 0.25)
      
          real den_organic, den_quartz, den_ice
      
      !     den_organic - density of soil organic particles (Mg/m^3)
      !         Particle density of organic matter
      !         1.37 in Baver, Soil Physics, 1972 p.44 (humus)
      !         1.1  in Marshall and Holmes, Soil Physics, 1979, p.277 (soil organic matter)
      
      !     den_quartz - density of soil quartz articles (Mg/m^3)
      
            parameter(den_organic = 1.23)
            parameter(den_quartz = 2.65)
      parameter(den_ice = .92)
      
       real min_snow_den
            real melt_snow_den
            real max_snow_den
            real heat_fusion
      
      !     min_snow_den - minimum density of new snow (kg/m^3)
      !     melt_snow_den - snow density above which melt water is released (kg/m^3)
      !     max_snow_den - maximum possible snow density (kg/m^3)
      !     heat_fusion -  latent heat of fusion for water (J/kg)
      
            parameter(min_snow_den = 100.0)
            parameter(melt_snow_den = 350.0)
            parameter(max_snow_den = 522.0)
      parameter(heat_fusion = 337000.0)
      
      ! soil vapor properties required for calculations
            real  rgas, molewater, zerokelvin
            real  denwat, gravconst, potwilt, potfc, potfcs
            real  diffuntp, atmstand, templapse, tempstand, rair
      !     rgas       - universal gas constant (joules/(mole degree K)
      !     molewater  - molecular weight of water (kg/mole)
      !     zerokelvin - kelvin equivalent of zero degree centigrade
      !     denwat     - density of the liquid (water) (kg/m^3)
      !     gravconst  - acceleration due to gravity (m/s^2)
      !     potwilt    - matric potential for wilting point (15 bar) in meters of water
      !     potfc      - matric potential for field capacity (1/3 bar) in meters of water
      !     potfcs     - matric potential for field capacity (1/10 bar) in meters of water
      !     diffuntp   - Binary diffusion coefficient for water vapor in air at
      !                  normal temperature and pressure (0 C, 1 standard atmosphere) (m^2/s)
      !     atmstand   - standard atmosphere (kilopascals)
      !     templapse  - temperature lapse rate for troposphere (degree K/meter)
      !     tempstand  - standard temperature used to find standard atmosphere (degree K)
      !     rair       - gas constant for air (J/kg/K or m^2/s^2/K)
      
      !*** Parameter Definitions ***
            parameter (rgas = 8.3143)
            parameter (molewater = 0.018)
            parameter (zerokelvin = 273.16)
            parameter (denwat = 1000.0)
            parameter (gravconst = 9.807)
            parameter (potwilt = -152.95) !in meters of water at max density
            parameter (potfc = -3.3989)   !in meters of water at max density
            parameter (potfcs = -1.01967)   !in meters of water at max density
            parameter (diffuntp = 2.12e-5)
            parameter (atmstand = 101.3)
            parameter (templapse = 0.0065)
            parameter (tempstand = 288.0)
      parameter (rair = 287.0)
      
           integer        mxtbln
      parameter      (mxtbln = 2000)
      
           integer          msieve
      real             mingsd

      parameter (msieve = 26)
      parameter (mingsd = 2.0)
      
      real halfperiod
      parameter( halfperiod = 43200 )

       
       