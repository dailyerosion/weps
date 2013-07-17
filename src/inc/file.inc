!$Author$
!$Date$
!$Revision$
!$HeadURL$

! note: 42 is reserved for use by saeinp.for routine


      ! input file numbers that are used and then immediately closed
      ! these numbers can be resused in multiple contexts (open then close)
      integer   lui1, lui2, lui3
      parameter (lui1=141)
      parameter (lui2=102)
      parameter (lui3=103)

!     no longer needed
!      integer   lui4
!      parameter (lui4=104)
!
!      integer   lui5
!      parameter (lui5=105)

      ! output file numbers that are used and then immediately closed
      ! these numbers can be resused in multiple contexts (open then close)
      integer   luo1, luo2, luo3
      parameter (luo1=111)
      parameter (luo2=112)
      parameter (luo3=113)

!     no longer needed
!      integer   luo4
!      parameter (luo4=114)
!
!      integer   luo5
!      parameter (luo5=115)

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
      integer   luolog
      parameter (luolog=116)
!
      integer   luodbg
      parameter (luodbg=117)
!
      integer   luoplt
      parameter (luoplt=118)

      ! logical unit number for management detail output file
      integer   luomanage
      parameter (luomanage=15)

! these are for debugging the crop and decomp biomass variables - LEW

      integer luocrop
      parameter (luocrop=17)

      integer luoshoot
      parameter (luoshoot=62)

      integer luoseason
      parameter (luoseason=59)

      integer luoinpt
      parameter (luoinpt=60)

      integer luoallcrop
      parameter (luoallcrop=61)

      integer   luod_above        !Above ground decomp output
      parameter (luod_above=18)
!
      integer   luod_below        !Below ground decomp output
      parameter (luod_below=19)
!
      integer   luocrp1
      parameter (luocrp1=121)

      ! made an array of max 30. Any larger and it stomps on another logical unit number
      integer   luodec(30)
      ! the unit number is now set in openfils.for
      ! parameter (luodec1=170)
      ! parameter (luodec30=199)
      ! make common to preserve values after files are opened
      common / decfiles / luodec

      integer   luobio1
      parameter (luobio1=125)

      ! logical unit number for soil conditioning index out files
      integer   luosci
      parameter (luosci=153)
      integer   luostir
      parameter (luostir=154)

!     these are for debugging the hydrology variables

      integer   luohydro
      parameter (luohydro=151)

      integer   luohlayers
      parameter (luohlayers=152)

      integer   luowater
      parameter (luowater=12)

      ! soil temperature output in heat.for
      integer   luotempsoil
      parameter (luotempsoil=13)

!     legacy debugging output files (when "debug" flags set)
      integer   luohdb
      parameter (luohdb=25)
      integer   luosdb
      parameter (luosdb=26)
      integer   luotdb
      parameter (luotdb=29)
      integer   luocdb
      parameter (luocdb=27)
      integer   luoddb
      parameter (luoddb=28)


      integer   luosurfwat
      parameter (luosurfwat=155)

      integer   luoweather
      parameter (luoweather=156)

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
      integer   luo_subday       ! subdaily wind output
      parameter (luo_subday=24)
      integer   luo_egrd         ! For daily grid output
      parameter (luo_egrd=201)
      integer   luo_erod         ! For daily erosion summary output
      parameter (luo_erod=202)
      integer   luo_emit         ! For subdaily erosion summary output
      parameter (luo_emit=203)
      integer   luo_sgrd         ! For subdaily grid output
      parameter (luo_sgrd=204)

!     saeinp output stand alone erosion single day simulation file
      integer   luo_saeinp
      parameter (luo_saeinp=42)

      ! Logical Unit number for "soilsurf.out" file
      integer   luosoilsurf
      parameter (luosoilsurf=160)

      ! Logical Unit number for "soilsurf.out" file
      integer   luosoillay
      parameter (luosoillay=161)

      ! Logical Unit number for "ci.out" file
      integer   luoci
      parameter (luoci=162)

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

