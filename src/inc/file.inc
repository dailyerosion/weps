!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
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

      ! Logical Unit number for "ci.out" file
      integer   luoci
      parameter (luoci=162)

! Note:  One must check main/openfils.for to ensure unit numbers are unique.
!        Not all files are using logical unit numbers yet.

