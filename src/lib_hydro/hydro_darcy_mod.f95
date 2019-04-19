!$Author$
!$Date$
!$Revision$
!$HeadURL$
module hydro_darcy_mod

    type sls001
      real :: ccmax
      real :: el0
      real :: h
      real :: hmin
      real :: hmxi
      real :: hu
      real :: rc
      real :: tn
      real :: uround
      integer :: icf
      integer :: ierpj
      integer :: iersl
      integer :: jcur
      integer :: jstart
      integer :: kflag
      integer :: l
      integer :: lyh
      integer :: lewt
      integer :: lacor
      integer :: lsavf
      integer :: lwm
      integer :: liwm
      integer :: meth
      integer ::  miter
      integer :: maxord
      integer :: maxcor
      integer :: msbp
      integer :: mxncf
      integer :: n
      integer :: nq
      integer :: nst
      integer :: nfe
      integer :: nje
      integer :: nqu
    end type sls001

    type slsa01
      real :: pdnorm
      integer :: jtyp
      integer :: mused
      integer :: mxordn
      integer :: mxords
    end type slsa01

    type slsoda_loc
      integer :: init
      integer :: mxstep
      integer :: mxhnil
      integer :: nhnil
      integer :: nslast
      integer :: nyh
      integer :: insufr
      integer :: insufi
      integer :: ixpr
      real :: tsw
    end type slsoda_loc

    type sstoda_loc
      integer :: ialth
      integer :: ipup
      integer :: lmax
      integer :: nqnyh
      integer :: nslp
      integer :: icount
      integer :: irflag
      real :: conit
      real :: crate
      real :: el(13)
      real :: elco(13,12)
      real :: hold
      real :: rmax
      real :: tesco(3,12)
      real :: cm1(12)
      real :: cm2(5)
      real :: pdest
      real :: pdlast
      real :: ratio
      real :: sm1(12)
    end type sstoda_loc

    type dvolwparam
!     defines for soil water properties
!     number of layers plus six variables
!     ponded depth, cumulative evaporation, cumulative runoff,
!     cumulative infiltration, cumulative drainage,
!     and cumulative rainfall
!     iwork and rwork arrays set for max 100 soil layers
!     plus 6 auxilary variables (neq = 106, ml=3, mu=1)
      integer :: liw ! (20 + NEQ) lsoda: length of integer work array
      integer :: lrw ! (22 + 10*NEQ + (2*ML+MU)*NEQ if JT = 4 or 5.) lsoda: length of real work array
      integer :: layrsn    ! number of soil layers
      integer :: istate    ! LSODA input control and result flag, preserved between days
      integer, dimension(:), allocatable :: iwork ! (liw) lsoda: integer work array (20+numeq)
      real :: t        ! time since last LSODA initialization, preserved between days
      real, dimension(:), allocatable :: rwork    ! (lrw) lsoda: real work array (22+10*numeq+(2*ml+mu)*numeq
      real, dimension(:), allocatable :: tlay    ! (mnsz) soil layer thickness (m)
      real, dimension(:), allocatable :: dist    ! (mnsz) distance from center of soil layer(index-1) to soil layer(index) (m)
      real, dimension(:), allocatable :: depth   ! (mnsz) distance from surface to center of soil layer (m)
      real, dimension(:), allocatable :: thetas  ! (mnsz) saturated volumetric water content
      real, dimension(:), allocatable :: thetar  ! (mnsz) volumetric water content where conductivity ceases (residual)
      real, dimension(:), allocatable :: ksat    ! (mnsz) saturated hydraulic conductivity (m/s)
      real, dimension(:), allocatable :: airentry ! (mnsz) Brooks and Corey air entry potential (m)
      real, dimension(:), allocatable :: lambda   ! (mnsz) Brooks and Corey pore interaction exponent
      real, dimension(:), allocatable :: theta80rh ! (mnsz) volumetric water content when soil air is 80 percent
                ! relative humidity (obtained from soil adsorption isotherm estimates)
      real, dimension(:), allocatable :: thetaw   ! (mnsz) wilting point (15 bar) volumetric water content
      real, dimension(:), allocatable :: soiltemp ! (mnsz) soil temperature (C)
      real :: airtmaxprev    ! maximum air temperature of the previous day (C)
      real :: airtmin        ! minimum air temperature today (C)
      real :: airtmax        ! maximum air temperature today (C)
      real :: airtminnext    ! minimum air temperature of the next day (C)
      real :: tdew           ! dewpoint temperature (C)
      real :: beginday       ! time of the beginning of the day (seconds)
      real :: lenday         ! length of the daylight (seconds)
      real :: sunrise        ! time of sunrise (seconds)
      real :: sunset         ! time of sunset (seconds)
      real :: evapendconstant ! cumulative evaporation where falling rate stage begins (m)
      real :: evapdaypot     ! potential evaporation for day (m)
      real :: evaptrans      ! soil vapor transmissivity (m/d^0.5)
      real :: evapamp        ! amplitude of the daily evaporation curve (m)
                             ! calculated from pi*dailydepth(m)/lenday/2.0
      real :: raindepth      ! total depth of a rainfall event (m)
      real :: rainstart      ! time of the start of a rainfall event (seconds)
      real :: rainend        ! time of the end of a rainfall event (seconds)
      real :: rainmid        ! time of the peak of a rainfall event (seconds)
      real :: soilslope      ! slope of the soil surface (m/m)
      real :: pondmax        ! maximum depth of soil surface ponding before runoff (m)
      real :: dw_friction    ! darcy weisbach friction factor for runoff flow rate calculation
      real :: slopelength    ! length of sheet flow runoff (overland flow element) (m)
      real :: atmpres        ! pressure of the ambient atmosphere (kPa)
      real :: surface_rate   ! rate of surface water flow into the soil (m/sec)
      real :: surface_start  ! time of the start of surface flow rate (seconds)
      real :: surface_end    ! time of the end of surface flow rate (seconds)
      real, dimension(:), allocatable :: source_rate ! (mnsz) rate of water source flow into the soil (m/sec)
      real, dimension(:), allocatable :: source_start ! (mnsz) time of the start of source flow rate (seconds)
      real, dimension(:), allocatable :: source_end ! (mnsz) time of the end of source flow rate (seconds)
      real, dimension(:), allocatable :: cond      ! (mnsz) value used from previous entry into dvolw
      real, dimension(:), allocatable :: potm      ! (mnsz) value used from previous entry into dvolw
      real, dimension(:), allocatable :: swm       ! (mnsz) value used from previous entry into dvolw
      real, dimension(:), allocatable :: soilrh    ! (mnsz) value used from previous entry into dvolw
      real, dimension(:), allocatable :: soilvapden ! (mnsz) value used from previous entry into dvolw
      real, dimension(:), allocatable :: soildiffu ! (mnsz) value used from previous entry into dvolw
      real, dimension(:), allocatable :: fluxv     ! (mnsz) vapor flux 
      real, dimension(:), allocatable :: fluxw     ! (mnsz) water flux
      real, dimension(:), allocatable :: lastvolw ! (mnsz+7) value saved from previous entry into dvolw
      real, dimension(:), allocatable :: prevvolw ! (mnsz+7) initial values of water content at beginning of hour
                 ! indexes greater than layrsn may be updated hourly
    end type dvolwparam


    type(sls001), dimension(:), allocatable :: sls1
    type(slsa01), dimension(:), allocatable :: slsa
    type(slsoda_loc), dimension(:), allocatable :: sloc
    type(sstoda_loc), dimension(:), allocatable :: stoc
    type(dvolwparam), dimension(:), allocatable :: dvwp 

  contains

    subroutine allocate_lsoda_sls1( nsubr )
      integer, intent(in) :: nsubr   ! number of subregions
      integer :: alloc_stat
      allocate( sls1(nsubr), stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate storage for lsoda sls1.'
      end if
    end subroutine allocate_lsoda_sls1

    subroutine deallocate_lsoda_sls1()
      integer :: dealloc_stat
      deallocate( sls1, stat=dealloc_stat )
      if( dealloc_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate storage for lsoda sls1.'
      end if
    end subroutine deallocate_lsoda_sls1

    subroutine allocate_lsoda_slsa( nsubr )
      integer, intent(in) :: nsubr   ! number of subregions
      integer :: alloc_stat
      allocate( slsa(nsubr), stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate storage for lsoda slsa.'
      end if
    end subroutine allocate_lsoda_slsa

    subroutine deallocate_lsoda_slsa()
      integer :: dealloc_stat
      deallocate( slsa, stat=dealloc_stat )
      if( dealloc_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate storage for lsoda slsa.'
      end if
    end subroutine deallocate_lsoda_slsa

    subroutine allocate_lsoda_sloc( nsubr )
      integer, intent(in) :: nsubr   ! number of subregions
      integer :: alloc_stat
      allocate( sloc(nsubr), stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate storage for lsoda sloc.'
      end if
    end subroutine allocate_lsoda_sloc

    subroutine deallocate_lsoda_sloc()
      integer :: dealloc_stat
      deallocate( sloc, stat=dealloc_stat )
      if( dealloc_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate storage for lsoda sloc.'
      end if
    end subroutine deallocate_lsoda_sloc

    subroutine allocate_lsoda_stoc( nsubr )
      integer, intent(in) :: nsubr   ! number of subregions
      integer :: alloc_stat
      integer :: isr
      allocate( stoc(nsubr), stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate storage for lsoda stoc.'
      end if
      ! initialize
      do isr = 1, nsubr
        stoc(isr)%sm1(1) = 0.5e0
        stoc(isr)%sm1(2) = 0.575e0
        stoc(isr)%sm1(3) = 0.55e0
        stoc(isr)%sm1(4) = 0.45e0
        stoc(isr)%sm1(5) = 0.35e0
        stoc(isr)%sm1(6) = 0.25e0
        stoc(isr)%sm1(7) = 0.20e0
        stoc(isr)%sm1(8) = 0.15e0
        stoc(isr)%sm1(9) = 0.10e0
        stoc(isr)%sm1(10) = 0.075e0
        stoc(isr)%sm1(11) = 0.050e0
        stoc(isr)%sm1(12) = 0.025e0
      end do

    end subroutine allocate_lsoda_stoc

    subroutine deallocate_lsoda_stoc()
      integer :: dealloc_stat
      deallocate( stoc, stat=dealloc_stat )
      if( dealloc_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate storage for lsoda stoc.'
      end if
    end subroutine deallocate_lsoda_stoc

    subroutine allocate_dvolw_param( nsubr, soil )
      use soil_data_struct_defs, only: soil_def
      integer, intent(in) :: nsubr   ! number of subregions
      ! NOTE: passes soil_in which is 1 based array, not soil which is 0 based
      type(soil_def), dimension(:), intent(inout) :: soil  ! contains nslay:

      integer :: alloc_stat
      integer :: sum_stat
      integer :: isr
      integer :: numeq
      integer :: ml
      integer :: mu

      allocate( dvwp(nsubr), stat=alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate storage for dvolw params.'
        return
      end if
      sum_stat = 0
      do isr = 1, nsubr

        ml=3
        mu=1
        numeq = soil(isr)%nslay + 6
        dvwp(isr)%liw = 20 + numeq
        dvwp(isr)%lrw = 22 + 10*numeq + (2*ml+mu)*numeq
        allocate( dvwp(isr)%iwork(dvwp(isr)%liw), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%rwork(dvwp(isr)%lrw), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat

        allocate( dvwp(isr)%tlay(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%dist(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%depth(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%thetas(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%thetar(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%ksat(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%airentry(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%lambda(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%theta80rh(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%thetaw(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%soiltemp(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%source_rate(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%source_start(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%source_end(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%cond(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%potm(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%swm(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%soilrh(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%soilvapden(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%soildiffu(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%fluxv(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%fluxw(soil(isr)%nslay), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%lastvolw(soil(isr)%nslay+7), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
        allocate( dvwp(isr)%prevvolw(soil(isr)%nslay+7), stat=alloc_stat )
        sum_stat = sum_stat + alloc_stat
      end do
      if( sum_stat .gt. 0 ) then
        write(*,*) 'Unable to allocate storage for dvolw params.'
      end if

    end subroutine allocate_dvolw_param

    subroutine deallocate_dvolw_param( nsubr )
      integer, intent(in) :: nsubr   ! number of subregions
      integer :: dealloc_stat
      integer :: sum_stat
      integer :: isr

      sum_stat = 0
      do isr = 1, nsubr

        deallocate( dvwp(isr)%iwork, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%rwork, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat

        deallocate( dvwp(isr)%tlay, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%dist, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%depth, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%thetas, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%thetar, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%ksat, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%airentry, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%lambda, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%theta80rh, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%thetaw, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%soiltemp, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%source_rate, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%source_start, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%source_end, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%cond, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%potm, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%swm, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%soilrh, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%soilvapden, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%soildiffu, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%fluxv, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%fluxw, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%lastvolw, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
        deallocate( dvwp(isr)%prevvolw, stat=dealloc_stat )
        sum_stat = sum_stat + dealloc_stat
      end do
      if( sum_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate storage for dvolw params.'
      end if

      deallocate( dvwp, stat=dealloc_stat )
      if( dealloc_stat .gt. 0 ) then
        write(*,*) 'Unable to deallocate storage for dvolw params.'
      end if
    end subroutine deallocate_dvolw_param

    subroutine darcy(isr, daysim, numeq, bszlyt, bszlyd, bulkden,     &
     &       theta, thetadmx, bthetas, bthetaf, bthetaw, bthetar,       &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav,               &
     &       bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt,            &
     &       rise, daylength, bhzep, dprecip, bwdurpt, bwpeaktpt,       &
     &       dirrig, bhdurirr, bhlocirr, bhzoutflow,                    &
     &       bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0,             &
     &       bhzea, bhzper, bhzrun, bhzinf, bhzwid,                     &
     &       bhzeasurf, evaplimit, vaptrans, bmrslp )

!     + + + PURPOSE + + +
!     This subroutine predicts on an hourly basis soil water profile,
!     soil water content at the soil-air interface, potential and
!     actual soil evaporation, runoff, ponding and deep percolation.

!     + + + KEYWORDS + + +
!     soil water redistribution, evaporation, runoff, deep percolation

      use weps_main_mod, only: am0ifl
      use soillay_mod, only: distriblay, intersect
      use file_io_mod, only: luowater
      use datetime_mod, only: get_simdate_doy, get_simdate_year
      use p1unconv_mod, only: pi, hrtosec, mtomm, mmtom
      use hydro_data_struct_defs, only: am0hfl, gravconst
      use air_water_mod, only: vaporden
      use hydro_data_struct_defs, only: claygrav80rh, orggrav80rh
      use hydro_util_mod, only: matricpot_bc, volwatadsorb, depstore

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: isr   ! subregion number
      integer daysim, numeq
      real bszlyt(*), bulkden(*), bszlyd(*), theta(0:*)
      real thetadmx(*), bthetas(*), bthetaf(*), bthetar(*), bthetaw(*)
      real bhrsk(*), bheaep(*), bh0cb(*), bsfcla(*), bsfom(*), bhtsav(*)
      real bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt
      real rise, daylength, bhzep, dprecip, bwdurpt, bwpeaktpt
      real dirrig, bhdurirr, bhlocirr, bhzoutflow
      real bbdstm, bbffcv, bslrro, bslrr, bmzele, bhrwc0(*)
      real bhzea, bhzper, bhzrun, bhzinf, bhzwid
      real bhzeasurf, evaplimit, vaptrans, bmrslp

! intent(in)
! daysim, numeq, bszlyt, bszlyd, bulkden,
! bthetas, bthetaf, bthetar, bhrsk,
! bheaep, bh0cb, bsfcla, bsfom, bhtsav,
! bwtdmxprev, bwtdmn, bwtdmx, bwtdmnnext, bwtdpt,
! rise, daylength, bhzep,
! dprecip, bwdurpt, bwpeaktpt,
! dirrig, bhdurirr, bhlocirr,
! bbdstm, bbffcv, bslrro, bslrr, bmzele, 
! bhzeasurf, evaplimit, vaptrans

! intent(inout)
! theta, thetadmx, bhrwc0,
! bhzea, bhzper, bhzrun, bhzinf, bhzwid

!     + + + ARGUMENT DEFINITIONS + + +
!     daysim     - day of the simulation (very useful for debugging, not necessary otherwise)
!     numeq      - number of equations to be solved = layrsn + 6
!     bszlyt(*)  - thickness of the soil layer (mm)
!     bszlyd(*)  - depth to bottom of the soil layer (mm)
!     theta(*)   - volumetric water content (m^3/m^3)
!     thetadmx(*)- daily maximum volumetric water content (m^3/m^3)
!     bulkden(*) - soil bulk density Mg/m^3)
!     bthetas(*) - saturated volumetric water content (m^3/m^3)
!     bthetaf(*) - field capacity volumetric water content (m^3/m^3)
!     bthetar(*) - residual (conductivity) volumetric water content (m^3/m^3)
!     bhrsk(*)   - saturated hydraulic conductivity (m/s)
!     bheaep(*)  - air entry potential (J/kg)
!     bh0cb(*)   - exponent of Campbell soil water release curve (unitless)
!     bsfcla(*)  - fraction of soil mineral content which is clay (unitless)
!     bsfom(*)   - fraction of total soil which is organic (unitless)
!     bhtsav(*)  - daily average soil temperature (C)
!     bwtdmxprev - maximum air temperature of previous day (C)
!     bwtdmn     - minimum air temperature (C)
!     bwtdmx     - maximum air temperature (C)
!     bwtdmnnext - minimum air temperature of next day (C)
!     bwtdpt     - dew point temperature (C)
!     rise       - time of the sunrise (hour of day)
!     daylength  - time of daylight (hours)
!     bhzep      - potential soil evaporation (mm/day)
!     dprecip     - rainfall depth after snow filter (mm)
!     bwdurpt    - duration of precipitation (hours)
!     bwpeaktpt  - normalized time to peak of precipitation (time to peak/duration)
!     dirrig   - Daily irrigation (mm)
!     bhdurirr - duration of irrigation water application (hours)
!     bhlocirr - emitter location point (m)
!                positive is above the soil surface
!                negative is below the soil surface
!     bhzoutflow - height of runoff outlet above field surface (m)
!     bbdstm     - total number of stems (#/m^2)
!     bslrro     - original random roughness height, after tillage, mm
!     bslrr      - Allmaras random roughness parameter (mm) 
!     bmzele     - Average site elevation (m)
!     bhrwc0(*)  - Hourly values of surface soil water content (not as in soil)
!     bhzea      - accumulated daily evaporation (mm) (comes in with a value set from snow evap)
!     bhzper     - accumulated daily drainage (deep percolation) (mm)
!     bhzrun     - accumulated daily runoff (mm)
!     bhzinf     - depth of water infiltrated (mm)
!     bhzwid     - Water infiltration depth into soil profile (mm)
!     bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     evaplimit - accumulated surface evaporation since last complete rewetting
!                 defining limit of stage 1 (energy limited) and start of 
!                 stage 2 (soil vapor transmissivity limited) evaporation (mm)
!     vaptrans  - vapor transmissivity (mm/d^.5)
!     bmrslp   - Average slope of subregion (mm/mm)

!     + + + LOCAL VARIABLES + + +
      integer    itol, itask, jt, iopt, neq(2)
      real       tday,tout,relerr(1),abserr(numeq)
      real       volw(numeq)
      integer    kindex, hourstep, yr
!      real       swc
      real       swci, ref_ranrough, ranrough
!      integer    lstep,lfunc,ljac
      real       temp, netflux(numeq)
      real       evap, runoff, drain, infil
      integer    iminlay, imaxlay
      real       evapratio, surf_cum
      integer    idoy
      real       laycenter, irrigstart, irrigmid

      real       delta_drip
      parameter  (delta_drip = 0.005) ! use to avoid flooding a thin 
                                      ! soil layer with drip irrigation 
!      real       temp_sat

!     + + + LOCAL DEFINITIONS + + +
!     itol     - lsoda: setting to make relerr and/or abserr scalar or array
!     itask    - lsoda: stepping mode
!     jt       - lsoda: flag for selection of jacobian matrix
!     iopt     - lsoda: flag for use of extra inputs
!     neq(1)   - lsoda: number of equations to be solved
!     tday     - time of day (seconds)
!     tout     - time of day for end of integration step (seconds)
!     relerr(1) - lsoda: relative error value to control integration accuracy
!     abserr(numeq) - lsoda: absolute error value to control integration accuracy
!     volw(1-5) - accumulated values integrate in time
!                   (1) accumulated rainfall depth (total surface water application)(m)
!                   (2) runoff depth (m)
!                   (3) evaporation depth (m)
!                   (4) depth of water infiltrated (m)
!                   (5) ponded water depth (m)
!     volw(numeq) - (iminlay:imaxlay) volume of water in a soil layer (m)
!                   (imaxlay+1) depth of water drained (m)
!     kindex   - array index for loops
!     hourstep - step counter for 24 hourly steps
!     yr       - year of simulation
!     swc      - total depth of water in soil profile (mm)
!     swci     - total depth of water in soil profile a start of day (mm)
!     ref_ranrough - random roughness after last tillage (m)
!     ranrough - present random roughness (m)
!     lstep    - preserve number of steps from previous lsoda call
!     lfunc    - preserve number of function evaluations from previous lsoda call
!     ljac     - preserve number of jacobian evaluations from previous lsoda call
!     temp     - temporary value
!     prevevap - previous hour accumulated soil surface evaporation depth (mm)
!     evap     - accumulated soil surface evaporation depth (mm)
!     prevrunoff - previous hour accumulated soil surface runoff (mm)
!     runoff   - accumulated soil surface runoff (mm)
!     prevdrain - previous hour accumulated soil drainage (mm)
!     drain    - accumulated soil drainage (mm)
!     iminlay  - minimum index for placement of soil layers in volw array
!     imaxlay  - maximum index for placement of soil layers in volw array
!     evapratio - ratio reduction in evaporation rate due to soil dryness
!     surf_cum  - updated hourly cummulative surface evaporation (mm)
!     layercenter - depth to the center of a soil layer (mm) (for output)
!     irrigstart - time of start point of irrigation (surface or subsurface) event (seconds)
!     irrigmid  - time of midpoint of irrigation (surface or subsurface) event (seconds)

!     + + + SUBROUTINES CALLED + + +
!     slsoda - livermore solver for ordinary differential equations

!     + + + FUNCTION DECLARATIONS + + +
!      external   dvolw, jac
!      real   volwatadsorb
!      real   volwat_matpot_bc
!      real   atmpreselev, depstore, fricfact, store
!      real   calctht0
!      real   evapredu
!      integer dayear
!      real   availwc
!      real   unsatcond_bc
!      real   unsatcond_pot_bc
!      real   matricfluxpot_bc
!      real   intersect

!      real   diffusive, vaporden

!      real   evap_dissag

!     + + + DATA INITIALIZATIONS + + +

!     + + + OUTPUT FORMATS + + +
! 2000   format('*','Time of Sunrise: ',f10.2,/,'*','Length of Day: '    &
!     &,f10.2,/,'*','Time of Sunset: ',f10.2)
! 2009   format ('*',' Hourly HYDROLOGY output ')
! 2010   format('*','date: ',i2,'/',i2,'/',i4,'          Subregion # '   &
!     &         ,i4)
! 2020   format ('*',22x,3('*'),' hourly soil water data ',3('*'))
! 2030   format ('*',79('-')/'*','hr',t7,'evap',t13,'run',t17,'dper',t24,&
!     &  'swc',t28,'theta(0)',1x,11('-'),'water content of soil layers', &
!     &  10('-')/,'*',t7,'--------mm--------',2x,25('-'),                &
!     &  'm^3/m^3',24('-'))
! 2040    format('*',18x,f8.3,1x,f6.3,1x,10(1x,f6.3))
! 2050    format(1x,i2,2f6.3,f4.1,f8.3,1x,f6.3,1x,10(1x,f6.3))
! 2060    format('*',79('-'))
! 2065    format('*','average daily wetness',6x,f6.3,1x,10(1x,f6.3))
! 2070    format('*','ep=',f7.2,' mm',' (cumulative amount of daily',    &
!     &         ' potential soil evaporation)')
! 2080    format('*','ea=',f7.2,' mm',' (cumulative amount of daily',    &
!     &         ' actual soil evaporation)')
! 2090    format('*','bhzper=',f6.2,' mm',' (cumulative amount of daily',&
!     &         ' deep percolation)')
! 2100    format('darcy: lrx, wc, wct, wfluxn, deltim, wfluxr(top), wflu &
!     &xr(bottom)',/,i2,7f11.3,/,7f11.3)
 3000 format('# sec daysim doy yr var depth volw netflux theta fluxv flu&
     &xw swm cond numeq = ',i3)
 3010 format(1x,f8.1,1x,i5,1x,i3,1x,i4,1x,i3,8(1x,g11.4))

!     + + + END SPECIFICATIONS + + +

! ***      write(*,*) 'in darcy'

      if( (am0ifl .eqv. .true.) .and. ((am0hfl(isr) .eq. 2)             &
     & .or. (am0hfl(isr) .eq. 3) .or. (am0hfl(isr) .eq. 6)              &
     & .or. (am0hfl(isr) .eq. 7)) ) then
          ! write header information to hourly (or sub-hourly output file
         write(luowater(isr), 3000) numeq
      end if
!     initialize step reporting counters
!      lstep = 0
!      lfunc = 0
!      ljac = 0

!     bhzea not zeroed since snow evap must be included
      bhzper = 0.0
      bhzrun = 0.0
      bhzinf = 0.0

!     initialize LSODA options
      call xsetf(0)   !do not print internal error messages

!     initialize values for the DVOLWPARAM common block
!     values are place in volw array position, which is structured with
!     surface variables first, then the soil surface to depth and 
!     then drainage at depth.
      dvwp(isr)%layrsn = numeq - 6
      iminlay = 6
      imaxlay = dvwp(isr)%layrsn + 5

      dvwp(isr)%tlay(1) = bszlyt(1) *  mmtom
      dvwp(isr)%dist(1) = dvwp(isr)%tlay(1) / 2.
      dvwp(isr)%depth(1) = dvwp(isr)%dist(1)
      do kindex = 2, dvwp(isr)%layrsn
          dvwp(isr)%tlay(kindex) = bszlyt(kindex) *  mmtom
        dvwp(isr)%dist(kindex) = (dvwp(isr)%tlay(kindex) + dvwp(isr)%tlay(kindex-1))/2.
        dvwp(isr)%depth(kindex) = dvwp(isr)%depth(kindex-1) + dvwp(isr)%dist(kindex)
      end do

      do kindex = 1, dvwp(isr)%layrsn
          dvwp(isr)%thetas(kindex) = bthetas(kindex)
          dvwp(isr)%thetaw(kindex) = bthetaw(kindex)
          dvwp(isr)%thetar(kindex) = bthetar(kindex)
          dvwp(isr)%ksat(kindex) = bhrsk(kindex)
          dvwp(isr)%airentry(kindex) = bheaep(kindex) / gravconst
          dvwp(isr)%lambda(kindex) = 1.0 / bh0cb(kindex)
          temp = bulkden(kindex)*1000.0  !convert Mg/m^3 to kg/m^3
          dvwp(isr)%theta80rh(kindex) = volwatadsorb( temp,                       &
     &                        bsfcla(kindex), bsfom(kindex),            &
     &                        claygrav80rh, orggrav80rh )
!          dvwp(isr)%thetaw(kindex) = volwat_matpot_bc(potwilt, dvwp(isr)%thetar(kindex),    &
!     &               dvwp(isr)%thetas(kindex), dvwp(isr)%airentry(kindex), dvwp(isr)%lambda(kindex))
          dvwp(isr)%soiltemp(kindex) = bhtsav(kindex)
          call matricpot_bc(theta(kindex),dvwp(isr)%thetar(kindex),dvwp(isr)%thetas(kindex),&
     &                 dvwp(isr)%airentry(kindex), dvwp(isr)%lambda(kindex), dvwp(isr)%thetaw(kindex),&
     &                 dvwp(isr)%theta80rh(kindex), dvwp(isr)%soiltemp(kindex),             &
     &                 dvwp(isr)%potm(kindex), dvwp(isr)%soilrh(kindex) )
          dvwp(isr)%swm(kindex) = dvwp(isr)%potm(kindex) - dvwp(isr)%depth(kindex)
      end do

      dvwp(isr)%airtmaxprev = bwtdmxprev
      dvwp(isr)%airtmin = bwtdmn
      dvwp(isr)%airtmax = bwtdmx
      dvwp(isr)%airtminnext = bwtdmnnext
      dvwp(isr)%tdew = bwtdpt
      dvwp(isr)%lenday = daylength * hrtosec
      dvwp(isr)%sunrise = rise * hrtosec
      dvwp(isr)%sunset = dvwp(isr)%sunrise + dvwp(isr)%lenday

!     evaporation limiting parameters for dvolw (convert all to m)
      dvwp(isr)%evapendconstant = evaplimit * mmtom
      dvwp(isr)%evapdaypot = bhzep * mmtom
      dvwp(isr)%evaptrans = vaptrans * mmtom
      ! partition potential evaporation over entire day
      ! make 10% apply all day, the rest to the sine curve
      if( dvwp(isr)%lenday.gt.0.0 ) then
          dvwp(isr)%evapamp = pi * 0.9 * bhzep * mmtom / dvwp(isr)%lenday / 2.0
      else
          dvwp(isr)%evapamp = 0.0
      end if

      ! set passed rain parameters
      dvwp(isr)%raindepth = dprecip * mmtom  !storm total depth (m)
      dvwp(isr)%rainstart = 1.0
      if( (bwdurpt.lt.0.08333333).and.(dprecip.gt.0.001) ) then !0.0833333 = 5 minutes = 300sec
          !enforce minimum time so solver does not miss rainfall. The
          !minimum is really between 60 and 120 for the example run, but to make sure used 300
          !add sanity check for duration amount relationship from
          !Linsley,R.K., M.A. Kohler, J.L.H. Paulhus. 1982. Hydrology for Engineers. p80.
          dvwp(isr)%rainend = dvwp(isr)%rainstart                                           &
     &            + max(300.0,10.0**(log10(dprecip)/0.48-1.90231428))
          dvwp(isr)%rainmid = dvwp(isr)%rainstart                                           &
     &        + max(dvwp(isr)%rainstart,min(dvwp(isr)%rainend,bwpeaktpt*300.0))
      else
          dvwp(isr)%rainend = dvwp(isr)%rainstart + max(300.0, bwdurpt * hrtosec)
          dvwp(isr)%rainmid = dvwp(isr)%rainstart                                           &
     &        + max(dvwp(isr)%rainstart,min(dvwp(isr)%rainend,bwpeaktpt*bwdurpt*hrtosec))
      end if
      ref_ranrough = bslrro * mmtom
      ranrough = bslrr * mmtom
      dvwp(isr)%soilslope = bmrslp
      dvwp(isr)%pondmax = depstore( ranrough, dvwp(isr)%soilslope, bhzoutflow )!maximum ponding depth in meters
      dvwp(isr)%dw_friction = fricfact( ref_ranrough, ranrough, bbdstm, bbffcv )
      dvwp(isr)%slopelength = 50.0  !temporarily set since value is not supplied
      dvwp(isr)%atmpres = atmpreselev( bmzele )

      ! set passed irrigation parameters
      ! zero out all input parameters
      dvwp(isr)%surface_rate = 0.0
      dvwp(isr)%surface_start = 0.0
      dvwp(isr)%surface_end = 0.0
      do kindex=1,dvwp(isr)%layrsn
          dvwp(isr)%source_rate(kindex) = 0.0
          dvwp(isr)%source_start(kindex) = 0.0
          dvwp(isr)%source_end(kindex) = 0.0
      end do
      ! set irrigmid for later use
      irrigmid = 0.0
      ! reset parameters if irrigation is applied
      if( dirrig .gt. 0.0 ) then
          if( bhlocirr .ge. 0.0 ) then
              ! apply irrigation water as surface water
              dvwp(isr)%surface_rate = dirrig * mmtom / (bhdurirr * hrtosec)
              dvwp(isr)%surface_start = 0.0
              dvwp(isr)%surface_end = bhdurirr * hrtosec
              ! set time parameters for use in setting time step
              irrigstart = dvwp(isr)%surface_start
              irrigmid = (dvwp(isr)%surface_start + dvwp(isr)%surface_end) / 2.0
          else
              ! add within layer source term to layers
              ! uses a finite interval to avoid overloading a thin layer
              call distriblay( dvwp(isr)%layrsn, bszlyd, bszlyt, dvwp(isr)%source_rate,     &
     &                         dirrig * mmtom / (bhdurirr * hrtosec),   &
     &                         max(0.0,-bhlocirr-delta_drip),           &
     &                         -bhlocirr+delta_drip )
              temp = 0.0
              do kindex=1,dvwp(isr)%layrsn
                  if( intersect( temp, bszlyd(kindex),                  &
     &                          max(0.0,-bhlocirr-delta_drip),          &
     &                         -bhlocirr+delta_drip ) .gt. 0.0 ) then
                      dvwp(isr)%source_end(kindex) = max( dvwp(isr)%source_end(kindex),     &
     &                                          bhdurirr * hrtosec )
                      ! set time parameters for use in setting time step
                      irrigstart = dvwp(isr)%source_start(kindex)
                      irrigmid = (dvwp(isr)%source_start(kindex)                  &
     &                         + dvwp(isr)%source_end(kindex)) / 2.0
                  end if 
              end do
          end if
      end if

!     initialize state of soil
      do kindex=1,dvwp(isr)%layrsn
          if(theta(kindex).le.0.0) then
             write(0,*)                                                 &
     &        'Error: darcy:begin theta<0',kindex,theta(kindex),daysim
             call exit (1)
             !stop
          endif
          volw(kindex+5) = theta(kindex)*dvwp(isr)%tlay(kindex)
          dvwp(isr)%lastvolw(kindex+5) = -1.0e15
      end do
      do kindex=iminlay,imaxlay
          dvwp(isr)%prevvolw(kindex) = volw(kindex)
      end do
      bhzwid = 0.0

!     initialize auxiliary variables for integration
!      if( daysim .eq. 1 ) then   !commented out to initialized every day anew
          do kindex=1,5
              dvwp(isr)%prevvolw(kindex) = 0.0
          end do
          dvwp(isr)%prevvolw(numeq) = 0.0
!      endif
      ! start surface evap at the daily acumulated total
      dvwp(isr)%prevvolw(3) = bhzeasurf * mmtom
      do kindex=1,5
          volw(kindex) = dvwp(isr)%prevvolw(kindex)
      end do
      volw(numeq) = dvwp(isr)%prevvolw(numeq)

      neq(1) = numeq !can't declare parameter as array, but must be passed as an array
      neq(2) = 0 !this flag is used to activate printing internal to dvolw
!      if(daysim.eq.48) then
!          neq(2) = 1
!      else
!          neq(2) = 0 !this flag is used to activate printing internal to dvolw
!      endif
      itol = 2   !lsoda relerr is scalar and abserr is an array
!      itol = 1   !lsoda relerr is scalar and abserr is scalar
      relerr(1) = 1.0e-4  ! relative error tolerance
      do kindex=1,numeq  
          abserr(kindex) = 1.0e-6   ! absolute error tolerance
      end do
      itask = 1  !lsoda returns value at tout
!      itask = 2    !put in single step mode for more error reporting
!      iopt = 0   !lsoda no extra inputs
      iopt = 1   !lsoda using optional inputs on rwork and iwork(5-10)
      do kindex=5,10
          dvwp(isr)%rwork(kindex) = 0.0
          dvwp(isr)%iwork(kindex) = 0
      end do
      dvwp(isr)%rwork(6) = 7200  !maximum allowed step size (seconds)
!      jt = 2     !lsoda internally generated Jacobian matrix
      jt = 5     !lsoda internally generated banded Jacobian matrix
      dvwp(isr)%iwork(1) = 3  !ml, lower half band width of jacobian matrix
      dvwp(isr)%iwork(2) = 1  !mu, upper half band width of jacobian matrix
      dvwp(isr)%iwork(6) = 5000 !maximum number of steps allowed before error generated

!      if( daysim .eq. 1 ) then    !commented out to initialized every day anew
          dvwp(isr)%istate = 1
          dvwp(isr)%beginday = 0.0
          dvwp(isr)%t = dvwp(isr)%beginday
!      endif
      hourstep = 1

!  print out zero hour initialization values
      if ((am0hfl(isr) .eq. 2) .or. (am0hfl(isr) .eq. 3) .or.           &
     &    (am0hfl(isr) .eq. 6) .or. (am0hfl(isr) .eq. 7)) then
         yr = get_simdate_year()
         idoy = get_simdate_doy()
         if( idoy .eq. 1 ) then
             write(luowater(isr),*)
             write(luowater(isr),*)
         else
             ! print a blank line to separate layer blocks
             write(luowater(isr),*)
         end if
         ! output from differencing array for above ground phenomena
         call dvolw(isr, neq, dvwp(isr)%t, volw, netflux)
         do kindex=1,5
             write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, kindex,     &
     &        .0, volw(kindex), netflux(kindex), 0.0, 0.0, 0.0, 0.0, 0.0
         end do
         ! set surface water content value and output above thetat(1)
!         temp = evap_dissag(                                            &
!     &               dvwp(isr)%t, dvwp(isr)%sunrise, dvwp(isr)%sunset, dvwp(isr)%lenday, dvwp(isr)%evapamp, dvwp(isr)%evapdaypot)
!         evapratio = netflux(3)/temp
         evapratio = evapredu(bhzeasurf, evaplimit, vaptrans, bhzep)
         theta(0) = calctht0(bszlyd, theta, dvwp(isr)%thetaw, evapratio)      !H-64,65,66
         write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, 0,              &
     &         0.0, 0.0, evapratio, theta(0), 0.0, 0.0, 0.0, 0.0
         ! output from differencing array continued into soil layers
         do kindex=6,numeq-1
             laycenter = bszlyd(kindex-5)-0.5*bszlyt(kindex-5)
             write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, kindex,     &
     &           laycenter,volw(kindex),netflux(kindex),theta(kindex-5),&
     &           dvwp(isr)%fluxv(kindex-5), dvwp(isr)%fluxw(kindex-5),                      &
     &           dvwp(isr)%swm(kindex-5), dvwp(isr)%cond(kindex-5)
         end do
         ! output from differencing array for drainage value
         kindex = numeq
         write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, kindex,         &
     &       0.0, volw(kindex), netflux(kindex), 0.0, 0.0, 0.0, 0.0, 0.0

!         if (am0ifl .eqv. .true.) then
!           ! write out main soil properties
!           write(*,*)'darcyprop: thetas thetaf thetaw bulkden bh0cb ',  &
!     &             'bheaep bhrsk'
!         end if
!
!         write(*,*)'darcyprop: ', dvwp(isr)%thetas(1), bthetaf(1), dvwp(isr)%thetaw(1),     &
!     &                bulkden(1), bh0cb(1), bheaep(1), bhrsk(1)
!
!         if( daysim .eq. 63 ) then
!           ! write out the soil properties for surface layer
!          write(*,*)'darcygraph: theta suction cond condp diffu vapden   &
!     &fluxpot'
!           do kindex = 1, 100
!             call matricpot_bc(kindex*dvwp(isr)%thetas(1)/100,dvwp(isr)%thetar(1),dvwp(isr)%thetas(1),&
!     &                      dvwp(isr)%airentry(1), dvwp(isr)%lambda(1), dvwp(isr)%thetaw(1),          &
!     &                      dvwp(isr)%theta80rh(1), dvwp(isr)%soiltemp(1),                  &
!     &                      matricpot, dvwp(isr)%soilrh(1) )
!             dvwp(isr)%swm(1) = matricpot - dvwp(isr)%depth(1)
!             dvwp(isr)%soilvapden(1) = vaporden( dvwp(isr)%soiltemp(1), dvwp(isr)%soilrh(1) )
!             dvwp(isr)%soildiffu(1) = diffusive(kindex*dvwp(isr)%thetas(1)/100, dvwp(isr)%thetas(1),  &
!     &                               dvwp(isr)%soiltemp(1), dvwp(isr)%atmpres )
!             dvwp(isr)%cond(1) = unsatcond_bc(kindex*dvwp(isr)%thetas(1)/100,dvwp(isr)%thetar(1),     &
!     &                    dvwp(isr)%thetas(1), dvwp(isr)%ksat(1),dvwp(isr)%lambda(1))
!             unsatcond = unsatcond_pot_bc( matricpot, dvwp(isr)%ksat(1),          &
!     &                   dvwp(isr)%airentry(1), dvwp(isr)%lambda(1))
!             temp = matricfluxpot_bc( matricpot, dvwp(isr)%airentry(1), dvwp(isr)%ksat(1),  &
!     &              dvwp(isr)%lambda(1) )
!             write(*,*)'darcygraph: ', kindex*dvwp(isr)%thetas(1)/100, -matricpot, &
!     &                  dvwp(isr)%cond(1), unsatcond, dvwp(isr)%soildiffu(1), dvwp(isr)%soilvapden(1),&
!     &                  temp
!           end do
!        end if

      end if

!     start loop in time
30    continue

      ! set initial time step to make sure water applications are found
      tday = dvwp(isr)%t - dvwp(isr)%beginday
      tout = dvwp(isr)%beginday + hourstep*hrtosec
      ! rainfall event
      temp = (dvwp(isr)%rainstart + dvwp(isr)%rainend) / 2.0
      if(      (dvwp(isr)%raindepth .gt. 0.0)                                     &
     &   .and. (tday .le. dvwp(isr)%rainstart)                                    &
     &   .and. (tout .gt. dvwp(isr)%rainstart)                                    &
     &   .and. (tout .gt. temp) ) then
          ! initializes solution routines
          dvwp(isr)%istate = 1
          ! guarantees that integration will find event
          tout = min(temp, hourstep*hrtosec)
          ! reset multiday tracking values
          dvwp(isr)%beginday = 0.0
          dvwp(isr)%t = tday
!          lstep = 0    !step reporting counters
!          lfunc = 0
!          ljac = 0
!          itask = 2    !put in single step mode for more error reporting
      end if
      ! irrigation event
      temp = min( tout, irrigmid )
      if(      (dirrig .gt. 0.0)                                        &
     &   .and. (tday .le. irrigstart)                                   &
     &   .and. (tout .gt. irrigstart)                                   &
     &   .and. (tout .gt. temp) ) then
          ! initializes solution routines
          dvwp(isr)%istate = 1
          ! guarantees that integration will find event
          tout = min(temp, hourstep*hrtosec)
          ! reset multiday tracking values
          dvwp(isr)%beginday = 0.0
          dvwp(isr)%t = tday
!          lstep = 0    !step reporting counters
!          lfunc = 0
!          ljac = 0
!          itask = 2    !put in single step mode for more error reporting
      end if
!     settings for single step mode, retaining values on the hour
!      itask = 5
!      dvwp(isr)%rwork(1) = tout

40    continue
      call slsoda(isr, dvolw, neq, volw, dvwp(isr)%t, tout, itol, relerr, abserr, itask, &
     &     dvwp(isr)%istate, iopt, dvwp(isr)%rwork, dvwp(isr)%lrw, dvwp(isr)%iwork, dvwp(isr)%liw, jac, jt)

!      if( daysim.eq.6321 ) then
!          write(*,*) 'steps, functions, jacobians, order:',
!     &    dvwp(isr)%iwork(11)-lstep,
!     &    dvwp(isr)%iwork(12)-lfunc,dvwp(isr)%iwork(13)-ljac,dvwp(isr)%iwork(14)
!      end if
!      lstep = dvwp(isr)%iwork(11)
!      lfunc = dvwp(isr)%iwork(12)
!      ljac = dvwp(isr)%iwork(13)

!-------------------------------------------------------------
!  Was the step successful?  If not, quit with an explanation.
!-------------------------------------------------------------
      if(dvwp(isr)%istate .lt. 0) then
          if(dvwp(isr)%istate.eq.-1) then
              dvwp(isr)%istate=2
              write(*,*) 'day',daysim,'time',dvwp(isr)%t,'5k steps ',             &
     &                  'infil=',( volw(4) - dvwp(isr)%prevvolw(4) ) * mtomm,     &
     &                  'drain=',(volw(numeq)-dvwp(isr)%prevvolw(numeq))*mtomm
!              do kindex=1,dvwp(isr)%layrsn
!                  write(*,*) 'darcy:s,r,e,b',bthetas(kindex),
!     &            bthetar(kindex),bheaep(kindex),bh0cb(kindex)
!                  theta(kindex) = volw(kindex+5)/dvwp(isr)%tlay(kindex)
!              end do
!              write(*,*) 'darcy:theta',(theta(kindex),kindex=1,dvwp(isr)%layrsn)
!              call dvolw(isr, neq,t,volw,netflux)
!              write(*,*) 'darcy:netflux',
!     &                  (netflux(kindex+5),kindex=1,dvwp(isr)%layrsn)
          else
             write(0,*)                                                 &
     &       "Error: Failed day:",daysim," time:",dvwp(isr)%t," istate:",dvwp(isr)%istate
              call exit(1)
          end if
          goto 40
      end if

!-------------------------------------
!     step was sucessful, continue on
!-------------------------------------
      if( (am0hfl(isr) .eq. 2) .or. (am0hfl(isr) .eq. 3)                &
     &  .or. (am0hfl(isr) .eq. 6) .or. (am0hfl(isr) .eq. 7) ) then
         ! blank line to separate each layer block
         write(luowater(isr),*)
         ! other values
         do kindex=1,dvwp(isr)%layrsn
           theta(kindex) = volw(kindex+5)/dvwp(isr)%tlay(kindex)
         end do
         ! output from differencing array for above ground phenomena
         call dvolw(isr, neq, dvwp(isr)%t, volw, netflux)
         do kindex=1,5
             write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, kindex,     &
     &       0.0, volw(kindex), netflux(kindex), 0.0, 0.0, 0.0, 0.0, 0.0
         end do
         ! set surface water content value and output above thetat(1)
         surf_cum = bhzeasurf + bhzea + (volw(3) - dvwp(isr)%prevvolw(3)) * mtomm
!         temp = evap_dissag(                                            &
!     &               dvwp(isr)%t, dvwp(isr)%sunrise, dvwp(isr)%sunset, dvwp(isr)%lenday, dvwp(isr)%evapamp, dvwp(isr)%evapdaypot)
!         evapratio = netflux(3)/temp
         evapratio = evapredu( surf_cum, evaplimit, vaptrans, bhzep )
         theta(0) = calctht0(bszlyd, theta, dvwp(isr)%thetaw, evapratio)      !H-64,65,66
         write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, 0,              &
     &         0.0, 0.0, evapratio, theta(0), 0.0, 0.0, 0.0, 0.0
         ! output from differencing array continued into soil layers
         do kindex=6,numeq-1
             laycenter = bszlyd(kindex-5) - 0.5*bszlyt(kindex-5)
             write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, kindex,     &
     &         laycenter, volw(kindex), netflux(kindex),theta(kindex-5),&
     &         dvwp(isr)%fluxv(kindex-5), dvwp(isr)%fluxw(kindex-5),                        &
     &         dvwp(isr)%swm(kindex-5), dvwp(isr)%cond(kindex-5)
         end do
         ! output from differencing array for drainage value
         kindex = numeq
         write(luowater(isr),3010) dvwp(isr)%t, daysim, idoy, yr, kindex,         &
     &     0.0, volw(kindex), netflux(kindex), 0.0, 0.0, 0.0, 0.0, 0.0
      end if

      if( dvwp(isr)%t.lt.(hourstep*hrtosec)) goto 30

!-------------------------------------
! completed the hour, sum up
!-------------------------------------
      do kindex=1,dvwp(isr)%layrsn
          theta(kindex) = volw(kindex+5)/dvwp(isr)%tlay(kindex)
          thetadmx(kindex) = max( thetadmx(kindex),theta(kindex) )
      end do

      swci = sum(volw(6:dvwp(isr)%layrsn+5)) * mtomm

!     create hourly and daily output values
      runoff = ( volw(2) - dvwp(isr)%prevvolw(2) ) * mtomm
      bhzrun = bhzrun + runoff
      evap = ( volw(3) - dvwp(isr)%prevvolw(3) ) * mtomm
      bhzea = bhzea + evap
      infil = ( volw(4) - dvwp(isr)%prevvolw(4) ) * mtomm
      bhzinf = bhzinf + infil
      drain = ( volw(numeq) - dvwp(isr)%prevvolw(numeq) ) * mtomm
      bhzper = bhzper + drain

!     evaporation ratio based on accumulation for this hour
      surf_cum = bhzeasurf + bhzea
      evapratio = evapredu( surf_cum, evaplimit, vaptrans, bhzep )

      ! evaporation ratio based on flux ratio
!      call dvolw(isr, neq, dvwp(isr)%t, volw, netflux)
!      temp = evap_dissag(                                               &
!     &               dvwp(isr)%t, dvwp(isr)%sunrise, dvwp(isr)%sunset, dvwp(isr)%lenday, dvwp(isr)%evapamp, dvwp(isr)%evapdaypot)
!      evapratio = netflux(3)/temp

      !theta(0) = theta(1)
      theta(0) = calctht0(bszlyd, theta, dvwp(isr)%thetaw, evapratio)      !H-64,65,66

      bhrwc0(hourstep) = theta(0)/bulkden(1)

!     update prevvolw to carry over to next hour or day
      do kindex=1,5
          dvwp(isr)%prevvolw(kindex) = volw(kindex)
      end do
      dvwp(isr)%prevvolw(numeq) = volw(numeq)

!    output the hourly info here
!      if( (am0hfl(isr) .eq. 2) .or. (am0hfl(isr) .eq. 3)                &
!     &   .or. (am0hfl(isr) .eq. 6) .or. (am0hfl(isr) .eq. 7) ) then
!          swc = sum(volw(iminlay:imaxlay)) * mtomm
!          write(12,2050) hourstep,evap, runoff, drain,                  &
!     &       swc,theta(0),(theta(kindex), kindex=1,dvwp(isr)%layrsn)
!      end if

!    Accumulating hourly soil wetness values
      do  kindex=0,dvwp(isr)%layrsn
          if( (theta(kindex).le.0.001) .and. (kindex.gt.0) ) then
              write(*,*)'darcy:end theta<0.001',kindex,theta(kindex)
              call dvolw(isr, neq,dvwp(isr)%t,volw,netflux)
              write(*,*) 'lay',kindex,':',dvwp(isr)%t,netflux(kindex+5),          &
     &           netflux(kindex+6), netflux(3)
              write(*,*) 'tcur,hu:',dvwp(isr)%rwork(13), dvwp(isr)%rwork(11)
!              stop
          endif
      end do

      if(      (dvwp(isr)%raindepth .gt. 0.0)                                     &
     &    .or. ((dirrig .gt. 0.0) .and. (bhlocirr .ge. 0.0)) ) then
        if( dvwp(isr)%raindepth .gt. 0.0 ) then
          if(     (tout .ge. max(dvwp(isr)%rainend, dvwp(isr)%surface_end))                 &
     &      .and. (bhzwid .le. 0.0) ) then
            bhzwid = store( iminlay, imaxlay, dvwp(isr)%prevvolw, volw, bszlyd )
          end if
        else
          if(     (tout .ge. dvwp(isr)%surface_end)                               &
     &      .and. (bhzwid .le. 0.0) ) then
            bhzwid = store( iminlay, imaxlay, dvwp(isr)%prevvolw, volw, bszlyd )
          end if
        end if
      end if

      hourstep = hourstep + 1
!-------------------------------------
!  If not done yet, take another step.
!-------------------------------------      
      if(hourstep.le.24) goto 30

! completed the day

!     this section should be enabled to extend solution over
!     multiple days when no outside process changes water contents
!      if( dvwp(isr)%beginday .lt. 100*SEC_PER_DAY )  then
!          dvwp(isr)%beginday = dvwp(isr)%beginday + SEC_PER_DAY
!      else
!          dvwp(isr)%istate = 1   !-- initializes solution routines
!          dvwp(isr)%beginday = 0.0
!          dvwp(isr)%t = dvwp(isr)%beginday
!          do kindex=1,5
!              dvwp(isr)%prevvolw(kindex) = 0.0
!          end do
!          dvwp(isr)%prevvolw(numeq) = 0.0
!      end if

      return
    end subroutine darcy

    real function atmpreselev( elevation )

!     returns the standard atmospheric pressure adjusted for elevation (kPa)
!     Approximation from Cuenca (1989) page 141

      use hydro_data_struct_defs, only: atmstand, gravconst, rair, templapse, tempstand
!*** Argument declarations ***
      real elevation
!     elevation - the elevation of the site above mean standard sea level (m)

      atmpreselev = atmstand                                            &
     &            * ((tempstand - templapse*elevation)/tempstand)       &
     &            ** (gravconst/(templapse*rair))

      return
    end function atmpreselev

    subroutine dvolw(isr, neqn,tsec,volw,wfluxn)

!     + + + PURPOSE + + +
!     Returns the values of net flux when given a simulation time and
!     state of the system. It is used by LSODA to integrate forward in time

!     + + + KEYWORDS + + +
!     soil water redistribution, evaporation, deep percolation, runoff

      use weps_main_mod, only: layer_weighting
      use air_water_mod, only: satvappres, vaporden
      use hydro_util_mod, only: matricpot_bc, matricpot_from_rh, unsatcond_bc
      use hydro_data_struct_defs, only: denwat, gravconst

!     + + + ARGUMENT DECLARATIONS + + +
      integer :: isr  ! subregion index
      integer neqn(*)
      real tsec, volw(*), wfluxn(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     neqn(1) - number of Ordinary Differential Equations to be integrated
!     neqn(2) - flag to activate internal printing
!     tsec    - time in seconds since lsoda was initialized
!     volw(*) - array of state variables
!               (1-5) - rain, runoff, evap, infil, pond (m)
!               (6-imaxlay) - volume of water in each soil layer (m)
!               (imaxlay+1) - drainage (m)
!     wfluxn(*) - net flux rates corresponding to volw(*) (m/s)

!*** FUNCTION DECLARATIONS ***
!      real unsatcond_bc
!      real unsatcond_pot_bc
!      real internode_wtcond_bc
!      real internode_cond_bc
!      real internode_wt_bc
!      real vaporden, diffusive
!      real airtempsin, satvappres
!      real evapredu, matricpot_from_rh
!      real evap_dissag

!*** LOCAL DECLARATIONS ***
      integer     lrx, imaxlay
      real theta(dvwp(isr)%layrsn), wfluxr(dvwp(isr)%layrsn+1)
      real conda(dvwp(isr)%layrsn), cond_wt
      real intenspeak, diffua(dvwp(isr)%layrsn)
      real airtemp, airvappres, airsatvappres, airhumid, airvapden
      real rainduration, tday, max_infil_rate, frac_pond_area
      real max_evap_rate, soil_evap_rate, pond_evap_rate
      real surface_vapor_rate, surface_capil_rate, air_mat_pot
      real pond_infil, rain_infil, rain_pond
      real lay_source(dvwp(isr)%layrsn)
      real t_potm, t_ksat, t_airentry, t_lambda

!*** LOCAL DEFINITIONS ***
!     locally, the soil layers go from 1 to layrsn. In the passed arrays,
!     the soil layers go from 6 to layrsn+5

!     set time of the beginning of the day
      tday = tsec - dvwp(isr)%beginday

!     soil state
      imaxlay = dvwp(isr)%layrsn + 5
      do lrx = 1,dvwp(isr)%layrsn
          theta(lrx) = volw(lrx+5)/dvwp(isr)%tlay(lrx)
          if( volw(lrx+5).ne.dvwp(isr)%lastvolw(lrx+5) ) then
!             Brooks and Corey soil properties
              call matricpot_bc(theta(lrx), dvwp(isr)%thetar(lrx), dvwp(isr)%thetas(lrx),   &
     &                      dvwp(isr)%airentry(lrx), dvwp(isr)%lambda(lrx), dvwp(isr)%thetaw(lrx),    &
     &                      dvwp(isr)%theta80rh(lrx), dvwp(isr)%soiltemp(lrx),              &
     &                      dvwp(isr)%potm(lrx), dvwp(isr)%soilrh(lrx) )
              dvwp(isr)%swm(lrx) = dvwp(isr)%potm(lrx) - dvwp(isr)%depth(lrx)
              dvwp(isr)%soilvapden(lrx) = vaporden( dvwp(isr)%soiltemp(lrx), dvwp(isr)%soilrh(lrx) )
              dvwp(isr)%soildiffu(lrx) = diffusive(theta(lrx), dvwp(isr)%thetas(lrx),       &
     &                               dvwp(isr)%soiltemp(lrx), dvwp(isr)%atmpres )
              dvwp(isr)%cond(lrx) = unsatcond_bc(theta(lrx),dvwp(isr)%thetar(lrx),          &
     &                    dvwp(isr)%thetas(lrx), dvwp(isr)%ksat(lrx),dvwp(isr)%lambda(lrx))
          end if
      end do

!     calc the conductivity and rate of flow in for each layer
      do lrx = 2,dvwp(isr)%layrsn
       if (layer_weighting == 5) then
         ! geometric mean
         ! conda(lrx) = (dvwp(isr)%cond(lrx-1)*dvwp(isr)%cond(lrx))**0.5
         t_potm = -(dvwp(isr)%potm(lrx-1)*dvwp(isr)%potm(lrx))**0.5
         t_ksat = (dvwp(isr)%ksat(lrx-1)*dvwp(isr)%ksat(lrx))**0.5
         t_airentry = -(dvwp(isr)%airentry(lrx-1)*dvwp(isr)%airentry(lrx))**0.5
         t_lambda = (dvwp(isr)%lambda(lrx-1)*dvwp(isr)%lambda(lrx))**0.5
         conda(lrx) = unsatcond_pot_bc(t_potm, t_ksat, t_airentry,      &
     &                                 t_lambda)
       else if (layer_weighting == 4) then
         conda(lrx) = internode_wtcond_bc( theta(lrx-1), theta(lrx),    &
     &          dvwp(isr)%thetar(lrx-1), dvwp(isr)%thetar(lrx), dvwp(isr)%thetas(lrx-1), dvwp(isr)%thetas(lrx), &
     &          dvwp(isr)%thetaw(lrx-1), dvwp(isr)%thetaw(lrx), dvwp(isr)%theta80rh(lrx-1),           &
     &          dvwp(isr)%theta80rh(lrx), dvwp(isr)%soiltemp(lrx-1), dvwp(isr)%soiltemp(lrx),         &
     &          dvwp(isr)%ksat(lrx-1), dvwp(isr)%ksat(lrx), dvwp(isr)%lambda(lrx-1), dvwp(isr)%lambda(lrx),     &
     &          dvwp(isr)%tlay(lrx-1), dvwp(isr)%tlay(lrx), dvwp(isr)%airentry(lrx-1), dvwp(isr)%airentry(lrx) )
       else if (layer_weighting == 3) then
         conda(lrx) = internode_cond_bc( dvwp(isr)%potm(lrx-1), dvwp(isr)%potm(lrx),        &
     &          dvwp(isr)%ksat(lrx-1), dvwp(isr)%ksat(lrx), dvwp(isr)%lambda(lrx-1), dvwp(isr)%lambda(lrx),     &
     &          dvwp(isr)%tlay(lrx-1), dvwp(isr)%tlay(lrx), dvwp(isr)%airentry(lrx-1), dvwp(isr)%airentry(lrx) )
       else if (layer_weighting == 2) then
          ! internodal conductivity, darcian means
          cond_wt = internode_wt_bc( dvwp(isr)%cond(lrx-1), dvwp(isr)%cond(lrx),            &
     &        dvwp(isr)%ksat(lrx-1), dvwp(isr)%ksat(lrx), dvwp(isr)%lambda(lrx-1), dvwp(isr)%lambda(lrx),       &
     &        dvwp(isr)%tlay(lrx-1), dvwp(isr)%tlay(lrx), dvwp(isr)%airentry(lrx-1), dvwp(isr)%airentry(lrx) )
          conda(lrx) = cond_wt*dvwp(isr)%cond(lrx-1) + (1.0-cond_wt)*dvwp(isr)%cond(lrx)
       else if (layer_weighting == 1) then
          ! proportional layer thickness weighted internodal conductivity
          conda(lrx) = (dvwp(isr)%cond(lrx-1)*dvwp(isr)%tlay(lrx-1)+dvwp(isr)%cond(lrx)*dvwp(isr)%tlay(lrx))    &
     &               / (2*dvwp(isr)%dist(lrx))
       else ! if (layer_weighting == 0) then !Currently the default
          ! unweighted arithmetic average internodal conductivity
          conda(lrx) = 0.5 * ( dvwp(isr)%cond(lrx-1) + dvwp(isr)%cond(lrx) )
       end if
          diffua(lrx) = (dvwp(isr)%soildiffu(lrx-1)*dvwp(isr)%tlay(lrx-1)                   &
     &                + dvwp(isr)%soildiffu(lrx)*dvwp(isr)%tlay(lrx)) / (2*dvwp(isr)%dist(lrx))
          dvwp(isr)%fluxw(lrx) = (dvwp(isr)%swm(lrx-1)-dvwp(isr)%swm(lrx)) * conda(lrx)               &
     &                / dvwp(isr)%dist(lrx)
          dvwp(isr)%fluxv(lrx) = ((dvwp(isr)%soilvapden(lrx-1)-dvwp(isr)%soilvapden(lrx))             &
     &              * diffua(lrx) / denwat) / dvwp(isr)%dist(lrx)
          wfluxr(lrx) = dvwp(isr)%fluxw(lrx) + dvwp(isr)%fluxv(lrx)
      end do
      wfluxr(dvwp(isr)%layrsn+1) = dvwp(isr)%cond(dvwp(isr)%layrsn)
!      wfluxr(dvwp(isr)%layrsn+1) = 0.0

!     boundary fluxes
!     rainfall intensity = wfluxn(1)
!     runoff rate = wfluxn(2)
!     evaporation rate = wfluxn(3)
!     infiltration rate = wfluxn(4)
!     change in ponded depth = wfluxn(5)
!     drainage rate = wfluxn(imaxlay+1)

!     rainfall intensity
      if(     (tday.ge.dvwp(isr)%rainstart)                                       &
     &   .and.(tday.le.dvwp(isr)%rainend)                                         &
     &   .and.(dvwp(isr)%raindepth.gt.0.0) ) then
          rainduration = dvwp(isr)%rainend - dvwp(isr)%rainstart
          intenspeak = 2.0*dvwp(isr)%raindepth/rainduration
          if( tday.lt.dvwp(isr)%rainmid ) then
              wfluxn(1) = intenspeak                                    &
     &                  * (tday-dvwp(isr)%rainstart)/(dvwp(isr)%rainmid-dvwp(isr)%rainstart)
          else if( tday.gt.dvwp(isr)%rainmid ) then
              wfluxn(1) = intenspeak                                    &
     &                         * (tday-dvwp(isr)%rainend)/(dvwp(isr)%rainmid-dvwp(isr)%rainend)
          else
              wfluxn(1) = intenspeak
          end if
      else
          wfluxn(1) = 0.0
      end if

      ! surface irrigation intensity
      if(      (tday .ge. dvwp(isr)%surface_start)                                &
     &   .and. (tday .le. dvwp(isr)%surface_end)                                  &
     &   .and. (dvwp(isr)%surface_rate .gt. 0.0) ) then
          wfluxn(1) = wfluxn(1) + dvwp(isr)%surface_rate
      end if

      dvwp(isr)%pondmax = max(0.001, dvwp(isr)%pondmax) !avoid div by zero
!     generate runoff if above retention limit (kind of like WEPP)
      if(volw(5).ge.dvwp(isr)%pondmax) then
          wfluxn(2) = ((volw(5)-dvwp(isr)%pondmax)**1.5)                          &
     &              * (8.0*gravconst*dvwp(isr)%soilslope/dvwp(isr)%dw_friction)**0.5        &
     &              /  dvwp(isr)%slopelength
      else
          wfluxn(2) = 0.0   ! no runoff
      end if

!     determine fraction of soil area covered by ponding
      if( volw(5).ge.0.0 ) then
          frac_pond_area = min(1.0,(volw(5)/dvwp(isr)%pondmax)**0.5)
      else
          frac_pond_area = 0.0
      end if

!     calculate maximum evaporation rate
!     this method using a lenday that is 12 hours (43200 sec) allows
!     running time continuously through many days
!      wfluxn(3) = max(0.0,dvwp(isr)%evapamp * sin(pi*(tday-dvwp(isr)%sunrise)/dvwp(isr)%lenday))
!     this method assumes that tday and sunrise and sunset are in the
!     same day and lenday can be actual daylight hours
      ! evapamp accounts for 90% of daily evaporation
      ! 10% remaining is distributed over the remainder of the day
      max_evap_rate = evap_dissag(                                      &
     &               tday, dvwp(isr)%sunrise, dvwp(isr)%sunset, dvwp(isr)%lenday, dvwp(isr)%evapamp, dvwp(isr)%evapdaypot)

!     find air relative humidity from diurnal air temperature
!     and dewpoint temperature
      if( tday .lt. 21600 ) then
          airtemp = airtempsin( tday, dvwp(isr)%airtmaxprev, dvwp(isr)%airtmin )
      else if( tday .lt. 64800 ) then
          airtemp = airtempsin( tday, dvwp(isr)%airtmax, dvwp(isr)%airtmin )
      else
          airtemp = airtempsin( tday, dvwp(isr)%airtmax, dvwp(isr)%airtminnext )
      end if

      airvappres = satvappres(dvwp(isr)%tdew)
      airsatvappres = satvappres(airtemp)
      airhumid = airvappres/airsatvappres
      airvapden = vaporden( airtemp, airhumid )

!      if( (dvwp(isr)%soilrh(1).ge.airhumid).and.(airhumid.le.0.99) ) then
!      if( (airhumid.le.0.99) ) then
          ! calculate reduction in evaporation rate due to dry soil
!          soil_evap_rate = max_evap_rate                                &
!     &                   * (dvwp(isr)%soilrh(1)-airhumid)/(1.0-airhumid)
!      else
!          soil_evap_rate = 0.0
!      end if

      surface_vapor_rate = ((dvwp(isr)%soilvapden(1)-airvapden)                   &
     &                 * dvwp(isr)%soildiffu(1) / denwat) / dvwp(isr)%dist(1)

      air_mat_pot = matricpot_from_rh( airhumid, airtemp )
      surface_capil_rate = (dvwp(isr)%swm(1) - air_mat_pot) *0.5*dvwp(isr)%cond(1) / dvwp(isr)%dist(1)

      soil_evap_rate = surface_vapor_rate + surface_capil_rate
      soil_evap_rate = max( -max_evap_rate, soil_evap_rate )
      soil_evap_rate = min( max_evap_rate, soil_evap_rate )

!      fluxw(1)
!      fluxv(1)
!      matricpot_from_rh( airhumid, airtemp )
!*** try this
!      soil_evap_rate = min( max_evap_rate,                              &
!     &                 ( (dvwp(isr)%soilvapden(1)-airvapden)                      &
!     &                 * dvwp(isr)%soildiffu(1) / denwat) / dvwp(isr)%dist(1))
!
!      if( neqn(2).gt.0 ) then
!          write(*,*)'dvolw:tcddf',
!     &    tday, airtemp, dvwp(isr)%soilvapden(1), airvapden, wfluxn(3)
!      endif
!      if( neqn(2).gt.0 ) then
!          if(max_evap_rate.gt.0.0)
!     &    write(*,*)'dvolw:evap',dvwp(isr)%soilrh(1),airhumid, max_evap_rate
!      endif
!*** end try this

!     split evaporation between soil and pond
!     fluxv(1) is negative since this is a loss to the soil layer
!     consequently it must be subtracted to make it an addition to evap.
      dvwp(isr)%fluxv(1) = -soil_evap_rate * (1.0-frac_pond_area)
      pond_evap_rate = max_evap_rate * frac_pond_area
      wfluxn(3) = -dvwp(isr)%fluxv(1) + pond_evap_rate

!     calculate max infiltration rate !note, the potential difference
!     should be (0.0-dvwp(isr)%swm(1)), but this only works if we account for
!     hysteresis. As it is written, infiltration will not exceed
!     saturation. When hysteresis is incorporated, sorbing soil will
!     have an airentry potential of 0.0 again making them match.
      max_infil_rate = (dvwp(isr)%airentry(1) - dvwp(isr)%swm(1))                           &
     &               * 0.5 * (dvwp(isr)%ksat(1)+dvwp(isr)%cond(1))/dvwp(isr)%dist(1)

!     add infiltration from both ponded area and rainfall area
      pond_infil = max_infil_rate * frac_pond_area
      rain_infil = min(wfluxn(1),max_infil_rate) * (1.0-frac_pond_area)
      wfluxn(4) = pond_infil + rain_infil
      dvwp(isr)%fluxw(1) = wfluxn(4)

!     rate rain is added to the pond
      rain_pond = wfluxn(1) - rain_infil

!     rate change in pond depth (rain - infil - evap - runoff)
      wfluxn(5) = rain_pond - pond_infil - pond_evap_rate - wfluxn(2)

!     resultant flux at surface layer
      wfluxr(1) = dvwp(isr)%fluxw(1) + dvwp(isr)%fluxv(1)

!     drainage
      wfluxn(imaxlay+1) = wfluxr(dvwp(isr)%layrsn+1)

!     calc the net flow into (+) or out of (-) each layer
      do lrx = 1, dvwp(isr)%layrsn
          ! turn within layer source terms on with time
          if(   (tday.ge.dvwp(isr)%source_start(lrx))                             &
     &    .and. (tday.le.dvwp(isr)%source_end(lrx))) then
              lay_source(lrx) = dvwp(isr)%source_rate(lrx)
          else
              lay_source(lrx) = 0.0
          end if
          ! find net flow
          wfluxn(lrx+5) = wfluxr(lrx) - wfluxr(lrx+1) + lay_source(lrx)
!          write(*,*) 'lay ', lrx, wfluxr(lrx), wfluxn(lrx)
          dvwp(isr)%lastvolw(lrx+5) = volw(lrx+5)
      end do

!      if( neqn(2).gt.0 ) then
!          if( (wfluxr(1).lt.0.0).or.(wfluxr(2).gt.0.0) )
!     &    write(*,*) 'flx 1:',wfluxr(1), wfluxr(2)
!      endif

      return
    end subroutine dvolw

!     dummy jacobian matrix
    subroutine jac (neq, t, y, ml, mu, pd, nrowpd)
      integer neq, ml, mu, nrowpd
      real t, y, pd
      dimension y(*), pd(nrowpd,*)
!     the full matrix case (jt = 1), ml and mu are ignored,
!     and the jacobian is to be loaded into pd in columnwise manner,
!     with df(i)/dy(j) loaded into pd(i,j).
!     in this case, dwfluxn(i)/dvolw(j) in pd(i,j)
      return
    end subroutine jac

! table lookup function archived here for reference
!      real function afgen(numpts, table, xval)
!     Finds two entries in table( ,1) which bracket the value of the independent
!     variable, xval, and then linearly interpolates between the two corresponding
!     entries in Table( ,2) to find the corresponding value of the dependent
!     variable.
!
!      integer   numpts
!      real    table(numpts,2), xval
!
!      integer maxindex, minindex, midindex
!
!      maxindex = numpts
!      minindex = 0  !Place limit on one end of output.
!	  Set limits on values the function may take.
!	  if( xval.lt.table(1,1)) then
!          afgen = table(1,2)
!      else if( xval.gt.table(numpts,1)) then
!          afgen = table(numpts,2)
!      else
!          do while((maxindex-minindex).gt.1)
!              midindex = (maxindex+minindex)/2
!              if((table(numpts,1).gt.table(1,1))
!     &            .eqv.(xval.gt.table(midindex,1))) then
!                  minindex = midindex
!              else
!                  maxindex = midindex
!              end if
!          end do
!          afgen = (xval-table(minindex,1))
!     &          * (table(minindex+1,2)-table(minindex,2))
!     &          / (table(minindex+1,1)-table(minindex,1))
!     &          + table(minindex,2)
!          write(*,*) 'afgen int:', afgen
!      end if
!
!      return
!      end
!
    real function evap_dissag(                                          &
     &               tday, sunrise, sunset, lenday, evapamp, evapdaypot)

      ! returns the potential evaporation rate (m/s) at the time tday (s)

      use p1unconv_mod, only: pi

      real, intent(in) :: tday        ! time of day from midnight (s)
      real, intent(in) :: sunrise     ! time of sunrise (s)
      real, intent(in) :: sunset      ! time of sunset (s)
      real, intent(in) :: lenday      ! length of day (s)
      real, intent(in) :: evapamp     ! amplitude of diurnal evaporation (m/s)
      real, intent(in) :: evapdaypot  ! daily total evaporation (m)

      real evap_rate                  ! evaporation rate (m/s)

      evap_rate = evapdaypot * 0.1 / 86400.0
      if( (tday.gt.sunrise) .and. (tday.lt.sunset) ) then
          evap_rate = evap_rate                                         &
     &                  + evapamp * sin(pi*(tday-sunrise)/lenday)
      endif

      evap_dissag = evap_rate

      return
    end function evap_dissag

    function internode_wtcond_bc( theta_up, theta_low, &
             thetar_up, thetar_low, thetas_up, thetas_low, &
             thetaw_up, thetaw_low, theta80rh_up, &
             theta80rh_low, soiltemp_up, soiltemp_low, &
             ksat_up, ksat_low, lambda_up, lambda_low, &
             thick_up, thick_low, airentry_up, airentry_low ) result( k_int )

      ! + + + PURPOSE + + +
      ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
      ! simulation of one-dimensional infiltration, drainage, and capillary rise in
      ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

      ! Using the method with uniform soil properties by averageing the 
      ! layer properties of the two adjoining layers.
      ! This routine uses semi-layer thickness weighted values of lambda,
      ! air entry potential and saturated hydraulic conductivity to estimate
      ! the internodal soil properties.

      ! + + + KEYWORDS + + +
      ! darcy, layering, hydraulic conductivity

      use hydro_util_mod, only: matricpot_bc

      ! + + + ARGUMENT DECLARATIONS + + +
      real, intent(in) :: theta_up
      real, intent(in) :: theta_low
      real, intent(in) :: thetar_up
      real, intent(in) :: thetar_low
      real, intent(in) :: thetas_up
      real, intent(in) :: thetas_low
      real, intent(in) :: thetaw_up
      real, intent(in) :: thetaw_low
      real, intent(in) :: theta80rh_up
      real, intent(in) :: theta80rh_low
      real, intent(in) :: soiltemp_up
      real, intent(in) :: soiltemp_low
      real, intent(in) :: ksat_up      ! saturated hydraulic conductivity of upper soil layer
      real, intent(in) :: ksat_low     ! saturated hydraulic conductivity of lower soil layer
      real, intent(in) :: lambda_up    ! pore size interaction factor of upper soil layer
      real, intent(in) :: lambda_low   ! pore size interaction factor of lower soil layer
      real, intent(in) :: thick_up     ! layer thickness of upper soil layer
      real, intent(in) :: thick_low    ! layer thickness of lower soil layer
      real, intent(in) :: airentry_up  ! air entry potential of upper soil layer
      real, intent(in) :: airentry_low ! air entry potential of lower soil layer
      real :: k_int                    ! hydraulic conductivity return value

      ! + + + LOCAL VARIABLES + + +
      real :: dist         ! distance between layer nodes
      real :: potm_up      ! matric potential of upper soil layer
      real :: potm_low     ! matric potential of lower soil layer
      real :: thetar       ! layer thickness weighted average
      real :: thetas       ! layer thickness weighted average
      real :: airentry     ! layer thickness weighted average airentry value
      real :: lambda       ! layer thickness weighted average lambda value
      real :: thetaw       ! layer thickness weighted average
      real :: theta80rh    ! layer thickness weighted average
      real :: soiltemp     ! layer thickness weighted average
      real :: ksat         ! layer thickness weighted average ksat value
      real :: soilrh       ! return value from subroutine, not used

      ! + + + END SPECIFICATIONS + + +

      ! distance between layer nodes
      dist = 0.5 * ( thick_up + thick_low )

      ! using an internodal thickness weighted average
      thetar = ( thetar_up * thick_up + thetar_low * thick_low ) / ( 2 * dist )
      thetas = ( thetas_up * thick_up + thetas_low * thick_low ) / ( 2 * dist )
      lambda = ( lambda_up * thick_up + lambda_low * thick_low ) / ( 2 * dist )
      airentry = ( airentry_up * thick_up + airentry_low * thick_low ) / ( 2 * dist )
      thetaw = ( thetaw_up * thick_up + thetaw_low * thick_low ) / ( 2 * dist )
      theta80rh = ( theta80rh_up * thick_up + theta80rh_low * thick_low ) / ( 2 * dist )
      soiltemp = ( soiltemp_up * thick_up + soiltemp_low * thick_low ) / ( 2 * dist )
      ksat = ( ksat_up * thick_up + ksat_low * thick_low ) / ( 2 * dist )

      call matricpot_bc(theta_up, thetar, thetas, airentry, lambda, thetaw, &
                      theta80rh, soiltemp, potm_up, soilrh )
      call matricpot_bc(theta_low, thetar, thetas, airentry, lambda, thetaw, &
                      theta80rh, soiltemp, potm_low, soilrh )

      k_int = int_cond_uni_bc( potm_up, potm_low, dist, ksat, lambda, airentry )

      return
    end function internode_wtcond_bc

    real function diffusive( theta, porosity, airtemp, atmpres )

!     calculation of the soil water vapor diffusivity in air (m^2/sec)
!     using the methods from Campbell (1985) to account for temperature,
!     pressure and air filled porosity

      use hydro_data_struct_defs, only: atmstand, diffuntp, zerokelvin

!*** Argument declarations ***
      real theta, porosity, airtemp, atmpres
!     theta    - volumetric soil water content
!     porosity - total soil porosity (air + water volume fraction)
!     airtemp  - air temperature (c)
!     atmpres  - atmospheric pressure (kPa)

!*** Local declarations ***
      real diffutp, airpore, poreb, porem
!     diffutp     - diffusivity adjusted for temperature and pressure (m^2/s)
!     soilairpore - soil air filed porosity (m^3/m^3)
!     poreb       - b coefficient for diffusivity air filled pore function
!     porem       - m coefficient for diffusivity air filled pore function
      parameter (poreb = 0.66)
      parameter (porem = 1.0)

      diffutp = diffuntp * atmstand / atmpres                           &
     &        * ((airtemp+zerokelvin)/zerokelvin)**2
      airpore = max(0.0,porosity - theta)
      diffusive = diffutp * poreb * airpore ** porem

      return
    end function diffusive

    real function airtempsin(tsec, tmax, tmin)

!     + + + PURPOSE + + +
!     Returns the value of air temperature as a function of time of day
!     using a sinusoidal approximation of temperature through the daily
!     maximum and daily minimum, which are assumed to occur at 6pm and
!     6am respectively.

      use p1unconv_mod, only: pi

!     + + + ARGUMENT DECLARATIONS + + +
      real  tsec, tmax, tmin

!     + + + ARGUMENT DEFINITIONS + + +
!     tsec  - time of day with 0 at midnight (seconds)
!     tmax  - daily maximum temperature (C)
!     tmin  - daily minimum temperature (C)

!*** LOCAL DECLARATIONS ***
      real halfperiod
      parameter( halfperiod = 43200 )

      airtempsin = 0.5*(tmax + tmin                                     &
     &           + (tmax-tmin)*sin(pi*(tsec/halfperiod +1.0)))

      return
    end function airtempsin

    real function calctht0( bszlyd, theta, thetaw, eratio )

!     + + + PURPOSE + + +

!     calctht0 - calculate surface water content based on extrapolation

!     + + + ARGUMENT DECLARATIONS + + +

      real bszlyd(*)
      real theta(0:*)
      real thetaw(*)
      real eratio

!     + + + ARGUMENT DEFINITIONS + + +

!     bszlyd - depth of layers
!     theta  - water content (m^3/m^3)
!     thetaw - wilting point (m^3/m^3)
!     eratio - actual surface evap / potential surface evap

!     + + + LOCAL VARIABLES + + +
      real thetax
      real thetae
      real theter

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     thetax - extrapolated surface soil water content
!     thetae - equivalent water content (theta/thetaw)
!     theter - evaporation ratio volumetric water content

!     + + + END SPECIFICATIONS + + +

      thetax = extra(bszlyd, theta)                             !h-64,65,66

      ! constrain  extrapolation
      ! - uppper limit assumes that if surface is wet, all effect will be 
      ! reflected in surface layer values
      ! - lower limit is arbitrary, but is well below the lower limit used in erosion
      ! to indicate no more erosion prevention effect of surface moisture
      if( thetax .gt. theta(1) ) then
          thetax = theta(1)
      else if( thetax .lt. 0.1 * thetaw(1) ) then
          thetax = 0.1 * thetaw(1)
      end if

      thetae = 0.24308 + 1.37918 / (1.0+EXP(-(eratio-0.44882)/0.081))        !h-85
      theter = thetae * thetaw(1)

      !calctht0 = min( thetax, theter )
      calctht0 = theter

      return
    end function calctht0

!     eratio - Ratio of actual to potential bare soil evaporation
!     thetae - Equivalent surface soil water content (m^3/m^3)
!     theter - Surface soil water content based on relationship
!              between evaporation ratio & equivalent soil water
!              content (m^3/m^3)
! ***      if (theta(1) .gt. ( thetaw(1) + awct*.70 )) then
! ***         if ( ephc .eq. 0.0 ) then
! ***            theta(0)= max(thetax , theta(0))
! ***            go to 290
! ***         else
! ***            theta(0)= thetax
! ***            go to 290
! ***         end if
! ***      end if
! ***
! ***      if ( ephc .gt. 0.0 ) then
! ***         eratio = eahc/ephc									!text after h-85
! ***
! ***c     This function estimates soil wetness at the soil-atmosphere
! ***c     interface based on a sigmoid curve that describes the relationship
! ***c     between evaporation ratio and surface soil wetness expressed as
! ***c     equivalent water content.
! ***
! ***         thetae = 0.24308+1.37918/
! ***     *            (1.0+EXP(-(eratio-0.44882)/0.081))					!h-85
! ***         theter = thetae*thetaw(1)
! ***         theta(0) = min(theter,thetax,theta(0))
! ***      else
! ***         theta(0) = max(thetax/2.0,theta(0))
! ***      end if
! ***

    real function evapredu( bhzeasurf, evaplimit, vaptrans, bhzep )

!     + + + PURPOSE + + +
!     This function returns the reduction in evaporation rate due to
!     soil drying (ratio of evap actual / evap potential)

!     + + + KEY WORDS + + +
!     soil evaporation dryness limit

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      real bhzeasurf, evaplimit, vaptrans, bhzep

!     + + + ARGUMENT DEFINITIONS + + +
!     bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     evaplimit - accumulated surface evaporation since last complete rewetting
!                 defining limit of stage 1 (energy limited) and start of 
!                 stage 2 (soil vapor transmissivity limited) evaporation (mm)
!     vaptrans  - vapor transmissivity (mm/d^.5)
!     bhzep     - daily potential evaporation (mm)

!     can be used with other depth units if they are consistent

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      real evapday

!     + + + LOCAL DEFINITIONS + + +
!     evapday  - evaporation time since the initiation of stage 2 evaporation

!     + + + END SPECIFICATIONS + + +

      ! reduce daily potential surface evaporation rate based on
      ! accumulated evaporation since last complete surface wetting
      if( (bhzeasurf .gt. evaplimit) .and. (bhzep .gt. 0.0) ) then
          evapday = ((bhzeasurf - evaplimit) / vaptrans) ** 2.0
          evapredu = min( 1.0, vaptrans                                 &
     &              * ((evapday+1)**0.5 - evapday**0.5) / bhzep )
      else
          evapredu = 1.0
      end if

      return
    end function evapredu

    real function extra (bszlyd, theta)

!     + + + PURPOSE + + +
!     This subroutine extrapolates soil water content to the surface
!     from the three uppermost simulation layers.  A numerical
!     solution known as Cramer's rule is used to obtain an estimate
!     of the extrapolated surface soil water content by solving the
!     three simultaneous equations that describe the relationship
!     between soil water content and soil depth for the three
!     uppermost simulation layers.
!     DATE:  09/22/93
!     MODIFIED:  10/06/93

!     + + + KEY WORDS + + +
!     soil, water content

!     + + + ARGUMENT DECLARATIONS + + +
      real bszlyd(*)
      real theta(0:*)

!     + + + ARGUMENT DEFINITIONS + + +
!     bszlyd  - Depth to bottom of soil layer from surface (mm)
!     theta   - soil water content by layer (m^3/m^3)

!     + + + LOCAL VARIABLES + + +
      real d
      real d1

!     + + + LOCAL DEFINITIONS + + +
!     d      - The determinant of the coefficient matrix.
!     d1     - The determinant of the matrix formed by substituting
!              load vector into column 1 of the coefficient matrix.

!     + + + END SPECIFICATIONS + + +


      d = (bszlyd(2)*bszlyd(3)**2) + (bszlyd(3)*bszlyd(1)**2) +         &
     &    (bszlyd(1)*bszlyd(2)**2) - (bszlyd(2)*bszlyd(1)**2) -         &
     &    (bszlyd(1)*bszlyd(3)**2) - (bszlyd(3)*bszlyd(2)**2)

      d1 = (theta(1)*bszlyd(2)*bszlyd(3)**2) +                          &
     &     (theta(2)*bszlyd(3)*bszlyd(1)**2) +                          &
     &     (theta(3)*bszlyd(1)*bszlyd(2)**2) -                          &
     &     (theta(3)*bszlyd(2)*bszlyd(1)**2) -                          &
     &     (theta(2)*bszlyd(1)*bszlyd(3)**2) -                          &
     &     (theta(1)*bszlyd(3)*bszlyd(2)**2)

! Check to make sure that "d" is not too close to zero (thetax gets big)
      if (d .lt. 0.0000001) then
          extra = 1.0e30
      else 
          extra= d1/d
      endif

      return
    end function extra

    real function fricfact(ref_ranrough, ranrough,                    &
     &                  tot_stems, tot_flat_cov )

!     + + + PURPOSE + + +
!     returns the darcy weisbach friction factor based on random roughness,
!     standing and flat biomass adapted from WEPP (chapter 10)

!     + + + KEY WORDS + + +
!     hydrology, overland flow

!     + + + ARGUMENT DECLARATIONS + + +
      real ref_ranrough, ranrough
      real tot_stems, tot_flat_cov

!     + + +  ARGUMENT DEFINITIONS + + +
!     ref_ranrough   - random roughness of soil surface after last tillage (m)
!     cum_rain       - accumulated rainfall since last rainfall (m)
!     ranrough       - present random roughness (m)
!     tot_stems      - total number of standing stems (#/m^2)
!     tot_flat_cov   - fraction of soil surface covered by flat biomass

!     + + + LOCAL VARIABLES + + +
      real coef_a, coef_b, coef_c, coef_d
      real coef_e, coef_f, coef_g, coef_h
      real f_stem_max, f_stem, f_flat, f_soil
      real tot_stems_max, f_soil_ref
      parameter( coef_a = 14.5 )
      parameter( coef_b = 1.55 )
      parameter( coef_c = 3.02 )
      parameter( coef_d = -5.04 )
      parameter( coef_e = -161.0 )
      parameter( coef_f = 0.5 )
      parameter( coef_g = 1.13 )
      parameter( coef_h = -3.09 )

      parameter( f_stem_max = 12 )
      parameter( tot_stems_max=220 )

!     this relationship is unproven, but does give some variation as desired
      f_stem = f_stem_max * tot_stems / tot_stems_max

      f_flat = coef_a * tot_flat_cov ** coef_b

      f_soil_ref = exp( coef_c + coef_d * exp( coef_e * ref_ranrough ))
      if( ref_ranrough.le.ranrough ) then
           f_soil = coef_f * f_soil_ref ** coef_g
      else
           f_soil = coef_f * f_soil_ref ** coef_g                       &
     &            * exp( coef_h * (1.0-ranrough/ref_ranrough))
      endif

      fricfact = f_stem + f_flat + f_soil

      return
    end function fricfact

    real function internode_wt_bc(cond_up, cond_low,                  &
     &              ksat_up, ksat_low, lambda_up, lambda_low,           &
     &              thick_up, thick_low, airentry_up, airentry_low )

!     + + + PURPOSE + + +
!     Gastó, J.M., J. Grifoll, and Y. Cohen. 2002. Estimation of Internodal
!     Permeabilities for Numerical Simulation of Unsaturated Flows. Water
!     Resources Research, vol. 38, no. 12. Variable names correspond to
!     the convention used in the article.

!     Examples in the article suggest that uniform layer properties be
!     used for all internodal calculations, and that a node be placed at
!     the intersection of the two layers. The weakness of this requirement
!     shows up when considering soil with gradually varying properties.
!     This routine uses semi-layer thickness weighted values of lambda
!     and air entry potential to estimate the internodal soil properties.

!     + + + KEYWORDS + + +
!     darcy, layering, hydraulic conductivity

!     + + + ARGUMENT DECLARATIONS + + +
      real cond_up, cond_low
      real ksat_up, ksat_low, lambda_up, lambda_low
      real thick_up, thick_low, airentry_up, airentry_low

!     + + + ARGUMENT DEFINITIONS + + +
!     cond_up - unsaturated hydraulic conductivity of upper soil layer
!     cond_low - unsaturated hydraulic conductivity of lower soil layer
!     ksat_up - saturated hydraulic conductivity of upper soil layer
!     ksat_low - saturated hydraulic conductivity of lower soil layer
!     lambda_up - pore size interaction factor of upper soil layer
!     lambda_low - pore size interaction factor of lower soil layer
!     thick_up - layer thickness of upper soil layer
!     thick_low - layer thickness of lower soil layer
!     airentry_up - air entry potential of upper soil layer
!     airentry_low - air entry potential of lower soil layer

!     + + + PARAMETERS + + +
      real   a10, a11, a2, b01, b02, b1, c0, beta
      parameter( a10 = 0.208 )
      parameter( a11 = 0.634 )
      parameter( a2 = 0.191 )
      parameter( b01 = 0.690 )
      parameter( b02 = 2.294 )
      parameter( b1 = 0.049 )
      parameter( c0 = 0.020 )
      parameter( beta = 0.0080 )

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + LOCAL VARIABLES + + +
      real n, beta0, dist, lambda, b0
      real airentry, delta_z_star, c, b, a1, a
      real k_up, k_low, r

!     + + + LOCAL DEFINITIONS + + +
!     n - 
!     beta0 - 
!     dist - internodal distance (m)
!     lambda - pore size interaction factor (internodal average)
!     airentry - air entry potential (internodal average)
!     delta_z_star - nondimensional distance between nodes

!     + + + SUBROUTINES CALLED + + +

!     + + + FUNCTION DECLARATIONS + + +

!     + + + DATA INITIALIZATIONS + + +

!     + + + OUTPUT FORMATS + + +

!     + + + END SPECIFICATIONS + + +

      ! distance between layer nodes
      dist = 0.5 * ( thick_up + thick_low )

      ! using an internodal weighted average lambda
      lambda = ( lambda_up * thick_up + lambda_low * thick_low )        &
     &       / ( 2 * dist )
      n = lambda + 1.0

      ! equation 14
      beta0 = beta * n

      ! equation 13
      b0 = b01 * n / ( b02 * n - 1.0 )

      ! Nondimensional distance, using average of layer thickness for
      ! internadal distance and an internodal weighted average air entry
      ! potential
      airentry = ( airentry_up * thick_up + airentry_low * thick_low )  &
     &       / ( 2 * dist )
      delta_z_star = abs(dist / airentry)

      ! equation 12
      c = b0 + c0 * (n - 1.0) * delta_z_star

      ! equation 11
      b = b0 - b1 * delta_z_star
      ! keep b from going negative for large delta_z_star
      b = max( 1.0e-6, b )

      ! equation 10
      a1 = a10 + a11 * log10(n)

      ! equation 9
      a = (1.0 - a1 * delta_z_star) / (1.0 + a2 * n * n * delta_z_star)
      ! keep a from going negative for large delta_z_star
      a = max( 1.0e-6, a )

      ! equation 8b
      k_up = cond_up / ksat_up
      k_low = cond_low / ksat_low

      ! check for drying below the residual moisture content in denominator
      if( k_low .le. 0.0 ) then
          r = 1.0e38
      else
          r = k_up**b / k_low**c
      end if

      ! equation 8a
      internode_wt_bc = 1.0 /( 1.0 + ( a*r/(1.0 + beta0*r)))

      return
    end function internode_wt_bc

  function internode_cond_bc( potm_up, potm_low, &
           ksat_up, ksat_low, lambda_up, lambda_low, &
           thick_up, thick_low, airentry_up, airentry_low ) result( k_int )

    ! + + + PURPOSE + + +
    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! Szymkiewicz, A., R. Helmig. 2010. Comparison of conductivity averaging methods
    ! for one-dimensional unsaturated flow in layered soils. SRC Simtech No. 2010-80

    ! Szymkiewicz, A., R. Helmig show how to deal with different properties between
    ! layers. THe method designated CC-SZYM is implemented here, which involves
    ! interatively matching the interface flux value with material transitions.
    ! All the layers in WEPS have different properties as they are tilled and resettle,
    ! and many soil profiles have texture changes with depth.

    ! + + + KEYWORDS + + +
    ! darcy, layering, hydraulic conductivity

    ! + + + ARGUMENT DECLARATIONS + + +
    real, intent(in) :: potm_up      ! matric potential of upper soil layer
    real, intent(in) :: potm_low     ! matric potential of lower soil layer
    real, intent(in) :: ksat_up      ! saturated hydraulic conductivity of upper soil layer
    real, intent(in) :: ksat_low     ! saturated hydraulic conductivity of lower soil layer
    real, intent(in) :: lambda_up    ! pore size interaction factor of upper soil layer
    real, intent(in) :: lambda_low   ! pore size interaction factor of lower soil layer
    real, intent(in) :: thick_up     ! layer thickness of upper soil layer
    real, intent(in) :: thick_low    ! layer thickness of lower soil layer
    real, intent(in) :: airentry_up  ! air entry potential of upper soil layer
    real, intent(in) :: airentry_low ! air entry potential of lower soil layer
    real :: k_int                    ! hydraulic conductivity return value

    ! + + + LOCAL DEFINITIONS + + +
    real :: potm_c      ! interfacial matric potential
    real :: flux_delta  ! difference in interfacial flux between uppper an lower layer
    real :: kunsat_up   ! hydraulic conductivity value between upper layer node and interlayer node
    real :: kunsat_low  ! hydraulic conductivity value between lower layer node and interlayer node
    real :: dz_up       ! distance from node to layer interface for upper layer
    real :: dz_low      ! distance from node to layer interface for lower layer
    integer :: iter_cnt ! counter, iterations required to converge to value of potm_c giving internode flux equality

    real, parameter :: test_delta = 0.00001       ! accuracy level desired for flux_delta (m/s)

    ! + + + END SPECIFICATIONS + + +

    ! Equation 21 - equal flux at layer internodal interface, solve interatively
    ! initial guess is arithmetic mean
    potm_c = 0.5 * ( potm_up + potm_low )

    dz_up = 0.5*thick_up
    dz_low = 0.5*thick_low
    kunsat_up = int_cond_uni_bc( potm_up, potm_c, dz_up, ksat_up, lambda_up, airentry_up )
    kunsat_low = int_cond_uni_bc( potm_c, potm_low, dz_low, ksat_low, lambda_low, airentry_low )
    flux_delta = kunsat_up * ( (potm_c - potm_up) / (dz_up) - 1.0 ) &
               - kunsat_low * ( (potm_low - potm_c) / (dz_low) - 1.0 )

    iter_cnt = 0
    do while( (abs(flux_delta) .gt. test_delta) .and. (iter_cnt .le. 10) )
      potm_c = ( kunsat_up * ( potm_up / dz_up - 1.0 ) + kunsat_low * ( potm_low / dz_low - 1.0 ) ) &
             / ( kunsat_up / dz_up + kunsat_low / dz_low )
      kunsat_up = int_cond_uni_bc( potm_up, potm_c, dz_up, ksat_up, lambda_up, airentry_up )
      kunsat_low = int_cond_uni_bc( potm_c, potm_low, dz_low, ksat_low, lambda_low, airentry_low )
      flux_delta = kunsat_up * ( (potm_c - potm_up) / (dz_up) - 1.0 ) &
                 - kunsat_low * ( (potm_low - potm_c) / (dz_low) - 1.0 )
      iter_cnt = iter_cnt + 1
      if( iter_cnt .gt. 10 ) then
        !write(*,*) 'INTERNODE COND: potm_c convergence not obtained in 10 interations'
        write(*,*) 'INTERNODE COND: ', potm_up, potm_c, potm_low, kunsat_up, kunsat_low, flux_delta
      end if
    end do

    ! Equation 22 - 
    ! after convergence kunsat_up and kunsat_low are defined
    k_int = ( dz_up + dz_low ) / ( (dz_up/kunsat_up) + (dz_low/kunsat_low) )

    return
  end function internode_cond_bc

  function int_cond_uni_bc( potm_up, potm_low, dist, ksat, lambda, airentry ) result( k_int )
    ! This is the method used for internodal conductivity when soil properties are
    ! uniform between layers.  It is used within the method for layered soils.

    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! + + + KEYWORDS + + +
    ! darcy, layering, hydraulic conductivity

    ! + + + ARGUMENT DECLARATIONS + + +
    real, intent(in) :: potm_up  ! matric potential of upper soil layer (m)
    real, intent(in) :: potm_low ! matric potential of lower soil layer (m)
    real, intent(in) :: dist     ! distance between the two vertical nodes (m)
    real, intent(in) :: ksat     ! saturated hydraulic conductivity of soil layer (m/s)
    real, intent(in) :: lambda   ! pore size interaction factor of soil layer
    real, intent(in) :: airentry ! air entry potential of soil layer (m)
    real :: k_int                ! hydraulic conductivity return value

    ! + + + LOCAL VARIABLES + + +
    real :: d_potm        ! the difference in potential between uppper and lower "point"
    real :: cond_up       ! conductivity of the upper layer
    real :: cond_1        ! temporary conductivity value 1
    real :: cond_2        ! temporary conductivity value 2
    real :: potm_2        ! temporary potential value 2
    real :: temp_a        ! temporary value a
    real :: temp_b        ! temporary value b
    real :: temp_dzl      ! temporary value of ratio a/b

    ! + + + END SPECIFICATIONS + + +

    d_potm = potm_low - potm_up

    cond_up = unsatcond_pot_bc(potm_up, ksat, airentry, lambda)

    if( (d_potm .eq. 0.0) .or. (d_potm .eq. dist) ) then
       ! uniform or hydrostatic matric potential distribution
       k_int = cond_up
    else if( d_potm/dist .lt. 0.0 ) then
       ! infiltration
       cond_1 = unsatcond_int_bc(potm_up, potm_low, ksat, airentry, lambda)
       cond_2 = cond_up / (1 - d_potm / dist)
       k_int = max( cond_1, cond_2 )
    else if( d_potm/dist .lt. 1.0 ) then
       ! drainage
       cond_1 = cond_up / (1 - d_potm / dist)
       potm_2 = potm_low - d_potm * d_potm / dist
       cond_2 = unsatcond_pot_bc(potm_2, ksat, airentry, lambda)
       k_int = min( cond_1, cond_2 )
    else
       ! capillary rise
       cond_1 = unsatcond_int_bc(potm_up, potm_low-dist, ksat, airentry, lambda)
       potm_2 = potm_low - dist
       cond_2 = unsatcond_pot_bc(potm_2, ksat, airentry, lambda)
       if( cond_1 .ne. cond_2 ) then
           temp_a = (d_potm * d_potm + 4.0*(cond_2 / cond_1 - 1.0)*(d_potm - dist)*dist)**0.5 - d_potm
           temp_b = 2.0 * (cond_2 / cond_1 - 1.0)
           temp_dzl = temp_a / temp_b
           k_int = dist * cond_1 * cond_2 / ((dist - temp_dzl)*cond_1 + temp_dzl*cond_2)
       else
           k_int = cond_1
       end if
    end if

    return
  end function int_cond_uni_bc

  function unsatcond_int_bc( potm_1, potm_2, ksat, airentry, lambda ) result( k_int )
    ! returns the integrated unsaturated hydraulic conductivity as defined by
    ! Szymkiewicz, A. (2009), Approximation of internodal conductivities in numerical
    ! simulation of one-dimensional infiltration, drainage, and capillary rise in
    ! unsaturated soils, Water Resour. Res., 45, W10403, doi:10.1029/2008WR007654.

    ! *** Argument declarations ***
    real, intent(in) :: potm_1     ! matric potential one (meters of water)
    real, intent(in) :: potm_2     ! matric potential two (meters of water)
    real, intent(in) :: ksat       ! Saturated hydraulic conductivity (m/s)
    real, intent(in) :: airentry   ! Brooks and Corey air entry matric potential (m)
    real, intent(in) :: lambda     ! Brooks and Corey pore size interaction parameter 
    real :: k_int                  ! unsaturated hydraulic conductivity (m/s)

    ! *** Local Variables ***
    real fluxpot_1, fluxpot_2

    fluxpot_1 = matricfluxpot_bc( potm_1, airentry, ksat, lambda )
    fluxpot_2 = matricfluxpot_bc( potm_2, airentry, ksat, lambda )

    if( (fluxpot_2 .eq. fluxpot_1) .or. (potm_2 .eq. potm_1) ) then
      ! equal potentials so use one
      k_int = unsatcond_pot_bc(potm_1, ksat, airentry, lambda)
    else
      ! denominator not equal to zero
      k_int = (fluxpot_2 - fluxpot_1) / (potm_2 - potm_1)
    end if

    return
  end function unsatcond_int_bc

  function matricfluxpot_bc( potm, airentry, ksat, lambda ) result( fluxpot )
    ! returns: matric flux potential using the Brooks and Corey relationship (m^2/s))
    ! as shown in Ross, P.J. 2003. Modeling Soil Water and Solute Transport - Fast,
    ! Simplified Numerical Solutions. Agron. J. 95:1352-1361

    ! *** Argument declarations ***
    real, intent(in) :: potm       ! matric potential (meters of water)
    real, intent(in) :: airentry   ! Brooks/Corey air entry potential (m)
    real, intent(in) :: ksat       ! saturated hydraulic conductivity (m/s)
    real, intent(in) :: lambda     ! Brooks/Corey pore size interaction parameter
    real :: fluxpot                ! matric flux potential using the Brooks and Corey relationship (m^2/s))

    ! *** Local Variables ***
    real :: nu      ! Brooks and Corey term (modified)
    real :: potme   ! flux potential coefficient of integration

    nu = 2.5 + 2.0/lambda

    potme = ksat * airentry / (1.0-lambda*nu)

    if( potm .lt. airentry ) then
      ! potential is less than air entry potential
      fluxpot = potme * (potm/airentry)**(1.0-lambda*nu)
    else
      ! potential is greater than or equal to air entry potential
      fluxpot = potme + ksat*(potm-airentry)
    end if
    return
  end function matricfluxpot_bc

    real function store (minlay, maxlay, prevvolw, volw, laydepth)

!     + + + PURPOSE + + +
!     determines the infiltration depth of water from the soil surface (mm)
!     by checking for an increase in soil water content.
!     The depth is set to the layer where the soil water content has
!     not increased. The value is always set to include the first layer
!     since it will not be called unless water has been added.
!     

!     + + + ARGUMENT DECLARATIONS + + +
      integer minlay, maxlay
      real prevvolw(*), volw(*), laydepth(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn           - Number of soil layers used in the simulation
!     prevvolw(layrsn+6) - beginning of day volume of water in the soil profile (m)
!     volw(layrsn+6)     - after event volume of water in the soil profile (m)
!     laydepth(layrsn) - depth to bottom of soil layer (mm)

!     + + + LOCAL VARIABLES + + +
      integer lrx

!     + + + LOCAL DEFINITIONS + + +
!     lrx    - Loop counter

!     + + + END SPECIFICATIONS + + +

!     distribute the daily amount of water available for infiltration
!     into the soil profile throughout the simulation layers.

      store = laydepth(1)
      do lrx = minlay+1,maxlay
          if( volw(lrx).gt.prevvolw(lrx) ) then
              store = laydepth(lrx-minlay+1)
           else
              exit
           endif
      end do

      return
    end function store

  function unsatcond_pot_bc(potm, ksat, airentry, lambda) result( kunsat )
    ! returns the unsaturated hydraulic conductivity in same units as ksat as 
    ! defined by the Books and Corey function and the Mualem conductivity model
    ! as a function of the matric potential

    ! *** Argument declarations ***
    real, intent(in) :: potm       ! matric potential  (meters of water)
    real, intent(in) :: ksat       ! Saturated hydraulic conductivity (m/s)
    real, intent(in) :: airentry   ! Brooks and Corey air entry matric potential (m)
    real, intent(in) :: lambda     ! Brooks and Corey pore size interaction parameter 
    real :: kunsat                 ! unsaturated hydraulic conductivity (m/s)

    ! *** Local Variables ***
    real nu      ! Brooks and Corey term (modified)

    nu = 2.5 + 2.0/lambda

    if( potm .lt. airentry ) then
      ! unsaturated condition
      kunsat = ksat*(potm/airentry)**(-lambda*nu)
    else
      ! saturated condition
      kunsat = ksat
    end if

    return
  end function unsatcond_pot_bc

end module hydro_darcy_mod

