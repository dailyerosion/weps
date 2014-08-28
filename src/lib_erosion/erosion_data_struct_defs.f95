!$Author$
!$Date$
!$Revision$
!$HeadURL$

module erosion_data_struct_defs

  use Polygons_Mod

  implicit none

  ! defines state variables for each grid cell
  type cellsurfacestate
     integer :: csr       ! index of current subregion at grid point x,y
     integer :: car       ! index of current accounting region at grid point x,y
     integer :: surflay   ! index of the soil layer at the surface (0 indicates deposition horizon)
     real :: surfthk      ! thickness of the soil layer at the surface (mm) (could be thinner than original layer)
     real :: sf1          ! soil mass fraction in surface layer < 0.01 mm
     real :: sf10         ! soil mass fraction in surface layer < 0.1 mm
     real :: sf84         ! soil mass fraction in surface layer < 0.84 mm
     real :: sf200        ! soil mass fraction in surface layer < 2.00 mm
     real :: sf84mn       ! "effective" soil mass fraction in surface layer < 0.84 mm
                          ! needed for u* to be the threshold friction velocity.
     real :: svroc        ! soil rock volume in surface layer  !edit ljh 1-22-05
     real :: szcr         ! Consolidated (crust) thickness (mm)
     real :: sfcr         ! soil fraction with crust cover (decimal)
     real :: smlos        ! mass of loose erodible soil on crust (kg/m^2)
     real :: sflos        ! soil fraction covered with loose erodible soil on the crusted area
     real :: smaglos      ! mobile soil mass removable from aggregated surface by u* (kg/m^2).
     real :: dmlos        ! mobile soil mass change from erosion of aggregated
     real :: smaglosmx    ! max mobile soil reservoir of aggregateed sfc.(kg/m^2)
     real :: szrgh        ! Ridge height (mm)
     real :: sxprg        ! ridge spacing parallel the wind direction(mm)
     real :: slrr         ! soil random roughness (mm)
     real :: w0br         ! barrier wind reduction factor for the grid cell
     ! real :: w0hill     ! ratio of hill to open, flat, field friction velocity as influenced by hills.
     real :: egt          ! Total soil loss at a grid point accumulated for a time period (kg/m^2)
     real :: egtcs        ! Total creep and saltation soil loss at grid point accumulated for a time period (kg/m^2)
     real :: egtss        ! Total suspension soil loss at grid point accumulated for a time period (kg/m^2)
     real :: egt10        ! Total < 10 micron soil loss at grid point accumulated for a time period (kg/m^2)
     real :: egt2_5       ! Total < 2.5 micron soil loss at grid point accumulated for a time period (kg/m^2)
     real :: wus          ! Soil surface friction velocity (m/s)
     real :: wust         ! Soil surface threshold friction velocity for emission (m/s)
     real :: wusto        ! Soil surface threshold friction velocity for emission (bare, smooth surface with sf84ic, wus minus flat biomass and wetness effects) (m/s)
     real :: wusp         ! Soil surface threshold friction velocity for transport capacity (m/s)
  end type cellsurfacestate

  type by_soil_layer
     real :: aszlyt    ! aszlyt(l,s), R, (s1layr.inc) Soil layer thickness (mm)
     real :: asdblk    ! asdblk(l,s), R, (s1phys.inc) Soil layer bulk density (Mg/m^3)
     real :: asfsan    ! asfsan(l,s),R,(s1dbh.inc) Soil layer sand content (Mg/Mg)
     real :: asfvfs    ! asfvfs(l,s), R, (s1dbh.inc) Soil layer very fine sand (Mg/Mg)
     real :: asfsil    ! sfsil(l,s),R,(s1dbh.inc) Soil layer silt content (Mg/Mg)
     real :: asfcla    ! asfcla(l,s),R,(s1dbh.inc) Soil layer clay content (Mg/Mg)
     real :: asvroc    ! asvroc(l,s), R, (s1dbh.inc) Soil layer rock volume (m^3/m^3)
     real :: asdagd    ! asdagd(l,s),R,(s1agg.inc) Soil layer agg density (Mg/m^3)
     real :: aseags    ! aseags(l,s), R, (s1agg.inc) Soil layer agg stability ln(J/kg)
     real :: aslagm    ! aslagm(l,s), R, (s1agg.inc) Soil layer GMD (mm)
     real :: aslagn    ! aslagn(l,s), R, (s1agg.inc) Soil layer minimum agg size (mm)
     real :: aslagx    ! aslagx(l,s), R, (s1agg.inc) Soil layer maximum agg size (mm)
     real :: as0ags    ! as0ags(l,s), R, (s1agg.inc) Soil layer GSD (mm/mm)
     real :: ahrwcw    ! ahrwcw(l,s), R, (h1db1.inc) Soil layer wilting point water content (Mg/Mg)
     real :: ahrwca    ! ahrwca(l,s), R, (h1db1.inc) Soil layer water content (Mg/Mg)
  end type by_soil_layer

  type subregionsurfacestate
     ! ERODIN inputs
     real :: adzht_ave  ! adzht_ave(s), R, Average residue height (m)
     real :: aczht      ! aczht(s), R, Crop height (m)
     real :: acrsai     ! acrsai(s), R, Crop stem area index (m^2/m^2)
     real :: acrlai     ! acrlai(s), R, Crop leaf area index (m^2/m^2)
     real :: adrsaitot  ! adrsaitot(s), R, Residue stem area index (m^2/m^2)
     real :: adrlaitot  ! adrlaitot(s), R, Residue leaf area index (m^2/m^2)
     real :: acxrow     ! acxrow(s) Crop row spacing (m)
     integer :: ac0rg   ! ac0rg(s)  Crop seed placement (0 - furrow, 1 - ridge)
     real :: abffcv     ! abffcv(s), R, (b1geom.inc) Flat biomass cover (m^2/m^2)
     integer :: nslay   ! nslay(s), I, (s1layr.inc) Number of soil layers
     type(by_soil_layer), dimension(:), allocatable :: bsl
     real :: asfcr      ! asfcr(s), R, (s1surf.inc) Surface crust fraction (m^2/m^2)
     real :: aszcr      ! aszcr(s), R, (s1surf.inc) Surface crust thickness (mm)
     real :: asflos     ! asflos(s), R, (s1surf.inc) Fraction of loose material on surface (m^2/m^2)
     real :: asmlos     ! asmlos(s), R, (s1surf.inc) Mass of loose material on crust (kg/m^2)
     real :: asdcr      ! asdcr(s), R, (s1surf.inc) Soil crust density (Mg/m^3)
     real :: asecr      ! asecr(s), R, (s1surf.inc) Soil crust stability ln(J/kg)
     real :: aslrr      ! aslrr(s), R, (s1sgeo.inc) Allmaras random roughness (mm)
     real :: aszrgh     ! aszrgh(s), R, (s1sgeo.inc) Ridge height (mm)
     real :: asxrgs     ! asxrgs(s), R, (s1sgeo.inc) Ridge spacing (mm)
     real :: asxrgw     ! asxrgw(s), R, (s1sgeo.inc) Ridge width (mm)
     real :: asargo     ! asargo(s), R, (s1sgeo.inc) Ridge orientation (deg)
     real :: asxdks     ! asxdks(s), R, (s1sgeo.inc) Dike spacing (mm)
     real :: ahzsnd     ! ahzsnd(s), R, (s1sgeo.inc) Snow depth (mm)
     real, dimension(:), allocatable :: ahrwc0
     ! derived
     real :: abrsai     ! abrsai - Biomass stem area index (m^2/m^2)
     real :: abrlai     ! abrlai - Biomass leaf area index (m^2/m^2)
     real :: abzht      ! abzht  - Composite weighted average biomass height (m)
     real :: sxprg      ! sxprg  - ridge spacing parallel the wind direction(mm)
     real :: acanag     ! acanag - coefficient of abrasion for aggregates (1/m)
     real :: acancr     ! acancr - coefficient of abrasion for crust (1/m)
     real :: asf10an    ! asf10an - soil fraction pm10 in abraded suspension
     real :: asf10en    ! asf10en - soil fraction pm10 in emitted suspension
     real :: asf10bk    ! asf10bk - soil fraction pm10 in saltation breakage suspension
     real :: sfd1       ! soil fraction less than 0.01 mm diameter
     real :: sfd10      ! soil fraction less than 0.1 mm diameter
     real :: sfd84      ! soil fraction less than 0.84 mm diameter
     real :: sfd200     ! soil fraction less than 2.0 mm diameter
     real :: sf10ic     ! initial condition (modified) of soil fraction less than 0.1 mm diameter
     real :: sf84ic     ! initial condition (modified) of soil fraction less than 0.84 mm diameter

  end type subregionsurfacestate

  type threshold
     integer :: erosion   ! flag, 0 - erosion was not entered, 1 - erosion was entered
     integer :: snowdepth ! flag, 0 - snow depth too low to prevent erosion, 1 - snow depth deeper than threshold, prevents erosion

     real :: wus_anemom   ! anemometer located friction velocity for critical no erosion condition
     real :: wus_random   ! site surface random roughness adjusted friction velocity for critical no erosion condition
     real :: wus_ridge    ! site surface oriented roughness adjusted friction velocity for critical no erosion condition
     real :: wus_biodrag  ! site biodrag adjusted friction velocity for critical no erosion condition
     real :: wus          ! friction velocity for critical no erosion condition (biodrag added in)

     real :: bare      ! bare friction veolocity greater
     real :: flat_cov  ! flat cover increases threshold
     real :: surf_wet  ! surface wetness increases threshold
     real :: ag_den    ! ag density increases threshold
     real :: wust      ! resultant threshold friction velocity

     real :: sfd84   ! fraction of the surface material less than 0.84 mm in diameter
     real :: asvroc  ! fraction of the surface matherial greater than 2 mm in diameter
     real :: wzzo    ! aerodynamic roughness length of the soil surface below canopy (mm)
     real :: sfcv    ! fraction of soil surface which is non emitting

  end type threshold

  type subdailyvalues
     real :: awu   ! Average subdaily wind speed (m/s)
                   ! This variable contains the value of the average subdaily wind speeds for the day
                   ! (valid only when wind speed is greater than the threshold velocity).
     real :: awdir ! Average subdaily wind direction (degrees)
                   ! This variable contains the value of the average subdaily wind direction
                   ! corresponding to the average subdaily wind speed for the subdaily period.
  end type subdailyvalues

!  type simulationregionvalues
     real :: awdair           ! Daily average air density (Kg/m^3) (set by getcli daily) 
     real :: awzypt           ! Average yearly total precipitation (mm)
     integer :: ntstep        ! Number of timesteps per day for erosion.
     integer :: erod_interval ! surface updating interval within erosion.
                              ! This variable contains the number of seconds the surface is updated within the erosion submodel.
                              ! (currently settable as a commandline option within the standalone version of the erosion submodel)
     real :: anemht           ! Standardized anemometer height (m)
     real :: awzzo            ! Weather station aerodynamic roughness height (mm)
     real :: awzdisp          ! Weather station zero plane displacement height (mm)
     integer :: wzoflg        ! Flag = 0 for anem. and  constant awwzo at wx. stations
                              ! Flag = 1 for anem. and variable awwzo at field.
     real :: awadir           ! Predominant daily wind direction (degrees)
     real :: awhrmx           ! Hour maximum daily wind speed occurs (hr)
     real :: awudmx           ! Maximum daily wind speed (m/s)
     real :: awudmn           ! Minimum daily wind speed (m/s)
     real :: awudav           ! Average daily wind speed (m/s)
     type(subdailyvalues), dimension(:), allocatable :: subday
     logical :: am0eif        ! flag to run initialization of EROSION, If .true. then run initialization subroutines.
     integer :: am0efl        ! flag to print EROSION output
                              ! 0 - then print no output
                              ! 1 - then print detailed output,
                              ! 3 - print heading and reset to val of 98  (WEPS)
                              ! 98 - print hourly emissions for erosion day (WEPS)
                              ! 4 - print heading and reset to val of 99  (tsterode)
                              ! 99 - print period emissions for erosion day (tsterode)
                              ! 6 - then call sb1out to print debug data to "tst.out"
 ! end type simulationregionvalues

contains

  function create_cellsurfacestate(xdim, ydim) result(cellstate)
     integer, intent(in) :: xdim
     integer, intent(in) :: ydim
     type(cellsurfacestate), dimension(:,:), allocatable :: cellstate

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     allocate(cellstate(0:xdim,0:ydim), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     ! allocate soil layer arrays

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for cellstate'
        stop 1
     end if
  end function create_cellsurfacestate

  subroutine destroy_cellsurfacestate(cellstate)
     type(cellsurfacestate), dimension(:,:), allocatable, intent(inout) :: cellstate

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! deallocate arrays
     deallocate(cellstate, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for cellstate'
     end if
  end subroutine destroy_cellsurfacestate

  ! NOTE: defined as subroutine to accomodate sweep usage. Values are assigned to non-array elements before number of layers is known.
  subroutine create_subregionsurfacestate(nslay, nswet, subrsurf)
     integer, intent(in) :: nslay             ! number of soil layers
     integer, intent(in) :: nswet             ! number of surface wetness values
     type(subregionsurfacestate), intent(inout) :: subrsurf  ! this needs to retain values already in non array entities for erodin in sweep

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     subrsurf%nslay = nslay

     sum_stat = 0
     ! allocate soil layer array
     allocate(subrsurf%bsl(1:nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(subrsurf%ahrwc0(1:nswet), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for biomatter'
        stop 1
     end if
  end subroutine create_subregionsurfacestate

  subroutine destroy_subregionsurfacestate(subrsurf)
     type(subregionsurfacestate), intent(inout) :: subrsurf

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! deallocate arrays
     deallocate(subrsurf%bsl, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(subrsurf%ahrwc0, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for biomatter'
     end if
  end subroutine destroy_subregionsurfacestate

  function create_threshold(nsubr) result(noerod)
     integer, intent(in) :: nsubr
     type(threshold), dimension(:), allocatable :: noerod

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     allocate(noerod(nsubr), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     ! allocate soil layer arrays

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for noerod'
        stop 1
     end if
  end function create_threshold

  subroutine destroy_threshold(noerod)
     type(threshold), dimension(:), allocatable, intent(inout) :: noerod

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! deallocate arrays
     deallocate(noerod, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for noerod'
     end if
  end subroutine destroy_threshold

end module erosion_data_struct_defs



