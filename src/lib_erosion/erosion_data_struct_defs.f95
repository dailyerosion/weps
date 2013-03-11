!$Author$
!$Date$
!$Revision$
!$HeadURL$

module erosion_data_struct_defs
  implicit none

  ! defines state variables for each grid cell
  type cellsurfacestate
     real :: csr     ! index of current subregion at grid point x,y
     integer :: surflay   ! index of the soil layer at the surface (0 indicates deposition horizon)
     real :: surfthk      ! thickness of the soil layer at the surface (mm) (could be thinner than original layer)

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
     real :: aczht      ! aczht(s), R, (c1glob.inc) Crop height (m)
     real :: acrsai     ! acrsai(s), R, (c1glob.inc) Crop stem area index (m^2/m^2)
     real :: acrlai     ! acrlai(s), R, (c1glob.inc) Crop leaf area index (m^2/m^2)
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
     real :: acanag     ! acanag - coeffienct of abrasion for aggregates (1/m)
     real :: acancr     ! acancr - coeffienct of abrasion for crust (1/m)
     real :: asf10an    ! asf10an - soil fraction pm10 in abraded suspension
     real :: asf10en    ! asf10en - soil fraction pm10 in emitted suspension
     real :: asf10bk    ! asf10bk - soil fraction pm10 in saltation breakage suspension
     real :: sfd1       ! soil fraction less than 0.01 mm diameter
     real :: sfd10      ! soil fraction less than 0.1 mm diameter
     real :: sfd84      ! soil fraction less than 0.84 mm diameter
     real :: sfd200     ! soil fraction less than 2.0 mm diameter

  end type subregionsurfacestate

contains

  function create_cellsurfacestate(xdim, ydim) result(cellstate)
     integer, intent(in) :: xdim
     integer, intent(in) :: ydim
     type(cellsurfacestate) :: cellstate

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate soil layer arrays
!     allocate(cellstate%  (0:xdim,0:ydim), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for biomatter'
        stop 1
     end if
  end function create_cellsurfacestate

  subroutine destroy_cellsurfacestate(cellstate)
     type(cellsurfacestate), intent(inout) :: cellstate

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! deallocate arrays
!     deallocate(cellstate% , stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for biomatter'
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

end module erosion_data_struct_defs



