!$Author$
!$Date$
!$Revision$
!$HeadURL$

module soil_data_struct_defs

  integer, dimension(:), allocatable :: am0sfl    ! flag to print SOIL output
                                                     ! 0 = no output
                                                     ! 1 = detailed output file created
  integer, dimension(:), allocatable :: am0sdb    ! flag to print SOIL variables before and after the call to SOIL
                                                     ! 0 = no output
                                                     ! 1 = output
  type soil_def
     ! metadata
     character*(512) :: sinfil        ! soil input file name
     character*(160) :: am0sid        ! soil identification
     character*(80)  :: am0tax        ! soil taxonomic order
     character*(80)  :: am0localphase ! soil local phase
     character*(20)  :: SoilLossTol   ! NRCS assigned "soil loss tolerance" value

     integer :: nslay          ! number of soil layers

     ! intrinsic - properties that are instrinsic, modify by changing the material in the soil
     real :: asfald     ! Dry soil albedo
     real :: asfalw     ! Wet soil albedo
     real :: restrict_depth    ! depth to impermeable layer/restricting zone (mm)
     real :: bedrock_depth     ! depth to bedrock (mm)
     real :: amrslp            ! Average subregion slope (m/m)
     real :: SFCov             ! NRCS "Surface Fragment Cover" or "Surface Layer Fragment" fraction (%)
     real :: SoilRockFragments ! fraction of soil volume that is soil rock fragments (m^3/m^3)
     real, dimension(:), allocatable :: asvroc    ! Soil layer rock volume (m^3/m^3)
     real, dimension(:), allocatable :: asfsan    ! Soil layer sand content (Mg/Mg)
     real, dimension(:), allocatable :: asfsil    ! Soil layer silt content (Mg/Mg)
     real, dimension(:), allocatable :: asfcla    ! Soil layer clay content (Mg/Mg)
     real, dimension(:), allocatable :: as0ph     ! PH (0-14)
     real, dimension(:), allocatable :: asfcce    ! Soil layer calcium carbonate equivalent [CaCO3] (kg/kg) ? (dec %)
     real, dimension(:), allocatable :: asfcec    ! Soil layer cation exchange capacity (cmol/kg) (meq/100g)
     real, dimension(:), allocatable :: asfom     ! Soil layer organic matter content (Mg/Mg)
     real, dimension(:), allocatable :: asdwblk   ! Soil layer bulk density at 1/3 bar (Mg/m^3)
     real, dimension(:), allocatable :: asdsblk   ! Soil layer settled bulk density (Mg/m^3)
     real, dimension(:), allocatable :: asdprocblk ! Soil layer proctor bulk density (Mg/m^3)
     real, dimension(:), allocatable :: aslagn    ! minimum agg size (mm)
     real, dimension(:), allocatable :: aslagx    ! maximum agg size (mm)
     real, dimension(:), allocatable :: aseagm    ! soil layer mean aggregate stabillity (J/m^2)
     real, dimension(:), allocatable :: aseagmn   ! soil layer minimum aggregate stability
     real, dimension(:), allocatable :: aseagmx   ! soil layer maximum aggregate stability
     real, dimension(:), allocatable :: aslmin    ! min values of geom. mean agg. diameter (eq. S-45, S-46)
     real, dimension(:), allocatable :: aslmax    ! max values of geom. mean agg. diameter (eq. S-45, S-46)
     real, dimension(:), allocatable :: asfcle    ! Linear extensibility ((Mg/m^3)/(Mg/m^3))
     real, dimension(:), allocatable :: asfvcs    ! Soil layer content of very coarse sand (Mg/Mg)
     real, dimension(:), allocatable :: asfcs     ! Soil layer content of coarse sand (Mg/Mg)
     real, dimension(:), allocatable :: asfms     ! Soil layer content of medium sand (Mg/Mg)
     real, dimension(:), allocatable :: asffs     ! Soil layer content of fine sand (Mg/Mg)
     real, dimension(:), allocatable :: asfvfs    ! Soil layer content of very fine sand sand (Mg/Mg)
     real, dimension(:), allocatable :: asfwdc    ! Soil layer content of water dispersible clay (Mg/Mg)
                                              ! Not used - not input in Version 1.0 IFC file

     ! state - properties indicating the state of the soil (can change without material changing)
     real :: aszrgh     ! Ridge height (mm)
     real :: aszrho     ! Original ridge height, after tillage, (mm)
     real :: asxrgw     ! Ridge width (mm)
     real :: asxrgs     ! Ridge spacing (mm)
     real :: asargo     ! Ridge orientation (deg)
     real :: asxdks     ! Dike spacing (mm)
     real :: asxdkh     ! Dike Height (mm)
     real :: aslrr      ! Allmaras random roughness (mm)
     real :: aslrro     ! Original random roughness height, after tillage, mm
     real :: asfcr      ! Surface crust fraction (m^2/m^2)
     real :: aszcr      ! Surface crust thickness (mm)
     real :: asflos     ! Fraction of loose material on surface (m^2/m^2)
     real :: asmlos     ! Mass of loose material on crust (kg/m^2)
     real :: asdcr      ! Soil crust density (Mg/m^3)
     real :: asecr      ! Soil crust stability ln(J/kg)
     real :: watertable_depth  ! depth to watertable (mm)
     real :: WaterErosion ! water erosion soil loss
     real, dimension(:), allocatable :: aszlyt    ! Soil layer thickness (mm)
     real, dimension(:), allocatable :: asdblk    ! Soil layer bulk density (Mg/m^3)
     real, dimension(:), allocatable :: asdagd    ! agg density (Mg/m^3)
     real, dimension(:), allocatable :: aseags    ! agg stability ln(J/kg)
     real, dimension(:), allocatable :: aslagm    ! GMD (mm)
     real, dimension(:), allocatable :: as0ags    ! GSD (mm/mm)

     ! derived - calculate values from state and intrinsics that are used by other process modules
     real :: acanag     ! coefficient of abrasion for aggregates (1/m)
     real :: acancr     ! coefficient of abrasion for crust (1/m)
     real :: asf10an    ! soil fraction pm10 in abraded suspension
     real :: asf10en    ! soil fraction pm10 in emitted suspension
     real :: asf10bk    ! soil fraction pm10 in saltation breakage suspension
     real, dimension(:), allocatable :: asdpart   ! Soil layer average particle density adjusted from mineral only
                                              ! to include organic matter content
     real, dimension(:), allocatable :: aszlyd    ! Depth to bottom of each soil layer for each subregion (mm)
     real, dimension(:), allocatable :: asdwsrat  ! Nondimensional ratio of wet to settled bulk density
     real, dimension(:), allocatable :: asdblk0   ! Soil layer bulk density from previous day
                                              ! for use in hydro to update parameters based on bulk density changes

     real, dimension(:), allocatable :: ahrwc     ! Soil water content (Mg/Mg)
     real, dimension(:), allocatable :: ahrwcdmx  ! daily maximum soil water content (Mg/Mg)
     real, dimension(:), allocatable :: aheaep    ! Soil air entry potential (J/kg)
     real, dimension(:), allocatable :: ah0cb     ! Power of Brooks and Corey water release curve model (unitless)
     real, dimension(:), allocatable :: ahrsk     ! Saturated soil hydraulic conductivity (m/s)

     real, dimension(:), allocatable :: ahrwcr    ! Soil layer residual water content (Mg/Mg)
     real, dimension(:), allocatable :: ahrwcw    ! Soil layer wilting point water content (Mg/Mg)
     real, dimension(:), allocatable :: ahrwcf    ! Soil layer field capacity water content (Mg/Mg)
     real, dimension(:), allocatable :: ahrwcs    ! Soil layer saturated water content (Mg/Mg)
     real, dimension(:), allocatable :: ahrwca    ! Available soil layer water content (Mg/Mg)
     real, dimension(:), allocatable :: ahrwc1    ! Soil layer water content at 0.1 bar (Mg/Mg)
     real, dimension(:), allocatable :: ahfredsat ! fraction of soil porosity that will be filled with water
                                              ! while wetting under normal field conditions due to entrapped air
  end type soil_def

contains

  subroutine allocate_soil(soil)
     type(soil_def), intent(inout) :: soil

     ! local variable
     integer :: nsoillay
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! summation of status return values

     nsoillay = soil%nslay

     ! allocate below ground arrays
     sum_stat = 0

     ! intrinsic
     allocate(soil%asvroc(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfsan(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfsil(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfcla(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%as0ph(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfcce(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfcec(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfom(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdwblk(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdsblk(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdprocblk(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aslagn(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aslagx(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aseagm(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aseagmn(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aseagmx(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aslmin(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aslmax(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfcle(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfvcs(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfcs(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfms(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asffs(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfvfs(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asfwdc(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     ! state
     allocate(soil%aszlyt(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdblk(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdagd(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aseags(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aslagm(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%as0ags(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     ! derived
     allocate(soil%asdpart(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aszlyd(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdwsrat(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%asdblk0(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwc(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwcdmx(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%aheaep(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ah0cb(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrsk(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwcr(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwcw(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwcf(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwcs(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwca(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahrwc1(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(soil%ahfredsat(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for soil'
        stop 1
     end if
  end subroutine allocate_soil

  subroutine deallocate_soil(soil)
     type(soil_def), intent(inout) :: soil

     ! local variable
     integer :: dealloc_stat  ! deallocation status return
     integer :: sum_stat      ! summation of status return values

     ! deallocate below ground arrays
     sum_stat = 0

     ! intrinsic
     deallocate(soil%asvroc, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfsan, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfsil, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfcla, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%as0ph, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfcce, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfcec, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfom, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdwblk, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdsblk, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdprocblk, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aslagn, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aslagx, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aseagm, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aseagmn, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aseagmx, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aslmin, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aslmax, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfcle, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfvcs, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfcs, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfms, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asffs, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfvfs, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asfwdc, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     ! state
     deallocate(soil%aszlyt, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdblk, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdagd, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aseags, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aslagm, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%as0ags, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     ! derived
     deallocate(soil%asdpart, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aszlyd, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdwsrat, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%asdblk0, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwc, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwcdmx, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%aheaep, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ah0cb, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrsk, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwcr, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwcw, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwcf, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwcs, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwca, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahrwc1, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(soil%ahfredsat, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for soil'
     end if
  end subroutine deallocate_soil

end module soil_data_struct_defs

