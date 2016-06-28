!$Author$
!$Date$
!$Revision$
!$HeadURL$

module biomaterial
  implicit none

  ! defines mass of all plant parts
  type biostate_mass
     real :: standstem      ! standing stem mass (kg/m^2)
     real :: standleaf      ! standing leaf mass (kg/m^2)
     real :: standstore     ! standing storage mass (kg/m^2) (head with seed, or vegetative head (cabbage, pineapple))
     real :: flatstem       ! flat stem mass (kg/m^2)
     real :: flatleaf       ! flat leaf mass (kg/m^2)
     real :: flatstore      ! flat storage mass (kg/m^2)
     real :: flatrootstore  ! flat storage root mass (kg/m^2)
     real :: flatrootfiber  ! flat fibrous root mass (kg/m^2)
     ! defines mass of plant parts that are below ground by soil layer
     ! note: in this context, allocatable does not work, pointer does!
     real, dimension(:), pointer :: stemz          ! buried stem mass by layer (kg/m^2)
     real, dimension(:), pointer :: leafz          ! buried leaf mass by layer (kg/m^2)
     real, dimension(:), pointer :: storez         ! buried (from above ground) storage mass by layer (kg/m^2)
     real, dimension(:), pointer :: rootstorez     ! buried storage root mass by layer (kg/m^2)
                                                   ! tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts)
     real, dimension(:), pointer :: rootfiberz     ! buried fibrous root mass by layer (kg/m^2)
  end type biostate_mass

  type biostate_geometry
     real :: zht            ! "stem" height (m)
     real :: dstm           ! Number of stems per unit area (#/m^2)
     real :: xstmrep        ! a representative diameter so that dstm*xstmrep*zht=rsai
     real :: grainf         ! internally computed grain fraction of reproductive mass
     integer :: hyfg        ! flag indicating the part of plant to which the "grain fraction" GRF is applied
                            ! when removing that plant part for yield
                            ! 0     GRF applied to above ground storage (seeds, reproductive)
                            ! 1     GRF times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
                            ! 2     GRF applied to all aboveground biomass (forage)
                            ! 3     GRF applied to leaf mass (tobacco)
                            ! 4     GRF applied to stem mass (sugarcane)
                            ! 5     GRF applied to below ground storage mass (potatoes, peanuts)
     real :: zshoot         ! length of actively growing shoot from root biomass (m)
     real :: zrtd           ! root depth (m)
  end type biostate_geometry

  type biostate_growth
     logical :: am0cgf      ! flag if set to .true. then run CROP growth subroutines.
     logical :: am0cif      ! flag if set to .true. then run CROP growth initialization subroutine.
     real :: thucum         ! crop accumulated heat units
     real :: trthucum       ! accumulated root growth heat units (degree-days)

     real :: zgrowpt        ! depth in the soil of the growing point (m)
     real :: fliveleaf      ! fraction of standing plant leaf which is living (transpiring)
     real :: leafareatrend  ! direction in which leaf area is trending.
                            ! Saves trend even if leaf area is static for long periods.
     real :: stemmasstrend  ! direction in which stem mass is trending.
                            ! Saves trend even if stem mass is static for long periods.

     integer :: twarmdays      ! number of consecutive days that the temperature has been above the minimum growth temperature
     real :: tchillucum     ! accumulated chilling units (days)
     real :: thardnx        ! hardening index for winter annuals (range from 0 t0 2)

     real :: thu_shoot_beg  ! heat unit total for beginning of shoot grow from root storage period
     real :: thu_shoot_end  ! heat unit total for end of shoot grow from root storage period
     real :: mshoot         ! crop shoot mass grown from root storage (kg/m^2)
                            ! this is a "breakout" mass and does not represent a unique pool
                            ! since this mass is destributed into below ground stem and
                            ! standing stem as each increment of the shoot is added
     real :: mtotshoot      ! total mass of shoot growing from root storage biomass (kg/m^2)
                            ! in the period from beginning to completion of emegence heat units

     integer :: dayap       ! number of days of growth completed since crop planted
     integer :: dayam       ! number of days since crop matured
     integer :: dayspring   ! day of year in which a winter annual released stored growth
  end type biostate_growth

  type biostate_decomp    ! from decomp/decomp.inc
     integer :: resday    ! calendar days after residue initiation
     integer :: resyear   ! index counting each new residue initiation
     real :: cumdds       ! cumulative decomp days for standing res. by pool (days)
     real :: cumddf       ! cummlative decomp days for surface res. by pool (days)
     real, dimension(:), pointer :: cumddg       ! cumm. decomp days below ground res by pool and layer (days)
  end type biostate_decomp

  type bioderived
     real :: mbgstem      ! buried residue stem mass (kg/m^2)
     real :: mbgleaf      ! buried residue leaf mass (kg/m^2)
     real :: mbgstore     ! buried residue storage mass (kg/m^2)

     real :: mbgrootstore ! buried storage root mass (kg/m^2)
                          ! tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts)

     real :: mbgrootfiber ! buried fibrous root mass (kg/m^2)

     real :: m            ! Total mass (standing + flat + roots + buried) (kg/m^2)
     real :: mst          ! Standing mass (standstem + standleaf + standstore) (kg/m^2)
     real :: mf           ! Flat mass (flatstem + flatleaf + flatstore) (kg/m^2)
     real :: mrt          ! Buried root mass (rootfiber + rootstore)(kg/m^2)
     real :: mbg          ! Buried mass (kg/m^2) Excludes root mass below the surface.
     real, dimension(:), pointer :: mrtz           ! Buried root mass by soil layer (kg/m^2)
     real, dimension(:), pointer :: mbgz           ! Buried mass by soil layer (kg/m^2)

     real :: rsai         ! Residue stem area index (m^2/m^2)
     real :: rlai         ! Residue leaf area index (m^2/m^2)
     real, dimension(:), pointer :: rsaz           ! stem area index by height (1/m)
     real, dimension(:), pointer :: rlaz           ! leaf area index by height (1/m)

     real :: rcd          ! effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
                          ! (combination of leaf area and stem area indices)
     real :: ffcv         ! biomass cover - flat (m^2/m^2)
     real :: fscv         ! biomass cover - standing (m^2/m^2)
     real :: ftcv         ! biomass cover - total (m^2/m^2) (ffcv + fscv)
     real :: fcancov      ! fraction of soil surface covered by canopy (m^2/m^2)
  end type bioderived

  type biodatabase ! from c1db1.inc, c1gen.inc
     real, dimension(1:5) :: dkrate ! array of decomposition rate parameters
                                    ! acdkrate(1) - standing residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(2) - flat residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(3) - buried residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(4) - root residue mass decomposition rate (d<1) (g/g/day)
                                    ! acdkrate(5) - stem residue number decline rate (d<1) (#/m^2/day)? (fall rate)
     real :: ddsthrsh     ! threshhold number of decomp. days before stems begin to fall
     real :: xstm         ! mature crop stem diameter (m)
     real :: covfact      ! flat residue cover factor (m^2/kg)
     real :: resevapa     ! coefficient a in relation ea/ep = exp(resevapa * (flat mass kg/m^2)**resevapb)
     real :: resevapb     ! coefficient b in relation ea/ep = exp(resevapa * (flat mass kg/m^2)**resevapb)
     real :: sla          ! residue specific leaf area
     real :: ck           ! residue light extinction coeffficient (fraction)
     integer :: rbc       ! residue burial class
                          ! 1   o Fragile-very small (soybeans) residue
                          ! 2   o Moderately tough-short (wheat) residue
                          ! 3   o Non fragile-med (corn) residue
                          ! 4   o Woody-large residue
                          ! 5   o Gravel-rock
  end type biodatabase

  type bio_output_units
     integer :: dec
  end type bio_output_units

  type biomatter
     character*(80) :: bname       ! the name of the biomaterial
     type(bio_output_units) :: luo
     type(biostate_mass) :: mass
     type(biostate_geometry) :: geometry
     type(biostate_growth) :: growth
     type(biostate_decomp) :: decomp
     type(bioderived) :: deriv
     type(biodatabase) :: database
  end type biomatter


  type biototal
     real :: dstmtot      ! total number of stems  per unit area (#/m^2)
     real :: zht_ave      ! Weighted ave height across pools (m)
     real :: zmht         ! Tallest biomass height across pools (m)
     real :: xstmrep      ! a representative diameter so that dstm*xstmrep*zht=rsai
     real :: zrtd         ! root depth (m)

     real :: mstandstore  ! Total reproductive mass (standing) (kg/m^2)
     real :: mflatstore   ! Total reproductive mass (flat) (kg/m^2)
     real :: mtot         ! Total mass across pools (standing + flat + roots + buried) (kg/m^2)
     real :: mtotto4      ! Total mass across pools (standing + flat + roots + buried to a 4 inch depth) (kg/m^2)
     real :: msttot       ! Standing mass across pools (standstem + standleaf + standstore) (kg/m^2)
     real :: mftot        ! Flat mass across pools (flatstem + flatleaf + flatstore) (kg/m^2)
     real :: mbgtot       ! Buried mass across pools (kg/m^2)
     real :: mbgtotto4    ! Buried (to a 4 inch depth) mass across pools (kg/m^2)
     real :: mbgtotto15   ! Buried (to a 15 cm depth) mass across pools (kg/m^2)
     real :: mrttot       ! Buried root mass across pools (kg/m^2)
     real :: mrttotto4    ! Buried (to a 4 inch depth) root mass across pools (kg/m^2)
     real :: mrttotto15   ! Buried (to a 15 cm depth) root mass across pools (kg/m^2)
     real, dimension(:), pointer :: mrtz           ! Buried root mass by soil layer (kg/m^2)
     real, dimension(:), pointer :: mbgz           ! Buried mass by soil layer (kg/m^2)

     real :: rsaitot      ! total of stem area index across pools (m^2/m^2)
     real :: rlaitot      ! total of leaf area index across pools (m^2/m^2)
     real, dimension(:), pointer :: rsaz           ! stem area index by height (1/m)
     real, dimension(:), pointer :: rlaz           ! leaf area index by height (1/m)

     real :: rcdtot       ! effective Biomass silhouette area across pools (SAI+LAI) (m^2/m^2)
                          ! (combination of leaf area and stem area indices)

     real :: ffcvtot      ! biomass cover across pools - flat (m^2/m^2)
     real :: fscvtot      ! biomass cover across pools - standing (m^2/m^2)
     real :: ftcvtot      ! biomass cover across pools - total (m^2/m^2)
                          ! (adffcvtot + adfscvtot)
     real :: ftcancov     ! fraction of soil surface covered by canopy across pools (m^2/m^2)
     real :: evapredu     ! composite evaporation reduction from across pools (ea/ep ratio)

     real :: xrow         ! row spacing (m)
     integer :: c0rg      ! seeding location in relation to ridge, 0 - plant in furrow, 1 - plant on ridge

!     abdstm - Total number of stems (#/m^2) (live and dead) May be a weighted summation.
!     abzht  - Composite weighted average biomass height (m)
!     abzmht - Tallest biomass height (m) greatest of daily crop or residue height
!     abm - Total biomass (kg/m^2) standing + roots + flat + buried + yield
!     abmst - Standing biomass - above ground (kg/m^2)
!     abmf    - Flat biomass (kg/m^2)
!     abmbg - Buried biomass (kg/m^2)
!     abmrt - Buried root biomass (kg/m^2)
!     abmbgz - Buried biomass by soil layer (kg/m^2)
!     abmrtz - Buried root biomass by soil layer (kg/m^2)
!     abrsai - Biomass stem area index (m^2/m^2)
!     abrlai - Biomass leaf area index (m^2/m^2)
!     abrcd  - effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
!              (combination of leaf area and stem area indices)
!     abrsaz - Biomass stem area index by height (1/m)
!     abrlaz - Biomass leaf area index by height (1/m)
!     abffcv - Biomass cover - flat  (m^2/m^2)
!     abfscv - Biomass cover - standing  (m^2/m^2)
!     abftcv - Biomass cover - total  (m^2/m^2)
!              (sum of abffcv and abfscv)
!     abfcancov - fraction of soil surface covered by all canopy (m^2/m^2)
!     abevapredu - composite evaporation reduction from crop and residue materials (ea/ep ratio)
!     acxrow - Crop row spacing (m)
!     ac0rg  - Crop seeding location in relation to ridge
!         0     o plant in furrow
!         1     o plant on ridge

  end type biototal

  type decomp_factors
     real :: aqua    ! sum of precip, irrigation and snow melt (mm)
     integer :: weti     ! days since anticedent moisture (4 to 0) index
     real :: iwcsy       ! daily water coefficient from previous day standing res.  (0 to 1)
     real :: idds   ! daily decomposition day for standing residue (0 to 1)
     real :: itcs   ! daily temperature coef. for above ground res. (0 to 1)
     real :: iwcs   ! daily water coefficient for standing residues (0 to 1)
!     real :: itca   ! daily temperature coef. for above ground res. (0 to 1) (removed to allow different temperatures for standing vs flat)
     real :: iddf   ! daily decomposition day for surface residue (0 to 1)
     real :: itcf   ! daily temperature coef. for above ground res. (0 to 1)
     real :: iwcf   ! daily water coefficient for surface residues (0 to 1)
     real, dimension(:), pointer :: iddg   ! decomp. day for below ground residue by soil layer (0 to 1)
     real, dimension(:), pointer :: itcg   ! temperature coef. below ground res. by soil layer (0 to 1)
     real, dimension(:), pointer :: iwcg   ! water coef. for below ground res. by soil layer (0 to 1)
  end type decomp_factors

contains

  subroutine print_biomatter(biomat)
     type(biomatter), intent(in) :: biomat

     integer :: idx

     write(*,*) 'biomatter name: ', trim(adjustl(biomat%bname))
     write(*,*) 'output unit:    ', biomat%luo%dec
     write(*,*) 'mass standing   ', biomat%mass%standstem, biomat%mass%standleaf, biomat%mass%standstore
     write(*,*) 'mass flat       ', biomat%mass%flatstem, biomat%mass%flatleaf, biomat%mass%flatstore, &
                                    biomat%mass%flatrootstore, biomat%mass%flatrootfiber
     do idx = 1, size(biomat%mass%rootstorez)
        write(*,*) 'mass buried ', idx, biomat%mass%stemz(idx), biomat%mass%leafz(idx), biomat%mass%storez(idx), &
                                        biomat%mass%rootstorez(idx), biomat%mass%rootfiberz(idx)
     end do
     !write(*,*) '', biomat%geometry
     !write(*,*) '', biomat%growth
     !write(*,*) '', biomat%decomp
     !write(*,*) '', biomat%deriv
     !write(*,*) '', biomat%database
  end subroutine print_biomatter

  subroutine print_biototal(biotot)
     type(biomatter), intent(in) :: biotot

  end subroutine print_biototal

  function create_biomatter(nsoillay, ncanlay) result(biomat)
     integer, intent(in) :: nsoillay
     integer, intent(in) :: ncanlay
     type(biomatter) :: biomat

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(biomat%mass%stemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%mass%leafz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%mass%storez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%mass%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%mass%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(biomat%decomp%cumddg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(biomat%deriv%mrtz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%deriv%mbgz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(biomat%deriv%rsaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biomat%deriv%rlaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for biomatter'
        stop 1
     end if
  end function create_biomatter

  subroutine destroy_biomatter(biomat)
     type(biomatter), intent(inout) :: biomat

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     deallocate(biomat%mass%stemz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%mass%leafz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%mass%storez, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%mass%rootstorez, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%mass%rootfiberz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     deallocate(biomat%decomp%cumddg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     deallocate(biomat%deriv%mrtz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%deriv%mbgz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     deallocate(biomat%deriv%rsaz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biomat%deriv%rlaz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for biomatter'
     end if
  end subroutine destroy_biomatter

  function create_biototal(nsoillay, ncanlay) result(biotot)
     integer, intent(in) :: nsoillay
     integer, intent(in) :: ncanlay
     type(biototal) :: biotot

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(biotot%mrtz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biotot%mbgz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(biotot%rsaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(biotot%rlaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for biototal'
        stop 1
     end if
  end function create_biototal

  subroutine destroy_biototal(biotot)
     type(biototal), intent(inout) :: biotot

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     deallocate(biotot%mrtz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biotot%mbgz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     deallocate(biotot%rsaz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(biotot%rlaz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for biomatter'
     end if
  end subroutine destroy_biototal

  function create_decomp_factors(nsoillay) result(decompfac)
     integer, intent(in) :: nsoillay
     type(decomp_factors) :: decompfac

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below ground arrays
     allocate(decompfac%iddg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(decompfac%itcg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(decompfac%iwcg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for decompfac'
        stop 1
     end if
  end function create_decomp_factors

  subroutine destroy_decomp_factors(decompfac)
     type(decomp_factors), intent(inout) :: decompfac

     ! local variable
     integer :: dealloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     deallocate(decompfac%iddg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(decompfac%itcg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(decompfac%iwcg, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( dealloc_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for decompfac'
        stop 1
     end if
  end subroutine destroy_decomp_factors

end module biomaterial



