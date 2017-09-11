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

     integer :: rg      ! seeding location in relation to ridge, 0 - plant in furrow, 1 - plant on ridge
     integer :: rsfg    ! row spacing flag
                        ! 0      o Broadcast Planting
                        ! 1      o Use Specified Row Spacing
                        ! 2      o Use Existing Ridge Spacing
     real :: xrow       ! row spacing (m)
     real :: dpop       ! Crop seeding density (#/m^2)


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

     real :: twarmdays      ! number of consecutive days that the temperature has been above the minimum growth temperature
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

  type biodatabase ! from c1db1, c1gen .inc
     integer :: idc     ! The crop type number (1 = annual, perennial, . . .)
                          ! 1,4 Summer Annual
                          ! 2,5 Winter Annual
                          ! 3,6 Perennial
                          ! 7   Biannual with tuber dormancy
                          ! 8   Perennial with staged crown bud release
     integer :: baflg   ! flag for biomass adjustment action
                          ! 0 - normal crop growth
                          ! 1 - find biomass adjustment factor for target yield
                          ! 2 - Use given biomass adjustment factor
     real :: baf        ! biomass adjustment factor
     real :: yraf       ! yield to biomass ratio adjustment factor
     integer :: tdtm    ! days from planting to maturity for summer crops, or the days
                        ! from start of spring growth to maturity for winter and perennial crops. 
     real :: thum       ! accumulated heat units from planting to maturity, or from 
                        ! start of growth to maturity for perennial crops
     real :: zmrt       ! Maximum crop root depth (m)
     real :: zmxc       ! Maximum crop height (m)
     real :: grf        ! Fraction of reproductive biomass that is grain (Mg/Mg)
     real :: ehu0       ! heat unit index leaf senescence starts
     real :: tverndel   ! thermal delay coefficient pre-vernalization
     real :: bceff      ! biomass conversion efficiency
     real :: alf        ! leaf mass partitioning coefficient a
     real :: blf        ! leaf mass partitioning coefficient b
     real :: clf        ! leaf mass partitioning coefficient c
     real :: dlf        ! leaf mass partitioning coefficient d
     real :: arp        ! reproductive mass partitioning coefficient a
     real :: brp        ! reproductive mass partitioning coefficient b
     real :: crp        ! reproductive mass partitioning coefficient c
     real :: drp        ! reproductive mass partitioning coefficient d
     real :: aht        ! plant height coefficient a
     real :: bht        ! plant height coefficient b
     real :: ssa        ! stem area to mass coefficient a, result is m^2 per plant
     real :: ssb        ! stem area to mass coefficient b, argument is kg per plant
     real :: hue        ! heat unit index where emergence is complete
     real :: dmaxshoot  ! maximum number of shoots possible from each plant
     integer :: transf  ! db input flag:
                          ! 0 = crop is planted using stored biomass of seed or vegatative propagants
                          ! 1 = crop is planted as a transplant with roots, stems and leaves present
     real :: storeinit  ! db input, crop storage root mass initialzation (mg/plant)
     real :: fshoot     ! crop ratio of shoot diameter to length
     real :: growdepth  ! depth of growing point at time of planting (m)
     real :: fleafstem  ! crop leaf to stem mass ratio for shoots
     real :: shoot      ! mass from root storage required for each regrowth shoot (mg/shoot)
                        ! seed shoots are smaller and adjusted for available seed mass
     real :: diammax    ! crop maximum plant diameter (m)

     real :: fleaf2stor ! fraction of assimilate partitioned to leaf that is diverted to root store
     real :: fstem2stor ! fraction of assimilate partitioned to stem that is diverted to root store
     real :: fstor2stor ! fraction of assimilate partitioned to standing storage (reproductive) that is diverted to root store
     real :: yld_coef   ! yield coefficient (kg/kg)     harvest_residue = acyld_coef(kg/kg) * Yield + acresid_int (kg/m^2)
     real :: resid_int  ! residue intercept (kg/m^2)   harvest_residue = acyld_coef(kg/kg) * Yield + acresid_int (kg/m^2)
     real :: zloc_regrow ! location of regrowth point (+ on stem, 0 or negative from crown)
     real :: topt       ! Optimal temperature for plant growth (deg C)
     real :: tmin       ! Minimum temperature for plant growth (deg C)
     real :: fd1(1:2)   ! xy coordinate for 1st pt on frost damage curve
     real :: fd2(1:2)   ! xy coordinate for 2nd pt on frost damage curve

     real :: ytgt       ! target yield (in units shown below)
     character*(80) :: ynmu ! string for name of units in which yield of interest will be reported
     real :: ycon       ! conversion factor from Kg/m^2 to units named in acynmu (all dry weight)
     real :: ywct       ! water content at which yield is to be reported (percent)
     integer :: thudf   ! heat units or days to maturity flag
                        ! 0      o Days to maturity and average conditions used to find heat units
                        ! 1      o Heat units specified used directly
     integer :: plant_day   ! planting date (day of month)
     integer :: plant_month ! planting date (month of rotation year)
     integer :: plant_rotyr ! planting date (rotation year)

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

  type bio_prevday
     real :: standstem    ! crop standing stem mass (kg/m^2)
     real :: standleaf    ! crop standing leaf mass (kg/m^2)
     real :: standstore   ! crop standing storage mass (kg/m^2) (head with seed, or vegetative head (cabbage, pineapple))
     real :: flatstem     ! crop flat stem mass (kg/m^2)
     real :: flatleaf     ! crop flat leaf mass (kg/m^2)
     real :: flatstore    ! crop flat storage mass (kg/m^2)
     real :: mshoot       ! mass of shoot growing from root storage biomass (kg/m^2)
     real :: mtotshoot    ! total mass of shoot growing from root storage biomass (kg/m^2)
                          ! in the period from beginning to completion of emegence heat units
     real, dimension(:), allocatable :: bgstemz      ! crop stem mass below soil surface by layer (kg/m^2)
     real, dimension(:), allocatable :: rootstorez   ! crop root storage mass by soil layer (kg/m^2)
                                                     ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
     real, dimension(:), allocatable :: rootfiberz   ! crop root fibrous mass by soil layer (kg/m^2)
     real :: ht           ! Crop height (m)
     real :: zshoot       ! length of actively growing shoot from root biomass (m)
     real :: stm          ! Number of crop stems per unit area (#/m^2)
                          ! It is computed by taking the tillering factor times the plant population density.
     real :: rtd          ! Crop root depth (m)
     integer :: dayap     ! number of days of growth completed since crop planted
     real :: hucum        ! crop accumulated heat units
     real :: rthucum      ! crop accumulated heat units with no vernalization/photoperiod delay
     real :: grainf       ! internally computed grain fraction of reproductive mass
     real :: chillucum    ! accumulated chilling units (days)
     real :: liveleaf     ! fraction of standing plant leaf which is living (transpiring)
     integer :: dayspring ! day of year in which a winter annual releases stored growth
     real :: cancov       ! crop canopy cover (fraction)
  end type bio_prevday

  type residue_pointer
     type(residue_pointer), pointer :: oldResidue
     integer :: resday    ! calendar days after residue initiation
     integer :: resyear   ! index counting each new residue initiation
     real :: cumdds       ! cumulative decomp days for standing res. by pool (days)
     real :: cumddf       ! cummlative decomp days for surface res. by pool (days)
     real, dimension(:), allocatable :: cumddg       ! cumm. decomp days below ground res by pool and layer (days)
     real :: standstem    ! crop standing stem mass (kg/m^2)
     real :: standleaf    ! crop standing leaf mass (kg/m^2)
     real :: standstore   ! crop standing storage mass (kg/m^2) (head with seed, or vegetative head (cabbage, pineapple))

     real :: flatstem    ! crop flat stem mass (kg/m^2)
     real :: flatleaf    ! crop flat leaf mass (kg/m^2)
     real :: flatstore   ! crop flat storage mass (kg/m^2)

     real :: flatrootstore ! crop flat root storage mass (kg/m^2)
                           ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
     real :: flatrootfiber ! crop flat root fibrous mass (kg/m^2)

     real, dimension(:), allocatable :: bgstemz    ! crop buried stem mass by layer (kg/m^2)
     real, dimension(:), allocatable :: bgleafz    ! crop buried leaf mass by layer (kg/m^2)
     real, dimension(:), allocatable :: bgstorez   ! crop buried storage mass by layer (kg/m^2)

     real, dimension(:), allocatable :: bgrootstorez ! crop root storage mass by layer (kg/m^2)
                                                     ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
     real, dimension(:), allocatable :: bgrootfiberz ! crop root fibrous mass by layer (kg/m^2)

     real :: zht    ! Crop height (m)
     real :: dstm   ! Number of crop stems per unit area (#/m^2)
                   ! It is computed by taking the tillering factor times the plant population density.
     real :: xstmrep   ! a representative diameter so that acdstm*acxstmrep*aczht=acrsai
     real :: zrtd      ! Crop root depth (m)
     real :: grainf    ! internally computed grain fraction of reproductive mass
  end type residue_pointer

  type plant_pointer
     type(plant_pointer), pointer :: oldPlant
     character*(80) :: bname       ! the name of the biomaterial
     type(bio_output_units) :: luo
     type(biostate_mass) :: mass
     type(biostate_geometry) :: geometry
     type(biostate_growth) :: growth
     type(bio_prevday) :: prev
     type(residue_pointer), pointer :: decomp
     type(bioderived) :: deriv
     type(biodatabase) :: database
  end type plant_pointer

  type plants_struct
     type(plant_pointer), pointer :: plant
  end type plants_struct

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
  end type biototal

  type decomp_factors
     real :: aqua    ! sum of precip, irrigation and snow melt (mm)
     integer :: weti     ! days since anticedent moisture (4 to 0) index
     real :: iwcsy       ! daily water coefficient from previous day standing res.  (0 to 1)
     real :: idds   ! daily decomposition day for standing residue (0 to 1)
     real :: itcs   ! daily temperature coef. for standing residue (0 to 1)
     real :: iwcs   ! daily water coefficient for standing residue (0 to 1)
!     real :: itca   ! daily temperature coef. for above ground res. (0 to 1) (removed to allow different temperatures for standing vs flat)
     real :: iddf   ! daily decomposition day for surface residue (0 to 1)
     real :: itcf   ! daily temperature coef. for surface residue (0 to 1)
     real :: iwcf   ! daily water coefficient for surface residue (0 to 1)
     real, dimension(:), pointer :: iddg   ! decomp. day for below ground residue by soil layer (0 to 1)
     real, dimension(:), pointer :: itcg   ! temperature coef. below ground res. by soil layer (0 to 1)
     real, dimension(:), pointer :: iwcg   ! water coef. for below ground res. by soil layer (0 to 1)
  end type decomp_factors

  integer, parameter :: ncanlay = 5

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

  function create_biomatter(nsoillay) result(biomat)
     integer, intent(in) :: nsoillay
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

  function create_biototal(nsoillay) result(biotot)
     integer, intent(in) :: nsoillay
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

  function create_bio_prevday(nsoillay) result(prevday)
     integer, intent(in) :: nsoillay
     type(bio_prevday) :: prevday

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(prevday%bgstemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(prevday%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(prevday%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for bio_prevday'
        stop 1
     end if
  end function create_bio_prevday

  subroutine destroy_bio_prevday(prevday)
     type(bio_prevday), intent(inout) :: prevday

     ! local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     ! allocate below and above ground arrays
     deallocate(prevday%bgstemz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(prevday%rootstorez, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat
     deallocate(prevday%rootfiberz, stat=dealloc_stat)
     sum_stat = sum_stat + dealloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for biomatter'
     end if
  end subroutine destroy_bio_prevday

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

  function plantCreate(plantPntr, nsoillay) result(plantNew)
     type(plant_pointer), pointer :: plantPntr
     integer, intent(in) :: nsoillay
     type(plant_pointer), pointer :: plantNew

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     allocate(plantPntr, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to allocate new Plant pointer.'
     end if

     ! initialize pointer to NULL
     nullify(plantPntr%oldPlant)

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(plantPntr%mass%stemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%mass%leafz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%mass%storez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%mass%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%mass%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(plantPntr%prev%bgstemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%prev%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%prev%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(plantPntr%deriv%mrtz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%deriv%mbgz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(plantPntr%deriv%rsaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantPntr%deriv%rlaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for plantPntr'
        stop 1
     end if

     ! new plant, no decomp yet
     nullify(plantPntr%decomp)

     plantNew => plantPntr
        
  end function plantCreate

  function plantAdd(plantPntr, nsoillay) result(plantNew)
     type(plant_pointer), pointer :: plantPntr
     integer, intent(in) :: nsoillay
     type(plant_pointer), pointer :: plantNew

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     allocate(plantNew, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to allocate new Plant pointer.'
     end if

     sum_stat = 0
     ! allocate below and above ground arrays
     allocate(plantNew%mass%stemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%mass%leafz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%mass%storez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%mass%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%mass%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(plantNew%prev%bgstemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%prev%rootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%prev%rootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(plantNew%deriv%mrtz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%deriv%mbgz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(plantNew%deriv%rsaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(plantNew%deriv%rlaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for plantNew'
        stop 1
     end if

     ! new plant, no decomp yet
     nullify(plantNew%decomp)

     ! point to previous plant
     plantNew%oldPlant => plantPntr

  end function plantAdd

  subroutine plantDestroy(plantPntr)
     type(plant_pointer), pointer :: plantPntr

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
     type(residue_pointer), pointer :: residuePntr

     ! check for older plants
     if( associated(plantPntr%oldPlant) ) then
        write(*,*) 'ERROR: older Plant exists, unable to execute plantDestroy'
        stop 1       
     end if

     sum_stat = 0
     ! deallocate below and above ground arrays
     deallocate(plantPntr%mass%stemz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%mass%leafz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%mass%storez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%mass%rootstorez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%mass%rootfiberz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     deallocate(plantPntr%prev%bgstemz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%prev%rootstorez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%prev%rootfiberz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     deallocate(plantPntr%deriv%mrtz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%deriv%mbgz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     deallocate(plantPntr%deriv%rsaz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(plantPntr%deriv%rlaz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for plantPntr'
        stop 1
     end if

     ! remove all decomp mass for this plant
     do while( associated(plantPntr%decomp) )
        residuePntr => plantPntr%decomp
        do while( associated(residuePntr) )
           if( associated(residuePntr%oldResidue) ) then
              ! older residue exists, point to it
              residuePntr => residuePntr%oldResidue
           else
              ! this is the oldest residue, delete it
              call residueDestroy(residuePntr)
           end if
        end do
     end do
        
     ! delete memory and nullify
     deallocate(plantPntr, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to deallocate Plant pointer.'
     end if
     nullify(plantPntr)

  end subroutine plantDestroy

  function residueCreate(residuePntr, nsoillay) result(residueNew)
     type(residue_pointer), pointer :: residuePntr
     integer, intent(in) :: nsoillay
     type(residue_pointer), pointer :: residueNew

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     allocate(residuePntr, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to allocate new Residue pointer.'
     end if

     ! initialize pointer to NULL
     nullify(residuePntr%oldResidue)

      sum_stat = 0
     allocate(residuePntr%cumddg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residuePntr%bgstemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residuePntr%bgleafz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residuePntr%bgstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residuePntr%bgrootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residuePntr%bgrootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for residuePntr'
        stop 1
     end if

     residueNew => residuePntr

  end function residueCreate

  function residueAdd(residuePntr, nsoillay) result(residueNew)
     type(residue_pointer), pointer :: residuePntr
     integer, intent(in) :: nsoillay
     type(residue_pointer), pointer :: residueNew

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     allocate(residueNew, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to allocate new Plant pointer.'
     end if

     sum_stat = 0
     allocate(residueNew%cumddg(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%bgstemz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%bgleafz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%bgstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%bgrootstorez(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%bgrootfiberz(nsoillay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for residueNew'
        stop 1
     end if

     residueNew%oldResidue => residuePntr

  end function residueAdd

  subroutine residueDestroy(residuePntr)
     type(residue_pointer), pointer :: residuePntr

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     ! check for older residue
     if( associated(residuePntr%oldResidue) ) then
        write(*,*) 'ERROR: older Residue exists, unable to execute residueDestroy'
        stop 1       
     end if

     sum_stat = 0
     deallocate(residuePntr%cumddg, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%bgstemz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%bgleafz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%bgstorez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%bgrootstorez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%bgrootfiberz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for residuePntr'
        stop 1
     end if

     deallocate(residuePntr, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to allocate new Residue pointer.'
     end if
     nullify(residuePntr)

  end subroutine residueDestroy

end module biomaterial



