!$Author$
!$Date$
!$Revision$
!$HeadURL$

module biomaterial

  use upgm_mod
  use environment_state_mod

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
     double precision :: xstmrep        ! a representative diameter so that dstm*xstmrep*zht=rsai
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
                        ! 1      o Uses Specified Row Spacing
                        ! 2      o Uses Existing Ridge Spacing
     real :: xrow       ! row spacing (m)
     real :: dpop       ! Crop seeding density (#/m^2)
     real :: zfurcut      ! estimated furrow bottom depth below flat soil surface (mm)
     real :: ztransprtmin ! root depth where transpiration depth reduction begins (m)
     real :: ztransprtmax ! root depth where transpiration depth equals root depth (m)
  end type biostate_geometry

  type biostate_growth
     logical :: am0cif      ! flag if set to .true. then run CROP growth initialization subroutine.
     logical :: growing     ! flag set to indicate that crop is growing
     logical :: shoot_growing     ! flag set to indicate that shoot growth occuring
     logical :: can_regrow  ! flag set to indicate that crop is able to regrow (past bc0hue, partition to root store)
     logical :: do_regrow   ! flag set to indicate that regrow has been triggered
     logical :: can_harden  ! flag set to indicate that crop respond to hardening stimulus
     logical :: lastday     ! flag set to indicate the last day of crop growth
     double precision :: thucum         ! crop accumulated heat units
     double precision :: trthucum       ! accumulated root growth heat units (degree-days)

     real :: zgrowpt        ! depth in the soil of the growing point (m)
     double precision :: fliveleaf      ! fraction of standing plant leaf which is living (transpiring)
     double precision :: leafareatrend  ! direction in which leaf area is trending.
                            ! Saves trend even if leaf area is static for long periods.
     double precision :: stemmasstrend  ! direction in which stem mass is trending.
                            ! Saves trend even if stem mass is static for long periods.

     double precision :: twarmdays      ! number of days that the temperature has been above the minimum growth temperature with decay
     double precision :: tcolddays      ! number of days that the temperature has been below the minimum growth temperature with decay
     double precision :: tchillucum     ! accumulated chilling units (days)
     double precision :: thardnx        ! hardening index for winter annuals (range from 0 t0 2)

     double precision :: thu_shoot_beg  ! heat unit total for beginning of shoot grow from root storage period
     double precision :: thu_shoot_end  ! heat unit total for end of shoot grow from root storage period
     double precision :: mtotleaf       ! total mass released from root storage biomass (kg/m^2)
                                        ! in the period from beginning to completion of leaf emergence heat units
     double precision :: thu_leaf_beg   ! heat unit index (fraction) for beginning of leaf emergence from root storage period
     double precision :: thu_leaf_end   ! heat unit index (fraction) for end of leaf emergence from root storage period
     real :: mshoot         ! crop shoot mass grown from root storage (kg/m^2)
                            ! this is a "breakout" mass and does not represent a unique pool
                            ! since this mass is destributed into below ground stem and
                            ! standing stem as each increment of the shoot is added
     real :: mtotshoot      ! total mass of shoot growing from root storage biomass (kg/m^2)
                            ! in the period from beginning to completion of emegence heat units

     integer :: dayap       ! number of days of growth completed since crop planted
     integer :: dayam       ! number of days since crop matured
     integer :: dayspring   ! day of year in which a winter annual/perennial released stored growth
     integer :: dayfall     ! day of year in which a deciduous/evergreen perennial dropped all/some leaves/needles

     real :: ptp            ! plant transpiration potential
     real :: pta            ! plant transpiration actual
     real :: fwsf           ! Crop growth water stress factor (unitless) (0.0 - no growth, 1.0 - full growth)
  end type biostate_growth

  type bioderived
     real :: mbgstem      ! buried stem mass (kg/m^2)
     real :: mbgleaf      ! buried leaf mass (kg/m^2)
     real :: mbgstore     ! buried storage mass (kg/m^2)
     real :: mbgrootstore ! buried storage root mass (kg/m^2)
                          ! tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts)
     real :: mbgrootfiber ! buried fibrous root mass (kg/m^2)

     real :: m            ! Total mass (standing + flat + roots + buried) (kg/m^2)
     real :: mst          ! Standing mass (standstem + standleaf + standstore) (kg/m^2)
     real :: mf           ! Flat mass (flatstem + flatleaf + flatstore + flatrootstore + flatrootfiber) (kg/m^2)
     real :: mrt          ! Buried root mass (rootfiber + rootstore)(kg/m^2)
     real :: mbg          ! Buried mass (kg/m^2) Excludes root mass below the surface.
     real :: dmrtto4      ! Buried root mass (rootfiber + rootstore)(kg/m^2) (in SCI depth)
     real :: dmbgto4      ! Buried mass (kg/m^2) Excludes root mass below the surface. (in SCI depth)
     real :: dmrtto15     ! Buried root mass (rootfiber + rootstore)(kg/m^2) (in WEPP depth)
     real :: dmbgto15     ! Total mass (standing + flat + roots + buried) (kg/m^2) (in WEPP depth)
     real, dimension(:), pointer :: mrtz           ! Buried root mass by soil layer (kg/m^2)
     real, dimension(:), pointer :: mbgz           ! Buried mass by soil layer (kg/m^2)

     double precision :: rsai         ! stem area index (m^2/m^2)
     real :: rlai         ! leaf area index (m^2/m^2)
     real, dimension(:), pointer :: rsaz           ! stem area index by height (1/m)
     real, dimension(:), pointer :: rlaz           ! leaf area index by height (1/m)

     real :: rcd          ! effective Biomass silhouette area (SAI+LAI) (m^2/m^2)
                          ! (combination of leaf area and stem area indices)
     real :: ffcv         ! biomass cover - flat (m^2/m^2)
     real :: fscv         ! biomass cover - standing (m^2/m^2)
     real :: ftcv         ! biomass cover - total (m^2/m^2) (ffcv + fscv)
     real :: fcancov      ! fraction of soil surface covered by canopy (m^2/m^2)
     real :: ztranspdepth ! depth in soil from which transpiration is extracted (m)
                          ! when crop is furrow planted, this is deeper than root depth
                          ! and is used in place of it when calling transp subroutine
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
                          ! 2 - Uses given biomass adjustment factor
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
     integer :: plant_doy   ! planting date (day of year)
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
     integer :: num       ! sequence number for pool
     integer :: luo       ! logical unit output number created when file opened
  end type bio_output_units

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
     real, dimension(:), allocatable :: stemz      ! crop stem mass below soil surface by layer (kg/m^2)
     real, dimension(:), allocatable :: rootstorez   ! crop root storage mass by soil layer (kg/m^2)
                                                     ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
     real, dimension(:), allocatable :: rootfiberz   ! crop root fibrous mass by soil layer (kg/m^2)
     real :: ht           ! Crop height (m)
     real :: zshoot       ! length of actively growing shoot from root biomass (m)
     real :: stm          ! Number of crop stems per unit area (#/m^2)
                          ! It is computed by taking the tillering factor times the plant population density.
     real :: rtd          ! Crop root depth (m)
     integer :: dayap     ! number of days of growth completed since crop planted
     double precision :: hucum        ! crop accumulated heat units
     double precision :: rthucum      ! crop accumulated heat units with no vernalization/photoperiod delay
     real :: grainf       ! internally computed grain fraction of reproductive mass
     double precision :: chillucum    ! accumulated chilling units (days)
     double precision :: liveleaf     ! fraction of standing plant leaf which is living (transpiring)
     integer :: dayspring ! day of year in which a winter annual/perennial releases stored growth
     integer :: dayfall   ! day of year in which a deciduous/evergreen perennial dropped all/some leaves/needles
     real :: cancov       ! crop canopy cover (fraction)
  end type bio_prevday

  type residue_pointer
     type(residue_pointer), pointer :: olderResidue
     type(bio_output_units) :: bout
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

     real, dimension(:), allocatable :: stemz    ! crop buried stem mass by layer (kg/m^2)
     real, dimension(:), allocatable :: leafz    ! crop buried leaf mass by layer (kg/m^2)
     real, dimension(:), allocatable :: storez   ! crop buried storage mass by layer (kg/m^2)

     real, dimension(:), allocatable :: rootstorez ! crop root storage mass by layer (kg/m^2)
                                                     ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
     real, dimension(:), allocatable :: rootfiberz ! crop root fibrous mass by layer (kg/m^2)

     real :: zht    ! Crop height (m)
     real :: dstm   ! Number of crop stems per unit area (#/m^2)
                   ! It is computed by taking the tillering factor times the plant population density.
     double precision :: xstmrep   ! a representative diameter so that acdstm*acxstmrep*aczht=acrsai
     real :: zrtd      ! Crop root depth (m)
     real :: grainf    ! internally computed grain fraction of reproductive mass
     type(bioderived) :: deriv

  end type residue_pointer

  type plant_pointer
     type(plant_pointer), pointer :: olderPlant
     character*(80) :: bname       ! the name of the plant
     integer :: pday               ! day of month it was planted
     integer :: pmon               ! month it was planted
     integer :: psimyr             ! simulation year it was planted
     integer :: residueIndex     ! index for all residue pools created under this plant
     type(bio_output_units) :: bout
     type(biostate_mass) :: mass
     type(biostate_geometry) :: geometry
     type(biostate_growth) :: growth
     type(bio_prevday) :: prev
     type(residue_pointer), pointer :: residue
     type(bioderived) :: deriv
     type(biodatabase) :: database
     type(upgm) :: upgm_grow
     type(environment_state) :: env
  end type plant_pointer

  type plants_struct
     type(plant_pointer), pointer :: plant
     integer :: plantIndex ! index used for detailed plant/residue output
  end type plants_struct

  type biototal
     real :: dstmtot      ! total number of stems  per unit area (#/m^2)
     real :: zht_ave      ! Weighted ave height across pools (m)
     real :: zmht         ! Tallest biomass height across pools (m)
     double precision :: xstmrep      ! a representative diameter so that dstm*xstmrep*zht=rsai
     integer :: dayap     ! most recent planting (days)

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
     real :: rlailive     ! living leaf area
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

  function create_decomp_factors(nsoillay) result(decompfac)
     integer, intent(in) :: nsoillay
     type(decomp_factors) :: decompfac

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
     integer :: idx         ! soil lyer loop index

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

     ! set intial values
     decompfac%aqua = 0.0
     decompfac%weti = 0
     decompfac%iwcsy = 0.0
     decompfac%idds = 0.0
     decompfac%itcs = 0.0
     decompfac%iwcs = 0.0
     decompfac%iddf = 0.0
     decompfac%itcf = 0.0
     decompfac%iwcf = 0.0

     do idx = 1, nsoillay
       decompfac%iddg(idx) = 0.0
       decompfac%itcg(idx) = 0.0
       decompfac%iwcg(idx) = 0.0
     end do

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

  function plantAdd(plantPntr, plantIndex, nsoillay) result(plantNew)
     type(plant_pointer), pointer :: plantPntr
     integer, intent(inout) :: plantIndex      ! index used for detailed plant/residue output
     integer, intent(in) :: nsoillay
     type(plant_pointer), pointer :: plantNew

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
     integer :: idx

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

     allocate(plantNew%prev%stemz(nsoillay), stat=alloc_stat)
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

     ! new plant, no residue yet
     nullify(plantNew%residue)

     if( associated(plantPntr) ) then
        ! point to previous plant
        plantNew%olderPlant => plantPntr
     else
        ! this is the first plant
        nullify(plantNew%olderPlant)
     end if

     ! upgm inititalized to null
     nullify(plantNew%upgm_grow%plant)

     ! increment global plant index
     plantIndex = plantIndex + 1
     ! carry index along with plant
     plantNew%bout%num = plantIndex
     ! initialize residue index for this plant (this passes the plant number into the file name string)
     plantNew%residueIndex = 0
     ! show output unit number as uninitialized
     plantNew%bout%luo = -1

     ! initialize all values
     plantNew%mass%standstem = 0.0
     plantNew%mass%standleaf = 0.0
     plantNew%mass%standstore = 0.0
     plantNew%mass%flatstem = 0.0
     plantNew%mass%flatleaf = 0.0
     plantNew%mass%flatstore = 0.0
     plantNew%mass%flatrootstore = 0.0
     plantNew%mass%flatrootfiber = 0.0
     do idx = 1, nsoillay
        plantNew%mass%stemz(idx) = 0.0
        plantNew%mass%leafz(idx) = 0.0
        plantNew%mass%storez(idx) = 0.0
        plantNew%mass%rootstorez(idx) = 0.0
        plantNew%mass%rootfiberz(idx) = 0.0
     end do

     plantNew%geometry%xrow = 0.0
     plantNew%geometry%zht = 0.0
     plantNew%geometry%dstm = 0.0
     plantNew%geometry%xstmrep = 0.0d0
     plantNew%geometry%zshoot = 0.0
     plantNew%geometry%zrtd = 0.0
     plantNew%geometry%grainf = 0.0
     plantNew%geometry%zfurcut = 0.0
     plantNew%geometry%ztransprtmin = 0.0
     plantNew%geometry%ztransprtmax = 0.0
     ! initialize row placement to be on the ridge
     plantNew%geometry%rg = 1
     ! initialize harvestable yield fraction flag
     plantNew%geometry%hyfg = 0

     ! plant not growing, just created
     plantNew%growth%am0cif = .false.
     plantNew%growth%growing = .false.
     plantNew%growth%shoot_growing = .false.
     plantNew%growth%can_regrow = .false.
     plantNew%growth%do_regrow = .false.
     plantNew%growth%thucum = 0.0
     plantNew%growth%trthucum = 0.0
     plantNew%growth%zgrowpt = 0.0
     plantNew%growth%fliveleaf = 1.0
     plantNew%growth%leafareatrend = 0.0
     plantNew%growth%stemmasstrend = 0.0
     plantNew%growth%twarmdays = 0.0
     plantNew%growth%tcolddays = 0.0
     plantNew%growth%tchillucum = 0.0d0
     plantNew%growth%thardnx = 0.0d0
     plantNew%growth%thu_shoot_beg = 0.0d0
     plantNew%growth%thu_shoot_end = 0.0d0
     plantNew%growth%mtotleaf = 0.0d0
     plantNew%growth%thu_leaf_beg = 0.0d0
     plantNew%growth%thu_leaf_end = 0.0d0
     plantNew%growth%mshoot = 0.0
     plantNew%growth%mtotshoot = 0.0
     plantNew%growth%dayap = 0
     plantNew%growth%dayam = 0
     plantNew%growth%dayspring = 0
     plantNew%growth%dayfall = 0
     plantNew%growth%ptp = 0.0
     plantNew%growth%pta = 0.0
     plantNew%growth%fwsf = 1.0

     plantNew%deriv%mbgstem = 0.0
     plantNew%deriv%mbgleaf = 0.0
     plantNew%deriv%mbgstore = 0.0
     plantNew%deriv%mbgrootstore = 0.0
     plantNew%deriv%mbgrootfiber = 0.0

     plantNew%deriv%m = 0.0
     plantNew%deriv%mst = 0.0
     plantNew%deriv%mf = 0.0
     plantNew%deriv%mrt = 0.0
     plantNew%deriv%mbg = 0.0
     plantNew%deriv%dmrtto4 = 0.0
     plantNew%deriv%dmbgto4 = 0.0
     plantNew%deriv%dmrtto15 = 0.0
     plantNew%deriv%dmbgto15 = 0.0
     do idx = 1, nsoillay
        plantNew%deriv%mrtz(idx) = 0.0
        plantNew%deriv%mbgz(idx) = 0.0
     end do

     plantNew%deriv%rsai = 0.0d0
     plantNew%deriv%rlai = 0.0
     do idx = 1, ncanlay
        plantNew%deriv%rsaz(idx) = 0.0
        plantNew%deriv%rlaz(idx) = 0.0
     end do

     plantNew%deriv%rcd = 0.0
     plantNew%deriv%ffcv = 0.0
     plantNew%deriv%fscv = 0.0
     plantNew%deriv%ftcv = 0.0
     plantNew%deriv%fcancov = 0.0
     plantNew%deriv%ztranspdepth = 0.0

     plantNew%database%xstm = 0.0
     plantNew%database%rbc = 1
     plantNew%database%covfact = 0.0
     plantNew%database%ck = 0.0

     ! initialize crop yield reporting parameters in case harvest call before planting
     plantNew%bname = ''
     plantNew%database%ynmu = ''
     plantNew%database%ycon = 1.0
     plantNew%database%ywct = 0.0

     ! initialize crop type id to 0 indicating no crop type is growing
     plantNew%database%idc = 0
     plantNew%database%baflg = 0
     plantNew%database%sla = 0.0
     plantNew%geometry%dpop = 0.0

     ! initialize decomp parameters since they are used before a crop is growing
     do idx = 1, size(plantNew%database%dkrate)
        plantNew%database%dkrate(idx) = 0.0
     end do
     plantNew%database%ddsthrsh = 0.0

     ! values that need initialization for cdbug calls (before initial crop entry)
     plantNew%database%tdtm = 0

     plantNew%database%shoot = 0.0

  end function plantAdd

  subroutine plantDestroy(plantPntr)
     ! destroys a plant from within the plant pointer chain
     ! while preserving the chain
     type(plant_pointer), pointer :: plantPntr

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
     type(plant_pointer), pointer :: olderPlant

     ! close open output file
     if( plantPntr%bout%luo .gt. 0 ) then
       close( plantPntr%bout%luo )
     end if

     ! check for older plants
     if( associated(plantPntr%olderPlant) ) then
        ! preserve pointer to olderPlant
        olderPlant => plantPntr%olderPlant
     else
        ! no olderPlant
        nullify(olderPlant)
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

     deallocate(plantPntr%prev%stemz, stat=alloc_stat)
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

     ! delete upgm
     call upgm_delete(plantPntr%upgm_grow)
     
     ! remove all residue mass for this plant
     call residueDestroyAll(plantPntr%residue)
        
     ! delete memory and nullify
     deallocate(plantPntr, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to deallocate Plant pointer.'
     end if

     if( associated(olderPlant) ) then
       ! older plant now in this spot
       plantPntr => olderPlant
     else
       ! no older plants
       nullify(plantPntr)
     end if

  end subroutine plantDestroy

  subroutine plantDestroyAll(plantPntr)
     type(plant_pointer), pointer :: plantPntr

     ! remove all plants from this plant and older
     do while( associated(plantPntr) )
       call plantDestroy(plantPntr)
     end do
        
  end subroutine plantDestroyAll

  subroutine plantPrint(plantPntr, nsoillay)
     type(plant_pointer), pointer :: plantPntr
     integer, intent(in) :: nsoillay

     ! local variable
     type(residue_pointer), pointer :: thisResidue
     integer :: idx
     real :: totmass
     real :: standmass
     real :: flatmass

     ! print mass values
     if ( associated(plantPntr) ) then
       write(*,*) 'Plant stand: ', plantPntr%mass%standstem, plantPntr%mass%standleaf, plantPntr%mass%standstore
       standmass = plantPntr%mass%standstem + plantPntr%mass%standleaf + plantPntr%mass%standstore

       write(*,*) 'Plant  flat: ', plantPntr%mass%flatstem, plantPntr%mass%flatleaf, plantPntr%mass%flatstore, &
                                   plantPntr%mass%flatrootstore, plantPntr%mass%flatrootfiber
       flatmass = plantPntr%mass%flatstem + plantPntr%mass%flatleaf + plantPntr%mass%flatstore &
                + plantPntr%mass%flatrootstore + plantPntr%mass%flatrootfiber

       totmass = standmass + flatmass
       do idx = 1, nsoillay
         write(*,*) 'Plant below: ', idx, plantPntr%mass%stemz(idx), plantPntr%mass%leafz(idx), plantPntr%mass%storez(idx), &
                                          plantPntr%mass%rootstorez(idx), plantPntr%mass%rootfiberz(idx)
         totmass = totmass + plantPntr%mass%stemz(idx) + plantPntr%mass%leafz(idx) + plantPntr%mass%storez(idx) &
                           + plantPntr%mass%rootstorez(idx) + plantPntr%mass%rootfiberz(idx)

       end do

       write(*,*) 'PLANT STANDMASS HT FLAT: ', standmass, plantPntr%geometry%zht, flatmass

       thisResidue => plantPntr%residue
       do while( associated(thisResidue) )
         call residuePrint( thisResidue, nsoillay)
         thisResidue => thisResidue%olderResidue
       end do
     else
       write(*,*) 'No Plant'
     end if
        
  end subroutine plantPrint

  function residueAdd(residuePntr, residueIndex, nslay) result(residueNew)

     type(residue_pointer), pointer :: residuePntr
     integer, intent(inout) :: residueIndex ! index for all residue pools created under this plant
     integer, intent(in) :: nslay
     type(residue_pointer), pointer :: residueNew

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
     integer :: idx

     allocate(residueNew, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to allocate new Plant pointer.'
     end if

     sum_stat = 0
     allocate(residueNew%cumddg(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%stemz(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%leafz(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%storez(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%rootstorez(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%rootfiberz(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(residueNew%deriv%mrtz(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%deriv%mbgz(nslay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     allocate(residueNew%deriv%rsaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     allocate(residueNew%deriv%rlaz(ncanlay), stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory for residueNew'
        stop 1
     end if

     if( associated(residuePntr) ) then
        ! point to previous residue
        residueNew%olderResidue => residuePntr
     else
        nullify(residueNew%olderResidue)
     end if

     ! increment plant residue index
     residueIndex = residueIndex + 1
     ! carry index along with residue
     residueNew%bout%num = residueIndex
     ! show residue output unit as uninitialized
     residueNew%bout%luo = -1

     ! initialize decomp age params
     residueNew%resday = 0
     residueNew%resyear = 1
     residueNew%cumdds = 0.0
     residueNew%cumddf = 0.0
     do idx=1,nslay
       residueNew%cumddg(idx) = 0.0
     end do

       ! zero all residue amounts
     residueNew%standstem = 0.0
     residueNew%standleaf = 0.0
     residueNew%standstore = 0.0
     residueNew%flatstem = 0.0
     residueNew%flatleaf = 0.0
     residueNew%flatstore = 0.0
     residueNew%flatrootstore = 0.0
     residueNew%flatrootfiber = 0.0
     ! layer thickness can be anything > 0 since setting all values to zero
     do idx = 1, nslay
       residueNew%stemz(idx) = 0.0
       residueNew%leafz(idx) = 0.0
       residueNew%storez(idx) = 0.0
       residueNew%rootstorez(idx) = 0.0
       residueNew%rootfiberz(idx) = 0.0
     end do

     ! set other state variables
     residueNew%zht = 0.0
     residueNew%dstm = 0.0
     residueNew%xstmrep = 0.0d0
     residueNew%zrtd = 0.0
     residueNew%grainf = 0.0

     residueNew%deriv%mbgstem = 0.0
     residueNew%deriv%mbgleaf = 0.0
     residueNew%deriv%mbgstore = 0.0
     residueNew%deriv%mbgrootstore = 0.0
     residueNew%deriv%mbgrootfiber = 0.0

     residueNew%deriv%m = 0.0
     residueNew%deriv%mst = 0.0
     residueNew%deriv%mf = 0.0
     residueNew%deriv%mbg = 0.0
     residueNew%deriv%mrt = 0.0
     residueNew%deriv%dmrtto4 = 0.0
     residueNew%deriv%dmbgto4 = 0.0
     residueNew%deriv%dmrtto15 = 0.0
     residueNew%deriv%dmbgto15 = 0.0
     do idx = 1, nslay
        residueNew%deriv%mrtz(idx) = 0.0
        residueNew%deriv%mbgz(idx) = 0.0
     end do

     residueNew%deriv%rsai = 0.0d0
     residueNew%deriv%rlai = 0.0
     do idx = 1, ncanlay
        residueNew%deriv%rsaz(idx) = 0.0
        residueNew%deriv%rlaz(idx) = 0.0
     end do

     residueNew%deriv%rcd = 0.0
     residueNew%deriv%ffcv = 0.0
     residueNew%deriv%fscv = 0.0
     residueNew%deriv%ftcv = 0.0
     residueNew%deriv%fcancov = 0.0
     residueNew%deriv%ztranspdepth = 0.0

  end function residueAdd

  subroutine residueDestroy(residuePntr)
     type(residue_pointer), pointer :: residuePntr

     ! local variable
     integer :: alloc_stat  ! allocation status return
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
     type(residue_pointer), pointer :: olderResidue

     ! close open output file
     if( residuePntr%bout%luo .gt. 0 ) then
       close( residuePntr%bout%luo )
     end if

     ! check for older residue
     if( associated(residuePntr%olderResidue) ) then
        ! preserve pointer to olderResidue
        olderResidue => residuePntr%olderResidue
     else
        ! no olderResidue
        nullify(olderResidue)
     end if

     sum_stat = 0
     deallocate(residuePntr%cumddg, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%stemz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%leafz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%storez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%rootstorez, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%rootfiberz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     deallocate(residuePntr%deriv%mrtz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%deriv%mbgz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     deallocate(residuePntr%deriv%rsaz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat
     deallocate(residuePntr%deriv%rlaz, stat=alloc_stat)
     sum_stat = sum_stat + alloc_stat

     if( sum_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to deallocate memory for residuePntr'
        stop 1
     end if

     deallocate(residuePntr, stat=alloc_stat)
     if( alloc_stat .gt. 0 ) then
        write(*,'(a,i0)') 'Unable to deallocate Residue pointer.'
     end if

     if( associated(olderResidue) ) then
       ! older residue now in this spot
       residuePntr => olderResidue
     else
       ! no more old residue
       nullify(residuePntr)
     end if

  end subroutine residueDestroy

  subroutine residueDestroyAll(residuePntr)
     type(residue_pointer), pointer :: residuePntr

     ! remove all residue mass from this mass and older
     do while( associated(residuePntr) )
       call residueDestroy(residuePntr)
     end do
        
  end subroutine residueDestroyAll

  subroutine residuePrint(residuePntr, nsoillay)
     type(residue_pointer), pointer :: residuePntr
     integer, intent(in) :: nsoillay

     ! local variable
     integer :: idx
     real :: totmass
     real :: totstand
     real :: totflat
     real :: totburied
     real :: layersum

     ! print mass values
     totburied = 0.0
     if ( associated(residuePntr) ) then
       write(*,*) 'Residue stand: ', residuePntr%standstem, residuePntr%standleaf, residuePntr%standstore
       totstand = residuePntr%standstem + residuePntr%standleaf + residuePntr%standstore

       write(*,*) 'Residue  flat: ', residuePntr%flatstem, residuePntr%flatleaf, residuePntr%flatstore, &
                                   residuePntr%flatrootstore, residuePntr%flatrootfiber
       totflat = residuePntr%flatstem + residuePntr%flatleaf + residuePntr%flatstore &
               + residuePntr%flatrootstore + residuePntr%flatrootfiber

       do idx = 1, nsoillay
         layersum = residuePntr%stemz(idx) + residuePntr%leafz(idx) + residuePntr%storez(idx) &
                           + residuePntr%rootstorez(idx) + residuePntr%rootfiberz(idx)
         write(*,*) 'RESID BY LAY: ', idx, residuePntr%stemz(idx), residuePntr%leafz(idx), residuePntr%storez(idx), &
                                           residuePntr%rootstorez(idx), residuePntr%rootfiberz(idx), layersum
         totburied = totburied + layersum

       end do

       totmass = totstand + totflat + totburied
       write(*,*) 'RESID TOTALS: ', totmass, totstand, totflat, totburied, residuePntr%deriv%m, residuePntr%zht

     else
       write(*,*) 'No Residue'
     end if
        
  end subroutine residuePrint

  subroutine plantPrintAll(plantPntr, nsoillay)
     type(plant_pointer), pointer :: plantPntr
     integer, intent(in) :: nsoillay

     ! local variable
     type(plant_pointer), pointer :: thisPlant
     type(residue_pointer), pointer :: thisResidue

     ! print mass values
     thisPlant => plantPntr
     if( .not. associated(thisPlant) ) then
       write(*,*) 'NO PLANT'
     end if

     do while( associated(thisPlant) )
       write(*,*) 'PLANT NAME: ', trim( thisPlant%bname )
       call plantPrint( thisPlant, nsoillay )

       thisResidue => thisPlant%residue
       do while( associated(thisResidue) )
         call residuePrint( thisResidue, nsoillay)
         thisResidue => thisResidue%olderResidue
       end do
       thisPlant => thisPlant%olderPlant
     end do
        
  end subroutine plantPrintAll

  subroutine plantSumAll(plantPntr, nsoillay)

     use weps_main_mod, only: daysim
     use datetime_mod, only: get_simdate_doy

     type(plant_pointer), pointer :: plantPntr
     integer, intent(in) :: nsoillay

     ! local variable
     type(plant_pointer), pointer :: thisPlant
     type(residue_pointer), pointer :: thisResidue
     integer :: idx
     real :: totmass
     real :: standstem
     real :: standleaf
     real :: standstore
     real :: flatstem
     real :: flatleaf
     real :: flatstore
     real :: flatrootstore
     real :: flatrootfiber
     real :: rootstorez
     real :: rootfiberz
     real :: stemz
     real :: resstandmass
     real :: resflatmass
     real :: resburied
     real :: resburiedroot
     real :: layersumburied
     real :: layersumroot

     integer:: doy
     real :: convert

     integer :: poolcount

     ! sum total mass values
     standstem = 0.0
     standleaf = 0.0
     standstore = 0.0
     flatstem = 0.0
     flatleaf = 0.0
     flatstore = 0.0
     flatrootstore = 0.0
     flatrootfiber = 0.0
     rootstorez = 0.0
     rootfiberz = 0.0
     stemz = 0.0
     resstandmass = 0.0
     resflatmass = 0.0
     resburied = 0.0
     resburiedroot = 0.0

     poolcount = 0

     thisPlant => plantPntr
     do while( associated(thisPlant) )
       !write(*,*) 'Plant stand: ', thisPlant%mass%standstem, thisPlant%mass%standleaf, thisPlant%mass%standstore
       standstem = standstem + thisPlant%mass%standstem
       standleaf = standleaf + thisPlant%mass%standleaf
       standstore = standstore + thisPlant%mass%standstore

       !write(*,*) 'Plant  flat: ', thisPlant%mass%flatstem, thisPlant%mass%flatleaf, thisPlant%mass%flatstore, &
       !                            thisPlant%mass%flatrootstore, thisPlant%mass%flatrootfiber
       flatstem = flatstem + thisPlant%mass%flatstem
       flatleaf = flatleaf + thisPlant%mass%flatleaf
       flatstore = flatstore + thisPlant%mass%flatstore
       flatrootstore = flatrootstore + thisPlant%mass%flatrootstore
       flatrootfiber = flatrootfiber + thisPlant%mass%flatrootfiber

       do idx = 1, nsoillay
         !write(*,*) 'Plant below: ', idx, thisPlant%mass%stemz(idx), thisPlant%mass%leafz(idx), thisPlant%mass%storez(idx), &
         !                                 thisPlant%mass%rootstorez(idx), thisPlant%mass%rootfiberz(idx)
         rootstorez = rootstorez + thisPlant%mass%rootstorez(idx)
         rootfiberz = rootfiberz + thisPlant%mass%rootfiberz(idx)
         stemz = stemz + thisPlant%mass%stemz(idx)

       end do

       thisResidue => thisPlant%residue
       do while( associated(thisResidue) )
         ! total pool count
         poolcount = poolcount + 1

         !write(*,*) 'Residue stand: ', thisResidue%standstem, thisResidue%standleaf, thisResidue%standstore
         resstandmass = resstandmass + thisResidue%standstem + thisResidue%standleaf + thisResidue%standstore

         !write(*,*) 'Residue  flat: ', thisResidue%flatstem, thisResidue%flatleaf, thisResidue%flatstore, &
         !                            thisResidue%flatrootstore, thisResidue%flatrootfiber
         resflatmass = resflatmass + thisResidue%flatstem + thisResidue%flatleaf + thisResidue%flatstore &
                     + thisResidue%flatrootstore + thisResidue%flatrootfiber

         do idx = 1, nsoillay
           layersumburied = thisResidue%stemz(idx) + thisResidue%leafz(idx) + thisResidue%storez(idx)
           layersumroot = thisResidue%rootstorez(idx) + thisResidue%rootfiberz(idx)
           !write(*,*) 'RESID BY LAY: ', idx, thisResidue%stemz(idx), thisResidue%leafz(idx), thisResidue%storez(idx), &
           !                                  thisResidue%rootstorez(idx), thisResidue%rootfiberz(idx), layersum
           resburied = resburied + layersumburied
           resburiedroot = resburiedroot + layersumroot
         end do

         thisResidue => thisResidue%olderResidue
       end do
       thisPlant => thisPlant%olderPlant
     end do

     totmass = resstandmass + resflatmass + resburied + resburiedroot &
             + standstem + standleaf + standstore &
             + flatstem + flatleaf + flatstore + flatrootstore + flatrootfiber &
             + rootstorez + rootfiberz + stemz

     doy = get_simdate_doy()

     if( doy .eq. 1 ) then
       write(*,*) 'SUMALL: '
       write(*,*) 'SUMALL: '
     end if

!     write(*,*) 'SUMALL: ', daysim, doy, resstandmass, resflatmass, resburied, resburiedroot &
!                          , standstem, standleaf, 0.2*standstore, 0.8*standstore &
!                          , flatstem, flatleaf, flatstore, flatrootstore, flatrootfiber &
!                          , rootstorez, rootfiberz, stemz, totmass

     ! stacked in order of columns 3 4 5 6 16 18 7 8 17 9 10 19
     convert = 1.0       ! output kg/m^2
     ! convert = 8921.79 ! output lbs/acre
!     write(*,*) 'SUMALL: ', daysim, doy, convert * (resstandmass) & ! 3
!                , convert * (resstandmass + resflatmass) & ! 4
!                , convert * ( resstandmass + resflatmass + resburied) & ! 5
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot) & ! 6
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot +rootstorez + stemz + standstem) & ! 7
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot +rootstorez + stemz + standstem + standleaf) & ! 8
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot +rootstorez + stemz + standstem + standleaf &
!                          + rootfiberz + 0.2*standstore) & ! 9
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot +rootstorez + stemz + standstem + standleaf &
!                          + rootfiberz + 0.2*standstore + 0.8*standstore) & ! 10
!                , convert * ( flatstem) & ! 11
!                , convert * ( flatleaf) & ! 12
!                , convert * ( flatstore) & ! 13
!                , convert * ( flatrootstore) & ! 14
!                , convert * ( flatrootfiber) & ! 15
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot + rootstorez) & ! 16
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot +rootstorez + stemz + standstem + standleaf &
!                          + rootfiberz) & ! 17
!                , convert * ( resstandmass + resflatmass + resburied + resburiedroot +rootstorez + stemz) & ! 18
!                , convert * ( totmass) ! 19

     write(*,*) 'SUMALL: ', daysim, doy, convert * (resstandmass) & ! 3
                , convert * (resflatmass) & ! 4
                , convert * (resburied) & ! 5
                , convert * (resburiedroot) & ! 6
                , convert * (standstem) & ! 7
                , convert * (standleaf) & ! 8
                , convert * (0.2*standstore) & ! 9
                , convert * (0.8*standstore) & ! 10
                , convert * (flatstem) & ! 11
                , convert * (flatleaf) & ! 12
                , convert * (flatstore) & ! 13
                , convert * (flatrootstore) & ! 14
                , convert * (flatrootfiber) & ! 15
                , convert * (rootstorez) & ! 16
                , convert * (rootfiberz) & ! 17
                , convert * (stemz) & ! 18
                , convert * ( totmass) &
                , poolcount ! 20

        
  end subroutine plantSumAll

  subroutine plantSumPool(plantPntr, nsoillay, label)

     use weps_main_mod, only: daysim

     type(plant_pointer), pointer :: plantPntr
     integer, intent(in) :: nsoillay
     character(*) :: label

     real :: poolsum

     ! local variable
     type(plant_pointer), pointer :: thisPlant
     type(residue_pointer), pointer :: thisResidue
     integer :: idx
     real :: resstandmass
     real :: resflatmass
     real :: layersumburied
     real :: layersumroot

     integer :: poolcount

     poolcount = 0

     thisPlant => plantPntr
     do while( associated(thisPlant) )

       thisResidue => thisPlant%residue
       do while( associated(thisResidue) )
         ! total pool count
         poolcount = poolcount + 1

         resstandmass = thisResidue%standstem + thisResidue%standleaf + thisResidue%standstore
         resflatmass = thisResidue%flatstem + thisResidue%flatleaf + thisResidue%flatstore &
                     + thisResidue%flatrootstore + thisResidue%flatrootfiber

         layersumburied = 0.0
         layersumroot = 0.0
         do idx = 1, nsoillay
           layersumburied = layersumburied + thisResidue%stemz(idx) + thisResidue%leafz(idx) + thisResidue%storez(idx)
           layersumroot = layersumroot + thisResidue%rootstorez(idx) + thisResidue%rootfiberz(idx)
         end do

         poolsum = resstandmass + resstandmass + layersumburied + layersumroot

         write(*,*) label, daysim, poolcount, poolsum, trim(thisPlant%bname)

         thisResidue => thisResidue%olderResidue
       end do
       thisPlant => thisPlant%olderPlant
     end do

  end subroutine plantSumPool

end module biomaterial



