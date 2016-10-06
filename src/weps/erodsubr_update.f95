!$Author$
!$Date$
!$Revision$
!$HeadURL$

subroutine erodsubr_update( sr, restot, croptot, biotot, h1et, subrsurf )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
    use subregions_mod
    use biomaterial, only: biototal
    use hydro_data_struct_defs, only: hydro_derived_et
    use erosion_data_struct_defs, only: subregionsurfacestate

!     +++ ARGUMENT DECLARATIONS +++
    integer sr                               ! subregion index (eventually obsolete)
    type(biototal), intent(in) :: restot
    type(biototal), intent(in) :: croptot
    type(biototal), intent(in) :: biotot
    type(hydro_derived_et), intent(in) :: h1et
    type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     +++ ARGUMENT DEFINITIONS +++

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  's1dbh.inc'
      include  's1layr.inc'
      include  's1phys.inc'
      include  's1agg.inc'
      include  'h1db1.inc'

!     +++ LOCAL VARIABLES +++
      integer :: idx

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     idx - loop index

!     +++ END SPECIFICATIONS +++

    subrsurf%adzht_ave = restot%zht_ave
    subrsurf%aczht = croptot%zht_ave

    subrsurf%acrsai = croptot%rsaitot
    subrsurf%acrlai = croptot%rlaitot

    subrsurf%adrsaitot = restot%rsaitot
    subrsurf%adrlaitot = restot%rlaitot

    subrsurf%acxrow = croptot%xrow
    subrsurf%ac0rg = croptot%c0rg

    subrsurf%abffcv = biotot%ffcvtot

    do idx = 1, nslay(sr)

        subrsurf%bsl(idx)%aszlyt = aszlyt(idx,sr)
        subrsurf%bsl(idx)%asdblk = asdblk(idx,sr)
        subrsurf%bsl(idx)%asfsan = asfsan(idx,sr)
        subrsurf%bsl(idx)%asfvfs = asfvfs(idx,sr)
        subrsurf%bsl(idx)%asfsil = asfsil(idx,sr)
        subrsurf%bsl(idx)%asfcla = asfcla(idx,sr)
        subrsurf%bsl(idx)%asvroc = asvroc(idx,sr)
        subrsurf%bsl(idx)%asdagd = asdagd(idx,sr)
        subrsurf%bsl(idx)%aseags = aseags(idx,sr)
        subrsurf%bsl(idx)%aslagm = aslagm(idx,sr)
        subrsurf%bsl(idx)%aslagn = aslagn(idx,sr)
        subrsurf%bsl(idx)%aslagx = aslagx(idx,sr)
        subrsurf%bsl(idx)%as0ags = as0ags(idx,sr)

        subrsurf%bsl(idx)%ahrwcw = ahrwcw(idx,sr)
        subrsurf%bsl(idx)%ahrwca = ahrwca(idx,sr)

    end do

    subrsurf%ahzsnd = h1et%zsnd

    do idx = 1, 24
        subrsurf%ahrwc0(idx) = ahrwc0(idx,sr)
    end do

    ! derived
    subrsurf%abrsai = biotot%rsaitot
    subrsurf%abrlai = biotot%rlaitot
    subrsurf%abzht = biotot%zht_ave
!     real :: sxprg      ! sxprg  - ridge spacing parallel the wind direction(mm)
!     real :: acanag     ! acanag - coefficient of abrasion for aggregates (1/m)
!     real :: acancr     ! acancr - coefficient of abrasion for crust (1/m)
!     real :: asf10an    ! asf10an - soil fraction pm10 in abraded suspension
!     real :: asf10en    ! asf10en - soil fraction pm10 in emitted suspension
!     real :: asf10bk    ! asf10bk - soil fraction pm10 in saltation breakage suspension
!     real :: sfd1       ! soil fraction less than 0.01 mm diameter
!     real :: sfd10      ! soil fraction less than 0.1 mm diameter
!     real :: sfd84      ! soil fraction less than 0.84 mm diameter
!     real :: sfd200     ! soil fraction less than 2.0 mm diameter
!     real :: sf10ic     ! initial condition (modified) of soil fraction less than 0.1 mm diameter
!     real :: sf84ic     ! initial condition (modified) of soil fraction less than 0.84 mm diameter

    return
    end

