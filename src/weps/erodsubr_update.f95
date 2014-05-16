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
      include  's1surf.inc'
      include  's1sgeo.inc'
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
    subrsurf%abzht = biotot%zht_ave

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

    subrsurf%asfcr = asfcr(sr)
    subrsurf%aszcr = aszcr(sr)
    subrsurf%asflos = asflos(sr)
    subrsurf%asmlos = asmlos(sr)
    subrsurf%asdcr = asdcr(sr)
    subrsurf%asecr = asecr(sr)
    subrsurf%aslrr = aslrr(sr)
    subrsurf%aszrgh = aszrgh(sr)
    subrsurf%asxrgs = asxrgs(sr)
    subrsurf%asxrgw = asxrgw(sr)
    subrsurf%asargo = asargo(sr)
    subrsurf%asxdks = asxdks(sr)
    subrsurf%ahzsnd = h1et%zsnd

    do idx = 1, 24
        subrsurf%ahrwc0(idx) = ahrwc0(idx,sr)
    end do

    return
    end

