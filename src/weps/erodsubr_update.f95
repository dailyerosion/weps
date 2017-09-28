!$Author$
!$Date$
!$Revision$
!$HeadURL$

subroutine erodsubr_update( sr, soil, crop, restot, croptot, biotot, h1et, subrsurf )

!     +++ PURPOSE +++
!     print out input file for stand alone erosion

!     + + + Modules Used + + +
    use subregions_mod
    use soil_data_struct_defs, only: soil_def
    use biomaterial, only: biototal, biomatter
    use hydro_data_struct_defs, only: hydro_derived_et
    use erosion_data_struct_defs, only: subregionsurfacestate, awzypt
    use process_mod, only: sbpm10, sbsfdi

!     +++ ARGUMENT DECLARATIONS +++
    integer sr                               ! subregion index (eventually obsolete)
    type(soil_def), intent(in) :: soil  ! soil for this subregion
    type(biomatter), intent(in) :: crop
    type(biototal), intent(in) :: restot
    type(biototal), intent(in) :: croptot
    type(biototal), intent(in) :: biotot
    type(hydro_derived_et), intent(in) :: h1et
    type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     +++ ARGUMENT DEFINITIONS +++

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
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

    subrsurf%acxrow = crop%geometry%xrow
    subrsurf%ac0rg = crop%geometry%rg

    subrsurf%abffcv = biotot%ffcvtot

    subrsurf%asfcr = soil%asfcr
    subrsurf%aszcr = soil%aszcr
    subrsurf%asflos = soil%asflos
    subrsurf%asmlos = soil%asmlos
    subrsurf%asdcr = soil%asdcr
    subrsurf%asecr = soil%asecr
    subrsurf%aslrr = soil%aslrr
    subrsurf%aszrgh = soil%aszrgh
    subrsurf%asxrgs = soil%asxrgs
    subrsurf%asxrgw = soil%asxrgw
    subrsurf%asargo = soil%asargo

    do idx = 1, soil%nslay

        subrsurf%bsl(idx)%aszlyt = soil%aszlyt(idx)
        subrsurf%bsl(idx)%asdblk = soil%asdblk(idx)
        subrsurf%bsl(idx)%asfsan = soil%asfsan(idx)
        subrsurf%bsl(idx)%asfvfs = soil%asfvfs(idx)
        subrsurf%bsl(idx)%asfsil = soil%asfsil(idx)
        subrsurf%bsl(idx)%asfcla = soil%asfcla(idx)
        subrsurf%bsl(idx)%asvroc = soil%asvroc(idx)
        subrsurf%bsl(idx)%asdagd = soil%asdagd(idx)
        subrsurf%bsl(idx)%aseags = soil%aseags(idx)
        subrsurf%bsl(idx)%aslagm = soil%aslagm(idx)
        subrsurf%bsl(idx)%aslagn = soil%aslagn(idx)
        subrsurf%bsl(idx)%aslagx = soil%aslagx(idx)
        subrsurf%bsl(idx)%as0ags = soil%as0ags(idx)

        subrsurf%bsl(idx)%ahrwcw = soil%ahrwcw(idx)
        subrsurf%bsl(idx)%ahrwca = soil%ahrwca(idx)

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
    call sbpm10( subrsurf%bsl(1)%aseags, subrsurf%asecr, subrsurf%bsl(1)%asfcla, &
              subrsurf%bsl(1)%asfsan, awzypt, subrsurf%acanag, subrsurf%acancr, &
              subrsurf%asf10an, subrsurf%asf10en, subrsurf%asf10bk )

    ! calculate fraction less than 10 microns diameter from asd
    call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
              subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 0.01, subrsurf%sfd1 )
    ! store initial sf1
    subrsurf%sf1ic = subrsurf%sfd1

    ! calculate fraction less than 100 microns diameter from asd
    call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
              subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 0.1, subrsurf%sfd10 )
    ! store initial sf10
    subrsurf%sf10ic = subrsurf%sfd10

    ! calculate fraction less than 0.84 mm diameter from asd
    call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
              subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 0.84, subrsurf%sfd84 )
    ! store initial sf84
    subrsurf%sf84ic = subrsurf%sfd84
    subrsurf%sf84ic = min(0.9999, max(subrsurf%sf84ic,0.0001))            !set limits

    ! calculate fraction less than 2 mm diameter from asd
    call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
             subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 2.0, subrsurf%sfd200 )
    ! store initial sf200
    subrsurf%sf200ic = subrsurf%sfd200
    subrsurf%sf200ic = min(0.9999, max(subrsurf%sf200ic,0.0001))            !set limits

    write(*,*) 'SUBRUPDATE: ', sr, subrsurf%sf1ic, subrsurf%sf10ic, subrsurf%sf84ic, subrsurf%sf200ic

    return
    end

