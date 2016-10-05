!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine save_soil(isr, subrsurf)

! ***************************************************************** LEW
! Saves and Restores soil surface and layer properties
! for re-initialization during WEPS "calibration" Runs.
!
!     Edit History
!  Aug 21, 2005 - LEW

      use erosion_data_struct_defs, only: subregionsurfacestate

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'

      include 'soil_save.inc'

      integer :: isr
      type(subregionsurfacestate), intent(in) :: subrsurf  ! subregion surface conditions

      integer :: ldx

      ! write(*,*) 'isr', isr
      ! write(*,*) 'nslay(isr)', nslay(isr)
      Zsfald(isr) = subrsurf%asfald
      Zmrslp(isr) = amrslp(isr)
      ZSFCov(isr) = SFCov(isr)
      Zbedrock_depth(isr) = bedrock_depth(isr)
      Zrestrict_depth(isr) = restrict_depth(isr)
!   Crust Properties
      Zszcr(isr) = subrsurf%aszcr
      Zsdcr(isr) = subrsurf%asdcr
      Zsecr(isr) = subrsurf%asecr
      Zsfcr(isr) = subrsurf%asfcr
      Zsmlos(isr) = subrsurf%asmlos
      Zsflos(isr) = subrsurf%asflos
!   Surface roughness Properties
      Zslrr(isr) = aslrr(isr)
      Zslrro(isr) = aslrro(isr)
      Zsargo(isr) = asargo(isr)
      Zszrgh(isr) = aszrgh(isr)
      Zsxrgs(isr) = asxrgs(isr)
      Zsxrgw(isr) = asxrgw(isr)

!    Not sure if these surface variables need to be here
      ! Zszrho(isr) = aszrho(isr)  ! not defined yet
      ! Zsxdks(isr) = asxdks(isr)
      ! Zsxdkh(isr) = asxdkh(isr)
      ! Zs0rrk(isr) = as0rrk(isr)

!   Other
      Zsfalw(isr) = subrsurf%asfalw

!   Zero based indexes
      ! Zsfsan(0, isr) = asfsan(0, isr) ! Not defined yet (or used?)
      ! Zsfsil(0, isr) = asfsil(0, isr)
      ! Zsfcla(0, isr) = asfcla(0, isr)
      ! Zsvroc(0, isr) = asvroc(0, isr)

      ! Zslagm(0, isr) = aslagm(0, isr)
      ! Zs0ags(0, isr) = as0ags(0, isr)
      ! Zslagx(0, isr) = aslagx(0, isr)
      ! Zslagn(0, isr) = aslagn(0, isr)
      ! Zsdagd(0, isr) = asdagd(0, isr)
      ! Zseags(0, isr) = aseags(0, isr)

      ! Zsdblk(0, isr) = asdblk(0, isr)
      !write(*,*) 'start do loop'
      do ldx = 1, nslay(isr)
          !write(*,*) 'ldx', ldx
!       Layer thicknesses
          Zszlyt(ldx, isr) = aszlyt(ldx, isr)
!       IP surface physical properties
          Zsfsan(ldx, isr) = asfsan(ldx, isr)
          Zsfsil(ldx, isr) = asfsil(ldx, isr)
          Zsfcla(ldx, isr) = asfcla(ldx, isr)
          Zsvroc(ldx, isr) = asvroc(ldx, isr)
          Zsfvcs(ldx, isr) = asfvcs(ldx, isr)
          Zsfcs(ldx, isr)  = asfcs(ldx, isr)
          Zsfms(ldx, isr)  = asfms(ldx, isr)
          Zsffs(ldx, isr)  = asffs(ldx, isr)
          Zsfvfs(ldx, isr) = asfvfs(ldx, isr)
          Zsdwblk(ldx, isr)= asdwblk(ldx, isr)
!       IP soil chemical properties
          Zsfom(ldx, isr)  = asfom(ldx, isr)
          Zs0ph(ldx, isr)  = as0ph(ldx, isr)
          Zsfcce(ldx, isr) = asfcce(ldx, isr)
          Zsfcec(ldx, isr) = asfcec(ldx, isr)
          Zsfcle(ldx, isr) = asfcle(ldx, isr)
!       IC Aggregate properties
          Zslagm(ldx, isr) = aslagm(ldx, isr)
          Zs0ags(ldx, isr) = as0ags(ldx, isr)
          Zslagx(ldx, isr) = aslagx(ldx, isr)
          Zslagn(ldx, isr) = aslagn(ldx, isr)
          Zsdagd(ldx, isr) = asdagd(ldx, isr)
          Zseags(ldx, isr) = aseags(ldx, isr)
!       IC soil hydrologic properties
          Zsdblk(ldx, isr) = asdblk(ldx, isr)
          Zsdblk0(ldx, isr) = asdblk0(ldx, isr)
          Zhrwc(ldx, isr)  = ahrwc(ldx, isr)
!       soil hydrologic (water release curve) properties
          Zhrwcs(ldx, isr) = ahrwcs(ldx, isr)
          Zhrwcf(ldx, isr) = ahrwcf(ldx, isr)
          Zhrwcw(ldx, isr) = ahrwcw(ldx, isr)
!       soil hydrologic (water release curve) properties
          Zh0cb(ldx, isr)  = ah0cb(ldx, isr)
          Zheaep(ldx, isr) = aheaep(ldx, isr)
          Zhrsk(ldx, isr)  = ahrsk(ldx, isr)
!       other variables by depth
          Zsdsblk(ldx, isr)= asdsblk(ldx, isr)
          Zsdpart(ldx, isr)= asdpart(ldx, isr)
          Zsdwsrat(ldx, isr)= asdwsrat(ldx, isr)
          Zhfredsat(ldx, isr)= ahfredsat(ldx, isr)
      end do

      return
      end

      subroutine restore_soil(isr, subrsurf)

      use erosion_data_struct_defs, only: subregionsurfacestate

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'

      include 'soil_save.inc'

      integer :: isr
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

      integer :: ldx

      subrsurf%asfald = Zsfald(isr)
      amrslp(isr) = Zmrslp(isr)
      SFCov(isr) = ZSFCov(isr)
      bedrock_depth(isr) = Zbedrock_depth(isr)
      restrict_depth(isr) = Zrestrict_depth(isr)
!   Crust Properties
      subrsurf%aszcr = Zszcr(isr)
      subrsurf%asdcr = Zsdcr(isr)
      subrsurf%asecr = Zsecr(isr)
      subrsurf%asfcr = Zsfcr(isr)
      subrsurf%asmlos = Zsmlos(isr)
      subrsurf%asflos = Zsflos(isr)
!   Surface roughness Properties
      aslrr(isr) = Zslrr(isr)
      aslrro(isr) = Zslrro(isr)
      asargo(isr) = Zsargo(isr)
      aszrgh(isr) = Zszrgh(isr)
      asxrgs(isr) = Zsxrgs(isr)
      asxrgw(isr) = Zsxrgw(isr)

!    Not sure if these surface variables need to be here
      ! aszrho(isr) = Zszrho(isr)
      ! asxdks(isr) = Zsxdks(isr)
      ! asxdkh(isr) = Zsxdkh(isr)
      ! as0rrk(isr) = Zs0rrk(isr)

!   Other
      subrsurf%asfalw = Zsfalw(isr)

!   aero based indexes
      ! asfsan(0, isr) = Zsfsan(0, isr)
      ! asfsil(0, isr) = Zsfsil(0, isr)
      ! asfcla(0, isr) = Zsfcla(0, isr)
      ! asvroc(0, isr) = Zsvroc(0, isr)

      ! aslagm(0, isr) = Zslagm(0, isr)
      ! as0ags(0, isr) = Zs0ags(0, isr)
      ! aslagx(0, isr) = Zslagx(0, isr)
      ! aslagn(0, isr) = Zslagn(0, isr)
      ! asdagd(0, isr) = Zsdagd(0, isr)
      ! aseags(0, isr) = Zseags(0, isr)

      ! asdblk(0, isr) = Zsdblk(0, isr)

      do ldx = 1, nslay(isr)
!       Layer thicknesses
          aszlyt(ldx, isr) = Zszlyt(ldx, isr)
!       IP surface physical properties
          asfsan(ldx, isr) = Zsfsan(ldx, isr)
          asfsil(ldx, isr) = Zsfsil(ldx, isr)
          asfcla(ldx, isr) = Zsfcla(ldx, isr)
          asvroc(ldx, isr) = Zsvroc(ldx, isr)
          asfvcs(ldx, isr) = Zsfvcs(ldx, isr)
          asfcs(ldx, isr)  = Zsfcs(ldx, isr)
          asfms(ldx, isr)  = Zsfms(ldx, isr)
          asffs(ldx, isr)  = Zsffs(ldx, isr)
          asfvfs(ldx, isr) = Zsfvfs(ldx, isr)
          asdwblk(ldx, isr)= Zsdwblk(ldx, isr)
!       IP soil chemical properties
          asfom(ldx, isr)  = Zsfom(ldx, isr)
          as0ph(ldx, isr)  = Zs0ph(ldx, isr)
          asfcce(ldx, isr) = Zsfcce(ldx, isr)
          asfcec(ldx, isr) = Zsfcec(ldx, isr)
          asfcle(ldx, isr) = Zsfcle(ldx, isr)
!       IC Aggregate properties
          aslagm(ldx, isr) = Zslagm(ldx, isr)
          as0ags(ldx, isr) = Zs0ags(ldx, isr)
          aslagx(ldx, isr) = Zslagx(ldx, isr)
          aslagn(ldx, isr) = Zslagn(ldx, isr)
          asdagd(ldx, isr) = Zsdagd(ldx, isr)
          aseags(ldx, isr) = Zseags(ldx, isr)
!       IC soil hydrologic properties
          asdblk(ldx, isr) = Zsdblk(ldx, isr)
          asdblk0(ldx, isr) = Zsdblk0(ldx, isr)
          ahrwc(ldx, isr)  = Zhrwc(ldx, isr)
!       soil hydrologic (water release curve) properties
          ahrwcs(ldx, isr) = Zhrwcs(ldx, isr)
          ahrwcf(ldx, isr) = Zhrwcf(ldx, isr)
          ahrwcw(ldx, isr) = Zhrwcw(ldx, isr)
!       soil hydrologic (water release curve) properties
          ah0cb(ldx, isr)  = Zh0cb(ldx, isr)
          aheaep(ldx, isr) = Zheaep(ldx, isr)
          ahrsk(ldx, isr)  = Zhrsk(ldx, isr)
!       other variables by depth
          asdsblk(ldx, isr)= Zsdsblk(ldx, isr)
          asdpart(ldx, isr)= Zsdpart(ldx, isr)
          asdwsrat(ldx, isr)= Zsdwsrat(ldx, isr)
          ahfredsat(ldx, isr)= Zhfredsat(ldx, isr)
      end do

      return
      end
