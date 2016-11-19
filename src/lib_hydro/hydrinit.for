!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine hydrinit(isr, soil, h1et, wp)

! Contains init code from main

      use weps_interface_defs, only: saxpar
      use soil_data_struct_defs, only: soil_def
      use hydro_data_struct_defs, only: hydro_derived_et
      use wepp_param_mod, only: wepp_param

      include 'p1werm.inc'
      include 'h1db1.inc'
      include 'h1hydro.inc'
      include 'h1balance.inc'
      include 'h1temp.inc'
      include 'hydro/htheta.inc'

      include 'main/main.inc'
      include 'h1scs.inc'
      
      include 'wepp_erosion.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      type(soil_def), intent(inout) :: soil  ! soil for this subregion
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp

      integer idx
      real ltheta(mnsz)
      real sand(mnsz),clay(mnsz),orgmat(mnsz)

      do idx = 1, soil%nslay
        ! Initialize the water holding capacity variable
        soil%ahrwca(idx) = soil%ahrwcf(idx) - soil%ahrwcw(idx)
        ! set volumetric water content to initialize reporting variable
        ltheta(idx) = soil%ahrwc(idx) * soil%asdblk(idx)
      end do

      ! Set infiltration water depth to 0.0
      ahzwid(isr) = 0.0
      ahzeasurf(isr) = 0.0

      ! soil layer temperature, ice fraction
      do idx = 1, soil%nslay
          ahtsav(idx, isr) = 0.0
          ahfice(idx, isr) = 0.0
      end do

      ahzsno(isr) = 0.0
      ahtsno(isr) = 0.0
      ahfsnfrz(isr) = 0.0

      ! set hydrologic balance variables
      initswc(isr) = dot_product(ltheta(1:soil%nslay),                  &
     &                           soil%aszlyt(1:soil%nslay))
      initsnow(isr) = ahzsno(isr)
      initday(isr) = daysim

      presswc(isr) = initswc(isr)
      pressnow(isr) = initsnow(isr)
      presday(isr) = initday(isr)

      cumprecip(isr) = 0.0
      cumirrig(isr) = 0.0
      cumrunoff(isr) = 0.0
      cumevap(isr) = 0.0
      cumtrans(isr) = 0.0
      cumdrain(isr) = 0.0
      hprevrotation(isr) = 1

!     Initialize irrigation type and depth so values are set if no
!     irrigation processes are invoked
      h1et%zirr = 0.0
      ahratirr(isr) = 0.0
      ahdurirr(isr) = 0.0
      ahlocirr(isr) = 0.0
      am0monirr(isr) = 0
      ahmadirr(isr) = 0.0
      ahminirr(isr) = 0.0
      ahndayirr(isr) = 0
      ahmintirr(isr) = 0

      h1et%zrun = 0.0
      ahzsmt(isr) = 0.0
      ah0cng(isr) = 0.0
      ah0cnp(isr) = 0.0
      ahfwsf(isr) = 1.0
      h1et%zper = 0.0

      do idx = 1, soil%nslay
          ahtsmx(idx, isr) = 0.0
          ahtsmn(idx, isr) = 0.0
      end do

      do idx = 1, soil%nslay
          ! Soil layer sand content (Mg/Mg)
          sand(idx) = soil%asfsan(idx)
          ! Soil layer clay content (Mg/Mg)		
          clay(idx) = soil%asfcla(idx)
          ! Soil layer organic matter content (Mg/Mg)		
          orgmat(idx) = soil%asfom(idx)
      end do
	
      call saxpar(sand,clay,orgmat,soil%nslay,wp%saxwp,wp%saxfc,        &
     &    wp%saxenp,wp%saxpor,wp%saxA, wp%saxB,wp%saxks)

      ! Added for WEPP bookeeping      
      wp%totalPrecip = 0.0
      wp%totalRunoff = 0.0
      wp%precipEvents = 0
      wp%runoffEvents = 0
      wp%snowmeltEvents = 0
      wp%totalSnowrunoff = 0.0
      wp%prev_crust_frac = -1.0
      wp%rkecum = 0.0
      ! End WEPP addition      

      ahztranspdepth(isr) = 0.0
      ahzfurcut(isr) = 0.0
      ahztransprtmin(isr) = 0.0
      ahztransprtmax(isr) = 0.0

      ! initializing a previously un-init'd variable
      h1et%zea = 0.0
      h1et%zep = 0.0
      h1et%zeta = 0.0
      h1et%zetp = 0.0
      h1et%zpta = 0.0
      h1et%zptp = 0.0
      h1et%zsnd = 0.0
      h1et%snow_protect = 0.0
      return
      end
