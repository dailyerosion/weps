!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine hydrinit(isr)

! Contains init code from main

      include 'p1werm.inc'
      include 'h1db1.inc'
      include 'h1et.inc'
      include 'h1hydro.inc'
      include 'h1balance.inc'
      include 's1layr.inc'
      include 's1phys.inc'
      include 'h1temp.inc'
      include 'hydro/htheta.inc'

      include 'main/main.inc'
      include 'w1clig.inc'
      include 'h1scs.inc'

      integer isr, idx
      real ltheta(mnsz)

      do idx = 1, nslay(isr)
        ! Initialize the water holding capacity variable
        ahrwca(idx,isr) = ahrwcf(idx, isr) - ahrwcw(idx,isr)
        ! set volumetric water content to initialize reporting variable
        ltheta(idx) = ahrwc(idx,isr) * asdblk(idx,isr)
      end do

      ! Set infiltration water depth to 0.0
      ahzwid(isr) = 0.0
      ahzeasurf(isr) = 0.0

      ! soil layer temperature, ice fraction
      do idx = 1, nslay(isr)
          ahtsav(idx, isr) = 0.0
          ahfice(idx, isr) = 0.0
      end do

      ahzsno(isr) = 0.0
      ahtsno(isr) = 0.0
      ahfsnfrz(isr) = 0.0
      ahzsnd(isr) = 0.0

      ! set hydrologic balance variables
      initswc(isr) = dot_product(ltheta(1:nslay(isr)),                  &
     &                           aszlyt(1:nslay(isr),isr))
      initsnow(isr) = ahzsno(isr)
      initday(isr) = daysim

      presswc(isr) = initswc(isr)
      pressnow(isr) = initsnow(isr)
      presday(isr) = initday(isr)

      cumprecip(isr) = 0.0
      cumrunoff(isr) = 0.0
      cumevap(isr) = 0.0
      cumtrans(isr) = 0.0
      cumdrain(isr) = 0.0
      hprevrotation(isr) = 1

!     Initialize irrigation type and depth so values are set if no
!     irrigation processes are invoked
      ahzirr(isr) = 0.0
      ahratirr(isr) = 0.0
      ahdurirr(isr) = 0.0
      ahlocirr(isr) = 0.0
      am0monirr(isr) = 0
      ahmadirr(isr) = 0.0
      ahminirr(isr) = 0.0
      ahndayirr(isr) = 0
      ahmintirr(isr) = 0

      rkecum = 0.0

      ! first call to hdbug, finds this uninitialized, It is set in hydro/et
      awrrh = 0.0
      ahzrun(isr) = 0.0
      ahzsmt(isr) = 0.0
      ah0cng(isr) = 0.0
      ah0cnp(isr) = 0.0

      do idx = 1, nslay(isr)
          ahtsmx(idx, isr) = 0.0
          ahtsmn(idx, isr) = 0.0
      end do


      ahztranspdepth(isr) = 0.0
      ahzfurcut(isr) = 0.0
      ahztransprtmin(isr) = 0.0
      ahztransprtmax(isr) = 0.0

      ! initializing a previously un-init'd variable
      ahzpta = 0

      return
      end
