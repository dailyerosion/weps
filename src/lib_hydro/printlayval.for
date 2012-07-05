!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine printlayval( daysim, layrsn,                           &
     &       bszlyt, bszlyd, bulkden,                                   &
     &       theta, thetas, thetaf, thetaw, thetar,                     &
     &       bhrsk, bheaep, bh0cb, bsfcla, bsfom, bhtsav )

!     + + + PURPOSE + + +
!     This subroutine print out soil hydro properties by layer

!     + + + KEYWORDS + + +
!     output hydro

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim, layrsn
      real bszlyt(*), bszlyd(*), bulkden(*)
      real theta(0:*), thetas(*), thetaf(*), thetar(*), thetaw(*)
      real bhrsk(*), bheaep(*), bh0cb(*), bsfcla(*), bsfom(*), bhtsav(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     daysim     - day of the simulation (very useful for debugging, not necessary otherwise)
!     bszlyt(*)  - thickness of the soil layer (mm)
!     bszlyd(*)  - depth to bottom of the soil layer (mm)
!     bulkden(*) - soil bulk density Mg/m^3)
!     theta(*)   - volumetric water content (m^3/m^3)
!     thetas(*)  - saturated volumetric water content (m^3/m^3)
!     thetaf(*)  - field capacity volumetric water content (m^3/m^3)
!     thetar(*)  - residual (conductivity) volumetric water content (m^3/m^3)
!     thetaw(*)  - wilting point volumetric water content (m^3/m^3)
!     bhrsk(*)   - saturated hydraulic conductivity (m/s)
!     bheaep(*)  - air entry potential (J/kg)
!     bh0cb(*)   - exponent of Campbell soil water release curve (unitless)
!     bsfcla(*)  - fraction of soil mineral content which is clay (unitless)
!     bsfom(*)   - fraction of total soil which is organic (unitless)
!     bhtsav(*)  - daily average soil temperature (C)

!     + + + PARAMETERS + + +
      real   pi
      parameter( pi = 3.1415927 )

!     + + + COMMON BLOCKS + + +
      include 'file.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/vapprop.inc'
      include 'hydro/clayomprop.inc'

!     + + + LOCAL VARIABLES + + +
      integer    idx, day, mo, yr
      integer    idoy
      real       availwat, temp
      real       unsatcond, matricpot, soilrh
      real       laycenter, sat_rat
      real       airentry, lambda, theta80rh

!     + + + LOCAL DEFINITIONS + + +
!     idx   - array index for loops
!     day      - day of month
!     mo       - month of year
!     yr       - year of simulation
!     availwat  - soil plant availale water content (for output)
!     unsatcond - unsaturated hydraulic conductivity (m/s) (for output)
!     matricpot - soil matric potential (m) (for output)
!     layercenter - depth to the center of a soil layer (mm) (for output)
!     sat_rat - saturation ratio (for output)

!     + + + SUBROUTINES CALLED + + +
!     slsoda - livermore solver for ordinary differential equations

!     + + + FUNCTION DECLARATIONS + + +
      integer dayear
      real    availwc
      real    unsatcond_bc
      real    volwatadsorb
      real    volwat_matpot_bc

!     + + + END SPECIFICATIONS + + +

      call caldatw(day,mo,yr)
      idoy = dayear(day, mo, yr)
      if( idoy .eq. 1 ) then
         ! insert double blank line to break years into blocks for graphing
         write(luohlayers,*)
         write(luohlayers,*)
      else
         ! print a single blank line to separate layer blocks
         write(luohlayers,*)
      end if
      do idx=1,layrsn
         lambda = 1.0 / bh0cb(idx)
         availwat = availwc( theta(idx), thetaw(idx), thetaf(idx) )
         unsatcond = unsatcond_bc( theta(idx), thetar(idx),             &
     &               thetas(idx), bhrsk(idx), lambda )
         airentry = bheaep(idx) / gravconst
         temp = bulkden(idx)*1000.0  !convert Mg/m^3 to kg/m^3
         theta80rh = volwatadsorb( temp, bsfcla(idx), bsfom(idx),       &
     &               claygrav80rh, orggrav80rh )
         call matricpot_bc( theta(idx), thetar(idx), thetas(idx),       &
     &        airentry, lambda, thetaw(idx), theta80rh, bhtsav(idx),    &
     &        matricpot, soilrh )
         laycenter = bszlyd(idx) - 0.5*bszlyt(idx)
         sat_rat = (theta(idx)-thetar(idx)) / (thetas(idx)-thetar(idx))
 2190    format(1x,i5,1x,i3,1x,i4,1x,i3,1x,16g11.3)
         write(luohlayers,2190) daysim, idoy, yr, idx, laycenter,       &
     &         theta(idx), thetas(idx), thetaf(idx), thetaw(idx),       &
     &         thetar(idx), availwat, sat_rat, bhtsav(idx),             &
     &         unsatcond, -matricpot, soilrh, bulkden(idx),             &
     &         -airentry, bh0cb(idx), bhrsk(idx)
      end do

      return
      end
