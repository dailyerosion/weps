!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine param_blkden_adj( nlay, bsdblk, bsdblk0,               &
     &                         bsdpart, bhrwcf, bhrwcw, bhrwca,         &
     &                         bsfcla, bsfom,                           &
     &                         bh0cb, bheaep, bhrsk )

!     + + + PURPOSE + + +
!     
!     This subroutine adjusts the air entry potential and saturated hydraulic
!     for changes in bulk density

!     + + + KEYWORDS + + +
!     bulk density adjustment hydraulic parameters

      include 'hydro/clayomprop.inc'

!     + + + ACCESSED COMMON BLOCK VARIABLE DEFINITIONS + + +
!     claygrav80rh - gravimetric water content of clay at 80 percent relative humidity
!     orggrav80rh  - gravimetric water content of soil organic matter at 80 percent relative humidity

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bsdblk(*), bsdblk0(*)
      real bsdpart(*), bhrwcf(*), bhrwcw(*), bhrwca(*)
      real bsfcla(*), bsfom(*)
      real bh0cb(*), bheaep(*), bhrsk(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay     - number of soil layers to be updated
!     bsdblk   - bulk density (Mg/m^3)
!     bsdblk0  - previous day bulk density (Mg/m^3)
!     bsdpart  - particle density (Mg/m^3)
!     bhrwcf   - gravimetric 1/3 bar water
!     bhrwcw   - gravimetric 15 bar water
!     bhrwca   - gravimetric plant available water
!     bsfcla   - fraction of soil mineral portion which is clay
!     bsfomf   - fraction of total soil mass which is organic matter
!     bh0cb    - Brooks and Corey pore size interation exponent b
!     bheaep   - Brooks and Corey air entry potential
!     bhrsk    - Saturated hydraulic conductivity (m/s)

!     + + + FUNCTION DECLARATIONS + + +
      real volwatadsorb
      real volwat_matpot_bc

!     + + + LOCAL VARIABLES + + +
      integer lay
      real thetas, thetaf, thetaw, thetar
      real temp, temp1

!     + + + LOCAL VARIABLE DEFINITIONS + + +

!     + + + END SPECIFICATIONS + + + 

      do lay=1,nlay
!         adjust air entry potential from Campbell (1985) pg 46
          bheaep(lay) = bheaep(lay)                                     &
     &                * (bsdblk(lay)/bsdblk0(lay))**(0.67*bh0cb(lay))
!         adjust saturated hydraulic conductivity based on Campbell (1985) pg 54
          bhrsk(lay) = bhrsk(lay)                                       &
     &               * (bsdblk0(lay)/bsdblk(lay))**(1.34*bh0cb(lay))
!         update previous day bulk density since changes done
          bsdblk0(lay) = bsdblk(lay)

!             reverse calculation of field capacity and wilting point
          thetas = 1 - bsdblk(lay) / bsdpart(lay)   ! saturation

!!        use theta corresponding to 80% relhum in soil for thetar
          temp = bsdblk(lay)*1000.0  !convert Mg/m^3 to kg/m^3
          thetar = volwatadsorb( temp, bsfcla(lay), bsfom(lay),         &
     &                           claygrav80rh, orggrav80rh )

          temp = -33.33
          temp1 = 1.0 / bh0cb(lay)
          thetaf = volwat_matpot_bc(temp, thetar, thetas,               &
     &                              bheaep(lay), temp1)
          temp = -1500.0
          thetaw = volwat_matpot_bc(temp, thetar, thetas,               &
     &                              bheaep(lay), temp1)

!         update gravimetric values for these properties
          bhrwcf(lay) = thetaf / bsdblk(lay)        ! field capacity
          bhrwcw(lay) = thetaw / bsdblk(lay)        ! wilting point
          bhrwca(lay) = bhrwcf(lay) - bhrwcw(lay)   ! plant available capacity
      end do

      end
