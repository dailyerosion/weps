!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function huc1 (bwtdmx, bwtdmn, bctmax, bctmin)

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     Calculate single day heat units for given temperatures

!     + + + KEYWORDS + + +
!     Heat units, daylength

!     + + + ARGUMENT DECLARATIONS + + +
      real bwtdmx, bwtdmn, bctmax, bctmin

!     + + + ARGUMENT DEFINITIONS + + +
!     bwtdmx - daily maximum air temperature
!     bwtdmn - daily minimum air temperature
!     bctopt - optimum crop growth temperature
!     bctmin - minimum crop growth temperature

!     + + + OUTPUT FORMATS + + +
!2000 FORMAT('+',109x,2(F8.2,1X))

!     + + + FUNCTION DECLARATIONS + + +
      real heatunit

!     + + + END OF SPECIFICATIONS + + +

      huc1 = heatunit(bwtdmx, bwtdmn, bctmin)                           &
     &     - heatunit(bwtdmx, bwtdmn, bctmax)
      if (huc1.lt.0.) huc1=0.

      return
      end
