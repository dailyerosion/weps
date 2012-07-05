!$Author$
!$Date$
!$Revision$
!$HeadURL$
 
      real function temps(bwtdmx, bwtdmn, bctopt, bctmin)

!     Author : Amare Retta
!     + + + PURPOSE + + +
!     To calculate the temperature stress factor
!     This algorithms was taken from the EPIC subroutine cgrow.

!     + + + KEWORDS + + +
!     Temperature stress

!     + + + ARGUMENT DECLARATIONS + + +
      real bwtdmx, bwtdmn, bctopt, bctmin

!     + + + ARGUMENT DEFINITIONS + + +
!     bwtdmx - daily maximum air temperature
!     bwtdmn - daily minimum air temperature
!     bctopt - optimum crop growth temperature
!     bctmin - minimum crop growth temperature

!     + + + LOCAL VARIABLES + + +
      real tgx, x1, rto, dst0

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     tgx - difference between the soil surface temperature and the minimum
!           temperature for plant growth
!     x1  - difference between the optimum and minimum temperatures for plant
!           growth
!     rto - interim variable

!     + + + END OF SPECIFICATIONS + + +

!     calculate temperature stress factor
!     following one statement to be removed when soil temperature is available
      dst0 = (bwtdmx + bwtdmn) / 2.0
      tgx=dst0-bctmin
      if (tgx.le.0.) tgx=0.
      x1=bctopt-bctmin
      rto=tgx/x1
      temps=sin(1.5707*rto)
      if (rto.gt.2.) temps=0.

      ! this reduces temperature stress around the optimum
      temps = temps**0.25

      return
      end
