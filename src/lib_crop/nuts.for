!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!     file name: nuts.for

      subroutine nuts (y1, y2, uu)

!     + + + PURPOSE + + +
!     This subroutine calculates a nutrient stress factor caused by limited
!     supply of N or P.

!     + + + KEYWORDS + + +
!     Nutrient stress

!     + + + COMMON BLOCKS + + +

      include 'crop/cparm.inc'

!     + + + LOCAL VARIABLES + + +
      real y1, y2, yy, uu

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     y1 - cummulative amount of N or P taken by the plant (kg/ha) - supply
!     y2 - potential amount of N or P needed by the plant (kg/ha) - demand
!     yy - scaled ratio of supply over demand
!     uu - N or P stress factor
!     yy replaces uu where appropriate to minimize confusion
!     a_s8,b_s8 are used instead of scrp(8,1) and scrp(8,2)

!     + + + END OF SPECIFICATIONS + + +

      if (y2 .eq. 0.) goto 3
      yy = 200. * (y1 / y2 - .5)
      if (yy .gt. 0.) go to 2
      uu = 0.
      go to 3
 2    continue
      uu = yy / (yy + exp(a_s8 - b_s8 * yy))
 3    continue

      return
      end
