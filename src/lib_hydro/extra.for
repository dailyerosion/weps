!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!     file name: extra.for

      real function extra (bszlyd, theta)

!     + + + PURPOSE + + +
!     This subroutine extrapolates soil water content to the surface
!     from the three uppermost simulation layers.  A numerical
!     solution known as Cramer's rule is used to obtain an estimate
!     of the extrapolated surface soil water content by solving the
!     three simultaneous equations that describe the relationship
!     between soil water content and soil depth for the three
!     uppermost simulation layers.
!     DATE:  09/22/93
!     MODIFIED:  10/06/93

!     + + + KEY WORDS + + +
!     soil, water content

!     + + + ARGUMENT DECLARATIONS + + +
      real bszlyd(*)
      real theta(0:*)

!     + + + ARGUMENT DEFINITIONS + + +
!     bszlyd  - Depth to bottom of soil layer from surface (mm)
!     theta   - soil water content by layer (m^3/m^3)

!     + + + COMMON BLOCKS + + +

      include 'p1werm.inc'

!     + + + LOCAL COMMON BLOCKS + + +
! ***      include 'hydro/htheta.inc'

!     + + + LOCAL VARIABLES + + +
      real d
      real d1

!     + + + LOCAL DEFINITIONS + + +
!     d      - The determinant of the coefficient matrix.
!     d1     - The determinant of the matrix formed by substituting
!              load vector into column 1 of the coefficient matrix.

!     + + + END SPECIFICATIONS + + +


      d = (bszlyd(2)*bszlyd(3)**2) + (bszlyd(3)*bszlyd(1)**2) +         &
     &    (bszlyd(1)*bszlyd(2)**2) - (bszlyd(2)*bszlyd(1)**2) -         &
     &    (bszlyd(1)*bszlyd(3)**2) - (bszlyd(3)*bszlyd(2)**2)

      d1 = (theta(1)*bszlyd(2)*bszlyd(3)**2) +                          &
     &     (theta(2)*bszlyd(3)*bszlyd(1)**2) +                          &
     &     (theta(3)*bszlyd(1)*bszlyd(2)**2) -                          &
     &     (theta(3)*bszlyd(2)*bszlyd(1)**2) -                          &
     &     (theta(2)*bszlyd(1)*bszlyd(3)**2) -                          &
     &     (theta(1)*bszlyd(3)*bszlyd(2)**2)

! Check to make sure that "d" is not too close to zero (thetax gets big)
      if (d .lt. 0.0000001) then
          extra = 1.0e30
      else 
          extra= d1/d
      endif

      return
      end
