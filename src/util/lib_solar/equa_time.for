!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function   equa_time(idoy)

!     + + + PURPOSE + + +
!     This function calculates the declination of the earth with respect
!     the sun based on the day of the year

!     + + + KEYWORDS + + +
!     solar equation of time

!     + + + ARGUMENT DECLARATIONS + + +
      integer idoy

!     + + + ARGUMENT DEFINITIONS + + +
!     idoy   - Day of year

!     + + + LOCAL VARIABLES + + +
      real b

!     + + + LOCAL DEFINITIONS + + +
!     b      - sub calculation (time of year, radians)

!     + + + COMMON BLOCKS + + +
      include 'p1unconv.inc'

!     + + + END SPECIFICATIONS + + +

!     Calculate time of year (b)
      b = (360.0/365.0)*(idoy-81.25) * degtorad				!h-55
      equa_time = 9.87*sin(2*b)-7.53*cos(b)-1.5*sin(b)		!h-54

      return
      end
