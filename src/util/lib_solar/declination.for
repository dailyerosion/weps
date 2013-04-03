!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function   declination(idoy)

!     + + + PURPOSE + + +
!     This function calculates the declination of the earth with respect
!     the sun based on the day of the year

!     + + + KEYWORDS + + +
!     solar declination

      use p1unconv_mod, only: degtorad

!     + + + ARGUMENT DECLARATIONS + + +
      integer idoy

!     + + + ARGUMENT DEFINITIONS + + +
!     idoy   - Day of year

!     + + + LOCAL VARAIBLES + + +
      real b

!     + + + LOCAL DEFINITIONS + + +
!     b      - sub calculation (time of year, radians)

!     + + + END SPECIFICATIONS + + +

!     Calculate declination angle (dec)
      b = (360.0/365.0)*(idoy-81.25) * degtorad            !h-55
      declination = 23.45*sin(b)                           !h-58

      return
      end
