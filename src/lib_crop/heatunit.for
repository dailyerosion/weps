!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function heatunit( tmax, tmin, thres )

!     calculates the amount of heat units in degree-days above a
!     threshold temperature assuming a fully sinusoidal daily 
!     temperature cycle with maximum and minimum 12 hours apart.

!     + + + ARGUMENT DECLARATIONS + + +
      real tmax, tmin, thres

!     + + + ARGUMENT DEFINITIONS + + +
!     tmax   - maximum daily air temperature
!     tmin   - minimum daily air temperature
!     thres  - threshold temperature (such as minimum temperature for growth)

!     + + + LOCAL VARIABLES + +
      real tmean, range, theta, pi

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     tmean  - arithmetic average of tmax and tmin
!     range  - daily range of maximum and minimum temperature
!     theta  - point where threshold and air temperature are equal
!              defines integration limits

!     + + + PARAMETERS + + +
      parameter (pi      = 3.1415927)

!     + + + END INITIALIZATIONS + + +

      if (thres .ge. tmax) then
          heatunit = 0.0
      else if ((thres .le. tmin) .or. (tmax .le. tmin)) then
          tmean = (tmax + tmin) / 2.0
          heatunit = tmean - thres
      else
          tmean = (tmax + tmin) / 2.0
          range = (tmax - tmin) / 2.0
          theta = asin( (thres - tmean) / range )
          heatunit = ( (tmean - thres) * (pi/2.0 - theta)               &
     &             + range * cos(theta) ) / pi
      end if

      return
      end

