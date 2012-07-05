!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function   hourangle(dlat, dec, riseangle)

!     + + + PURPOSE + + +
!     This function calculates the hour angle (degrees)
!     of sunrise (-), sunset (+) based on the declination of the earth

!     + + + KEYWORDS + + +
!     sunrise sunset hourangle

!     + + + ARGUMENT DECLARATIONS + + +
      real dlat
      real dec
      real riseangle

!     + + + ARGUMENT DEFINITIONS + + +
!     dlat   - Latitude of the site, degrees (north > 0, south < 0)
!     dec    - declination of earth with respect to the sun (degrees)
!     riseangle - angle of earths rotation where sunrise occurs
!                 this varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight hourangle

!     + + + LOCAL VARIABLES + + +
      real coshr
      real dlat_rad, dec_rad
      real dlat_rad_lim

      parameter( dlat_rad_lim = 1.57079 ) ! pi/2 minus a small bit
!      parameter( dlat_rad_lim = 1.570796327 ) ! pi/2

!     + + + LOCAL DEFINITIONS + + +
!     coshr   - Cosine of hour angle at sunrise
!     dlat_rad - latitude of site, converted to radians
!     dec_rad - declination of earth wrt the sun (radians)

!     + + + COMMON BLOCKS + + +
      include 'p1unconv.inc'

!     + + + END SPECIFICATIONS + + +

!     convert to radians
      dlat_rad = dlat * degtorad
      dec_rad = dec * degtorad

!     Calculate the cosine of hour angle (h) at sunset
!     To get the sunrise hour angle, take the negative.
!     Using the equation from "Solar Thermal Energy Systems,
!     Howell, Bannerot, Vliet, 1982, page 51 equation 3-4)
!     modified to account for atmospheric refraction as in
!     NOAA document (it just indicates that the sun is seen
!     before it physically is above the horizon)
!     ie. not at 90 degrees, but 90.833 degrees
!     THis expression is undefined at 90 and -90 degrees. If
!     roundoff error pushes it beyond the answer flips. Limit
!     set here to get correct answer at 90 and -90 degrees.
      dlat_rad = max( -dlat_rad_lim, min(dlat_rad_lim, dlat_rad))
      coshr = cos(riseangle*degtorad)/(cos(dlat_rad)*cos(dec_rad))      &
     &      - tan(dlat_rad)*tan(dec_rad)

!     check for artic circle conditions
      if( coshr.ge.1.0) then
          hourangle = 0.0          !sunrise occurs at solar noon
      else if( coshr.le.-1.0) then
          hourangle = 180.0        !the sun is always above the horizon
      else
          hourangle = acos(coshr) * radtodeg
      end if

      return
      end
