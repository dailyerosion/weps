!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function   daylen(dlat,idoy,riseangle)

!     + + + PURPOSE + + +
!     This function calculates the daylength (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  The inputs for the function are day of the year, and latitude
!     of the site.

!     + + + KEYWORDS + + +
!     day length

!     + + + ARGUMENT DECLARATIONS + + +
      integer idoy
      real dlat
      real riseangle

!     + + + ARGUMENT DEFINITIONS + + +
!     idoy   - Day of year
!     dlat   - Latitude of the site, degrees (north > 0, south < 0)
!     riseangle - angle of earths rotation where sunrise occurs
!                 this varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight daylength

!     + + + LOCAL VARIABLES + + +
      real dec, h

!     + + + LOCAL DEFINITIONS + + +
!     dec    - declination of earth with respect to the sun (degrees)
!     h      - Hour angle (degrees)

!     + + + FUNCTION DECLARATIONS + + +
      real declination
      real hourangle

!     + + + END SPECIFICATIONS + + +

!     declination angle (dec)
      dec = declination(idoy)

!     sunrise or sunset hour angle
      h = hourangle(dlat, dec, riseangle)

!     Calculate the length of the day
      daylen= 2.0*h/15.0

      return
      end
