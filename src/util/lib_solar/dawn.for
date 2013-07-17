!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function   dawn(dlat,dlong,idoy,riseangle)

!     + + + PURPOSE + + +
!     This function calculates the time of sunrise (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  The inputs for the function are day of the year, latitude
!     of the site, and longitude of the site.

!     + + + KEYWORDS + + +
!     sunrise

!     + + + ARGUMENT DECLARATIONS + + +
      integer idoy
      real dlat
      real dlong
      real riseangle

!     + + + ARGUMENT DEFINITIONS + + +
!     idoy   - Day of year
!     dlat   - Latitude of the site, degrees (north > 0, south < 0)
!     dlong  - Longitude of the site, degrees (east > 0, west < 0)
!     riseangle - angle of earths rotation where sunrise occurs
!                 this varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight hourangle

!     + + + LOCAL VARIABLES + + +
      real dec, e, h, sn

!     + + + LOCAL DEFINITIONS + + +
!     dec    - declination of earth with respect to the sun (degrees)
!     e      - Equation of time (minutes)
!     h      - Hour angle (degrees)
!     sn     - Solar noon (hour of the day, midnight = 0.0)

!     + + + FUNCTION DECLARATIONS + + +
      real declination
      real hourangle
      real equa_time

!     + + + END SPECIFICATIONS + + +

!     declination angle (dec)
      dec = declination(idoy)

!     sunset hour angle (noon is zero degrees, sunset (+), sunrise (-))
      h = hourangle(dlat, dec, riseangle)

!     equation of time (e)
      e = equa_time(idoy)

!     Calculate solar noon (sn)
      sn = 12.0-e/60.0-4.0*(15*nint(-dlong/15.0)+dlong)/60.0 !h-53

!     Calculate the time of sunrise (rise)
      dawn = sn - h/15.0                                   !h-52

!     to prevent errors of bleed over into previous day where
!     where daylength is 24 hours, limit time of sunrise
      dawn = max(0.0, dawn)

      return
      end
