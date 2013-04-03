!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function radext(idoy, bmalat)

!     + + + purpose + + +
!     this subroutine estimates the incoming extraterrestial radiation
!     for a given location (Mj/m^2/day)

!     + + + key words + + +
!     radiation, solar, extraterrestrial

      use p1unconv_mod, only: pi, degtorad

!     + + + argument declarations + + +
      integer idoy
      real bmalat

!     + + + argument definitions + + +
!     idoy     - julian day of year, 1-366
!     bmalat   - latitude of the site, degrees

!     + + + COMMON BLOCK + + +
      include 'p1solar.inc'

!     + + + local variables + + +
      real rlat
      real dec
      real rdec
      real dr
      real ws
      real ra1
      real ra2

!     + + + local definitions + + +
!     dec    - declination of the earth with respect to the sun (degrees)
!     dr     - direct radiation variation with distance from sun of earth orbit
!     ra1, ra2 - intermediate calculations
!     rlat   - latitude (radians)
!     rdec   - declination (radians)
!     ws     - sunset hour angle (radians)

!     + + + parameters + + +

      real gsc

      parameter (gsc = 0.08202)

!     gsc - solar_constant in Mj/m^2-min (0.08202 Mj/m^2-min = 1367 W/m^2)

!     + + + data initialization + + +

!     + + + FUNCTION DECLARATIONS + + +
      real declination
      real hourangle

!     + + + end specifications + + +

!     convert to radians for trig functions
      rlat = bmalat * degtorad
      dec = declination(idoy)
      rdec  = dec * degtorad

!     compute factor for variable distance from sun along orbital path
      dr = 1 + 0.033*cos(2*pi*idoy/365)                                !h-21

      ws = hourangle(bmalat, dec, beamrise ) * degtorad
      ra1 = ((24.0*60.0)/pi)*gsc*dr                                    !h-20(a)
      ra2 = (ws*sin(rlat)*sin(rdec))+(cos(rlat)*cos(rdec)*sin(ws))     !h-20(b)
      radext = ra1*ra2                                                 !h-20

      return
      end
