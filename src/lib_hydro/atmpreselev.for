!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function atmpreselev( elevation )

!     returns the standard atmospheric pressure adjusted for elevation (kPa)
!     Approximation from Cuenca (1989) page 141

!*** Argument declarations ***
      real elevation
!     elevation - the elevation of the site above mean standard sea level (m)

!*** Include files ***
      include 'hydro/vapprop.inc'

      atmpreselev = atmstand                                            &
     &            * ((tempstand - templapse*elevation)/tempstand)       &
     &            ** (gravconst/(templapse*rair))

      return
      end
