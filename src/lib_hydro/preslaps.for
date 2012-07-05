!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function preslaps( elevation )

      ! returns the standard atmospheric pressure (kpa) as a function of
      ! elevation (m) based on curve fit by Abdu Durar to standard U.S.
      ! Atmosphere tables

      real elevation
      ! elevation  - elevation of location (m)

      real a1, a2, a3
      ! a1,a2,a3 - constants used to compute barametric pressure
      parameter   (a1 = 824.4996, a2 = 35702.8022, a3 = -607945000.)

      preslaps = a1 * exp(((elevation + a2)**2.) / a3)              !h-15

      return
      end
