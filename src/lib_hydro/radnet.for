!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function radnet( bcrlai, bweirr, snwc, sndp, bwtdmx, bwtdmn, &
     &                      bmalat, bsfalw, bsfald, idoy, bwtdpt )

!     + + + purpose + + +
!     this function estimates the net radiation for a given area (Mj/m^2/day)
!     using known solar radiation, air temperature, and vapor pressure
!     according to wright's (1982) modified version of penman's (1948)
!     relationship.

!     + + + key words + + +
!     radiation, hydrology, weps

      use solar_mod, only: radext

!     + + + argument declarations + + +
      real bcrlai, bweirr, snwc, sndp, bwtdmx, bwtdmn
      real bmalat, bsfalw, bsfald
      integer idoy
      real bwtdpt

!     + + + argument definitions + + +
!     bcrlai - plant leaf area index
!     bweirr - solar radiation, Mj/m^2/day
!     snwc   - water content of snow, mm
!     sndp   - actual depth of snow, mm
!     bwtdmx - maximum air temperature, c
!     bwtdmn - minimum air temperature, c
!     bmalat - latitude of the site, degrees
!     bsfalw - wet albedo
!     bsfald - dry albedo
!     idoy   - julian day of year, 1-366
!     bwtdpt - dew point temperature, c

!     + + + local variables + + +
      real albt
      real tmink
      real tmaxk
      real rna
      real rnb
      real ra
      real rso
      real a
      real a1
      real b
      real e
      real rno

!     + + + local definitions + + +
!     albt - composite albedo value (snow, plant, soil)
!     tmink, tmaxk - temperatures converted from degrees C to degrees K
!     rna  - net radiation term a
!     rnb  - net radiation term b
!     ra   - extraterrestrial radiation
!     rso  - terrestrial clear sky radiation
!     a, b - coefficients relating proportioning long wave and short wave
!            radiation exchange based on actual to clear sky ratio
!     a1, e - intermediate calculations
     
!     + + + parameters + + +

      real coeff,sbc

      parameter   (coeff= -2.9e-5, sbc = 4.903e-9)

!     coeff  - coefficient in the equation to estimate soil cover index
!     sbc    - stefan-boltzmann constant, mj/m^2/day

!     + + + FUNCTION DECLARATIONS + + +
      real albedo

!     + + + end specifications + + +

      tmaxk = bwtdmx + 273.15         !prereq h-17
      tmink = bwtdmn + 273.15         !prereq h-17

      ra = radext(idoy, bmalat)
      rso = 0.75*ra            !h-19

      if( rso.gt.1.0e-36 ) then
          if ((bweirr/rso).gt.(0.7)) then
            a = 1.126
            b = -0.07
          else
            a = 1.017
            b = -0.06
          end if
          a1 = 0.26 + 0.1*exp(-((0.0154*(idoy - 180))**2))   !h-18
          e = exp((16.78*bwtdpt - 117)/(bwtdpt + 237.3))
          rno = (sbc*(tmaxk**4+tmink**4)/2)*(a1 - 0.139 * sqrt(e)) !h-17(b)

          albt = albedo (bcrlai, snwc, sndp, bsfalw, bsfald)

          rna = (1-albt)*bweirr          !h-17(a)

          rnb = (a*(bweirr/rso) + b)        !h-17(c)

          radnet = rna-(rno*rnb)         !h-17
      else
          radnet = 0.0
      end if

      return
      end
