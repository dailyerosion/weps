!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   psd (sandm, siltm, claym, pgmd, pgsd)

!     + + + PURPOSE + + +
!     This subroutine calculates the soil geometric mean diameter and
!     geometric standard deviation from percent sand, silt, and clay
!     using geometric mean diameters within each of the three soil
!     particle size fractions.
!     From: Shirazi, M.A., Boersma, L. 1984. A unifying Quantitative
!     Analysis of Soil Texture. SOil Sci. Soc. Am. J. 48:142-147
!     DATE:  09/29/93

!     + + + KEY WORDS + + +
!     Geometric mean diameter (GMD), Geometric standard deviation (GSD)

!     + + + ARGUMENT DECLARATIONS + + +
      real claym
      real pgmd
      real pgsd
      real sandm
      real siltm

!     + + + ARGUMENT DEFINITIONS + + +
!     sandm  - Mass fraction of sand
!     siltm   - Mass fraction of silt
!     claym  - Mass fraction of clay
!     pgmd   - Geometric mean diameter of psd (mm)
!     pgsd   - Geometric std deviation of psd (mm)

!     + + + PARAMETERS + + +

      real   sandg, siltg, clayg

      parameter   (sandg = 1.025, siltg = 0.026, clayg = 0.001)

!     sandg - percent sand
!     siltg - percent silt
!     clayg - percent clay

!     + + + LOCAL VARIABLES + + +
      real a, b

!     + + + LOCAL DEFINITIONS + + +
!     a, b   - Temporary variables

!     + + + END SPECIFICATIONS + + +

!     calculate geometric mean diameter
          a = sandm*log(sandg)+siltm*log(siltg)+claym*log(clayg)
          pgmd = exp(a)

!     calculate geometric standard deviation
          b = (sandm*log(sandg)**2 + siltm*log(siltg)**2 +               &
     &         claym*log(clayg)**2)
          pgsd = exp(sqrt(b-a**2))

      return
      end
