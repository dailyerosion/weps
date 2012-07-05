!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      real function waterk (bd, cb, clay, silt)
!
!     + + + purpose + + +
!     this function estimates soil saturated hydraulic conductivity
!     if it is not readily available.  the function predicts saturated
!     hydraulic conductivity as a function of soil particle size dis-
!     tribution and bulk density (eq. 6.12a, p. 54)
!     reference:  campbell, g.s. 1985. soil physics with basic: trans-
!                 port models for soil-plant systems.  elsevier science
!                 publishers b.v.  amsterdam, the netherlands.
!
!     + + + argument declaration + + +
      real bd
      real cb
      real clay
      real silt
!
!     + + + argument definitions + + +
!     bd     - soil bulk density (Mg/m^3)
!     cb     - soil pore size scaling exponent
!     clay   - clay fraction
!     silt   - silt fraction
!     waterk - saturated hydraulic conductivity (m/s)

!     + + + end specifications + + +
!
      waterk = 3.92e-5*((1.3/bd)**(1.3*cb))*exp((-6.9*clay)-(3.7*silt))
!
      return
      end
