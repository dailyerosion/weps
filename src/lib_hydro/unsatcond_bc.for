!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function unsatcond_bc(theta, thetar, thetas, ksat, lambda)
!     returns the unsaturated hydraulic conductivity in same units as ksat as 
!     defined by the Books and Corey function and the Mualem conductivity model

!*** Argument declarations ***
      real  theta, thetar, thetas, ksat, lambda

!     theta      - present volumetric water content
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     thetas     - saturated volumetric water content
!     ksat       - Saturated hydraulic conductivity (L/T) (modify to set units returned)
!     lambda     - Brooks adn Corey pore size interaction parameter 


!*** Local variable declarations ***
      real  satrat, minsatrat
      parameter( minsatrat = 1.0e-2 )
!     satrat     - conductivity relative saturation ratio
!     minsatrat  - used to clamp unsatcond_bc to zero and prevent underflow

      satrat = min(1.0,(theta-thetar)/(thetas-thetar))
      if( satrat.lt.minsatrat ) then
          unsatcond_bc = 0.0
      else
          unsatcond_bc = ksat*satrat**(2.5+2.0/lambda)
      endif
      return
      end

