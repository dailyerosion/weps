!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function volwat_matpot_bc(matricpot,thetar,thetas,           &
     &                                 airentry,lambda)
!     computes the volumetric water content at a matric potential

!*** Argument declarations ***
      real matricpot, thetar, thetas, airentry, lambda
!     matricpot  - soil water matric potential (pressure units of airentry)
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     thetas     - saturated volumetric water content
!     airentry   - Van Genuchten parameter (1/pressure) (modify to set units returned)
!     lambda     - Brooks adn Corey pore size interaction parameter 

!*** Local variable declarations ***
      real  satrat
!     satrat     - conductivity relative saturation ratio

      satrat = (airentry/matricpot)**lambda
      volwat_matpot_bc = (thetas-thetar)*satrat + thetar
      return
      end
