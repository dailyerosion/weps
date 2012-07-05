!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine matricpot_bc(theta, thetar, thetas, airentry, lambda,  &
     &                        thetaw, theta80rh, soiltemp,              &
     &                        matricpot, soilrh )

!     returns: matricpot, soilrh
!     returns the matric potential in meters of water as defined by the 
!     Brooks and Corey function down to wilting point. Below wilting point,
!     the calculation is done based on clay and organic matter adsorption
!     isotherms. Coincidentally, the soil relative humidity is used to find 
!     the matric potential from the clay isotherms and is also returned for
!     potentials in the wetter range.

!*** Argument declarations ***
      real  theta, thetar, thetas, airentry, lambda
      real  thetaw, theta80rh, soiltemp
      real  matricpot, soilrh
!     theta      - present volumetric water content
!     thetar     - volumetric water content where hydraulic conductivity becomes zero
!     thetas     - saturated volumetric water content
!     airentry   - Brooks/Corey air entry potential (1/pressure) (modify to set units returned)
!     lambda     - Brooks/Corey pore size interaction parameter 
!     thetaw     - volumetric water content at wilting (15 bar or 1.5 MPa)
!     theta80rh  - volumetric water content at %80 relative humidity (300 bar or 30 MPa)
!     soiltemp   - soil temperature (C)
!     matricpot  - matric potential (meters of water)
!     soilrh     - relative humidity of soil air (fraction)

!*** function declarations ***
      real soilrelhum, matricpot_from_rh

!*** Include files ***
      include 'hydro/vapprop.inc'

!*** Local variable declarations ***
      real*4  satrat
!     satrat     - conductivity relative saturation ratio

      if( theta .ge. thetaw ) then
          satrat = (theta-thetar)/(thetas-thetar)
          if( satrat .le. 0.0 ) then
              write(0,*) 'matricpot_bc: thetar= ',thetar,               &
     &                   ' thetaw= ', thetaw
              write(0,*)                                                &
     &  'Error: residual water content is greater than wilting point'
              call exit(1)
              !stop
          else if( satrat .ge. 1.0 ) then
              matricpot = airentry
          else
              matricpot = airentry*satrat**(-1.0/lambda)
          end if
          soilrh = soilrelhum(theta, thetaw, theta80rh, soiltemp,       &
     &                        matricpot)
      else
          soilrh = soilrelhum(theta, thetaw, theta80rh, soiltemp,       &
     &                        matricpot)
          matricpot = matricpot_from_rh( soilrh, soiltemp )
      end if
      return
      end

