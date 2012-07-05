!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function matricpot_from_rh( soilrh, soiltemp )

!     returns: matricpot
!     returns the matric potential in meters of water as defined by the 
!     clay and organic matter adsorption isotherms.

!*** Argument declarations ***
      real  soilrh, soiltemp
!     soilrh     - relative humidity of soil air (fraction)
!     soiltemp   - soil temperature (C)

!*** Include files ***
      include 'hydro/vapprop.inc'

      matricpot_from_rh = rgas*(soiltemp+zerokelvin)*(log(soilrh))      &
     &                  / (molewater * gravconst)

      return
      end

