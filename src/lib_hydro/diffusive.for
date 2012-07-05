!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function diffusive( theta, porosity, airtemp, atmpres )

!     calculation of the soil water vapor diffusivity in air (m^2/sec)
!     using the methods from Campbell (1985) to account for temperature,
!     pressure and air filled porosity

!*** Argument declarations ***
      real theta, porosity, airtemp, atmpres
!     theta    - volumetric soil water content
!     porosity - total soil porosity (air + water volume fraction)
!     airtemp  - air temperature (c)
!     atmpres  - atmospheric pressure (kPa)

!*** Include files ***
      include 'hydro/vapprop.inc'

!*** Local declarations ***
      real diffutp, airpore, poreb, porem
!     diffutp     - diffusivity adjusted for temperature and pressure (m^2/s)
!     soilairpore - soil air filed porosity (m^3/m^3)
!     poreb       - b coefficient for diffusivity air filled pore function
!     porem       - m coefficient for diffusivity air filled pore function
      parameter (poreb = 0.66)
      parameter (porem = 1.0)

      diffutp = diffuntp * atmstand / atmpres                           &
     &        * ((airtemp+zerokelvin)/zerokelvin)**2
      airpore = max(0.0,porosity - theta)
      diffusive = diffutp * poreb * airpore ** porem

      return
      end
