!$Author$
!$Date$
!$Revision$
!$HeadURL$
! soil vapor properties required for calculations
      real  rgas, molewater, zerokelvin
      real  denwat, gravconst, potwilt, potfc, potfcs
      real  diffuntp, atmstand, templapse, tempstand, rair
!     rgas       - universal gas constant (joules/(mole degree K)
!     molewater  - molecular weight of water (kg/mole)
!     zerokelvin - kelvin equivalent of zero degree centigrade
!     denwat     - density of the liquid (water) (kg/m^3)
!     gravconst  - acceleration due to gravity (m/s^2)
!     potwilt    - matric potential for wilting point (15 bar) in meters of water
!     potfc      - matric potential for field capacity (1/3 bar) in meters of water
!     potfcs     - matric potential for field capacity (1/10 bar) in meters of water
!     diffuntp   - Binary diffusion coefficient for water vapor in air at
!                  normal temperature and pressure (0 C, 1 standard atmosphere) (m^2/s)
!     atmstand   - standard atmosphere (kilopascals)
!     templapse  - temperature lapse rate for troposphere (degree K/meter)
!     tempstand  - standard temperature used to find standard atmosphere (degree K)
!     rair       - gas constant for air (J/kg/K or m^2/s^2/K)

!*** Parameter Definitions ***
      parameter (rgas = 8.3143)
      parameter (molewater = 0.018)
      parameter (zerokelvin = 273.16)
      parameter (denwat = 1000.0)
      parameter (gravconst = 9.807)
      parameter (potwilt = -152.95) !in meters of water at max density
      parameter (potfc = -3.3989)   !in meters of water at max density
      parameter (potfcs = -1.01967)   !in meters of water at max density
      parameter (diffuntp = 2.12e-5)
      parameter (atmstand = 101.3)
      parameter (templapse = 0.0065)
      parameter (tempstand = 288.0)
      parameter (rair = 287.0)
