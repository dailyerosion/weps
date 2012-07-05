!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real min_snow_den
      real melt_snow_den
      real max_snow_den
      real heat_fusion

!     min_snow_den - minimum density of new snow (kg/m^3)
!     melt_snow_den - snow density above which melt water is released (kg/m^3)
!     max_snow_den - maximum possible snow density (kg/m^3)
!     heat_fusion -  latent heat of fusion for water (J/kg)

      parameter(min_snow_den = 100.0)
      parameter(melt_snow_den = 350.0)
      parameter(max_snow_den = 522.0)
      parameter(heat_fusion = 337000.0)
