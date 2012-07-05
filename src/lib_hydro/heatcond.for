!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function heatcond(bsdblk, theta, thetas, bhtsav, bhfice,     &
     &                       bsfsan, bsfsil, bsfcla, bsfom)

!     + + + PURPOSE + + +
!     This function returns the volumetric heat capacity of the soil
!     given mass fractions of the soil constituents. (J/kg C)

!     + + + KEYWORDS + + +
!     soil heat capacity

!     + + + ARGUMENT DECLARATIONS + + +
      real bsdblk, theta, thetas, bhtsav, bhfice
      real bsfsan, bsfsil, bsfcla, bsfom

!     + + + ARGUMENT DEFINITIONS + + +
!     bsdblk  - Soil bulk density (Mg/m^3)
!     theta   - soil layer water content (m^3/m^3 bulk soil)
!     thetas  - soil water content at saturation (m^3/m^3 bulk soil)
!     bhtsav  - soil layer average daily temperature (C)
!     bhfice  - mass fraction of soil water which is ice (kg ice/kg water)
!     bsfsan  - Sand mass fractions (kg clay/kg soil mineral)
!     bsfsil  - Silt mass fractions (kg clay/kg soil mineral)
!     bsfcla  - Clay mass fractions (kg clay/kg soil mineral)
!     bsfom   - Organic matter fraction (kg organic matter/kg soil)

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/heatcond.inc'
      include 'hydro/partden.inc'

!     + + + LOCAL VARIABLES + + +
      real fac_a, fac_b, fac_c, fac_d, fac_e
      real kersten, deg_sat, cond_dry, cond_sat
      real volf_quartz, volf_mineral, volf_organic, volf_solid
      real cond_soil, cond_water, volf_liq_water

!     + + + LOCAL VARIABLE DEFINITION + + +
!     fac_a - sub calculation
!     fac_b - sub calculation
!     fac_c - sub calculation
!     fac_d - sub calculation
!     fac_e - sub calculation
!     kersten - kersten number to proportion thermal conductivity between dry and saturated values
!     deg_sat - degree of soil saturation with water
!     cond_dry - dry soil thermal conductivity (J/s m C) or (W/m C)
!     cond_sat - saturated soil thermal conductivity (J/s m C) or (W/m C)
!     volf_quartz - volumetric fraction of soil which is quartz (m^3/m^3 bulk soil)
!     volf_mineral - volumetric fraction of soil which is remaining mineral (m^3/m^3 bulk soil)
!     volf_organic - volumetric fraction of soil which is organic (m^3/m^3 bulk soil)
!     volf_solid - volumetric fraction of soil which is solid (excluding organic) (m^3/m^3 bulk soil)
!     cond_soil - soil solids thermal conductivity (J/s m C) or (W/m C)
!     cond_water - liquid water thermal conductivity (J/s m C) or (W/m C)
!     volf_liq_water - volumetric fraction of liquid water (m^3/m^3 bulk soil)

!     + + + END SPECIFICATIONS + + +

      ! Thermal Conductivity volumetrically weighted based on
      ! method by Campbell (1985) as defined in:
      ! Bristow, K.I. 2002. Thermal Conductivity. in Dane, J.H. and
      ! Topp, G.C. eds. Methods of Soil Analysis, Part 4, Physical Methods. 
      ! Soil Science Society of America, Inc. Madison, Wisconsin, USA

      ! BIG NOTE: this approximation does not account for organic matter content or ice.
      ! A small attempt was made to slant this method by only using the volume
      ! fractions of mineral elements, implying that organic matter conducts like air.
      ! The full treatment of temperature, organic matter and ice effects needs to use
      ! the other method in the same reference drawn from DeVries (1963)

      ! NOTE: (1-bsfom) gives (kg mineral soil/kg soil)
      ! air is not included

!      volf_quartz = bsfsan * (1.0-bsfom) * bsdblk / den_quartz
!      volf_mineral = (bsfsil+bsfcla) * (1.0-bsfom) * bsdblk/den_quartz
!      volf_solid = volf_quartz + volf_mineral
!      volf_organic = bsfom * bsdblk / den_organic

!      fac_a = (0.57 + 1.73*volf_quartz + 0.93*volf_mineral)             &
!     &      / (1.0 - 0.47*volf_quartz - 0.49*volf_mineral)              &
!     &      - 2.8*volf_solid*(1.0-volf_solid)

!      fac_b = 2.8 * volf_solid
!      fac_c = 1.0 + ( 2.6 / bscla**0.5 )
!      fac_d = 0.03 + 0.7 * volf_solid * volf_solid
!      fac_e = 4.0

!      heatcond = fac_a + fac_b*theta                                    &
!     &         - (fac_a - fac_d) * exp(-(fac_c*theta)**fac_e)

!========================================
! alternate method accounting for soil freezing

!     volf_quartz - volumetric fraction of soil solids which is quartz (m^3/m^3 soil solids)
!     volf_mineral - volumetric fraction of soil which is remaining mineral (m^3/m^3 soil solids)
!     volf_organic - volumetric fraction of soil which is organic (m^3/m^3 soil solids)
!     volf_solid - volumetric fraction of soil which is solid (excluding organic) (m^3/m^3 bulk soil)
!     volf_water - volumetric fraction of saturated soil which is unfrozen water(m^3/m^3 bulk soil)

      ! Thermal Conductivity volumetrically weighted based on
      ! method by Johansen (1975) in:
      ! Peters-Lidard, C.D., E. Blackburn, X. Liang, and E.F. Wood. 1998.
      ! The effect of soil thermal conductivity parameterization on surface
      ! energy fluxes and temperatures. Journal of the Atmospheric Sciences
      ! vol. 55 pgs. 1209-1224

      ! dry soil thermal conductivity
      cond_dry = (135.0*bsdblk + 64.7) / (2700.0 - 947.0*bsdblk)

      ! liquid water thermal conductivity from Bristow in Dane and Topp (2002)
      if( bhtsav .gt. 0.0 ) then
          cond_water = 0.552 + 0.00234*bhtsav - 1.1e-5*bhtsav*bhtsav
      else
          cond_water = 0.552
      end if

      ! total soil volume fraction of unfrozen water
      volf_liq_water = thetas * den_ice * (1.0 - bhfice)                &
     &               / (den_ice * (1.0 - bhfice) + bhfice)

      ! volume fraction of quartz
      volf_quartz = bsfsan * (1.0-bsfom)                                &
     &            / ( 1.0+ bsfom*(den_quartz/den_organic - 1.0) )

      ! volume fraction of organic matter
      volf_organic = bsfom * (1.0-bsfsan)                               &
     &             / ( (1.0-bsfom)*den_organic/den_quartz + bsfom )

      ! soil solid portion thermal conductivity
      cond_soil = quartzheatcond**volf_quartz                           &
     &          * mineralheatcond**(1.0-volf_quartz-volf_organic)       &
     &          * organicheatcond**volf_organic

      ! saturated soil thermal conductivity
      cond_sat = cond_soil**(1-thetas)                                  &
     &         * iceheatcond**(thetas-volf_liq_water)                   &
     &         * cond_water**volf_liq_water

      ! degree of soil saturation
      deg_sat = max( 0.0, min(1.0, theta / thetas) )

      ! kersten number for unfrozen soil
      if( deg_sat .gt. 10**(-1.0/(1.0 - 0.3*bsfsan)) ) then
          kersten = (1.0 - 0.3*bsfsan) * log10(deg_sat) + 1.0
      else
          kersten = 0.0
      end if
      ! modify based on degree of soil layer that is frozen
      kersten = kersten * (1.0-bhfice) + deg_sat*bhfice

      ! thermal conductivity is between dry and saturated
      heatcond = kersten * (cond_sat - cond_dry) + cond_dry

      return
      end

