!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine setlsnow(snow_wat, snow_froz_old, snow_froz_new,       &
     &                    snow_depth, snow_temp, bwtdmx )

!     + + + PURPOSE + + +
!     This subroutine increases the snow density based on the temperature,
!     snow depth, and liquid water content

!     + + + KEY WORDS + + +
!     settling snow

      use p1unconv_mod, only: mmtom

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/snowprop.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real snow_wat, snow_froz_old, snow_froz_new
      real snow_depth, snow_temp, bwtdmx

!     + + + ARGUMENT DEFINITIONS + + +
!     snow_wat - depth of water contained in snow layer (mm)
!     snow_froz_old - old fraction of snow layer water content which is frozen
!     snow_froz_new - new fraction of snow layer water content which is frozen
!     snow_depth  - actual thickness of snow layer (mm)
!     snow_temp - temperature of snow layer (C)
!     bwtdmx - maximum daily air temperature (C)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      real snow_den, term2

!     + + + LOCAL DEFINITIONS + + +
!     snow_den - calculated value of snow density (kg/m^3)
!     term2 - intermediate term value

!     + + + END SPECIFICATIONS + + +

      ! reduce depth based on change in frozen fraction
      ! assuming that frozen portion remains constant density
      ! and melted portion fills voids, never increase depth
      snow_depth = snow_depth * min(1.0, snow_froz_new / snow_froz_old)

      if( snow_depth .gt. 0.0 ) then
          ! snow has depth, find density
          ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
          ! units: kg/m^2 / (mm * mmtom) = kg/m^3
          snow_den = snow_wat / (snow_depth * mmtom)

          ! add an increase due to compaction. With single layer
          ! estimate use part of snow water content as overburden
          snow_den = snow_den * (1.0 + 0.08 * snow_wat                  &
     &               * exp(0.08 * snow_temp - 21.0 * snow_den/1000.0))

          ! add an increase due to settling
          ! snow density factor
          if( snow_den .gt. 150.0 ) then
              term2 = exp(-46.0*(snow_den - 150.0))
          else
              term2 = 1.0
          end if
          snow_den = snow_den * (1.0 + 0.24 * exp(0.04*snow_temp)*term2)

          ! increase density to compensate for using average temperature
          snow_den = snow_den * ( 1.0 + max( 0.0, bwtdmx/25.0 ) )

          ! compute new depth based on density
          snow_depth = snow_wat / (snow_den * mmtom)
      end if

      return
      end
