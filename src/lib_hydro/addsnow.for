!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine addsnow(dprecip, dirrig, bwzdpt, bhzirr, bhlocirr,     &
     &                   bwtdmn, bwtdmx, bwtdpt, bmzele,                &
     &                   bhzsno, bhtsno, bhfsnfrz, bhzsnd )

!     + + + PURPOSE + + +
!     This subroutine checks added water to see if it is snow and then
!     properly adjusts the snow water content, depth, frozen to liquid
!     ratio and temperature

!     + + + KEY WORDS + + +
!     add snow

!     + + + COMMON BLOCKS + + +
      include 'p1unconv.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/snowprop.inc'
      include 'hydro/heatcap.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real dprecip, dirrig, bwzdpt, bhzirr, bhlocirr
      real bwtdmn, bwtdmx, bwtdpt, bmzele
      real bhzsno, bhtsno, bhfsnfrz, bhzsnd

!     + + + ARGUMENT DEFINITIONS + + +
!     dprecip - depth of precipitation reaching soil surface through snow (mm)
!     dirrig  - depth of irrigation reaching soil surface through snow (mm)
!     bwzdpt  - depth of precipitation added (mm)
!     bhzirr  - depth of irrigation added (mm)
!     bhlocirr - location of irrigation emitter (mm)
!     bwtdmn  - Daily minimum air temperature (C)
!     bwtdmx  - Daily maximum air temperature (C)
!     bwtdpt  - dew point temperature (C)
!     bmzele  - elevation (m)
!     bhzsno  - depth of water contained in snow layer (mm)
!     bhtsno  - temperature of snow layer (C)
!     bhfsnfrz  - fraction of snow layer water content which is frozen
!     bhzsnd  - actual thickness of snow layer (mm)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      real t_air, t_wb
      real new_energy, new_mass, new_depth
      real snow_den

!     + + + LOCAL DEFINITIONS + + +
!     t_air  - daily average air temperature (C)
!     t_wb   - wet bulb temperature (C)
!     new_energy - energy content of new snow (J/m^2)
!     new_mass - mass of new snow (kg/m^2)
!     new_depth - depth associated with new snow (mm) (indirect density)
!     snow_den - calculated value of snow density (kg/m^3)

!     + + + FUNCTIONS CALLED + + +
      real wetbulb

!     + + + SUBROUTINES CALLED + + +
!     statesnow

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

      ! find daily average air temperature
      t_air = 0.5 * (bwtdmn + bwtdmx)

      if( (bhzirr .gt. 0.0) .and. (bhlocirr .gt. 0.0) ) then
          ! irrigation water applied above or within snow layer
          ! add as liquid water at air temperature (0 and above)
          ! set mass of added liquid. units as above
          new_mass = bhzirr

          ! calculate energy content of new water. units as above
          new_energy = new_mass * max(0.0, t_air) * waterheatcap
          new_depth = 0.0

          ! update state of snow cover and return liquid output
          call statesnow( dirrig, new_mass, new_energy, new_depth,      &
     &                    bhzsno, bhtsno, bhfsnfrz, bhzsnd )
      else
          ! irrigation water applied below snow layer
          ! return directly as water applied to soil
          ! no change in snow state
          dirrig = bhzirr
      end if

      if( bwzdpt .gt. 0.0 ) then
          ! temperature check
          if( t_air .le. 0.0 ) then
              ! added water is snow, adjust snow total water content,
              ! average temperature, fraction liquid ratio, and total depth

              ! find wet bulb temperature from daily average air temperature
              t_wb = wetbulb( t_air, bwtdpt, bmzele )

              ! set mass of new snow added. units as above
              new_mass = bwzdpt

              ! calculate energy content of new snow. units as above
              new_energy = new_mass * (t_air * iceheatcap - heat_fusion)

              ! set physical depth of new snow (use new snow density)
              ! units: kg/m^2 / kg/m^3 = m * mtomm = mm
              if( t_wb .gt. -15.0 ) then
                  if( t_wb .lt. 0.0 ) then
                      snow_den = min_snow_den + 1.7 * (t_wb + 15.0)**1.5
                  else
                      snow_den = 150.0
                  end if
              else
                  snow_den = min_snow_den
              end if
              new_depth = mtomm * new_mass / snow_den
          else
              ! added water is liquid
              ! set mass of added liquid. units as above
              new_mass = bwzdpt

              ! calculate energy content of new water. units as above
              new_energy = new_mass * t_air * waterheatcap
              new_depth = 0.0
          end if

          ! update state of snow cover and return liquid output
          call statesnow( dprecip, new_mass, new_energy, new_depth,     &
     &                    bhzsno, bhtsno, bhfsnfrz, bhzsnd )
      else
          ! if no precipitation then set return value (so it is set :-)
          dprecip = bwzdpt
      end if

      return
      end
