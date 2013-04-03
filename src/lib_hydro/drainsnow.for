!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine drainsnow(dh2o, bhzsno, bhfsnfrz, bhzsnd )

!     + + + PURPOSE + + +
!     This subroutine checks for drainage snow density and releases the 
!     excess water to bring the snow density down to the drainage density
!     and adjusts the snow water content and frozen to liquid ratio

!     + + + KEY WORDS + + +
!     drain snow

      use p1unconv_mod, only: mtomm, mmtom

!     + + + LOCAL COMMON BLOCKS + + +
      include 'hydro/snowprop.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real dh2o, bhzsno, bhfsnfrz, bhzsnd

!     + + + ARGUMENT DEFINITIONS + + +
!     dh2o    - depth of water drained from snow (mm)
!     bhzsno  - depth of water contained in snow layer (mm)
!     bhfsnfrz  - fraction of snow layer water content which is frozen
!     bhzsnd  - actual thickness of snow layer (mm)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      real snow_den, new_fsnfrz

!     + + + LOCAL DEFINITIONS + + +
!     snow_den - calculated value of snow density (kg/m^3)
!     new_fsnfrz - new fraction of snow layer water content which is frozen

!     + + + END SPECIFICATIONS + + +

      ! check for density and drain water if above the drainage density
      ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
      ! units: kg/m^2 / (mm * mmtom) = kg/m^3
      if( bhzsnd .gt. 0.0 ) then
          snow_den = bhzsno / (bhzsnd * mmtom)

          ! check against maximum densities
          if( snow_den .gt. melt_snow_den ) then
              ! melt water will be discharged
              ! find frozen water content fraction corresponding to melt snow density
              ! cannot remove more than available liquid
              new_fsnfrz = min(1.0, bhfsnfrz * snow_den / melt_snow_den)
              ! water released from snow layer
              ! remember kg/m^2 = mm of water using standard density
              dh2o = bhzsno * ( (1.0-bhfsnfrz) - (bhfsnfrz/new_fsnfrz)  &
     &             *(1.0-new_fsnfrz) )
              bhzsno = bhzsno - dh2o
              bhfsnfrz = new_fsnfrz
          else
              dh2o = 0.0
          end if

          ! adjust depth to not exceed maximum snow density
          bhzsnd = max(bhzsnd, mtomm * bhzsno / max_snow_den )
      else
          ! snow depth has collapsed (by melting)
          dh2o = bhzsno
          bhzsno = 0.0
          bhfsnfrz = 0.0
      end if

      return
      end
