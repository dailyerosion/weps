!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function albedo (bcrlai, snwc, sndp, bsfalw, bsfald)

!     + + + purpose + + +
!     this subroutine estimates the net radiation for a given area
!     using known solar radiation, air temperature, and vapor pressure
!     according to wright's (1982) modified version of penman's (1948)
!     relationship.

!     + + + key words + + +
!     radiation, hydrology, weps
      use p1unconv_mod, only: mtomm

!     + + + argument declarations + + +
      real bcrlai
      real snwc
      real sndp
      real bsfalw
      real bsfald

!     + + + argument definitions + + +
!     bsfald   - dry albedo
!     bsfalw   - wet albedo
!     bcrlai   - plant leaf area index
!     snwc     - water content of snow, mm
!     sndp     - depth of snow, mm

!     + + + COMMON BLOCK + + +
      include 'p1const.inc'
      include 'p1werm.inc'
      include 'hydro/htheta.inc'
      include 'hydro/snowprop.inc'

!     + + + local variables + + +
      real snow_den
      real albs
      real albsn
      real sci
      real snci
      real pci

!     + + + local definitions + + +
!     snow_den - density of the snow
!     albs   - soil albedo
!     albsn  - snow albedo
!     sci    - soil albedo fraction
!     snci   - snow albedo fraction
!     pci    - plant albedo fraction

!     + + + parameters + + +
      real albp
      real alb_snow_max
      real alb_snow_min
      parameter   (albp = 0.23)            ! albedo of plants
      parameter   (alb_snow_max = 0.6)     ! albedo of new snow
      parameter   (alb_snow_min = 0.2)     ! albedo of fully dense snow

!     + + + data initialization + + +

!     + + + end specifications + + +

      ! estimate snow albedo

      ! using mass of existing snow (snwc)
      ! units: mm * (1m/1000mm) * 1000 kg/m^3 = kg/m^2
      ! and physical depth of snow (sndp)
      ! units: 1000 mm/m * kg/m^2 / mm = kg/m^3
      if( sndp .gt. 0.0 ) then
          snow_den = mtomm * snwc / sndp
          albsn = alb_snow_min + (alb_snow_max - alb_snow_min)          &
     &          *(max_snow_den - snow_den)/(max_snow_den - min_snow_den)
      else
          albsn = alb_snow_max
      end if

!     estimate the surface albedo
      if ( snwc .ge. 5.0 ) then
        albedo = albsn                          !snow covers surface & plants
      else
        snci = snwc / 5.0                       !coverage factor for snow
        pci = min(bcrlai/3, 1.0)                !coverage factor for plants based upon leaf area index
        if (pci + snci .gt. 1.0) pci = 1.0 - snci  !make sure factors sum to 1
        sci = 1.0 - (pci + snci)                !soil albedo factor is what is left over
        if (sci.gt.0.0) then                    !need to calc soil albedo
          albs = bsfald + (bsfalw - bsfald) *                           &
     &      (theta(0) - thetaw(1)) / (thetaf(1) - thetaw(1))
          albs = max(albs, bsfalw)   !no less than wet  (wet is less than dry)
          albs = min(albs, bsfald)   !no greater than dry
          albedo = snci*albsn + pci * albp + sci * albs
        else
          albedo = snci*albsn + pci * albp
        endif
      endif

      return
      end
