!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     file 'p1erode.inc'

!     These parameter variables are used to specify the scope 
!     of specific variable values within the erosion submodel

      real  SLRR_MIN
      real  SLRR_MAX
      real  WZZO_MIN
      real  WZZO_MAX

!     This variable determines the whether surface updating from erosion
!     is enabled or not

      integer SURF_UPD_FLG

!     + + + VARIABLE DECLARATIONS + + +

      parameter (SLRR_MIN = 1.5)  !minimum RR value (mm)
      parameter (SLRR_MAX = 100.0)!maximum RR value (mm)
      parameter (WZZO_MIN = 0.5)  !minimum wwzo value (mm)
      parameter (WZZO_MAX = 30.0) !maximum wwzo value (mm)


      common /p1erode/ SURF_UPD_FLG

      save :: /p1erode/

!     + + + VARIABLE DEFINITIONS + + +

!     SLRR_MIN - Minimum RR value allowed for grid cell
!                random roughness (mm)
!     SLRR_MAX - Maximum RR value allowed for grid cell
!                random roughness (mm)
!     WZZO_MIN - Minimum wwzo value allowed for grid cell
!                bare, non-ridged, surface aerodynamic roughness (mm)
!                (currently this corresponds to ~1.67mm RR)
!     WZZO_MAX - Maximum wwzo value allowed for grid cell
!                bare, non-ridged, surface aerodynamic roughness (mm)
!                (currently this corresponds to ~100mm RR)

!     SURF_UPD_FLG - value = 0, surface updating is disabled
!                    value = 1, surface updating is enabled
