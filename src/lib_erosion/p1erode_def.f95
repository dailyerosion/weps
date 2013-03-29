!$Author$
!$Date$
!$Revision$
!$HeadURL$

module p1erode_def

!     These parameter variables are used to specify the scope 
!     of specific variable values within the erosion submodel

      real, parameter :: SLRR_MIN = 1.5     !minimum RR value (mm)
      real, parameter :: SLRR_MAX = 100.0   !maximum RR value (mm)
      real, parameter :: WZZO_MIN = 0.5     !minimum wwzo value (mm)
      real, parameter :: WZZO_MAX = 30.0    !maximum wwzo value (mm)


!     SLRR_MIN - Minimum RR value allowed for grid cell random roughness (mm)
!     SLRR_MAX - Maximum RR value allowed for grid cell random roughness (mm)
!     WZZO_MIN - Minimum wwzo value allowed for grid cell
!                bare, non-ridged, surface aerodynamic roughness (mm)
!                (currently this corresponds to ~1.67mm RR)
!     WZZO_MAX - Maximum wwzo value allowed for grid cell
!                bare, non-ridged, surface aerodynamic roughness (mm)
!                (currently this corresponds to ~100mm RR)

end module p1erode_def
