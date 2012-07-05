!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function movewind( meas_wind, meas_za, meas_zo, meas_zd,     &
     &                          loc_za, loc_zo, loc_zd)

!     + + + PURPOSE + + +
      ! returns wind velocity in the same units as the measured wind 
      ! adjusted from measured height, roughness, zero plane displacement
      ! to the location height, roughness and zero plane displacement.
      ! Reference: Jensen, Burman, Allen. 1989. ASCE 70, Evapotranspiration
      ! and irrigation water requirements. Adjustment for differential roughness
      ! included as a power function (Hagen reference: Panofsky and Dutton, 1984)

!     + + + KEY WORDS + + +
!     log law wind velocity adjustment

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      real meas_wind, meas_za, meas_zo, meas_zd
      real loc_za, loc_zo, loc_zd

!     + + + ARGUMENT DEFINITIONS + + +
!     meas_wind - measured wind velocity (units same as output units)
      ! these parameters should all have the same units
!     meas_za - measured wind anemometer height
!     meas_zo - measured wind aerodynamic roughness
!     meas_zd - measured wind zero plane displacement
!     loc_za - location wind velocity height
!     loc_zo - location wind aerodynamic roughness
!     loc_zd - location wind zero plane displacement

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +

!     + + + LOCAL DEFINITIONS + + +

!     + + +   FUNCTION CALLS +++

!     + + + END SPECIFICATIONS + + +

      movewind = meas_wind * ( log( (loc_za - loc_zd) / loc_zo )        &
     &         / log( (meas_za - meas_zd) / meas_zo ) )                 &
     &         * ( (loc_zo / meas_zo)**0.067 )

      return
      end
