!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function evapredu( bhzeasurf, evaplimit, vaptrans, bhzep )

!     + + + PURPOSE + + +
!     This function returns the reduction in evaporation rate due to
!     soil drying (ratio of evap actual / evap potential)

!     + + + KEY WORDS + + +
!     soil evaporation dryness limit

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      real bhzeasurf, evaplimit, vaptrans, bhzep

!     + + + ARGUMENT DEFINITIONS + + +
!     bhzeasurf - accumulated surface evaporation since last complete rewetting (mm)
!     evaplimit - accumulated surface evaporation since last complete rewetting
!                 defining limit of stage 1 (energy limited) and start of 
!                 stage 2 (soil vapor transmissivity limited) evaporation (mm)
!     vaptrans  - vapor transmissivity (mm/d^.5)
!     bhzep     - daily potential evaporation (mm)

!     can be used with other depth units if they are consistent

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      real evapday

!     + + + LOCAL DEFINITIONS + + +
!     evapday  - evaporation time since the initiation of stage 2 evaporation

!     + + + END SPECIFICATIONS + + +

      ! reduce daily potential surface evaporation rate based on
      ! accumulated evaporation since last complete surface wetting
      if( (bhzeasurf .gt. evaplimit) .and. (bhzep .gt. 0.0) ) then
          evapday = ((bhzeasurf - evaplimit) / vaptrans) ** 2.0
          evapredu = min( 1.0, vaptrans                                 &
     &              * ((evapday+1)**0.5 - evapday**0.5) / bhzep )
      else
          evapredu = 1.0
      end if

      return
      end
