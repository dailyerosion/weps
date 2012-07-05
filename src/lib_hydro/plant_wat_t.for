!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function plant_wat_t( begind, endd, thetaf, thetaw,          &
     &                           bszlyd, nlay )

!     + + + PURPOSE + + +
!     Determines the amount of water in any soil interval between any
!     two water contents.

!     + + + ARGUMENT DECLARATIONS + + +
      real begind, endd
      integer nlay
      real thetaf(nlay), thetaw(nlay), bszlyd(nlay)

!     + + + ARGUMENT DEFINITIONS + + +
!     begind - uppper depth of soil interval
!     endd   - lower depth of soil interval
!     nlay   - number of layers in soil input array
!     thetaf - wetter soil water content value by layer (mm/mm)
!     thetaw - dryer soil water content value by layer (mm/mm)
!     bszlyd - depth to bottom of soil layers (mm)

!     + + + LOCAL VARIABLES + + +
      integer lay
      real sumwat, depth, thick

!     + + + LOCAL DEFINITIONS + + +
!     lay    - layer index
!     sumwat - running sum of water as added from each layer
!     depth  - cumulative depth in soil
!     prevdepth - previous cumulative depth in soil
!     thick  - thickness of soil slice whose water content is being
!              added to sum

!     + + + FUNCTIONS CALLED + + +
      real intersect

!     + + + END SPECIFICATIONS + + +

      sumwat = 0.0
      depth = 0.0
      do lay = 1,nlay
          ! find thickness of intersection between soil layer and 
          ! desired interval
          thick = intersect( depth, bszlyd(lay), begind, endd )
          if( thick .gt. 0.0 ) then
              sumwat = sumwat + (thetaf(lay) - thetaw(lay)) * thick
          end if
          depth = bszlyd(lay)
      end do
      plant_wat_t = sumwat

      return
      end
