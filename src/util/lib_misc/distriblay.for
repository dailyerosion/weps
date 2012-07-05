!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine distriblay( nlay, bszlyd, bszlyt, layval,              &
     &                       insertval, begind, endd )

!     + + + PURPOSE + + +
!     Distributes a quantity of material over an underground interval
!     adding material in each layer in proportion to the fraction of
!     the interval that is each soil layer.

!     NOTE: insertval and layval need the same units

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay
      real bszlyd(nlay), bszlyt(nlay), layval(nlay)
      real insertval, begind, endd

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay   - number of layers in soil input array
!     bszlyd - depth to bottom of soil layers (mm)
!     bszlyt - thickness of soil layers (mm)
!     layval - the layer bins that will be supplemented by added material
!     insertval - the quantity of material to be distributed into layval
!     begind - uppper depth of soil interval (mm)
!     endd   - lower depth of soil interval (mm)

!     + + + LOCAL VARIABLES + + +
      integer lay
      real depth, prevdepth, thick, interval_thick

!     + + + LOCAL DEFINITIONS + + +
!     lay    - layer index
!     depth  - depth to bottom of present soil layer (mm)
!     prevdepth - previous cumulative depth in soil (mm)
!     thick  - thickness of soil slice (intersection of soil interval
!              and soil layer) (mm)
!     interval_thick - thickness of soil interval over which quantity
!                      of material is being inserted. (mm)

!     + + + FUNCTIONS CALLED + + +
      real intersect

!     + + + END SPECIFICATIONS + + +

      ! interval thickness is used repeatedly, calculate here
      interval_thick = endd - begind
      ! start at soil surface
      depth = 0.0
      do lay = 1,nlay
          ! set depth of layer upper boundary
          prevdepth = depth
          ! set depth of layer lower boundary
          depth = bszlyd(lay)
          if( interval_thick .gt. 0.0 ) then
          ! find thickness of intersection between layer and interval
              thick = intersect( prevdepth, depth, begind, endd )
              if( thick .gt. 0.0 ) then
                  ! put proportional amount in this layer
                  layval(lay) = layval(lay)                             &
     &                        + insertval * thick / interval_thick
              end if
          else
              ! zero interval thickness
              if( (endd .le. depth) .and. (endd .gt. prevdepth) ) then
                  ! interval in this layer, put all in this layer
                  layval(lay) = layval(lay) + insertval
              end if
          end if
      end do

      return
      end
