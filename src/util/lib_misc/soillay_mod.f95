!$Author$
!$Date$
!$Revision$
!$HeadURL$
module soillay_mod

  contains

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
    end subroutine distriblay

    real function intersect( begind_a, endd_a, begind_b, endd_b )

!     + + + PURPOSE + + +
!     returns the intersection interval "distance" of two intervals
!     each defined by a greater and lesser value
!     Obviously the units must be consistent.

!     + + + ARGUMENT DECLARATIONS + + +
      real begind_a, endd_a, begind_b, endd_b

!     + + + ARGUMENT DEFINITIONS + + +
!     begind_a - lesser value of interval a
!     endd_a   - greater value of interval a
!     begind_b - lesser value of interval b
!     endd_b   - greater value of interval b

!     + + + END SPECIFICATIONS + + +

      if( (endd_a .gt. begind_b) .and. (begind_a .lt. endd_b) ) then
          ! some part of interval b intersects interval a
          if( (begind_a .le. begind_b) .and. (endd_a .ge. endd_b) ) then
              ! interval a completely surrounds interval b
              intersect = endd_b - begind_b
          else if( begind_a .le. begind_b ) then
              ! top part of interval b intersects farther end of interval a
              intersect = endd_a - begind_b
          else if( endd_a .ge. endd_b ) then
              ! farther end of interval b intersects nearer end of interval a
              intersect = endd_b - begind_a
          else
              ! interval b completely surrounds interval a
              intersect = endd_a - begind_a
          end if
      else
          intersect = 0.0
      end if

      return
    end function intersect

    real function valbydepth(layrsn, bszlyd, lay_val, ai_flag,        &
     &                         depthtop, depthbot)

!     + + + PURPOSE + + +
!     Find the average of any soil property based on layer thickness
!     containing the soil property

!     + + + KEY WORDS + + +
!     averaging, interpolation

      use p1unconv_mod, only: mmtom

!     + + + ARGUMENT DECLARATIONS + + +
      integer layrsn
      real bszlyd(layrsn), lay_val(layrsn)
      integer ai_flag
      real depthtop, depthbot

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn  - Number of soil layers used in the simulation
!     bszlyd  - distance from surface to bottom of layer (L)
!     lay_val - layer based array of values to be averaged or interpolated
!     ai_flag - flag indicating averaging scheme used
!           0 - entire layer assumed to have same value
!           1 - value assumed valid at center of layer, with continuous
!               transition to next layer center
!           2 - value is amount in layer (not property) and result is a sum to depth specified
!     depthtop - depth in the soil of the top of the segment to be averaged (L)
!     depthbot - depth in the soil of the bottom of the segment to be
!                averaged or interpolated (L)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      integer lay, indextop, indexbot
      real dmlayr(layrsn)

!     + + + LOCAL DEFINITIONS + + +
!     lay    - soil layer index

!     + + + FUNCTIONS CALLED + + +

!     + + + SUBROUTINES CALLED + + +

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

      indextop = 0
      indexbot = 0

      dmlayr(1) = 0.5 * bszlyd(1) * mmtom

      lay = 1
      if( depthtop .le. bszlyd(lay) ) then
          indextop = lay
      end if
      if( depthbot .le. bszlyd(lay) ) then
          indexbot = lay
      end if

      do lay=2, layrsn
          ! find center of the layers
          dmlayr(lay) = 0.5 * mmtom * (bszlyd(lay-1) + bszlyd(lay))

          ! check if top depth contained in layer
          if( (depthtop .le. bszlyd(lay)) .and.                         &
     &        (depthtop .gt. bszlyd(lay-1)) ) then
              indextop = lay
          end if
          ! check if bottom depth contained in layer
          if( (depthbot .le. bszlyd(lay)) .and.                         &
     &        (depthbot .gt. bszlyd(lay-1)) ) then
              indexbot = lay
          end if
      end do

      ! check for top depth past soil bottom
      if( depthtop .gt. bszlyd(layrsn) ) then
          indextop = layrsn
      end if
      ! check for bottom depth past soil bottom
      if( depthbot .gt. bszlyd(layrsn) ) then
          indexbot = layrsn
      end if

      if( ai_flag .eq. 0 ) then
          ! average weighted by layer thickness
          if( indextop .eq. indexbot ) then
            ! entirely in the same layer, use layer value
            valbydepth = lay_val(indextop)
          else
            ! crosses one or more layer boundaries
            ! this is a layer thickness weighted average
            ! first section, layer containing depthtop
            valbydepth = lay_val(indextop) * (bszlyd(indextop)-depthtop)
            do lay=indextop+1, indexbot-1
              ! add in layers contained in interval
              valbydepth = valbydepth                                   &
     &                   + lay_val(lay) * (bszlyd(lay)-bszlyd(lay-1))
            end do
            ! last section, layer containing depthbot
            valbydepth = valbydepth                                     &
     &               + lay_val(indexbot) * (depthbot-bszlyd(indexbot-1))
            ! divide by total depth to get average
            valbydepth = valbydepth / (depthbot - depthtop)
          end if
      else if( ai_flag .eq. 2 ) then
          ! sum of all values in full layers and proportional from partial layers
          if( indextop .eq. indexbot ) then
            ! entirely in the same layer, use layer value
            if( indextop .gt. 1 ) then
              valbydepth = lay_val(indextop) * (depthbot - depthtop)    &
     &                   / (bszlyd(indextop) - bszlyd(indextop-1))
            else
              valbydepth = lay_val(indextop) * (depthbot - depthtop)    &
     &                   / bszlyd(indextop)
            end if
          else
            ! crosses one or more layer boundaries
            ! this is a layer thickness proportion
            ! first section, layer containing depthtop
            if( indextop .gt. 1 ) then
              valbydepth = lay_val(indextop)*(bszlyd(indextop)-depthtop)&
     &                   / (bszlyd(indextop) - bszlyd(indextop-1))
            else
              valbydepth = lay_val(indextop)*(bszlyd(indextop)-depthtop)&
     &                   / bszlyd(indextop)
            end if
            do lay=indextop+1, indexbot-1
              ! add in layers contained in interval
              valbydepth = valbydepth + lay_val(lay)
            end do
            ! last section, layer containing depthbot
            valbydepth = valbydepth + lay_val(indexbot)                 &
     &                 * (depthbot - bszlyd(indexbot-1))                &
     &                 / (bszlyd(indexbot) - bszlyd(indexbot-1))
          end if
      else ! if( ai_flag .eq. 1 ) then
          valbydepth = 0.0
          write(*,*) "valbydepth: ai_flag method not yet implemented"
          write(*,*) "ai_flag = ", ai_flag
          stop
      end if

      return
    end function valbydepth

end module soillay_mod
