!$Author$
!$Date$
!$Revision$
!$HeadURL$
      real function valbydepth(layrsn, bszlyd, lay_val, ai_flag,        &
     &                         depthtop, depthbot)

!     + + + PURPOSE + + +
!     Find the average of any soil property based on layer thickness
!     containing the soil property

!     + + + KEY WORDS + + +
!     averaging, interpolation

!     + + + COMMON BLOCKS + + +
      include 'p1unconv.inc'

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      integer layrsn
      real bszlyd(layrsn), lay_val(layrsn)
      integer ai_flag
      real depthtop, depthbot

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn  - Number of soil layers used in the simulation
!     bszlyd  - distance from surface to bottom of layer (mm)
!     lay_val - layer based array of values to be averaged or interpolated
!     ai_flag - flag indicating averaging scheme used
!           0 - entire layer assumed to have same value
!           1 - value assumed valid at center of layer, with continuous
!               transition to next layer center
!     depthtop - depth in the soil of the top of the segment to be averaged
!     depthbot - depth in the soil of the bottom of the segment to be
!                averaged or interpolated

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

      ! check for bottom depth past soil bottom
      if( depthbot .gt. bszlyd(layrsn) ) then
          indexbot = layrsn
      end if

      if( ai_flag .eq. 0 ) then
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
      else if( ai_flag .eq. 1 ) then
          valbydepth = 0.0
          write(*,*) "valbydepth: ai_flag method 1 not yet implemented"
          stop
      end if

      return
      end