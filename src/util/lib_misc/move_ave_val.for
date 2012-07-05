!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine move_ave_val( nlay_old, bszlyd, valuearr,              &
     &                         nlay_new, laydepth_new )

!     + + + PURPOSE + + +
!     averages new layer values across old layers and moves new values
!     into the same array

!     + + + ARGUMENT DECLARATIONS + + +
      integer nlay_old, nlay_new
      real bszlyd(*), valuearr(*), laydepth_new(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nlay_old   - number of layers in old layering
!     bszlyd     - depth to bottom of old soil layers
!     valuearr   - soil property variable array
!     nlay_new   - number of layers in new layering
!     laydepth_new - depth to bottom of new soil layers

!     + + + LOCAL VARIABLES + + +
      integer lay
      real temparr(nlay_old), depth

!     + + + LOCAL DEFINITIONS + + +
!     lay     - layer index
!     temparr - temporary array to hold values from old layering
!     depth   - depth in soil of top of layer

!     + + + FUNCTIONS CALLED + + +
      real valbydepth

!     + + + END SPECIFICATIONS + + +

      ! save old array values to be used in averaging
      do lay = 1, nlay_old
          temparr(lay) = valuearr(lay)
      end do

      ! start from soil surface
      depth = 0.0
      do lay = 1, nlay_new
          valuearr(lay) = valbydepth(nlay_old, bszlyd, temparr, 0,      &
     &                               depth, laydepth_new(lay) )
          depth = laydepth_new(lay)
      end do

      return
      end