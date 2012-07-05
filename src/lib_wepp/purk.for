!$Author$
!$Date$
!$Revision$
!$HeadURL$

!***********************************************************************
! The master percolation component which routes the
!     percolated water down through the soil layers, and
!     and updates the water content of each soil layer.
!     For each layer above field capacity, the excess (VV)
!     is subjected to percolation.  The amount percolated
!     is the seepage (SEP).  The water content (ST) of
!     both layers is updated by the amount of seepage (SEP).
!     Immediately before PURK returns, any infiltration
!     from today's precip (FIN) is added to the top layer.
!***********************************************************************

      subroutine purk(nsl, st, fc, ul, hk, ssc, sep)

      implicit none

!***********************************************************************
!
!     nsl - number of soil layers
!     st - current available water content per soil layer (m)
!     fc - soil field capacity (m)
!     sep -  seepage (m)
!     ul - upper limit of water content per soil layer (m)
!     hk - a parameter that causes SC approach zero as soil water approaches FC
!     ssc - saturated hydraulic conductivity (m/s)
!
!***********************************************************************

!      use perc_sub
      integer, intent(in) :: nsl
      real, intent(in)  :: fc(*),ul(*), hk(*), ssc(*)

      real, intent(inout) :: st(*), sep

!     + + + LOCAL VARIABLES + + +
      real vv, sepsav
      integer k1, k2
!
!     + + + LOCAL DEFINITIONS + + +
!     vv     - Water excess beyond field capacity for the current
!              soil layer.
!     k1     - counter in main loop.
!     k2     - k1 + 1
!     sepsav - used to save SEP from bottom layer.  (Seepage from
!              the bottom layer is needed for water balance calcs.)

!      For each layer, starting with the BOTTOM layer,
!      percolate the water excess to the layer below.
!      (This approach is taken to avoid the compounding
!      effect caused by dumping percolated water from
!      the layer above, on top of existing water that
!      has not yet been subjected to percolation.)

      sepsav = 0.0
      do k1 = nsl, 1, -1
!       ------- compute water excess
        vv = st(k1) - fc(k1)
!       ------- when there is an excess....
        if (vv.gt.0.) then
          k2 = k1 + 1
!         --------- compute percolation through the layer.
          call perc(vv, k1, nsl, st, ul, hk(k1), ssc(k1), sep)
!         --------- reduce water content of current layer
          st(k1) = st(k1) - sep
          if (st(k1).lt.1e-10) st(k1) = 0.0

          if (k1.lt.nsl) then
!           ----------- add seepage to layer below
            st(k2) = st(k2) + sep
          else
!           ----------- "remember" seepage from bottom layer
            sepsav = sep
          end if
        end if

      end do
      sep = sepsav

      return
      end subroutine
