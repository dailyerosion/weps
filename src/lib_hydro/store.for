!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function store (minlay, maxlay, prevvolw, volw, laydepth)

!     + + + PURPOSE + + +
!     determines the infiltration depth of water from the soil surface (mm)
!     by checking for an increase in soil water content.
!     The depth is set to the layer where the soil water content has
!     not increased. The value is always set to include the first layer
!     since it will not be called unless water has been added.
!     

!     + + + ARGUMENT DECLARATIONS + + +
      integer minlay, maxlay
      real prevvolw(*), volw(*), laydepth(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     layrsn           - Number of soil layers used in the simulation
!     prevvolw(layrsn+6) - beginning of day volume of water in the soil profile (m)
!     volw(layrsn+6)     - after event volume of water in the soil profile (m)
!     laydepth(layrsn) - depth to bottom of soil layer (mm)

!     + + + LOCAL VARIABLES + + +
      integer lrx

!     + + + LOCAL DEFINITIONS + + +
!     lrx    - Loop counter

!     + + + END SPECIFICATIONS + + +

!     distribute the daily amount of water available for infiltration
!     into the soil profile throughout the simulation layers.

      store = laydepth(1)
      do lrx = minlay+1,maxlay
          if( volw(lrx).gt.prevvolw(lrx) ) then
              store = laydepth(lrx-minlay+1)
           else
              exit
           endif
      end do

      return
      end
