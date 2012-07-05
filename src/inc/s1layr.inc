!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 's1layr.inc'

!     This common block contains the state variables relating to
!     the WERM soil layer properties.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer          nslay(mnsub)
      real             aszlyt(mnsz+1, mnsub)
      real             aszlyd(mnsz, mnsub)
      real             asfcle(mnsz, mnsub)
      real             watertable_depth(mnsub)   ! Not used right now

      common / s1layr /                                                 &
     &                  nslay,                                          &
     &                  aszlyt,                                         &
     &                  aszlyd,                                         &
     &                  asfcle,                                         &
     &                  watertable_depth

      save :: /s1layr/

!     + + + VARIABLE DEFINITIONS + + +

!     nslay  - Number of soil layers being used for each subregion.

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          mnsz
!              typical        1          mnsz
!              ------------   ---------  ---------

!            - This variable maintains the number of active soil layers
!              for each subregion.

!     aszlyt - Soil layer thicknesses for each subregion (mm)
!     aszlyd - Depth to bottom of each soil layer for each subregion (mm)

!      asfcle - Linear extensibility ((Mg/m^3)/(Mg/m^3))

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          0.0        1.0
!              typical        0.0        1.0
!              ------------   ---------  ---------

!      watertable_depth - depth to watertable (mm)
!     ------------------------------------------------------------------
