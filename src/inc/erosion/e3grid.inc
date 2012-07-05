!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 'e3grid.inc'

!     This block common is contains variables used for a simulation run
!     for  EROSION.

!     + + + VARIABLE DECLARATIONS + + +

      integer          kbr
      integer          i1, i2, i3, i4, i5, i6
      real             awa, sin_awa, cos_awa, tan_awa

      common /e3grid/                                                   &
     &                 kbr,                                             &
     &                 i1, i2, i3, i4, i5, i6,                          &
     &                 awa, sin_awa, cos_awa, tan_awa

      save :: /e3grid/

!     + + + VARIABLE DEFINITIONS + + +

!     kbr    - wind quadrant key relative to simulation region
!
!              for barrier effect

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              1         8
!              typical            1         8
!              ------------   ---------  ---------

!


!     i1..i6 - do loop parameters defining grid update sequence
!              i4..i6 defines first update direction
!              i1..i3 defines second update directions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            -1          imax - 1
!              typical          -1          imax - 1
!              ------------   -----------  ---------

!

!              update direction always moves downwind

!     awa    - wind angle across simulation region relative to Y-axis

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        360
!              typical            0        360
!              ------------   ---------  ---------

!

!     sin_awa - precomputed sin of awa angle
!     cos_awa - precomputed cos of awa angle
!     tan_awa - precomputed tan of awa angle
