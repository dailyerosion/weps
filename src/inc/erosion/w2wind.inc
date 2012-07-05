!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 'w2wind.inc'

!     This block common is contains variables used for a simulation run
!     for EROSION. Included in sbwind and sberod; data sent to sbqout

!     + + + VARIABLE DECLARATIONS + + +

      real             wus   (0:mngdpt, 0:mngdpt)
      real             wust  (0:mngdpt, 0:mngdpt)
      real             wusp  (0:mngdpt, 0:mngdpt)
      real             wusto


      common /w2wind/                                                   &
     &                 wus, wust, wusp,wusto

      save :: /w2wind/

!     + + + VARIABLE DEFINITIONS + + +

!     wus    - Soil surface friction velocity (m/s)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        1.5
!              typical            0.30     0.6
!              ------------   ---------  ---------

!


!     wust   - Soil surface threhold friction velocity for emission(m/s)


!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0.35     1.75
!              typical            0.35     0.65
!              ------------   ---------  ---------

!


!     wusp   - Soil surface threshold friction velocity for transport
!              capacity (m/s)


!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0.35     1.750
!              typical            0.35     0.65
!              ------------   ---------  ---------



