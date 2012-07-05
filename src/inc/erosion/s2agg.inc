!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 's2agg.inc'

!     This block common is contains variables used for a simulation run
!     for EROSION.

!     + + + VARIABLE DECLARATIONS + + +

      real             sf1   (0:mngdpt, 0:mngdpt)
      real             sf10  (0:mngdpt, 0:mngdpt)
      real             sf84  (0:mngdpt, 0:mngdpt)
      real             sf200 (0:mngdpt, 0:mngdpt)
      real             sf84mn(0:mngdpt, 0:mngdpt)
      real             svroc (0:mngdpt, 0:mngdpt)  !edit ljh 1-22-05
      real             sf84ic
      real             sf10ic

      common /s2agg/                                                    &
     &                 sf1,  sf10, sf10ic,                              &
     &                 sf84, sf200,sf84mn, sf84ic, svroc

      save :: /s2agg/

!     + + + VARIABLE DEFINITIONS + + +

!     sf1  -   soil mass fraction in surface layer < 0.01 mm

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        .1
!              typical            0        .04
!              ------------   ---------  ---------

!     sf10  -  soil mass fraction in surface layer < 0.1 mm

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0         1.0
!              typical            0.1       0.8
!              ------------   ---------  ---------


!     sf84   - soil mass fraction in surface layer < 0.84 mm

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          1.0
!              typical            0.2        0.8
!              ------------   ---------  ---------

!     sf200   - soil mass fraction in surface layer < 2.00 mm

!              range values       min        max
!              ------------   ---------  ---------
!              fixed             0            1.0
!              typical           0.2          0.6
!              ------------   ---------  ---------

!     sf84mn   - "effective" soil mass fraction in surface layer < 0.84 mm
!                 needed for u* to be the threshold friction velocity.

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          1.0
!              typical            0.2        0.8
!              ------------   ---------  ---------

!     sf84ic   - soil mass fraction in surface layer < 0.84 mm
!                 at start of erosion event

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          1.0
!              typical            0.2        0.8
!              ------------   ---------  ---------

!     sf10ic   - soil mass fraction in surface layer < 0.10 mm
!                 at start of erosion event

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          1.0
!              typical            0.2        0.8
!              ------------   ---------  ---------

!     svroc   - soil rock volume in surface layer  !edit ljh 1-22-05
                 
!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          1.0
!              typical            0.         0.8
!              ------------   ---------  ---------
