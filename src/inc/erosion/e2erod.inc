!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 'e2erod.inc'

!     This block common is contains variables used for a simulation run
!     for EROSION.

!     Must have parameter include file: \inc\p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             egt  (0:mngdpt, 0:mngdpt)
      real             egtcs(0:mngdpt, 0:mngdpt)
      real             egtss(0:mngdpt, 0:mngdpt)
      real             egt10(0:mngdpt, 0:mngdpt)

      common /e2erod/  egt, egtcs, egtss, egt10

      save :: /e2erod/

!     + + + VARIABLE DEFINITIONS + + +

!     egt    - Total soil loss at a grid point accumulated
!              for a time period (kg/m^2)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        50
!              typical            0         2
!              ------------   ---------  ---------

!     egtcs  - Total creep and saltation soil loss at grid
!              point accumulated for a time period (kg/m^2)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0
!              typical            0.1
!              ------------   ---------  ---------

!     egtss  - Total suspension soil loss at grid point accumulated
!              for a time period (kg/m^2)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        20
!              typical            0.1       5
!              ------------   ---------  ---------

!     egt10  - Total < 10 micron soil loss at grid point accumulated
!              for a time period (kg/m^2)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          1.0
!              typical            0.01       0.1
!              ------------   ---------  ---------
