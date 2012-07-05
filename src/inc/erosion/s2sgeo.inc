!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 's2sgeo.inc'

!     This block common is contains variables used for a simulation run
!     for EROSION.

!     + + + VARIABLE DECLARATIONS + + +

      real             szrgh(0:mngdpt, 0:mngdpt)
      real             sxprg(mnsub)
      real             slrr(0:mngdpt, 0:mngdpt)



      common /s2sgeo/                                                   &
     &                 szrgh, sxprg,                                    &
     &                  slrr

      save :: /s2sgeo/

!     + + + VARIABLE DEFINITIONS + + +

!     szrgh  - Ridge height (mm)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        500
!              typical            0        200
!              ------------   ---------  ---------

!


!     sxprg  - ridge spacing parallel the wind direction(mm)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0.0       3000
!              typical             20       1000
!              ------------   ---------  ---------
!
!
!    slrr   - soil random roughness (mm)
!
!             range values      min       max
!              ------------   ---------  ---------
!              fixed              2        60
!              typical            2        30
!              ------------   ---------  ---------
!
