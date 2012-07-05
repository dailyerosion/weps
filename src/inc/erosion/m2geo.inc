!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 'm2geo.inc'

!     This block common is contains variables used for a simulation run
!     for MAIN and EROSION.

!     + + + VARIABLE DECLARATIONS + + +

      integer          imax
      integer          jmax
      real             ix
      real             jy
      integer          xgdpt    !specified # of grid data points in the x-dir
      integer          ygdpt    !specified # of grid data points in the y-dir
      integer          N_G_DPT  !# of grid data points under no barrier cond.
      integer          B_G_DPT  !# of grid data points to use if barrier exists
      real             MIN_GRID_SP !minimum targeted grid spacing (m)


      parameter (N_G_DPT = 30)
      parameter (B_G_DPT = 60)
      parameter (MIN_GRID_SP = 7.0)

      common /m2geo/                                                    &
     &                 imax, jmax,                                      &
     &                 ix, jy,                                          &
     &                 xgdpt, ygdpt

      save :: /m2geo/

!     + + + VARIABLE DEFINITIONS + + +

!     imax    - Number of grid intervals in x direction on EROSION grid

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              2        30
!              typical           20        20
!              ------------   ---------  ---------

!


!     jmax  -  Number of grid intervals in y direction on EROSION grid

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              2         30
!              typical           20         20
!              ------------   ---------  ---------

!


!     ix     - grid interval in x-direction(m)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            7.0       3000
!              typical          20.       2000
!              ------------   ---------  ---------

!

!     jy     - grid interval in y-direction (m)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            7.00      3000
!              typical         20.00      2000
!              ------------   ---------  ---------

