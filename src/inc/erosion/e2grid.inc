!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 'e2grid.inc'

!     This block common is contains variables used for a simulation run
!     for MAIN and EROSION.

!     + + + VARIABLE DECLARATIONS + + +

      integer          csr(0:mngdpt, 0:mngdpt)
      real      w0br(0:mngdpt,0:mngdpt,8)
!      real      w0hill(0:mngdpt, 0:mngdpt,8)




      common /e2grid/                                                   &
     &                csr,                                              &
     &                     w0br
!    &                          w0hill

      save :: /e2grid/
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     csr    - index of current subregion at grid point x,y
!
!              range values      min       max
!              ------------   ---------  ---------
!              fixed              2        30
!              typical           20        20
!              ------------   ---------  ---------
!
!    w0br    - ratio of shelter to open, flat, field friction velocity
!              at each grid point x,y as influenced by barriers.
!
!               range values      min       max
!              ------------   ---------  ---------
!              fixed              0         1
!              typical            0.1       1
!              ------------   ---------  ---------
!
!
!   w0hill   - ratio of hill to open, flat, field friction velocity
!              at each grid point x,y as influenced by hills.
!
!               range values      min       max
!              ------------   ---------  ---------
!              fixed              0         2
!              typical           .2         1.3
!              ------------   ---------  ---------
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
