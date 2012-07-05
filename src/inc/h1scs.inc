!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'h1scs.inc'
!
!     This common block contains the variables representing the SCS
!     runoff curve number for each subregion.
!
!     Parameter include files:  p1werm.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      real             ah0cng(mnsub)
      real             ah0cnp(mnsub)
!
      common / h1scs /                                                  &
     &                 ah0cng, ah0cnp

      save :: /h1scs/
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     ah0cng  - SCS runoff curve no. (good)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains
!
!
!     ah0cnp  - SCS runoff curve no. (poor)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the
!
!
!
!     ------------------------------------------------------------------
