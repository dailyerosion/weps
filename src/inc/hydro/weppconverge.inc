!$Author$
!$Date$
!$Revision$
!$HeadURL$

! parameter for wepp infiltration convergence criteria
      real  bal_prec
      real  min_dstep

!     bal_prec   - the precision to which the infiltration water balance 
!                  is required to converge.

!*** Parameter Definitions ***
      parameter (bal_prec = 0.0001)
      parameter (min_dstep = 0.001)
