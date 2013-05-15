!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains variables representing the acop
!     database properties for each subregion.

!     Parameter include files:  p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      real             actopt(mnsub)
      real             actmin(mnsub)
!      real             acfdla(mnsub)
!      real             acrdla(mnsub)
!      real             ac0caf(mnsub)
!      real             ac0psf(mnsub)
!      real             ac0pt1(2,mnsub)
!      real             ac0pt2(2,mnsub)
      real             ac0fd1(2,mnsub)
      real             ac0fd2(2,mnsub)
!      real             ac0ck(mnsub)

      common / c1db2 /                                                  &
!     &                 actopt, actmin, acfdla, acrdla,                  &
!     &                 ac0caf, ac0psf,                                  &
!     &                 ac0pt1, ac0pt2, ac0fd1, ac0fd2, ac0ck
     &                 actopt, actmin,                                  &
!     &                 ac0fd1, ac0fd2, ac0ck
     &                 ac0fd1, ac0fd2

      save :: /c1db2/

!     + + + VARIABLE DEFINITIONS + + +

!     actopt - Optimal temperature for plant growth (deg C)
!     actmin - Minimum temperature for plant growth (deg C)
!     acfdla - Fraction of growing season when LAI starts declining (?)
!     acrdla - LAI decline rate (?)
!     ac0caf - Critical aeration factor (?)
!     ac0psf - Pest stress factor (?)
!     ac0pt1 - xy coordinate for 1st pt on optimal LAI curve
!     ac0pt2 - xy coordinate for 2nd pt on optimal LAI curve
!     ac0fd1 - xy coordinate for 1st pt on frost damage curve
!     ac0fd2 - xy coordinate for 2nd pt on frost damage curve
!     ac0ck  - canopy light extinction coefficient (0.0 < ac0ck < 1.0)
