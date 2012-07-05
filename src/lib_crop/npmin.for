!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!

!    file name: npmin.for

      subroutine npmin (j)

!     + + + PURPOSE + + +
!     This subroutine computes mineral P flux between the labile(AP), active
!     mineral(PMN) and stable mineral(OP) P pools.

!     + + + COMMON BLOCKS + + +

       include 'p1werm.inc'

! local includes
      include 'crop/csoil.inc'
      include 'crop/chumus.inc'
      include 'crop/cfert.inc'
      include 'crop/cenvr.inc'

!     + + + LOCAL VARIABLES + + +
      real rto,rmn,roc
      integer j
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     rto - interim variable (2.171)
!     rmn - mineral P flow rate between labile and active P pools (kg/ha/d)
!     roc - mineral P flow rate between active and stable P pools (kg/ha/d)
!
!     + + + OUTPUT FORMAT + + +
!
!     + + + END OF SPECIFICATIONS + + +
!
!     S5=.1*SUT*EXP(.115*T(j)-2.88)
!     calculate amount of P flowing from labile to active(rmn>0) or from active
!     to labile(rmn<0) mineral P pools. modified eqn. 2.171. PMN(j)=amount of
!     active mineral P pool.
      rto=psp(j)/(1.-psp(j))
      rmn=(ap(j)-pmn(j)*rto)
      if (rmn.lt.0.) rmn=rmn*.1
!
!     calculate amount of P flowing from stable to active(roc>0) or from active
!     to stable(roc<0) mineral P pools.  eqn. 2.176. OP(j)=amount of stable
!     mineral P pool.

      roc=bk(j)*(4.*pmn(j)-op(j))
      if (roc.lt.0.) roc=roc*.1
      op(j)=op(j)+roc
      pmn(j)=pmn(j)-roc+rmn
      ap(j)=ap(j)-rmn
!     write (38,3017)jd,psp(j),pmn(j),op(j),ap(j),rmn
!3017 format (1x,i3,1x,5(f9.4,1x))
      return
      end
