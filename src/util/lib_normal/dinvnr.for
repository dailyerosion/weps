!$Author$
!$Date$
!$Revision$
!$HeadURL$

      DOUBLE PRECISION FUNCTION dinvnr(p,q)
!**********************************************************************
!
!     DOUBLE PRECISION FUNCTION DINVNR(P,Q)
!     Double precision NoRmal distribution INVerse
!
!
!                              Function
!
!
!     Returns X  such that CUMNOR(X)  =   P,  i.e., the  integral from -
!     infinity to X of (1/SQRT(2*PI)) EXP(-U*U/2) dU is P
!
!
!                              Arguments
!
!
!     P --> The probability whose normal deviate is sought.
!                    P is DOUBLE PRECISION
!
!     Q --> 1-P
!                    P is DOUBLE PRECISION
!
!
!                              Method
!
!
!     The  rational   function   on  page 95    of Kennedy  and  Gentle,
!     Statistical Computing, Marcel Dekker, NY , 1980 is used as a start
!     value for the Newton method of finding roots.
!
!
!                              Note
!
!
!     If P or Q .lt. machine EPS returns +/- DINVNR(EPS)
!
!**********************************************************************
!     .. Parameters ..
      INTEGER maxit
      PARAMETER (maxit=100)
      DOUBLE PRECISION eps
      PARAMETER (eps=1.0D-13)
      DOUBLE PRECISION r2pi
      PARAMETER (r2pi=0.3989422804014326D0)
      DOUBLE PRECISION nhalf
      PARAMETER (nhalf=-0.5D0)
!     ..
!     .. Scalar Arguments ..
      DOUBLE PRECISION p,q
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION strtx,xcur,cum,ccum,pp,dx
      INTEGER i
      LOGICAL qporq
!     ..
!     .. External Functions ..
      DOUBLE PRECISION stvaln
      EXTERNAL stvaln
!     ..
!     .. External Subroutines ..
      EXTERNAL cumnor
!     ..
!     .. Statement Functions ..
      DOUBLE PRECISION dennor,x

      dennor(x) = r2pi*exp(nhalf*x*x)
!     ..
!     .. Executable Statements ..
!
!     FIND MINIMUM OF P AND Q
!
      qporq = p .LE. q
      IF (.NOT. (qporq)) GO TO 10
      pp = p
      GO TO 20

   10 pp = q
!
!     INITIALIZATION STEP
!
   20 strtx = stvaln(pp)
      xcur = strtx
!
!     NEWTON INTERATIONS
!
      DO 30,i = 1,maxit
          CALL cumnor(xcur,cum,ccum)
          dx = (cum-pp)/dennor(xcur)
          xcur = xcur - dx
          IF (abs(dx/xcur).LT.eps) GO TO 40
   30 CONTINUE
      dinvnr = strtx
!
!     IF WE GET HERE, NEWTON HAS FAILED
!
      IF (.NOT.qporq) dinvnr = -dinvnr
      RETURN
!
!     IF WE GET HERE, NEWTON HAS SUCCEDED
!
   40 dinvnr = xcur
      IF (.NOT.qporq) dinvnr = -dinvnr
      RETURN

      END
