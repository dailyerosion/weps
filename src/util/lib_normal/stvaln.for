!$Author$
!$Date$
!$Revision$
!$HeadURL$

      DOUBLE PRECISION FUNCTION stvaln(p)
!
!**********************************************************************
!
!     DOUBLE PRECISION FUNCTION STVALN(P)
!                    STarting VALue for Neton-Raphon
!                calculation of Normal distribution Inverse
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
!
!                              Method
!
!
!     The  rational   function   on  page 95    of Kennedy  and  Gentle,
!     Statistical Computing, Marcel Dekker, NY , 1980.
!
!**********************************************************************
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION p
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION sign,y,z
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION xden(5),xnum(5)
!     ..
!     .. External Functions ..
      DOUBLE PRECISION devlpl
      EXTERNAL devlpl
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC dble,log,sqrt
!     ..
!     .. Data statements ..
      DATA xnum/-0.322232431088D0,-1.000000000000D0,-0.342242088547D0,  &
     &     -0.204231210245D-1,-0.453642210148D-4/
      DATA xden/0.993484626060D-1,0.588581570495D0,0.531103462366D0,    &
     &     0.103537752850D0,0.38560700634D-2/
!     ..
!     .. Executable Statements ..
      IF (.NOT. (p.LE.0.5D0)) GO TO 10
      sign = -1.0D0
      z = p
      GO TO 20

   10 sign = 1.0D0
      z = 1.0D0 - p
   20 y = sqrt(-2.0D0*log(z))
      stvaln = y + devlpl(xnum,5,y)/devlpl(xden,5,y)
      stvaln = sign*stvaln
      RETURN

      END
