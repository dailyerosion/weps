!$Author$
!$Date$
!$Revision$
!$HeadURL$

      SUBROUTINE cdfnor(which,p,q,x,mean,sd,status,bound)
!**********************************************************************
!
!      SUBROUTINE CDFNOR( WHICH, P, Q, X, MEAN, SD, STATUS, BOUND )
!               Cumulative Distribution Function
!               NORmal distribution
!
!
!                              Function
!
!
!     Calculates any one parameter of the normal
!     distribution given values for the others.
!
!
!                              Arguments
!
!
!     WHICH  --> Integer indicating  which of the  next  parameter
!     values is to be calculated using values  of the others.
!     Legal range: 1..4
!               iwhich = 1 : Calculate P and Q from X,MEAN and SD
!               iwhich = 2 : Calculate X from P,Q,MEAN and SD
!               iwhich = 3 : Calculate MEAN from P,Q,X and SD
!               iwhich = 4 : Calculate SD from P,Q,X and MEAN
!                    INTEGER WHICH
!
!     P <--> The integral from -infinity to X of the normal density.
!            Input range: (0,1].
!                    DOUBLE PRECISION P
!
!     Q <--> 1-P.
!            Input range: (0, 1].
!            P + Q = 1.0.
!                    DOUBLE PRECISION Q
!
!     X < --> Upper limit of integration of the normal-density.
!             Input range: ( -infinity, +infinity)
!                    DOUBLE PRECISION X
!
!     MEAN <--> The mean of the normal density.
!               Input range: (-infinity, +infinity)
!                    DOUBLE PRECISION MEAN
!
!     SD <--> Standard Deviation of the normal density.
!             Input range: (0, +infinity).
!                    DOUBLE PRECISION SD
!
!     STATUS <-- 0 if calculation completed correctly
!               -I if input parameter number I is out of range
!                1 if answer appears to be lower than lowest
!                  search bound
!                2 if answer appears to be higher than greatest
!                  search bound
!                3 if P + Q .ne. 1
!                    INTEGER STATUS
!
!     BOUND <-- Undefined if STATUS is 0
!
!               Bound exceeded by parameter number I if STATUS
!               is negative.
!
!               Lower search bound if STATUS is 1.
!
!               Upper search bound if STATUS is 2.
!
!
!                              Method
!
!
!
!
!     A slightly modified version of ANORM from
!
!     Cody, W.D. (1993). "ALGORITHM 715: SPECFUN - A Portabel FORTRAN
!     Package of Special Function Routines and Test Drivers"
!     acm Transactions on Mathematical Software. 19, 22-32.
!
!     is used to calulate the  cumulative standard normal distribution.
!
!     The rational functions from pages  90-95  of Kennedy and Gentle,
!     Statistical  Computing,  Marcel  Dekker, NY,  1980 are  used  as
!     starting values to Newton's Iterations which compute the inverse
!     standard normal.  Therefore no  searches  are necessary for  any
!     parameter.
!
!     For X < -15, the asymptotic expansion for the normal is used  as
!     the starting value in finding the inverse standard normal.
!     This is formula 26.2.12 of Abramowitz and Stegun.
!
!
!                              Note
!
!
!      The normal density is proportional to
!      exp( - 0.5 * (( X - MEAN)/SD)**2)
!
!
!**********************************************************************
!     .. Parameters ..
!     ..
!     .. Scalar Arguments ..
      DOUBLE PRECISION bound,mean,p,q,sd,x
      INTEGER status,which
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION z,pq
!     ..
!     .. External Functions ..

      DOUBLE PRECISION dinvnr,spmpar
      EXTERNAL dinvnr,spmpar
!     ..
!     .. External Subroutines ..
      EXTERNAL cumnor
!     ..
!     .. Executable Statements ..
!
!     Check arguments
!
      status = 0
      IF (.NOT. ((which.LT.1).OR. (which.GT.4))) GO TO 30
      IF (.NOT. (which.LT.1)) GO TO 10
      bound = 1.0D0
      GO TO 20

   10 bound = 4.0D0
   20 status = -1
      RETURN

   30 IF (which.EQ.1) GO TO 70
!
!     P
!
      IF (.NOT. ((p.LE.0.0D0).OR. (p.GT.1.0D0))) GO TO 60
      IF (.NOT. (p.LE.0.0D0)) GO TO 40
      bound = 0.0D0
      GO TO 50

   40 bound = 1.0D0
   50 status = -2
      RETURN

   60 CONTINUE
   70 IF (which.EQ.1) GO TO 110
!
!     Q
!
      IF (.NOT. ((q.LE.0.0D0).OR. (q.GT.1.0D0))) GO TO 100
      IF (.NOT. (q.LE.0.0D0)) GO TO 80
      bound = 0.0D0
      GO TO 90

   80 bound = 1.0D0
   90 status = -3
      RETURN

  100 CONTINUE
  110 IF (which.EQ.1) GO TO 150
!
!     P + Q
!
      pq = p + q
      IF (.NOT. (abs(((pq)-0.5D0)-0.5D0).GT.                            &
     &    (3.0D0*spmpar(1)))) GO TO 140
      IF (.NOT. (pq.LT.0.0D0)) GO TO 120
      bound = 0.0D0
      GO TO 130

  120 bound = 1.0D0
  130 status = 3
      RETURN

  140 CONTINUE
  150 IF (which.EQ.4) GO TO 170
!
!     SD
!
      IF (.NOT. (sd.LE.0.0D0)) GO TO 160
      bound = 0.0D0
      status = -6
      RETURN

  160 CONTINUE
!
!     Calculate ANSWERS
!
  170 IF ((1).EQ. (which)) THEN
!
!     Computing P
!
          z = (x-mean)/sd
          CALL cumnor(z,p,q)

      ELSE IF ((2).EQ. (which)) THEN
!
!     Computing X
!
          z = dinvnr(p,q)
          x = sd*z + mean

      ELSE IF ((3).EQ. (which)) THEN
!
!     Computing the MEAN
!
          z = dinvnr(p,q)
          mean = x - sd*z

      ELSE IF ((4).EQ. (which)) THEN
!
!     Computing SD
!
          z = dinvnr(p,q)
          sd = (x-mean)/z
      END IF

      RETURN

      END
