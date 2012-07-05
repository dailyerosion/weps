      SUBROUTINE BGNRND(X0, X, A, MRND)
      
      use wepp_interface_defs
      
      implicit none

!     + + + PURPOSE + + +
!
!     BGNRND IS USED TO INITIALIZE THE SEED FOR THE RANDOM NUMBER
!     GENERATOR RND
!
!     THIS ROUTINE, WITH DEBUG LINES ACTIVATED, CHECKS TO SEE IF THE 
!     UNIFORM RANDOM NUMBER GENERATOR, RND, WILL WORK IN DOUBLE OR 
!     SINGLE PRECISION
!
!     TO CHANGE TO SINGLE PRECISION, COMMENT OUT THE DOUBLE PRECISION 
!     STATEMENTS IN BGNRND AND RND AND CHANGE THE DATA STATEMENTS IN 
!     RND TO HAVE SINGLE PRECISION NUMBERS
!
!     X0  R*4  THE ROUTINE INITIALIZES X TO MIN(MAX(INT(X0),1),M-1)  
!     THIS INSURES X IS INITIALLY POSITIVE AND LESS THAN M
!
!     CALLED FROM HDRIVE
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-28-2005
!     CODED BY: D. FLANAGAN
!
!     + + + ARGUMENT DECLARATIONS + + +
!
      REAL, intent(in) :: X0
      real, intent(inout) :: X
      real, intent(out) :: A, MRND
!      
!     + + + ARGUMENT DEFINITIONS + + +
!
!     X0      - random number seed
!     X       - updated random number seed
!     A       - 
!     MRND    - maximum random number
!
!     + + + LOCAL VARIABLES + + +
      DOUBLE PRECISION U
!
!     + + + END SPECIFICATIONS + + +

!
!     TEST FOR DOUBLE PRECISION OPERATION
!
      X = 1.D0
      A = 16807.D0
      MRND = 2147483647.D0
      X = A * MRND
      U = A
!
      IF (.NOT.((X.NE.X+1.D0).AND.(U.NE.U+1.))) THEN
         WRITE(*,*) 'RANDOM NUMBER INITIALIZATION FAILED'
         STOP
      END IF
!     
!     TEST FOR SINGLE PRECISION OPERATION
!     
      U = X
!     IF (U.NE.U+1.) THEN
!     END IF
!     
!     INITIALIZE X
     
      X = AINT(X0)
      IF (X.LE.0.D0) X = 1.
      IF (X.GE.MRND) X = MRND - 1.D0

      RETURN
      END
