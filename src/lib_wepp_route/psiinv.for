!$Author$
!$Date$
!$Revision$
!$HeadURL$

      SUBROUTINE PSIINV(TIME, X, TSTAR, T, S, SI, OSINT, NS, A2,        & 
     &                  II, M, PSI, DPSI, OPSII, A, MRND)
     
      use wepp_interface_defs
      
      implicit none

!
!     + + + PURPOSE + + +
!
!     SUBROUTINE PSIINV IS USED TO COMPUTE BOTH PSI AND ITS DERIVATIVE
!     DPSI
!
!     THIS SUBROUTINES RETURNS THE INVERSE OF THE PSI FUNCTION
!
!     SINCE PSI IS CONVEX FOR FIXED TIME, A SIMPLE APPLICATION OF 
!     NEWTON'S METHOD WILL SUFFICE TO INVERT PSI(TIME,U)=X 
!
!     IF TIME IS CHANGED BY A SMALL AMOUNT BETWEEN CALLS, THE OLD 
!     PSIINV VALUE PROVIDES A GOOD STARTING POINT FOR THE NEXT VALUE
!
!     CALLED FROM HDEPTH
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-24-2005
!     CODED BY: D. FLANAGAN
!
!     + + + PARAMETER DECLARATIONS + + +
!
      INTEGER MXTIME
      PARAMETER (MXTIME = 1500)
!
!     + + + ARGUMENT DECLARATIONS + + +
!
      integer, intent(inout) :: II, NS
      real, intent(in) :: A, MRND, M, A2, TSTAR, T(MXTIME)
      real, intent(in) :: S(MXTIME), SI(MXTIME+1)
      real, intent(out) :: OSINT
      double precision, intent(inout) :: PSI, DPSI, OPSII
      DOUBLE PRECISION, intent(in) ::  TIME   
	  real, intent(inout) :: X 

!     + + + ARGUMENT DEFINITIONS + + +
!
!     NS      -
!     II      -
!     TIME    -
!     X       -
!     TSTAR   - time when rainfall excess stops (s)
!     T       -
!     S       -
!     SI      -
!     OSINT   -
!     A2      -
!     M       -
!     PSI     -
!     DPSI    -
!     OPSII   - OUTPUT VALUE FROM PSIINV (FORMERLY FUNCTION ARGUMENT)
!
!     + + + LOCAL VARIABLES + + +
!
      REAL TO, TECO, RNUMB
      DOUBLE PRECISION U, UM, UB, UT

!     + + + END SPECIFICATIONS + + +
!
      U = 0.
      TO = 0.
      TECO = MIN(TIME,TSTAR)
      UB = TO
      UT = TECO
!
!     U INITIALLY DOES NOT SEEM TO HAVE A VALUE JCAII 10-13-04
!     INITIALIZED THROUGH SALFORD FORTRAN
!
      IF (U.LE.TO.OR.TECO.LE.U) U = (TO+TECO) / 2.D0
!
   10 CALL PSIS(TIME, U, TSTAR, T, S, SI, OSINT, NS, A2, II,            &
     &          M, PSI, DPSI)
!
      IF (ABS(PSI-X).GE..005*X) THEN
!          
         IF (DPSI.EQ.0.D0) THEN
!         
            CALL RANDM(X, A, MRND, RNUMB)
            U = UB + (UT-UB) * RNUMB / 2.D0
         ELSE
            UM = U - (PSI-X) / DPSI
!
            IF (UM.LT.U) THEN
               UT = U
            ELSE
               UB = U
            END IF
!
            IF ((UT-UB).LT.0.00001D0) GO TO 20
!
            IF (UM.LE.UB) THEN
               U = (UB+U) / 2.D0
            ELSE IF (UM.LT.UT) THEN
               U = UM
            ELSE
               U = (UT+U) / 2.D0
            END IF
!
         END IF
!        
         GO TO 10
!
      END IF
!     
   20 OPSII = U
!     
      RETURN
      END
