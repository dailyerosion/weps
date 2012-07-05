      
      SUBROUTINE HDEPTH(T2, X, A1, A2, TSTAR, T, S, SI, NS, II, M,      & 
     &                 HDPTHO, A, MRND)
     
      use wepp_interface_defs
      
      implicit none

!     + + + PURPOSE + + +
!
!     DEPTH H IS THE SOLUTION TO THE PARTIAL DIFFERENTIAL EQUATION:
!
!     H1+ALPHA*M*H**(M-1)*H2=S ,T AND X NON-NEGATIVE    (1)
!
!     WHERE:
!
!     H1    =  PARTIAL OF H WITH RESPECT TO T,
!     H2    =  PARTIAL OF H WITH RESPECT TO X,
!     ALPHA =  CHEZY DEPTH-DISCHARGE COEFFICIENT,
!     M     =  CHEZY DEPTH-DISCHARGE EXPONENT,
!     S     =  STEP FUNCTION WITH NS INITIAL NON-ZERO STEPS THAT IS
!              ZERO AFTER THE LAST STEP, AND
!     H IS ZERO WHEN EITHER T OR X IS ZERO.
!
!     LET PHI(T) BE PSI(T,0), WHERE PSI(T,U) IS DEFINED TO BE THE 
!     INTEGRAL FROM U TO T WRT L OF THE M-1TH POWER OF THE INTEGRAL 
!     OF S FROM TIME U TO L  
!
!     PSI(T,U) STRICTLY DECREASES FROM PHI(T) TO 0 FOR FIXED T AND
!     FOR U BETWEEN 0 AND AM1N(T,TSTAR) (S>0 BEFORE TSTAR,S=0 AFTER) 
! 
!     LET PSIINV(T,-) BE THE INVERSE OF PSI(T,-) - THEN THE GENERAL 
!     SOLUTION TO (1) IS GIVEN BY:
!
!     FOR X >= X1, H(T,X)=INTEGRAL OF S FROM 0 TO T             (2)
!     FOR X <= X1, H(T,X)=INTEGRAL OF S FROM PSIINV(T,X2) TO T  (3)
!
!     WHERE X1 = ALPHA*M*PHI(T) AND X2 = X/(ALPHA*M).
!
!     SUBROUTINE PHI(-) COMPUTES PHI, SUBROUTINE PSIINV(T,-) COMPUTES 
!     THE INVERSE OF PSI(T,-), AND SUBROUTINE SINT(T) COMPUTES THE 
!     INTEGRAL OF S FROM 0 TO TIME
!
!     CALLED FROM HDRIVE
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-28-2005
!     CODED BY: D. FLANAGAN
!
!     + + + PARAMETER DECLARATIONS + + +
!      
      INTEGER MXTIME
      PARAMETER (MXTIME = 1500)

!     + + + ARGUMENT DECLARATIONS + + +

      INTEGER II, NS
      REAL SI(MXTIME+1),TSTAR, M, A, MRND
      real, intent(in) :: X, A1, A2, S(MXTIME), T(MXTIME)
      DOUBLE PRECISION, intent(in) :: T2
      real, intent(out) ::hdptho

!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     A1      -  coefficient = m*alpha
!     A2      -  coefficient = m-1
!     T2      -
!     X       -  equivalent slope length (m) - same as SLEN elsewhere
!     S       -
!     T       -
!     SI      -
!     TSTAR   - time when rainfall excess stops (s)
!     M       - CHEZY DEPTH-DISCHARGE EXPONENT
!     HDPTHO  -
!     A       -
!     MRND    - current random number
!
!     + + + LOCAL VARIABLES + + +
!  
      REAL TERM1, TERM2, OPHI, OSINT, XX
      DOUBLE PRECISION PSI, DPSI, OPSII

!     + + + END SPECIFICATIONS + + +
!
      XX = X / A1
!
      CALL PHI_SUB(T2, TSTAR, SI, NS, II, M, A2, S, T, OPHI)
      IF (XX.GE.OPHI) THEN
         CALL SINT(T2, T, TSTAR, II, S, SI, NS, OSINT)
         HDPTHO = OSINT
         RETURN
      END IF
!
!     REPLACE ALL FUNCTIONS WITH SUBROUTINE CALLS
!     HDEPTH = SINT(T2) - SINT(PSIINV(T2,XX))
!
      CALL SINT(T2, T, TSTAR, II, S, SI, NS, OSINT)
      TERM1 = OSINT
      CALL PSIINV(T2, XX, TSTAR, T, S, SI, OSINT, NS, A2,               &
     &            II, M, PSI, DPSI, OPSII, A, MRND)
      CALL SINT(OPSII, T, TSTAR, II, S, SI, NS, OSINT)
      TERM2 = OSINT

      HDPTHO = TERM1 - TERM2
!
      RETURN
      END
