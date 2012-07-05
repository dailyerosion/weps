      SUBROUTINE PSIS(TIME, UU, TSTAR, T, S, SI, OSINT, NS, A2, II,     &
     &                M, PSI, DPSI)
     
      use wepp_interface_defs
      
      implicit none

!     + + + PURPOSE + + +
!
!     THIS ROUTINE COMPUTES FUNCTION PSI AND ITS DERIVATIVE DPSI  
!
!     ESSENTIALLY, PSI AND DPSI CAN BE CALCULATED FROM THE INTEGRAL OF 
!     S WRT TIME (THE LATTER IS CALCULATED BY SINT)
!
!     CALLED FROM PSIINV
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-28-2005
!     CODED BY: D. FLANAGAN

!
!     + + + PARAMETER DECLARATIONS + + +
!
      INTEGER MXTIME
      PARAMETER (MXTIME = 1500)
!
!     + + + ARGUMENT DECLARATIONS + + +
!
      INTEGER, intent(in) :: NS
      integer, intent(inout) :: II
      real, intent(in) :: TSTAR, A2, M, SI(MXTIME+1), S(MXTIME)
      real, intent(in) ::  T(MXTIME)
      real, intent(out) :: OSINT
      double precision, intent(in) :: TIME, UU
      double precision, intent(out) :: DPSI, PSI

!     + + + ARGUMENT DEFINITIONS + + +
!
!     NS     -
!     II     -
!     TIME   -
!     TSTAR  -
!     T      -
!     S      -
!     SI     -
!     A2     -
!     M      -
!     OSINT  -
!     UU     -
!     PSI    -
!     DPSI   -
!
!     + + + LOCAL VARIABLES + + +
!
      DOUBLE PRECISION S1, S2, S1TOA2, S2TOA2, A, B, XU
      integer IU,IL,K
!
!     + + + END SPECIFICATIONS + + +

!
      IF (TIME.GE.TSTAR) THEN
         B = SI(NS+1)
         IU = NS + 1
      ELSE
         CALL SINT(TIME, T, TSTAR, II, S, SI, NS, OSINT)
         B = OSINT
         IU = II + 1
      END IF

      XU = UU
      CALL SINT(XU, T, TSTAR, II, S, SI, NS, OSINT)
      A = OSINT
      IL = II + 1
      K = IL
      PSI = 0.D0
      DPSI = 0.D0
      S1 = 0.D0
      S1TOA2 = 0.D0

   10 IF (K.NE.IU) THEN
         S2 = MAX(SI(K)-A,0.D0)
         S2TOA2 = S2 ** A2
         PSI = PSI + (S2*S2TOA2-S1*S1TOA2) / S(K-1)
         DPSI = DPSI + (S2TOA2-S1TOA2) / S(K-1)
         K = K + 1
         S1 = S2
         S1TOA2 = S2TOA2
         GO TO 10
      END IF

      S2 = MAX(B-A,0.D0)
      S2TOA2 = S2 ** A2
      
      PSI = (PSI+(S2*S2TOA2-S1*S1TOA2)/S(K-1)) / M
      DPSI = -S(IL-1) * (DPSI+(S2TOA2-S1TOA2)/S(K-1))
      
      IF (TIME.LE.TSTAR.OR.S2.EQ.0.) RETURN
   
      PSI = PSI + S2TOA2 * (TIME-TSTAR)
      DPSI = DPSI - S(IL-1) * A2 * S2TOA2 / S2 * (TIME-TSTAR)

      RETURN
      END
