      SUBROUTINE PHI_SUB(TIME, TSTAR, SI, NS, II, M, A2, S, T, OPHI)
      
      use wepp_interface_defs
      
      implicit none

!     + + + PURPOSE + + +
!
!     SEE SUBROUTINE PSIS, SINCE PHI(T)=PSI(T,0), FOR COMMENTS
!
!     CALLED FROM HDEPTH
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
      real, intent(in) :: TSTAR, SI(MXTIME+1), M, A2, S(MXTIME)
      real, intent(in) :: T(MXTIME)
      DOUBLE PRECISION, intent(in) :: TIME
      real, intent(out) :: OPHI
!
!     + + + ARGUMENT DEFINITIONS + + +
!
!     NS      -
!     II      -
!     TIME    -
!     TSTAR   -
!     SI      -
!     M       -
!     A2      -
!     OPHI    -
!
!     + + + LOCAL VARIABLES + + +
!
      INTEGER K, IU
      REAL B, S1TOM, S2TOM, OSINT

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
!     
      K = 2
      OPHI = 0.D0
      S1TOM = 0.D0
!     
   10 IF (K.NE.IU) THEN
         S2TOM = SI(K) ** M
         OPHI = OPHI + (S2TOM-S1TOM) / S(K-1)
         S1TOM = S2TOM
         K = K + 1
         GO TO 10
      END IF
!     
      IF (TIME.LE.TSTAR) THEN
        OPHI = (OPHI+(B**M-S1TOM)/S(K-1)) / M
      ELSE
        OPHI = OPHI + B ** A2 * (TIME-TSTAR)
      END IF
!
      RETURN
      END
