      SUBROUTINE SINT(TIME, T, TSTAR, II, S, SI, NS, OSINT)
      
      use wepp_interface_defs
      
	  implicit none

!     + + + PURPOSE + + +
!
!     SUBROUTINE SINT CALCULATES THE INTEGRAL OF S WRT 
!     FROM 0.0 TO TIME
!
!     CALLED FROM HDEPTH, PHI, PSIS
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
!
      DOUBLE PRECISION, intent(in) :: TIME
	  real, intent(in) :: TSTAR, T(MXTIME), S(MXTIME), SI(MXTIME+1)
	  integer, intent(inout) :: II
	  integer, intent(in) :: NS
	  real, intent(out) :: OSINT
     
!     + + + END SPECIFICATIONS + + +
!
      IF (TIME.LT.TSTAR) THEN
         IF (TIME.GE.T(II+1)) GO TO 20
         IF (TIME.LT.T(II)) GO TO 30
!        
   10    OSINT = SI(II) + S(II) * (TIME-T(II))
!
         RETURN
!        
   20    II = II + 1
!
         IF (TIME.GE.T(II+1)) GO TO 20
         GO TO 10
!
   30    II = II - 1
!
         IF (TIME.LT.T(II)) GO TO 30
         GO TO 10
      END IF
!
      OSINT = SI(NS+1)
!     
      RETURN
      END
