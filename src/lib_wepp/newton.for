!$Author$
!$Date$
!$Revision$
!$HeadURL$

      SUBROUTINE NEWTON(TIME, FFPAST, FFNOW, KS, SM)

!     + + + PURPOSE + + +
!     COMPUTES CUMULATIVE INFILTRATION VIA NEWTON'S METHOD

!     CALLED FROM GRNA
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-23-2005
!     CODED BY: D. FLANAGAN

!     + + + ARGUMENT DECLARATIONS + + +

      REAL TIME, FFPAST, FFNOW, KS, SM
     
!     + + + ARGUMENT DEFINITIONS + + +

!     TIME    -
!     FFPAST  -
!     FFNOW   -
!     KS      -
!     SM      -

!     + + + LOCAL INCLUDES + + +
      include "hydro/weppconverge.inc"

      REAL YY
      DOUBLE PRECISION NU1, DE1, TEST, XX

      YY = 0.10
      IF (FFPAST.NE.0.0) YY = FFPAST

   10 NU1 = TIME * KS - (YY-SM*ALOG(1.0+YY/SM))

      DE1 = YY / (SM+YY)
      TEST = NU1 / DE1
      XX = YY + TEST

      IF (ABS(TEST).GT. bal_prec) THEN
         YY = XX
         GO TO 10
      END IF

      FFNOW = YY

      RETURN
      END
