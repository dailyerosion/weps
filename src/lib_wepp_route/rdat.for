!$Author$
!$Date$
!$Revision$
!$Source: /weru/cvs/wepp/wepp.watbal/rdat.for,v $

      SUBROUTINE RDAT(NF, SI, SC, S, T, NS, ALPHA, M, A1, A2,           &
     &                ACV, HCV, SCV, TSTAR, SLEN, SLOPE)
     
      use wepp_interface_defs
      
      implicit none

!     + + + PURPOSE + + +
!     SUBROUTINE RDAT GETS THE INPUT FOR HDRIVE
!
!     CALLED FROM MAIN
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
      real, intent(in) :: SLEN, SLOPE, T(MXTIME), SC
      real, intent(out) :: ALPHA, M, A1,A2, TSTAR, SI(MXTIME+1)
      real, intent(out) ::  ACV(3), HCV(3), SCV(3)
      real, intent(inout) :: S(MXTIME)
      integer, intent(inout) :: NS
      integer, intent(in) :: NF

!     + + + ARGUMENT DEFINITIONS + + +
!
!     NF      -
!     SI      -
!     SC      -
!     S       -
!     T       -
!     NS      -
!     ALPHA   -
!     M       -
!     A1      -
!     A2      -
!     ACV     -
!     HCV     -
!     SCV     -
!     TSTAR   -

!     + + + LOCAL VARIABLES + + +
!
      INTEGER I, IM, IRES, IC, ICC      
      REAL RGHIDX(5), CHEZYC, FRCROU, FRCCOV, FRCSOL, FRCTOT, XN
!      
!     + + + END SPECIFICATIONS + + +
!
      DATA RGHIDX /35., 15., 5., 2., 0./
!
      HCV(1) = .001
      HCV(2) = .0833333
      HCV(3) = .001
      ACV(1) = 60.
      ACV(2) = 60.
      ACV(3) = 1.
      SCV(1) = 1.666666E-5
      SCV(2) = 0.
      SCV(3) = 0.

      M = 1.5
!          
!           READ THE RILL/COVER RELATIONSHIP CODE (1-5)
!
!           ------------------------------------------------
!           CODE      CHARACTERISTIC    DEPRESSION DEPTH
!                                        (MM)
!           ------------------------------------------------
!             1        VERY ROUGH           100 - 150 
!             2         MODERATE             50 - 100 
!             3           LOW                25 - 50 
!             4          SLIGHT               0 - 25 
!             5          SMOOTH                 0
!           ------------------------------------------------
!
!            READ (3,*) ICC 
            ICC = 3
!           
            FRCROU = 13.0 * (1.0-EXP(-0.0773*(RGHIDX(ICC))))
            FRCCOV = 18.52 * (SC) ** 1.267
            FRCSOL = 1.0
            FRCTOT = FRCROU + FRCCOV + FRCSOL
            CHEZYC = 8.854 / SQRT(FRCTOT)
!         
!        ALPHA = C * SQRT(S) WHERE C IS THE CHEZY ROUGHNESS
!        COEFFICIENT AND S IS THE SLOPE OF THE PLANE
!         
         ALPHA = CHEZYC * (SQRT(SLOPE))
  
!
!     CONVERT ALPHA AND EXCESS RATES TO INTERNAL LENGTH AND TIME
!
!     ALPHA: METRIC = M**(2-M)/MIN 
!            ENGLISH = F**(2-M)/MIN 
!            DEFAULT = FL**(2-M)/MF
!
!     EXCESS: METRIC  = M/MIN 
!             ENGLISH = F/MIN 
!             DEFAULT = FL/MFN
!     
!     COMPUTE CONSTANTS USED IN LATER CALCULATIONS
!     
      ALPHA = ALPHA * ACV(1)
      A1 = M * ALPHA
      A2 = M - 1.D0
!     
!     RAINFALL EXCESS DATA
!     
   

!      IF (NF.EQ.0) THEN
!        
         TSTAR = T(NS)
         NS = NS - 1
!        
         DO I = 1, 101
          SI(I) = 0.D0
         END DO 

         DO I = 1, NS
            S(I) = S(I) * SCV(1)
            IF (S(I).EQ.0.D0) S(I) = 1.D-8
         END DO
!        
!        COMPUTE SI(N) AS THE INTEGRAL OF S WRT T FROM 0 TO T(N) FOR N
!        BETWEEN 1 AND NS+1
!        
         SI(1) = 0.D0
!         
         DO I = 1, NS
            SI(I+1) = SI(I) + S(I) * (T(I+1)-T(I))
         END DO 
!
!      END IF
!     
      RETURN
      END
