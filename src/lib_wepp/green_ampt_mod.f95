!$Author$
!$Date$
!$Revision$
!$HeadURL$

module green_ampt_mod

      real, parameter :: bal_prec = 0.0001 ! the precision to which the infiltration water balance is required to converge.
      real, parameter :: min_dstep = 0.001

  contains

    SUBROUTINE grna( NF, DEPSTO, TR, R, RR, KS, SM,                   &
     &     NS, TF, RCUM, F, FF, RE, RECUM, TP,                          &
     &     RPRINT, DDEPSTO, RUNOFF, DUREXR, EFFINT, EFFDRR, IT )

!     + + + PURPOSE + + +

!     THIS PROGRAM CALCULATES INFILTRATION RATES AND DEPTHS FOR 
!     UNSTEADY RAIN USING THE GREEN AND AMPT INFILTRATION EQUATION 
!     AS MODIFIED BY MEIN AND LARSON.  THE EQUATION HAS THE FORM :

!     F = KS + KS*SM/FF
!           AND
!     KS*T = FF - SM*LN(1 + FF/SM)
! WHERE:
!     F = INFILTRATION RATE (L/T)
!     KS = SATURATED CONDUCTIVITY (L/T)
!     SM = EFFECTIVE MATRIC POTENTIAL (L)
!     FF = CUMULATIVE INFILTRATION (L)
!     LN = NATURAL LOG

! INPUT:
!     1. TWO COLUMN FILE
!               COL 1 = TIME (MINUTES)
!               COL 2 = RAINFALL RATE (MILLIMETERS/HOUR)
!     ON THE VAX THE FILE IS AN EDR FILE
!     ON THE PC THE FILE HAS A FORMAT 2F10.2

!     2. SATURATED CONDUCTIVITY (MILLIMETERS/HOUR)
!     3. EFFECTIVE MATRIC POTENTIAL (S*M) (MILLIMETERS)
!               WHERE:
!                       S = DIFFERENCE IN AVERAGE CAPILARY POTENTIAL
!                           BEFORE AND AFTER WETTING
!                       M = DIFFERENCE IN AVERAGE SOIL MOISTURE

!-----------------------------------------------------------
!                         PARAMETERS

! 1. R(I) ..............RAINFALL RATE
! 2. RCUM(I) ...........ACCUMULATED RAINFALL DEPTH
! 3. F(I) ..............INFILTRATION RATE
! 4. FF(I) .............ACCUMULATED INFILTRATION DEPTH
! 5. RE(I) .............RAINFALL EXCESS RATE
! 6. RECUM(I) ..........ACCUMULATED RAINFALL EXCESS DEPTH
! 7. CU ................INDICATOR OF PONDING IF NO PONDING AT BEGINNING
!       CU < 0 - NO PONDING    CU > 0 - PONDING
! 8. CP ................INDICATOR OF PONDING WHEN PONDED AT BEGINNING
!       CP < 0 - PONDING STOPS DURING INTERVAL   CP > 0 - PONDING
! 9. TR(I) .............RAINFALL TIMES
! 10. TP ...............TIME OF PONDING
! 11. TS ...............PSEUDOTIME TO ADJUST REAL TIME FOR INFILTRATION
! 12. T ................REAL TIME = TR(I)-TP+TS
! 13. PT ...............ACCUMULATED RAINFALL AT TIME OF PONDING
!-----------------------------------------------------------

!     CALLED FROM MAIN
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-22-2005
!     CODED BY: D. FLANAGAN

!     + + + PARAMETER DECLARATIONS + + +

      INTEGER MXTIME, MXPOND
      PARAMETER (MXTIME = 1500, MXPOND = 1000)

!     + + + ARGUMENT DECLARATIONS + + +
      INTEGER, intent(in) :: NF
      REAL, intent(in) :: DEPSTO, TR(MXTIME), R(MXTIME), RR(MXTIME),    &
     &     KS, SM
      INTEGER, intent(inout) :: NS
      REAL, intent(inout) :: TF(MXTIME), RCUM(MXTIME),                  &
     &     F(MXTIME), FF(MXTIME), RE(MXTIME), RECUM(MXTIME), TP(MXPOND),&
     &     RPRINT(MXTIME), DDEPSTO(MXTIME),                             &
     &     RUNOFF, DUREXR, EFFINT, EFFDRR
      INTEGER, intent(out) :: IT
     
!     + + + ARGUMENT DEFINITIONS + + +
!     DEPSTO  - Depression Storage (L)
!     TR      - time values in rainfall breakpoint array (T)
!     TF      - time values in infiltration/rainfall excess/depression storage array (T)
!     R       - rainfall rate (L/T)
!     RCUM    - accumulated rainfall depth (L)
!     F       - infiltration rate (L/T)
!     FF      - accumulated infiltration depth (L)
!     RE      - rainfall excess rate (L/T)
!     RECUM   - accumulated rainfall excess depth (L)
!     RR      - precalculated rainfall rate accumulation array (L)
!     NF      - number of values in infiltration arrays
!     KS      - saturated conductivity (L/T)
!     SM      - effective matric potential (L)
!     TP      - time of ponding (T)
!     NS      - number of element for end of runoff arrays
!     RPRINT  -
!     DDEPSTO - depth of depression storage at infiltration timestep (L)
!     RUNOFF  - RUNOFF DEPTH (L)
!     DUREXR  - DURATION OF RAINFALL EXCESS (T)
!     EFFINT  - EFFECTIVE RAINFALL INTENSITY
!     EFFDRR  - EFFECTIVE RAINFALL DURATION

!     + + + LOCAL VARIABLES + + +
!      INTEGER I, K, POND, IT, NP, NT
      INTEGER I, K, POND, NP, NT
      REAL RTEMP(MXTIME), DTIME
      REAL DURRE, SUMINT, KSM, TT, TS, CU, CP, FSUM
      REAL PT(MXPOND), PRECUM(MXPOND)
      DOUBLE PRECISION  XX

!     + + + LOCAL DEFINITIONS + + +
!     DTIME   - time step
!     DURRE   - duration of effective rainfall
!     SUMINT  - rainfall intensity sum
!     KSM     - product of KS and SM
!     TT      - temporary "real" time variable
!     TS      - pseudotime to adjust real time for infiltration
!     CU      - indicator of ponding if no ponding at beginning
!               CU < 0 - no ponding    CU > 0 - ponding
!     CP      - indicator of ponding when ponded at beginning
!               CP < 0 - ponding stops during interval   CP > 0 - ponding
!     XX      - local temporary variable (double precision)
!     FSUM    - infiltration sum value
!     PT      - accumulated rainfall at time of ponding
!     PRECUM  - accumulated rainfall excess at time of ponding

!     + + + END SPECIFICATIONS + + +

      KSM = KS * SM

      POND = 0
      FSUM = 0.0
      K = 1
      FF(1) = 0.0
      RECUM(1) = 0.0
      RCUM(1) = 0.0
      RTEMP(1) = R(1)
      DDEPSTO(1) = 0.0
      IT = 0
      NP = 0
      DO 10 I = 1, MXPOND
         TP(I) = 0.
   10 CONTINUE

!     START RUN

      DO 20 I = 2, NF
         K = K + 1

!        CHECK IF THERE IS PONDING IN PREVIOUS INTERVAL

         IF (POND.EQ.0) THEN

!           CASE ONE: NO PONDING IN PREVIOUS INTERVAL

            IF (R(I-1).LE.KS) THEN
               IF (R(I-1).EQ.0.0) THEN
                  FF(K) = FSUM
                  RECUM(K) = RECUM(K-1)
                  RCUM(K) = RR(I)
                  RTEMP(K) = R(I)
                  TF(K) = TR(I)
                  POND = 0
                  DDEPSTO(K) = 0.0
                  GO TO 20
               ELSE
                  FSUM = RR(I) - RECUM(K-1)
                  FF(K) = FSUM
                  RECUM(K) = RECUM(K-1)
                  RCUM(K) = RR(I)
                  RTEMP(K) = R(I)
                  TF(K) = TR(I)
                  POND = 0
                  DDEPSTO(K) = 0.0
                  GO TO 20
               END IF
            END IF

!           PONDING INDICATOR WHEN NO PONDING IN PREVIOUS INTERVAL

            CU = RR(I) - RECUM(K-1) - KSM / (R(I-1)-KS)

!           CASE ONE-A: NO PONDING

            IF (CU.LE.0.0) THEN
               FSUM = RR(I) - RECUM(K-1)
               FF(K) = FSUM
               RECUM(K) = RECUM(K-1)
               RCUM(K) = RR(I)
               RTEMP(K) = R(I)
               TF(K) = TR(I)
               POND = 0
               DDEPSTO(K) = 0.0
               GO TO 20
            END IF

!           CASE ONE-B: PONDING - GET TIME TO PONDING, TP

            POND = 1
            IT = IT + 1
            TP(IT) = (KSM/(R(I-1)-KS)-RR(I-1)+RECUM(K-1))/R(I-1)+TR(I-1)
            IF (TP(IT).LE.TR(I-1)) THEN
               ! ponding starts at beginning of present interval
               TP(IT) = TR(I-1)
               PT(IT) = RR(I-1)
            ELSE IF (TP(IT).LT.TR(I)) THEN
               ! ponding starts part way through the interval
               ! insert new time point
               FF(K) = FF(K-1) + R(I-1) * (TP(IT)-TR(I-1))
               RECUM(K) = RECUM(K-1)
               PT(IT) = RR(I-1) + (TP(IT)-TR(I-1)) * R(I-1)
               RCUM(K) = PT(IT)
               RTEMP(K) = R(I)
               TF(K) = TP(IT)
               DDEPSTO(K) = 0.0
               K = K + 1
            ELSE
               ! ponding starts exactly at the end of the interval
               ! do not insert new time point
               ! set variables like no pond above
               ! turn off pond flag and regress counter to allow ponding
               ! to start at the beginning of the next interval
               FSUM = RR(I) - RECUM(K-1)
               FF(K) = FSUM
               RECUM(K) = RECUM(K-1)
               RCUM(K) = RR(I)
               RTEMP(K) = R(I)
               TF(K) = TR(I)
               DDEPSTO(K) = 0.0
               POND = 0
               IT = IT - 1
               GO TO 20

               ! we get here when CU has a very small positive value
               ! 5.33840139E-10 caused it in the debug scenario where
               ! TP(IT) ends up being exactly equal to the TR(I) (to full single precision)
               ! this results in a zero time interval DTIME and divide by zero
               ! when finding F(I) and RE(I) at end of routine. (FAF - June 28, 2010)
            END IF
            ! save cumulative rainfall excess at this ponding point
            PRECUM(IT) = RECUM(K-1)

!           CUMULATIVE RAINFALL, PT, AT TIME TO PONDING, TP


            IF( SM .GT. 0.0 ) THEN
               ! infiltration with matric potential

               ! PSEUDOTIME - GET TIME SHIFT DUE TO INFILTRATION, TS
               XX = (PT(IT)-RECUM(K-1)) / SM
               TS = SM / KS * (XX-DLOG(1.D0+XX))

               ! REAL TIME, T
               TT = TR(I) - TP(IT) + TS

               ! CUMULATIVE INFILTRATION, NEWTONS METHOD ALA LJL
               CALL NEWTON(TT, FF(K-1), FF(K), KS, SM)
            ELSE
               ! infiltration with gravitational potential only
               FF(K) = PT(IT) - PRECUM(IT) + KS * (TR(I) - TP(IT))
            END IF

            DDEPSTO(K) = RR(I) - FF(K)
            IF( DDEPSTO(K) .GT. DEPSTO ) THEN
               RECUM(K) = DDEPSTO(K) - DEPSTO
               DDEPSTO(K) = DEPSTO
            ELSE
               RECUM(K) = RECUM(K-1)
            END IF
            RCUM(K) = RR(I)
            RTEMP(K) = R(I)
            TF(K) = TR(I)

         ELSE

!           CASE TWO: PONDING IN PREVIOUS INTERVAL

            IF( SM .GT. 0.0 ) THEN
               ! infiltration with matric potential
               TT = TR(I) - TP(IT) + TS
               CALL NEWTON(TT, FF(K-1), FF(K), KS, SM)
            ELSE
               ! infiltration with gravitational potential only
               FF(K) = PT(IT) - PRECUM(IT) + KS * (TR(I) - TP(IT))
            END IF

!           CHECK IF NO PONDING BEFORE END OF INTERVAL
            ! with ponding, depression storage adds to available water
            CP = RR(I) - FF(K) - RECUM(K-1)

!           CASE TWO-A: NO PONDING BEFORE END OF INTERVAL

            IF (CP.LT.0.0) THEN
               FF(K) = RR(I) - RECUM(K-1)
               FSUM = FF(K)
               DDEPSTO(K) = 0.0
               RECUM(K) = RECUM(K-1)
               RCUM(K) = RR(I)
               RTEMP(K) = R(I)
               TF(K) = TR(I)
               POND = 0
            ELSE

!              CASE TWO-B: PONDING CONTINUES MERRILY ON

               DDEPSTO(K) = RR(I) - FF(K)- RECUM(K-1)
               IF( DDEPSTO(K) .GT. DEPSTO ) THEN
                  RECUM(K) = DDEPSTO(K) - DEPSTO + RECUM(K-1)
                  DDEPSTO(K) = DEPSTO
               ELSE
                  RECUM(K) = RECUM(K-1)
               END IF
               RCUM(K) = RR(I)
               RTEMP(K) = R(I)
               TF(K) = TR(I)
            END IF
         END IF
   20 CONTINUE

      ! check for remaining depression storage
      IF( K .GT. 2 ) THEN
         DTIME = TF(K-1) - TF(K-2)
         DO WHILE( DDEPSTO(K) .GT. bal_prec ) ! newton is no more accurate than bal_prec
            K = K + 1
            TF(K) = TF(K-1) + DTIME

            ! CASE TWO: PONDING IN PREVIOUS INTERVAL
            IF( SM .GT. 0.0 ) THEN
               ! infiltration with matric potential
               TT = TF(K) - TP(IT) + TS
               CALL NEWTON(TT, FF(K-1), FF(K), KS, SM)
            ELSE
               ! infiltration with gravitational potential only
               FF(K) = PT(IT) - PRECUM(IT) + KS * (TF(K) - TP(IT))
            END IF

            ! CHECK IF NO PONDING BEFORE END OF INTERVAL
            ! with ponding, depression storage adds to available water
            CP = RR(NF) - FF(K) - RECUM(K-1)

            ! CASE TWO-A: NO PONDING BEFORE END OF INTERVAL
            IF (CP.LT.0.0) THEN
               FF(K) = RR(NF) - RECUM(K-1)
               DDEPSTO(K) = 0.0
            ELSE
               ! CASE TWO-B: PONDING CONTINUES MERRILY ON
               DDEPSTO(K) = RR(NF) - FF(K) - RECUM(K-1)
            END IF

            RECUM(K) = RECUM(K-1)
            RCUM(K) = RCUM(K-1)
            RTEMP(K) = 0.0

            ! if depth change too small, increase time step
            ! this allows much longer time for infilatration without
            ! exceeding the array length MXTIME
            IF( DDEPSTO(K-1) - DDEPSTO(K) .LT. min_dstep ) THEN
                DTIME = DTIME * 2
!                write(*,*) 'Double DTIME index k = ', k
            END IF

            ! absolutely avoid running off end of array
            IF( K .GE. MXTIME ) THEN
                write(*,*)                                              &
     &      'Warning, GRNA ponding water excess, water balance violated'
                EXIT
            END IF
         END DO
      END IF
      NS = K

      NT = 0
      DO 30 I = 1, NS - 1
         DTIME = TF(I+1) - TF(I)
         F(I) = (FF(I+1)-FF(I)) / DTIME
         IF (F(I).LT.0.0) F(I) = 0.0
         RPRINT(I) = RTEMP(I)
         RE(I) = (RECUM(I+1)-RECUM(I)) / DTIME
         IF (RE(I).LT.0.0) RE(I) = 0.0
         IF (RE(I).GT.0.0) THEN
            NT = I
            NP = NP + 1
         END IF
   30 CONTINUE

      F(NS) = 0.0
      RE(NS) = 0.0
      RUNOFF = RECUM(NS)
      DUREXR = MAX( 0.0, TF(NT+1) - TP(1) )
      FF(NS) = RCUM(NS) - RECUM(NS)

!     GET DURATION OF RAINFALL EXCESS AND EFFECTIVE RAINFALL INTENSITY

      DURRE = 0.0
      SUMINT = 0.0
      DO I = 1, NS-1
        IF (RE(I).GT.0.0) THEN
          DURRE = DURRE + TF(I+1) - TF(I)
          SUMINT = SUMINT + (TF(I+1)-TF(I)) * RTEMP(I)
        ENDIF
      END DO
      IF (DURRE.GT.0.0) THEN
        EFFINT = SUMINT/DURRE
        EFFDRR = DURRE
      ELSE
        EFFINT = 0.0
        EFFDRR = 0.0
      ENDIF

      RETURN
    END SUBROUTINE grna

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

      DOUBLE PRECISION NU1, DE1, TEST, YY

      YY = 0.10
      IF (FFPAST.NE.0.0) YY = FFPAST

   10 NU1 = TIME * KS - (YY-SM*LOG(1.0+YY/SM))

      DE1 = YY / (SM+YY)
      TEST = NU1 / DE1
      YY = YY + TEST

      IF (ABS(TEST).GT. bal_prec) THEN
         GO TO 10
      END IF

      FFNOW = YY

      RETURN
    END SUBROUTINE NEWTON

end module green_ampt_mod

