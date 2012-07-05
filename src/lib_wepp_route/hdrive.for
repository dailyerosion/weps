!$Author$
!$Date$
!$Revision$
!$Source: /weru/cvs/wepp/wepp.watbal/hdrive.for,v $
 
      SUBROUTINE HDRIVE(NQT, DURPQ, QTP, TPEE,                          &
     &                  DT, NS, QTOT, Q, TQ1,                           &
     &                  RECUM, T, S, SI, SLEN, ALPHA, M, DUREXR, A1,A2, &
     &                  TSTAR, PEAKRO, DURRUN)
     
      use wepp_interface_defs

!     + + + PURPOSE + + +
!
!     HDRIVE COMPUTES KINEMATIC FLOW DEPTHS ON A PLANE AT SELECTED
!     DISTANCES DOWN THE PLANE AND AT SELECTED TIME SPACING  
!
!     THE LATERAL INFLOW (RAINFALL EXCESS) IS PRESENTED AS A POSITIVE 
!     STEP FUNCTION UP TO A GIVEN TIME AND IS ZERO THEREAFTER (IF 
!     STEP VALUE IS GIVEN AS ZERO, IT IS SET TO 1.E-8)
!
!     SUBROUTINE HDEPTH COMPUTES DEPTH OF FLOW ON THE PLANE
!
!     CALLED FROM MAIN
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-28-2005
!     CODED BY: D. FLANAGAN
!
!     + + + PARAMETER DECLARATIONS + + +
!      
      implicit none
      INTEGER MXTIME
      PARAMETER (MXTIME = 1500)

!     + + + ARGUMENT DECLARATIONS + + +
!
      integer, intent(out) :: NQT
      real, intent(out) :: DURPQ, QTP, TPEE, QTOT(MXTIME), Q(MXTIME)
      real, intent(out) :: TQ1(MXTIME), PEAKRO, DURRUN
      real, intent(inout) :: T(MXTIME), S(MXTIME), SI(MXTIME+1)
      integer, intent(in) :: NS
      real, intent(in) :: RECUM(MXTIME), ALPHA, M, DUREXR, A1, A2
      real, intent(in) :: TSTAR, DT, SLEN


!     + + + ARGUMENT DEFINITIONS + + +
!
!     NS      - number of rainfall excess points
!     NQT     - number of runoff intervals
!     DURPQ   - ?duration of peak runoff (s)?
!     QTP     - ?ending time of peak (s)?
!     TPEE    - ?time to peak (s)?
!     DT      - infiltration time step (s)
!     QTOT    - ?total runoff?
!     Q       - runoff rate (s) [out]
!     TQ1     - time counter for excess rainfall and runoff [out]
!     RECUM   - accumulated rainfall excess depth (m) [in]
!     T       - real rainfall excess time (s) = tr(i)-tp+ts
!     S       -  rainfall excess rate (m/s)
!     SI      -  integral of rainfall excess
!     SLEN    -  slope length (m)
!     ALPHA   -  CHEZY DEPTH-DISCHARGE COEFFICIENT
!     M       -  CHEZY DEPTH-DISCHARGE EXPONENT
!     DUREXR  -  duration of rainfall excess (s)
!     A1      -  coefficient = m*alpha
!     A2      -  coefficient = m-1
!     TSTAR   -  time when rainfall excess stops (s)
!     PEAKRO  -  peak runoff rate (m^3/s)
!     DURRUN  -  duration of runoff (s)
!
!     + + + LOCAL VARIABLES + + +
!
      INTEGER BEGRUN, I, NQI, IQT, II, NT, NQ
      REAL I1, LQ, QTMAX, BEGTIM, D, QMAX, QMAX10, T1
      REAL X0, X, A, MRND, HDPTHO
      DOUBLE PRECISION T2, TQNEW
      
!     + + + END SPECIFICATIONS + + +
!

!	  CALL PRINT_BUG(DT, NS,                                            &
!     &    RECUM, T, S, SI, SLEN, ALPHA, M, DUREXR, A1, A2, TSTAR)
 
      NQT = 0
      QTP = 0
      TPEE = 0

      X0 = 1948.
      CALL BGNRND(X0, X, A, MRND)

      BEGRUN = 0
      II = 1
      NT = NS + 10
      NQ = NT
      I1 = 0.D0
      LQ = 0.
     
      DO I = 1, 1000
         QTOT(I) = 0.
         Q(I) = 0.
         TQ1(I) = 0.
      END DO
     
      QTMAX = 0.97 * RECUM(NS+1)
     
      I = 0

   20 I = I + 1

      IF (I.LE.(NS+1)) THEN
         T2 = T(I)
      ELSE
         T2 = T2 + DT
      END IF
     

      CALL HDEPTH(T2, SLEN, A1, A2, TSTAR, T, S, SI, NS, II,            &     
     &            M, HDPTHO, A, MRND)

      D = HDPTHO
      TQ1(I) = T2

      IF (BEGRUN.EQ.0.AND.D.NE.0.) THEN
         BEGRUN = 1
         BEGTIM = T2
      END IF

      Q(I) = ALPHA * D ** M
     
      IF (I.GT.1) THEN
         I1 = I1 + (Q(I)+LQ) * (T2-T1) / 2.0
         QTOT(I+1) = I1
      END IF
     
      LQ = Q(I)
      T1 = T2
      IF (I.EQ.1) QMAX = Q(I)
     
      IF (Q(I).GT.QMAX) THEN
         NQI = I
         QMAX = Q(I)
         QTP = T2
         IQT = I
         QMAX10 = .1 * QMAX
      END IF
     
      IF (I.LT.999) THEN
         IF ((QTOT(I+1)*(1000.0/SLEN)).LT.QTMAX) THEN
            GO TO 20
         ELSE
            IF (Q(I).GT.QMAX10) GO TO 20
         END IF
      END IF
     
      NQT = I + 1
      TQ1(NQT) = T2 + DT
      TQNEW = TQ1(NQT)
      CALL HDEPTH(TQNEW, SLEN, A1, A2, TSTAR, T, S, SI, NS, II,         &
     &            M, HDPTHO, A, MRND)
      D = HDPTHO
      Q(NQT) = ALPHA * D ** M
     
      PEAKRO = Q(NQI) * (60000.0/SLEN)

!	  print *,'NQT=',NQT
     
      DO I = IQT, NQT-1
         IF (Q(I+1).LT.Q(I)) THEN
            TPEE = TQ1(I)
            DURPQ = TPEE - QTP
            GO TO 40
         END IF
      END DO 
     
   40 IF ((TQ1(NQT)/60.0).GT.DUREXR) THEN
         DURRUN = (TQ1(NQT)-BEGTIM) / 60.0
      ELSE
         DURRUN = DUREXR
      END IF
     
      RETURN
      END
