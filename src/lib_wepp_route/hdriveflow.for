!$Author$
!$Date$
!$Revision$
!$HeadURL$

!
      SUBROUTINE HDRIVEFLOW(NS,NF,RECUM,SLEN,SLOPE,DUREXR,DT,TF,RE,     &     
     & SC,PEAKRO,DURRUN)
     
      use wepp_interface_defs
      
      implicit none

!
!
!     This is the entry point for setting up hdrive to run in WEPS 
!     model. In WEPS this code will be called right after grna in subroutine
!     waterbal. Before calling the hdrive subroutine the rdat subroutine
!     to do some initialization and setup variables not passed.
!
!     NS      - number of rainfall excess points (NS)
!     RECUM   - accumulated rainfall excess depth (RECUM)
!     SLEN    - slope length (amxsim(2,2)-amxsim(2,1))
!     SLOPE   - average slope of plane
!     DUREXR  - duration of rainfall excess (s). (DUREXR)
!     DT      - infiltration time step (s) (DTINF)
!     TF      - real rainfall excess time (s) (TF) [T in wepp]
!     RE      - rainfall excess rate m/s (RE) [S in wepp]
!     SC      - ground cover as a fraction
!     JDAY    - current julian day of simulation ()
!     DAY     - current day of simulation ()
!     MON     - current month of simulation ()
!     YR      - current year of simulation ()
!     PEAKRO  - peak runoff rate (m^3/s)
!     DURRUN  - duration of runoff (s)
!     
!
!
      INTEGER MXTIME, MXPOND
      PARAMETER (MXTIME = 1500, MXPOND = 1000)

      integer, intent(in) :: NF
	integer, intent(inout) :: NS
      real, intent(in) :: RECUM(MXTIME), SLEN, DUREXR, DT, TF(MXTIME),  &
     & RE(MXTIME), SLOPE, SC
      real, intent(out) :: PEAKRO,DURRUN

!     hdrive requires the following input(readonly) parameters to be setup
!     in addtion to the parameters passed. These are setup in the rdat
!     subroutine.
!    
!     ALPHA   -  CHEZY DEPTH-DISCHARGE COEFFICIENT
!               WEPS ->
!
!     M       -  CHEZY DEPTH-DISCHARGE EXPONENT.
!               WEPS -> constant set to 1.5
!
!     A1      -  coefficient = m*alpha
!               WEPS -> derived from M and ALPHA
!
!     A2      -  coefficient = m-1
!               WEPS -> derived from M 
!
!     TSTAR   -  time when rainfall excess stops (s)
!               WEPS -> derived from last element of T, T(NS)
!
!     
!     hdrive writes to the following output (writeonly) variables:
!     NQT     - number of runoff intervals. 
!     DURPQ   - ?duration of peak runoff (s)?
!     QTP     - ?ending time of peak (s)?
!     TPEE    - ?time to peak (s)?
!     QTOT    - ?total runoff?
!     Q       - runoff rate (s)
!     TQ1     - time counter for excess rainfall and runoff
!
!
!     hdrive requires the following parameters to be both read and written:
!     DT      - infiltration time step (s)
!               WEPS ->
!
!     T       - real rainfall excess time (s) = tr(i)-tp+ts. 
!               WEPS -> TF in WEPS.
!
!     S       - rainfall excess rate (m/s). 
!               WEPS -> RE in WEPS
!
!     SI      - integral of rainfall excess. SI(I+1) = SI(I) + S(I) * (T(I+1)-T(I))
!               WEPS -> derived from S and T
!     
!
!     + + + LOCAL VARIABLES + + +
!
      CHARACTER*80 TITLE
      INTEGER NR, NROUTE, I, NQT, NQ, J
      REAL TR(MXTIME), R(MXTIME), RCUM(MXTIME), F(MXTIME)
      REAL FF(MXTIME),  RR(MXTIME), DTC
      REAL DEPSTO, KS, SM, NU, DE, TN, TP(MXPOND)
      REAL ZZ, POR, SAT, CC 
      REAL S(MXTIME), T(MXTIME), Q(MXTIME+1), TQ(MXTIME)
      REAL TQ1(MXTIME), TRF(MXTIME), RF(MXTIME), QTOT(MXTIME)
      REAL RPRINT(MXTIME), ALPHA, M, N, TSTAR, ACV(3), HCV(3), SCV(3)
      REAL DUR, RUNOFF, DURPQ, QTP, DT_HR, DUREXR_HR
      REAL TPEE, SI(MXTIME+1), ISEED, A1, A2
      REAL EFFDRN, EFFINT, EFFDRR, DDEPSTO(MXTIME), RECUM_MM(MXTIME)
      DOUBLE PRECISION U  
!
     
      NQT = 0

	  do i=1, MXTIME
	     F(i) = 0.0
      end do

!     CALL INITIALIZATION SUBROUTINE

!      CALL INIT(TR, TF, R, RCUM, F, FF, RE, RECUM, RR, 
!     1          DT, DTC, NR, NF, KS, SM, NU, DE, TN, TP,
!     1          ZZ, POR, SAT, CC, SC,
!     1          S, T, Q, TQ, SLEN, NQ, NS, TQ1, TRF, RF, QTOT, RPRINT,
!     1          ALPHA, M, N, TSTAR, NROUTE, ACV, HCV, SCV)

 !     CALL IDAT(TR, R, RR, DT, DTC, NR, NF, TRF, RF, DUR)

 !     CALL PAREST(DEPSTO, KS, SM, POR, SAT, CC, SC)

 !     CALL GRNA( NF, DEPSTO, TR, R, RR, KS, SM,                         
 !    &     NS, TF, RCUM, F, FF, RE, RECUM, TP,                          
 !    &     RPRINT, DDEPSTO, RUNOFF, DUREXR, EFFINT, EFFDRR )
 
 !     CALL PRINT(1, 4, NQT, DURPQ, QTP, TPEE, RECUM,
 !    1           TITLE, KS, SM, POR, SAT, CC, SC, TRF, RF, NS,
 !    1           RE, TP, TF, RPRINT, RCUM, F, FF, DDEPSTO,
 !    1           SLEN, M, ALPHA, TQ1, Q, SCV, QTOT, HCV, DURRUN,
 !    1           RUNOFF, PEAKRO, NR, EFFDRN, EFFINT, EFFDRR)

      IF (RECUM(NF).EQ.0.0) THEN
         WRITE (*,1500) F(NS-1),NF, NS
         DO J = 1, NF
            WRITE (*,1400) J, RECUM(J)
         END DO 

!        STOP
      END IF

!     GET RAINFALL EXCESS INTO HDRIVE FORMAT

      DO I = 1, NS
         T(I) = TF(I) / 60.
         S(I) = RE(I) * 3600 * 1000
		 RECUM_MM(I) = RECUM(i) * 1000.
      END DO 

	  DT_HR = DT / 3600.
	  DUREXR_HR = DUREXR / 3600.
!     
!     COMPUTE RUNOFF HYDROGRAPH
!     
!     NF = 0
!

      CALL RDAT(NF, SI, SC, S, T, NS, ALPHA, M, A1, A2,                 &
     &                ACV, HCV, SCV, TSTAR, SLEN, SLOPE)
!
      CALL HDRIVE(NQT, DURPQ, QTP, TPEE,                                &
     &            DT_HR, NS, QTOT, Q, TQ1,                              &
     &            RECUM_MM, T, S, SI, SLEN, ALPHA, M, DUREXR_HR, A1,    &
     &            A2, TSTAR, PEAKRO, DURRUN)
!
!     COMPUTE EFFECTIVE DURATION OF RUNOFF FOR EROSION COMPONENT
!
!      IF(PEAKRO.GT.0.0)THEN
!        EFFDRN = RUNOFF/PEAKRO
!      ELSE
!        EFFDRN = 0.0
!      ENDIF
!
!      CALL PRINT(2, 4, NQT, DURPQ, QTP, TPEE, RECUM,
!     1           TITLE, KS, SM, POR, SAT, CC, SC, TRF, RF, NS,
!     1           RE, TP, TF, RPRINT, RCUM, F, FF, DDEPSTO,
!     1           SLEN, M, ALPHA, TQ1, Q, SCV, QTOT, HCV, DURRUN,
!     1           RUNOFF, PEAKRO, NR, EFFDRN, EFFINT, EFFDRR)
!
!     CHANGE UNITS ON HYDROLOGY OUTPUTS FOR EROSION MODULE TO THE
!     CORRECT UNITS FOR THE EROSION COMPONENT (M, S, ETC.)
!      RUNOFF = RUNOFF/1000.0
!      PEAKRO = PEAKRO/3.6E06
!      EFFDRN = EFFDRN*3600.0
!      EFFINT = EFFINT/3.6E06
!      EFFDRR = EFFDRR*3600.0
!     
     
!
    
!     
      return
 1400 format (' ',i3, ' ',f5.2)

 1500 FORMAT (//' *** NO RAINFALL EXCESS WITH CHOSEN PARAMETERS.'//     &
     &    '     FINAL INFILTRATION RATE = ', F10.3, 'MM/HR'//           &
     &    ' NF = ', i5,' NS= ', i5//                                    &
     &    '     PROGRAM STOP - CHOOSE NEW INFILTRATION PARAMETERS.'/)

      END
