      
      SUBROUTINE PRINT_BUG(DT, NS,RECUM, T, S, SI, SLEN,ALPHA, M,       &
     &    DUREXR, A1, A2, TSTAR)  
     
      use wepp_interface_defs                                 

 
      implicit none
      INTEGER MXTIME
      PARAMETER (MXTIME = 1500)

!     + + + ARGUMENT DECLARATIONS + + +
!
     
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
      print *,'----------------------------------------------------'
      print *,'NS=',NS,' DT=',DT,' SLEN=',SLEN,' TSTAR=',TSTAR,' M=',M
      print *,'  A1=',A1,' A2=',A2,' ALPHA=',ALPHA,' DUREXR=',DUREXR
 

      print *,'RECUM='
      print '(5f14.6)',(RECUM(i),i=1,NS)
      	
      print *,'T='
      print '(5f14.4)',(T(i),i=1,NS) 
      print *,'S='
      print '(5f14.6)',(S(i),i=1,NS)
      print *,'SI='
      print '(5f14.6)',(SI(i),i=1,NS)
     
      RETURN
      END
