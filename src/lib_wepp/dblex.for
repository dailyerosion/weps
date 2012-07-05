!$Author$
!$Date$
!$Revision$
!$HeadURL$

      SUBROUTINE DBLEX(NR, DELTFQ, TIMEDL, INTDL, TPD, IP)

!     + + + PURPOSE + + +

!     DOUBLE EXPONENTIAL DISTRIBUTION

!     1. FOR 0 <= TIMEDL(I) <= TP
!        I(TIMEDL(I)) = A * EXP(B*TIMEDL(I))
!        TIMEDL(I+1) = (1.0/B) * LOG(1.0 + B*FQ/A)

!     2. FOR TP <= TIMEDL(I) <= 1.0
!        I(TIMEDL(I)) = IP * EXP(-C*(TIMEDL(I)-TP))
!        TIMEDL(I) = TP - (1.0/C) * LOG(1.0-(C/IP)*(FQ-TP))

!     CALLED FROM DISAG
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-23-2005
!     CODED BY: D. FLANAGAN

!     + + + ARGUMENT DECLARATIONS + + +
      
      INTEGER NR
      REAL DELTFQ, TIMEDL(*), INTDL(*), IP, TPD

!     NR       -
!     DELTFQ   - REAL VALUE FOR INCREMENTAL NORMALIZED RAINFALL DEPTH
!     TIMEDL() - REAL ARRAY FOR DIMENSIONLESS ELAPSED TIME
!     INTDL()  - REAL ARRAY FOR DIMENSIONLESS RAINFALL INTENSITY
!     TPD      - RELATIVE TIME TO PEAK - TIME TO PEAK INTENSITY DIVIDED
!                BY STORM DURATION, DURD
!     IP       - RELATIVE PEAK INTENSITY - MAXIMUM INT/AVERAGE INT

!     + + + FUNCTION DECLARATIONS + + +
      REAL EQROOT

!     + + + LOCAL VARIABLES + + +
      INTEGER ERR, I
      REAL U, B, A
      REAL FQ, D
      real tpd_loc, ip_loc
      
!     + + + LOCAL DEFINITIONS + + +
!     I        - INTEGER VALUE FOR LOCAL LOOP
!     FQ       - REAL VALUE FOR CUMMULATIVE NORMALIZED RAINFALL DEPTH
!     tpd_loc - range restricted local value of TPD
!     ip_loc - range restricted local value of IP

!     + + + END SPECIFICATIONS + + +


!     CHECK TO MAKE SURE IP IS IN RANGE SO MACHINE CAN MAKE THE
!     CALCULATIONS WITHOUT A MACHINE OVERFLOW - MAKE IP LE 60.0
!     IF IP WAS GT 60.0
      ip_loc = min( IP, 60.0)

      ! this is a check for conditions that may cause DBLEX to fail (esp. the ZERO condition)
      tpd_loc = min( max( TPD, 0.00001), 0.99999)

!     NEWTON'S METHOD FOR B AND THEN A IN I(T) = A * EXP(B*T)

      U = EQROOT(1./ip_loc,ERR)
      if( ERR .eq. 1 ) then
          write(*,*) "dblex: EQROOT solution failed, ip_loc = ", ip_loc
      end if
      B = U / tpd_loc
      A = ip_loc * EXP(-U)

!     THE FORMULAS FOR DISAGGREGATION GIVE U = BTP = D(1-tpd_loc)

      D = U / (1.-tpd_loc)
      INTDL(1) = A

      FQ = 0.0
      DO I = 1, NR - 2
         FQ = FLOAT(I) * DELTFQ

         IF (FQ.LE.tpd_loc) THEN
            TIMEDL(I+1) = (1.0/B) * LOG(1.0+(B/A)*FQ)
         ELSE
            TIMEDL(I+1)=tpd_loc-(1.0/D)*LOG(1.0-(D/ip_loc)*(FQ-tpd_loc))
         END IF

      END DO 

      TIMEDL(NR) = 1.0

      DO I = 1, NR - 1
         INTDL(I) = DELTFQ / (TIMEDL(I+1)-TIMEDL(I))
      END DO 

      RETURN
      END
