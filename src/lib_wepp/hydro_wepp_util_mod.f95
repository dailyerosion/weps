!$Author$
!$Date$
!$Revision$
!$HeadURL$

module hydro_wepp_util_mod

  contains

    SUBROUTINE disag(NR, TRF, RF, P, DURD, TPD, IP)

!     + + + PURPOSE + + +

!     THIS SUBROUTINE DISSAGREGATES A STORM INTO A DOUBLE EXPONENTIAL 
!     INTENSITY PATTERN WITH RELATIVE TIME TO PEAK INTENSITY, TP, AND 
!     RELATIVE MAXIMUM INTENSITY, IP = MAX INT/AVE INT, 
!     SATISFYING 0 < TP < 1 AND IP >= 1

!     CALLED FROM IDAT
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-23-2005
!     CODED BY: D. FLANAGAN

!     + + + PARAMETER DECLARATIONS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      INTEGER NR
      REAL TRF(*), RF(*), P, DURD, TPD, IP

!     + + + ARGUMENT DEFINITIONS + + +
!     NR       - INTEGER VALUE FOR RAINFALL ARRAY DIMENSION
!     TRF      - REAL VALUE FOR TIME OF RAINFALL VALUE TO BEGIN (TIME)
!     RF       - REAL VALUE FOR RAINFALL DEPTH RATE (DEPTH/TIME)
!     P        - REAL VALUE FOR TOTAL RAINFALL (DEPTH )
!     DURD     - REAL VALUE FOR STORM DURATION (TIME)
!     TPD      - RELATIVE TIME TO PEAK - TIME TO PEAK INTENSITY DIVIDED
!                BY STORM DURATION, DURD
!     IP       - RELATIVE PEAK INTENSITY - MAXIMUM INT/AVERAGE INT

!     NOTE: THIS RETURNS THE SAME UNITS AS USED ON INPUT. SO IF INPUT IS
!           IN MM AND HOURS, TRF IS HOURS, AND RF IS MM/HOUR

!     + + + LOCAL VARIABLES + + +
      INTEGER I
      REAL DELTFQ, TIMEDL(NR), INTDL(NR)

!     + + + LOCAL DEFINITIONS + + +
!     I        - INTEGER VALUE FOR LOCAL LOOP
!     DELTFQ   - REAL VALUE FOR INCREMENTAL NORMALIZED RAINFALL DEPTH
!     INTDL()  - REAL ARRAY FOR DIMENSIONLESS RAINFALL INTENSITY
!     TIMEDL() - REAL ARRAY FOR DIMENSIONLESS ELAPSED TIME

!     + + + END SPECIFICATIONS + + +

!     INITIALIZE VARIABLES
      DELTFQ = 1.0 / FLOAT(NR-1)
      TIMEDL(1) = 0.0
      INTDL(NR) = 0.0

      IF (IP.LE.1.0) THEN
         CALL CONST(NR, DELTFQ, TIMEDL, INTDL)
      ELSE
         CALL DBLEX(NR, DELTFQ, TIMEDL, INTDL, TPD, IP)
      END IF

!     MAKE SURE LAST INTENSITY VALUE IS 0.0
      INTDL(NR) = 0.0

!     CALCULATE ACTUAL TIME AND INTENSITY
      DO I = 1, NR
         TRF(I) = TIMEDL(I) * DURD
         if( DURD .LE. 0.0 ) then
             ! duration is required to get rain, so zero it out
             RF(I) = 0.0
         else
             RF(I) = INTDL(I) * P / DURD
         end if
      END DO 

      RETURN
    END SUBROUTINE disag

    SUBROUTINE CONST(NR, DELTFQ, TIMEDL, INTDL)

!     CALCULATES STEP FUNCTIONS TO REPRESENT NINT 
!     DELTA T AND INTENSITY = 1.0 INTERVALS FOR CONSTANT INTENSITY
!     + + + PURPOSE + + +

!     CALLED FROM DISAG
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-23-2005
!     CODED BY: D. FLANAGAN

!     + + + ARGUMENT DECLARATIONS + + +

      INTEGER NR
      REAL DELTFQ, TIMEDL(*), INTDL(*) 
    
!     + + + ARGUMENT DEFINITIONS + + +
!     DELTFQ   - REAL VALUE FOR INCREMENTAL NORMALIZED RAINFALL DEPTH
!     TIMEDL() - REAL ARRAY FOR DIMENSIONLESS ELAPSED TIME
!     INTDL()  - REAL ARRAY FOR DIMENSIONLESS RAINFALL INTENSITY

!     + + + LOCAL VARIABLES + + +
      INTEGER I
      REAL FQ

!     + + + LOCAL DEFINITIONS + + +
!     I        - INTEGER VALUE FOR LOCAL LOOP
!     FQ       - REAL VALUE FOR CUMMULATIVE NORMALIZED RAINFALL DEPTH

!     + + + END SPECIFICATIONS + + +

      FQ = 0.0
      DO I = 1, NR - 1
         FQ = FQ + DELTFQ
         TIMEDL(I+1) = FQ
         INTDL(I) = 1.0
      END DO
       
      RETURN
    END SUBROUTINE CONST

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
            TIMEDL(I+1) = (1.0/B) * LOG(1.0d0+(B/A)*FQ)
         ELSE
            TIMEDL(I+1)=tpd_loc-(1.0/D)*LOG(1.0d0-(D/ip_loc)*(FQ-tpd_loc))
         END IF

      END DO 

      TIMEDL(NR) = 1.0

      DO I = 1, NR - 1
         INTDL(I) = DELTFQ / (TIMEDL(I+1)-TIMEDL(I))
      END DO 

      RETURN
    END SUBROUTINE DBLEX

    FUNCTION EQROOT(A,ERR)

!     + + + PURPOSE + + +

!     THIS FUNCTION SOLVES THE FOLLOWING EQUATION FOR U:

!       1 - EXP(-U) = A*U, A POSITIVE, U POSITIVE (UNLESS A=1).

!     NEWTON'S METHOD, WITH SPECIAL APRROXIMATIONS FOR SMALL AND 
!     LARGE U, IS USED.  THE RESULTS APPEAR TO BE ACCURATE TO MACHINE
!     PRECISION (REAL*4) AND REQUIRE AT MOST 2 ITERATIONS.

!     LET F(U) = (1 - EXP(-U))/U.

!     FOR A SMALL, F(1/A) = A - A*EXP(-1/A), AND THE LAST TERM IS 
!     SMALL,  SO 1/A IS AN APPROXIMATE SOLUTION.   FOR A <= .06, 
!     THE RELATIVE ERROR IN F AND IN U APPEARS TO BE LESS THAN 
!     REAL*4 PRECISION.

!     LET W = (3 - SQRT(24A - 15))/2.  THEN A = 1 - W/2 + U**2/6.  
!     FROM THE TAYLOR SERIES EXPANSION: 
!          EXP(-U) = 1 - U + U**2/2 - U**3/6 +R(U) 
!     WE GET F(W)=A - R(W)/W, WHERE 0 < R(W) < W**3/24.  FOR A CLOSE 
!     TO 1, W IS NEAR ZERO AND W IS AN APPROXIMATE SOLUTION.  

!     FOR .999 <= A,  THE RELATIVE ERROR IN F AND U APPEARS TO BE 
!     LESS THEN REAL*4 PRECISION.

!     BETWEEN .06 AND .999, NEWTON'S METHOD IS USED,  USING 
!     STARTING VALUES.

!     LET F(U1)=A AND U NEAR U1.    
!      |(A - F(U))/A|  =  |(F(U1) - F(U))/A|  
!         = |F'(C)*U1/A|*|(U1 - U)/U1|,  FOR SOME C BETWEEN U AND U1.
!     SINCE U IS NEAR U1, C AND U1 ARE APPROXIMATED BY U.  THUS FOR 
!     R=|A/(F'(U)*U)|, WE HAVE |(U - U1)/U1| ~ R*|(A - F(U))/A|.  
!     THE RELATIVE ERROR IN F AND U ARE SMALL IF THE RELATIVE ERROR 
!     IN F AND R TIMES THIS ERROR ARE SMALL.
!     EVALUATING F'(U) GIVES R = A/((U - 1)*F(U) - 1).

!     BECAUSE OF THE APPROXIMATIONS IN THE ABOVE, WE USE 1/2 THE
!     SMALLEST REAL*4 NUMBER IN TESTING FOR CONVERGENCE.

!     CALLED FROM DBLEX
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-30-2005
!     CODED BY: D. FLANAGAN

!     + + + ARGUMENT DECLARATIONS + + +

      REAL EQROOT, A
      INTEGER ERR  

!     + + + ARGUMENT DEFINITIONS + + +

!     EQROOT   - R*4  RETURNED POSITIVE SOLUTION TO THE EQUATION.
!     A        - R*4  POSITIVE CONSTANT IN THE EQUATION.
!     ERR      - I*2  0: EQUATION SOLVED.
!                     1: NO SOLUTION FOR GIVEN A.

!     + + + LOCAL VARIABLES + + +

      REAL*8 AA, D, E, F, R, S, U

!     + + + END SPECIFICATIONS + + +

!     CHECK A TO SEE IF THERE IS A SOLUTION.

      AA = DBLE(A)
      IF ((AA.LE.0.D0).OR.(1.D0.LT.AA)) THEN
         ERR = 1
         RETURN
      END IF

!     SPECIAL CASE: A=1 (EXACT LIMITING SOLUTION).

      IF (AA.EQ.1.D0) THEN
         ERR = 0
         EQROOT = 0.
         RETURN
      END IF

!     SPECIAL CASE: A SMALL (ANSWER GOOD TO MACHINE PERCISION).

      IF (AA.LE..06D0) THEN
         ERR = 0
         U = 1.D0 / AA
         EQROOT = SNGL(U)
         RETURN
      END IF

!     SPECIAL CASE: A CLOSE TO 1 (ANSWER GOOD TO ABOUT 10 PLACES).

      IF (.999D0.LE.AA) THEN
         ERR = 0
         U = (3.D0/2.D0) - SQRT(6.D0*AA-(15.D0/4.D0))
         EQROOT = SNGL(U)
         RETURN
      END IF

!     ESTIMATE STARTING VALUE FOR U.

      IF (AA.LE..2D0) THEN
         U = 1.D0 / AA
      ELSE IF (AA.LE..5D0) THEN
         U = .968732D0 / AA - 1.55098D0 * AA + .431653D0
      ELSE IF (AA.LE..94D0) THEN
         U = 1.13243D0 / AA - .928240D0 * AA - .207111
      ELSE
         U = (3.D0/2.D0) - SQRT(6.D0*AA-(15.D0/4.D0))
      END IF

!     ITERATE.

   10 CONTINUE
      E = EXP(-U)
      F = (1.D0-E) / U
      D = AA - F
      R = AA / ((U+1.D0)*F-1.D0)
      IF (R.LE.1.D0) THEN
         S = ABS(D/AA)
      ELSE
         S = ABS(R*D/AA)
      END IF
      IF (S.GE..59E-6) THEN
         U = U * (1.D0+D/(E-F))
         GO TO 10
      END IF

!     EXIT WITH SOLUTION.

      ERR = 0
      EQROOT = SNGL(U)
      RETURN
    END FUNCTION EQROOT

    subroutine frsoil(nsl,sscunf,LNfrst,ssc,sscv,dg,kfactor,slsic,    &
     &     saxfc,saxwp,saxA,saxB,saxpor,saxenp,saxks)
!
!     +++PURPOSE+++
!
!     The purpose of this program is to estimate saturated hydraulic conductivity
!     when frost exists.
!     We treat ice as air for saturated hydralic conductivity calculation.
!     Then the saturated K with ice would be as if the unsaturated K with 
!     liqiud water content = porosity - ice water content 
!     
!     Author(s):  Shuhui Dun, WSU
!     Date: 02/28/2008
!     Verified by: Joan Wu, WSU
!
!     +++PARAMETERS+++
 
!     +++ARGUMENT DECLARATIONS+++
      real, intent(in) ::  sscunf(*),dg(*)
      real, intent(in) :: kfactor
      integer, intent(in) :: nsl,LNfrst
      real, intent(in) :: saxfc(*),saxwp(*),saxA(*),saxB(*),saxpor(*)
      real, intent(in) :: saxenp(*),saxks(*),slsic(*)
      real, intent(out) :: ssc(*), sscv(*)
!
!     +++ARGUMENT DEFINITIONS+++
!    
!     sscunf - unfrozen saturated hydraulic conductivity (SSC)
!
!     +++COMMON BLOCKS+++
!
!
!     +++LOCAL VARIABLES+++
!
      integer  i
      real     slks(100)
      real     varsm,varwtp,varkus,vardp,tmpvr1,tmpvr2
!
!     +++LOCAL DEFINITIONS+++
!
!     slks -  saturated hydraulic conductivity of a fine layer m/s.
!
!     varsm  - soil moisture variable
!     varwtp - water potential variable
!     varkus - unsaturated K varible
!
!     tmpvr1 - variable for mathmatic mean
!     tmpvr2 - variable for harmonic mean
!
!     +++DATA INITIALIZATIONS+++
!
!     +++END SPECIFICATIONS+++
!
      Do 10 i = 1, nsl
!
      if (i.gt.LNfrst) then
!        deeper than the frost bottom
         ssc(i) = sscunf(i)
         sscv(i) = sscunf(i)
      else
!        in frost zone
         tmpvr1 = 0.
         tmpvr2 = 0.
         vardp = dg(i)

!        Estimate unsaturated hydraulic conductivity of a soil
!        using Saxton and Rawls, 2006
!
         if( slsic(i) .gt. 0.001) then
!            frost exists
!
!                as if soil water content at     
             varsm = saxpor(i) - slsic(i)/vardp
!
!                kfactor = 1E-5
!                 kfactor = 0.5
!
             if (varsm .le. 0.01) then
!                forst heave
                  slks(i) = kfactor*sscunf(i)
             else
                  call saxfun(i,varsm,varwtp, varkus,                   &
     &                 saxfc,saxwp,saxA,saxB,saxpor,saxenp,saxks)
                  if ((varkus/sscunf(i)).lt.kfactor) then
                     slks(i) = kfactor*sscunf(i)
                  else
                     slks(i) = varkus
                  endif
             endif
!
          else
!             no frost
             slks(i) = sscunf(i)
          endif
!              
          tmpvr1 = tmpvr1 + vardp*slks(i)
          tmpvr2 = tmpvr2 + vardp/slks(i)

!
         ssc(i) = tmpvr1/dg(i)
         sscv(i) = dg(i)/tmpvr2
!
      endif
10    continue
!
      return
    end subroutine frsoil

    real function effksat(uselan, clay, sand, cec, orgmat, rooty,     &
     &              rilcov, bascov, rescov, rrough, fbasr, fbasi, fresi)

! clay(i,iplane)
! sand(i,iplane)
! orgmat(1,iplane)
! cec(1,iplane)
! rooty(1,iplane)
! rilcov(iplane)
! fbasr(iplane)
! bascov(iplane)
! fresi(iplane)
! rescov(iplane))
! rrough(iplane)
! fbasi(iplane)

!     +++PURPOSE+++

!     The purpose of this program is to estimate the effective saturated
!     hydraulic conductivity in the surface layers. This is based on
!     routines copied from WEPP SCON.FOR

!     +++PARAMETERS+++
 
!     +++ARGUMENT DECLARATIONS+++
      integer, intent(in) :: uselan
      real, intent(in) ::  clay, sand, cec, orgmat, rooty
      real, intent(in) ::  rilcov, bascov, rescov, rrough
      real, intent(in) ::  fbasr, fbasi, fresi

!     +++ARGUMENT DEFINITIONS+++
    
!     clay - clay content of soil (fraction)
!     sand - sand content of soil (fraction)
!     orgmat - orgmat content of soil (fraction)
!     cec - cation exchange capacity of soil milliequivalent of hydrogen per 100 g (meq+/100g)
!                                    or numerically equal, the SI unit centi-mol per kg (cmol+/kg)
!     rooty - total root mass in a soil layer on day of simulation kg/m^2
!     rilcov - rill cover (0-1, unitless)
!     bascov - fraction of ground surface covered with basal vegetation (0-1)
!     rescov - residue cover (0-1)
!     rrough - 
!     fbasr - fraction of total basal cover located in rills (0-1)
!     fbasi - fraction of total basal cover located in interrills (0-1) 
!     fresi - fraction of total litter cover located in interrills (0-1)

!     +++COMMON BLOCKS+++

!     +++LOCAL VARIABLES+++

!     +++LOCAL DEFINITIONS+++

!     +++DATA INITIALIZATIONS+++

!     +++END SPECIFICATIONS+++

!           ELSE if in the top 2 soil layers and the input value of
!           conductivity has been set to zero, estimate a value.
!           (2/7/94 dcf from nearing)


      if(uselan.ne.2)then

          ! CONDUCTIVITY ESTIMATION FOR ALL LAND USES EXCEPT RANGE
          if (clay .le. 0.4) then
              if (cec .gt. 1.0) then
                  effksat = -0.265 + 0.0086 * (sand*100.0)**1.80        &
     &                    + 11.46 * (cec**(-0.75))
              else
                  effksat = 11.195 + 0.0086 * (sand*100.0)**1.80
              end if
          else
              effksat = 0.0066 * exp(244.0 / (clay*100.0))
          end if
      else

          ! RANGELAND CONDUCTIVITY ESTIMATION
          ! NEW KIDWELL EQUATION AS OF June 7, 1995   dcf
          if(rilcov .lt. 0.45)then
                effksat = 57.99                                         &
     &             - (14.05 * alog(cec))                                &
     &             + (6.20 * alog(rooty))                               &
     &             - (473.39 * (fbasr*bascov)**2)                       &
     &             + (4.78 * fresi*rescov)
          else
                effksat = -14.29                                        &
     &             - (3.40 * alog(rooty))                               &
     &             + (37.83 * sand)                                     &
     &             + (208.86 * orgmat)                                  &
     &             + (398.64 * rrough)                                  &
     &             - (27.39 * fresi*rescov)                             &
     &             + (64.14 * fbasi*bascov)
          endif

      endif

      ! Limit EFFECTIVE baseline conductivity value to 0.2 mm/hr minimum.
      if (effksat .lt. 0.2) effksat = 0.2

      ! Convert from mm/hr to meters/second
      effksat = effksat / 3.6e6


      return
    end function effksat

    SUBROUTINE parestsub(SAND, CLAY, SAT, CC, SC, KS, SM)

!     + + + PURPOSE + + +

!     THIS SUBROUTINE GETS KS AND SUCTION EITHER FROM USER OR 
!     ESTIMATES THE VALUES FROM SOIL TEXTURE AND CANOPY AND 
!     GROUND COVER.
!
!     CALLED FROM MAIN
!     AUTHOR(S): D. FLANAGAN, J. ASCOUGH
!     VERSION: THIS MODULE TAKEN FROM ASCOUGH STANDALONE IRS CODE
!     DATE CODED:  3-22-2005
!     CODED BY: D. FLANAGAN

!     + + + ARGUMENT DECLARATIONS + + +

      REAL, intent(in) :: SAND, CLAY, SAT, CC, SC
      REAL, intent(inout) :: KS, SM

!     + + + ARGUMENT DEFINITIONS + + +
!     SAND    - soil sand content (fraction)
!     CLAY    - soil clay content (fraction)
!     SAT     - degree of soil saturation  (fraction)
!     CC      - canopy cover (fraction)
!     SC      - ground cover (fraction)
!     KS      - effective saturated hydraulic conductivity (m/s)
!     SM      - effective soil matic potential (m)

!     + + + PARAMETERS + + +
      real mmtom, hrtosec
      parameter (mmtom = 0.001, hrtosec = 3600.0)

!     + + + PARAMETER DEFINITIONS + + +
!     mmtom  - Unit conversion constant (m/mm)
!     hrtosec - Unit conversion constant (seconds/hour)

!     + + + LOCAL VARIABLES + + +
      INTEGER IC
      REAL SF, POR

!     + + + LOCAL DEFINITIONS + + +
!     IC      - textural class code
!     SF      - soil suction factor (defined by table)
!     POR     - effective soil porosity (frac)

!     + + + END SPECIFICATIONS + + +

      REAL K1(12), K2(12), K3(12)
      DATA K1 /118., 30., 11., 6.5, 3.4, 2.5, 1.5, 1., .9, .6, .5, .4/
      DATA K2 /49., 63., 90., 110., 173., 190., 214., 210., 253., 260., &
     &        288., 310./
      DATA K3 /.4, .4, .41, .43, .49, .42, .35, .31, .43, .32, .42, .39/

!       READ THE SOIL TEXTURE CLASS CODE (1-12)
!
!                       KS      SF    EFF.
!     TEXTURE        (MM/HR)   (MM)   POR.  CODE
!     __________________________________________
!     SAND             118      49    0.40   1    
!     LOAMY SAND       30       63    0.40   2    
!     SANDY LOAM       11       90    0.41   3    
!     LOAM             6.5      110   0.43   4    
!     SILT LOAM        3.4      173   0.49   5    
!     SILT             2.5      190   0.42   6    
!     S. CLAY LOAM     1.5      214   0.35   7    
!     CLAY LOAM        1.0      210   0.31   8    
!     SL. CLAY LOAM    0.9      253   0.43   9    
!     SANDY CLAY       0.6      260   0.32   10    
!     SILTY CLAY       0.5      288   0.42   11    
!     CLAY             0.4      310   0.39   12    
!     __________________________________________

      ! find texture class from sand and clay fractions
      CALL usdatx( SAND, CLAY, IC)
        
      KS = K1(IC) * mmtom / hrtosec
      SF = K2(IC) * mmtom
      POR = K3(IC)
     
      SM = (1.0-SAT) * POR * SF
     
      KS = KS * EXP(8.9999999E-03*SC) * EXP(.0105*CC)
      
      RETURN
    END SUBROUTINE parestsub


!***********************************************************************
! The master percolation component which routes the
!     percolated water down through the soil layers, and
!     and updates the water content of each soil layer.
!     For each layer above field capacity, the excess (VV)
!     is subjected to percolation.  The amount percolated
!     is the seepage (SEP).  The water content (ST) of
!     both layers is updated by the amount of seepage (SEP).
!     Immediately before PURK returns, any infiltration
!     from today's precip (FIN) is added to the top layer.
!***********************************************************************

    subroutine purk(nsl, st, fc, ul, hk, ssc, sep)

      implicit none

!***********************************************************************
!
!     nsl - number of soil layers
!     st - current available water content per soil layer (m)
!     fc - soil field capacity (m)
!     sep -  seepage (m)
!     ul - upper limit of water content per soil layer (m)
!     hk - a parameter that causes SC approach zero as soil water approaches FC
!     ssc - saturated hydraulic conductivity (m/s)
!
!***********************************************************************

      integer, intent(in) :: nsl
      real, intent(in)  :: fc(*),ul(*), hk(*), ssc(*)

      real, intent(inout) :: st(*)
      real, intent(out) :: sep

!     + + + LOCAL VARIABLES + + +
      real vv, sepsav
      integer k1, k2

!     + + + LOCAL DEFINITIONS + + +
!     vv     - Water excess beyond field capacity for the current
!              soil layer.
!     k1     - counter in main loop.
!     k2     - k1 + 1
!     sepsav - used to save SEP from bottom layer.  (Seepage from
!              the bottom layer is needed for water balance calcs.)

!      For each layer, starting with the BOTTOM layer,
!      percolate the water excess to the layer below.
!      (This approach is taken to avoid the compounding
!      effect caused by dumping percolated water from
!      the layer above, on top of existing water that
!      has not yet been subjected to percolation.)

      sepsav = 0.0
      do k1 = nsl, 1, -1
!       ------- compute water excess
        vv = st(k1) - fc(k1)
!       ------- when there is an excess....
        if (vv.gt.0.) then
          k2 = k1 + 1
!         --------- compute percolation through the layer.
          call perc(vv, k1, nsl, st, ul, hk(k1), ssc(k1), sep)
!         --------- reduce water content of current layer
          st(k1) = st(k1) - sep
          if (st(k1).lt.1e-10) st(k1) = 0.0

          if (k1.lt.nsl) then
!           ----------- add seepage to layer below
            st(k2) = st(k2) + sep
          else
!           ----------- "remember" seepage from bottom layer
            sepsav = sep
          end if
        end if

      end do
      sep = sepsav

      return
    end subroutine purk

!**********************************************************************
!  Returns percolation; ie, seepage (SEP) from the bottom of the
!     current soil layer (into the layer below) when field capacity
!     in the current layer is exceeded.  Correction is made for
!     saturation of the layer below.
!**********************************************************************

    subroutine perc(vv, k1, nsl, st, ul, hk, ssc, sep)

      implicit none

      integer, intent(in) :: k1, nsl
      real, intent(in) :: vv, st(*), ul(*), hk, ssc

      real, intent(out) :: sep

!**********************************************************************
!   vv -  Water excess beyond field capacity for the current soil layer (m)
!   k1 - counter in main loop.
!   nsl - number of soil layers
!   st - current available water content per soil layer (m)
!   ul - upper limit of water content per soil layer (m)
!   hk -  a parameter that causes SC approach zero as soil water approaches FC
!   ssc - saturated hydraulic conductivity (m/s)
!   sep -  seepage (m)
!**********************************************************************

!     + + + LOCAL VARIABLES + + +
      real stz, fx, stu, cr, zz, funzz

!     + + + LOCAL DEFINITIONS + + +
!     stz    - percent saturation (expressed as a fraction)
!     fx     - correction factor for sat. hyd. cond. for unsat. soil
!              (equation 7.4.3)
!     stu    - percent saturation (fraction) of lower layer
!     cr     - correction factor for lower layer saturation
!              (equation 7.4.5)
!     zz     - travel time of water through the layer (days)
!              (a part of equation 7.4.19 -delta t/ti)
!     funzz  - a part of equation 7.4.1, 1-exp(-delta t/ti) but
!              linear form
!     sscz   - (not needed)


!      Compute percent saturation (fraction) in the current layer.
      stz = st(k1) / ul(k1)
      if (stz.lt.0.95) then
        fx = stz ** hk
        if (fx.lt.0.002) fx = 0.002
      else
        fx = 1.
      end if

!     Adjust the percolation rate for the saturation of the soil
!     layer below the current one.  (Chapter 7, equation 7.4.3)

!     Compute percent saturation (fraction) in the layer below.
      if (k1.lt.nsl) then
        stu = st(k1+1) / ul(k1+1)
        if (stu.ge.0.95) stu = 0.95
      else
        stu = 0.
      end if

      if (stu.lt.1.0) then
!       Correct for lower level saturation.  (Chapter 7, eq. 7.4.5)
        cr = sqrt(1.-stu)
!       Travel time of water (days) through the layer
        zz = 86400. * fx * ssc / vv

        if (zz.le.10.) then
!         Note: For positive values of ZZ, FUNZZ starts at 1.0, and
!         approaches a lower limit of zero at positive infinity.
!         (Chapter 7, equation 7.4.1)
          funzz = exp(-zz)
          sep = vv * (1.0-funzz) * cr
        else
!         If time > 10 days, FUNZZ approaches zero.
          sep = vv * cr
        end if
      else
        ! If lower level is saturated, there is no seepage
        ! from the current level....
        sep = 0.0
      end if

      return
    end subroutine perc

    real function rainenergy( ninten, timem, intensity)

!     + + + PURPOSE + + +
!     Implements water drop kinetic energy (rain) from WEPP idat.for
!     returns kinetic energy of rainfall (J/m^2)

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: ninten
      real, intent(in) :: timem(*), intensity(*)

!     + + + ARGUMENT DEFINITIONS + + +
!     nri     - number of rainfall breakpoint intervals
!     timem   - breakpoint time markers (sec)
!     intensity - breakpoint intensity (m/sec)

!     + + + PARAMETERS + + +
      real hrtosec
      parameter (hrtosec = 3600.0)

!     + + + PARAMETER DEFINITIONS + + +
!     convert hout time value to seconds

!     + + + LOCAL VARIABLES + + +
      integer idx
      real vtime, vint, vike
      real rkine

!     + + + LOCAL DEFINITIONS + + +
!     idx   - index for looping through array
!     vtime - interval time step (hr)
!     vint  - interval intensity (m/hr)
!     vike  - interval kinetic energy
!     rkine - kinetic energy of rainfall (J/m^2)

!     + + + DETAILED DESCRIPTION + + +

!     Calculate rainfall kinetic energy using equation from Van Doren
!     and Allmaras where KE is approximated by:
!
!     KE=(3.812+0.874 log10 RI)*time*RI
!
!     where KE is in J/cm2, Time is the duration(hr), and RI is the rainfall
!     intensity (m/hr).  Note: This equation is also given by Wischmeier for
!     english units.  To gain accuracy we apply to each time step.  I have
!     also developed an analytical solution to calculate KE for the WEPP
!     double exponential storm, however, I thought it may be more reasonable
!     to calculate KE based on the disaggregated storm.  Risse 11/4/93.

!     + + + END SPECIFICATIONS + + +

      rkine = 0.0
      do idx = 1, ninten - 1
        ! find time step in hours
        vtime = (timem(idx+1) - timem(idx)) / hrtosec
        ! convert intensity to meters per hour
        vint = intensity(idx) * hrtosec

        ! If intensity is greater than 3 in/hr energy does not increase as
        ! maximum drop size has been attained.
        if( vint .gt. 0.0765 ) vint = 0.0765
        if( (vtime .gt. 0.0) .and. (vint .gt. 0.0) ) then
          vike = (3.812 + 0.3796*log(vint)) * vtime * vint
        else
          vike = 0
        end if
        ! convert KE to J/m2
        vike = vike * 10000.0
        if( vike .gt. 0.0 ) rkine = vike + rkine
      end do

      rainenergy = rkine

      return
    end function rainenergy

    subroutine saxfun(lysoil,varsm,varwtp,varkus,                     &
     &  saxfc,saxwp,saxA,saxB,saxpor,saxenp,saxks)

!     Estimate soil water potential and unsaturated hydraulic conductivity usingSaxton&Rawl equation

!     Called from: 
!     Author(s): Shuhui Dun, WSU
!     Reference in User Guide: Saxton K.E. and Rawls W.J., 2006. 
!     Soil water characteristics estimates by texture and organic matter for hydraologic solution.
!     Soil SCI. SOC. AM. J., 70, 1569--1578

!     Version: 2008.
!     Date recoded: Febuary 26, 2008
!     Verified by: Joan Wu, WSU

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: lysoil
      real, intent(out) :: varwtp,varkus
      real, intent(in) :: varsm
      real, intent(in) :: saxfc(*),saxwp(*),saxA(*),saxB(*),saxpor(*)
      real, intent(in) :: saxenp(*),saxks(*)
      
!     + + + ARGUMENT DEFINITIONS + + +
!      varsm - soil water content
!      varwtp - soil water potital in meter 
!      varkus - unsaturated hydraulic conductivity (m/s)
!      lysoil - soil layer number

!     + + + LOCAL VARIABLES + + +
      real wtpkpa
!
!     + + + LOCAL DEFINITIONS + + +

!     sw1500: first solution 1500 kpa soil moisture
!     sw33: first solution 1500 kpa soil moisture
!     sws33:first solution SAT-33 kpa soil moisture
!     s33:  moisture SAT-33 kpa, normal density
!     spaen: first solution air entry tension, kpa

!    Estimate the water potential of the soil layer below the frozen front
!    using Saxton and Rawls, 2006

      if (varsm.lt.saxfc(lysoil)) then
!     water potential between 1500kpa and 33kpa
          if (varsm.lt.saxwp(lysoil)) then
              wtpkpa = 1500.
          else
          wtpkpa = saxA(lysoil) * varsm**(-saxB(lysoil))
          endif

          if (wtpkpa .gt. 1500) wtpkpa = 1500.

      elseif (varsm.ge.saxpor(lysoil)) then
!     Saturation
          wtpkpa = 0
      else
!     water potential between 33kpa and 0 kpa
          wtpkpa = 33.0 - (33.0 - saxenp(lysoil))*                      &
     &           (varsm - saxfc(lysoil))/                               &
     &           (saxpor(lysoil) - saxfc(lysoil))
          if (wtpkpa .lt. saxenp(lysoil)) wtpkpa = 0
      endif

!     Convert Kpa to meter of water
      varwtp = -wtpkpa / 10.

      if(varsm.lt.saxpor(lysoil)) then
          varkus = saxks(lysoil) * (varsm/saxpor(lysoil))               &
     &             **(3. + 2.0*saxB(lysoil))
      else
          varkus = saxks(lysoil)
      endif

      return
    end subroutine saxfun

    subroutine saxpar(sand, clay, orgmat, nsl, &
                      saxwp, saxfc, saxenp, saxpor, saxA, saxB, saxks)
      ! Estimate Saxton&Rawl equation parameters for a soil

      ! Called from: SR WINIT
      ! Author(s): Shuhui Dun, WSU
      ! Reference in User Guide: Saxton K.E. and Rawls W.J., 2006. 
      ! Soil water characteristics estimates by texture and organic matter for hydraologic solution.
      ! Soil SCI. SOC. AM. J., 70, 1569--1578

      ! After viewing the web page by Saxton http://hrsl.ba.ars.usda.gov/soilwater/Index.htm
      ! the minimum clay content modeled is around 5%, making the maximum sand content around 95%
      ! It also shows the maximum clay content to be around 60%. Clamping the input values to
      ! remain within these ranges, and adjusting the other components accordingly would prevent
      ! out of range errors such as wilting point becoming less than zero, but does not answer the
      ! question of what the values really should be for these extremes. For now, the wilting point
      ! value is prevented from going to zero.

      ! Version: 2008.
      ! Date recoded: Febuary 19, 2008
      ! Verified by: Joan Wu, WSU 

      real, intent(in) :: sand(*),clay(*),orgmat(*)
      integer, intent(in) :: nsl
      real, intent(out) :: saxwp(*)  ! 1500 kpa soil water content (wilting point)
      real, intent(out) :: saxfc(*)  ! 33 kpa soil water content (field capacity)
      real, intent(out) :: saxenp(*) ! air entry pressure (kpa)
      real, intent(out) :: saxpor(*) ! saturated water content
      real, intent(out) :: saxA(*)   ! moisture tension equation coefficient A
      real, intent(out) :: saxB(*)   ! moisture tension equation coefficient B
      real, intent(out) :: saxks(*)  ! saturated hydraulic conductivity (m/s)
      
      ! Saxton K.E. and Rawls W.J., 2006. Soil water characteristics estimates 
      ! by texture and organic matter for hydraologic solution.
      ! Soil SCI. SOC. AM. J., 70, 1569--1578

      integer i
      double precision :: sw1500 ! first solution 1500 kpa soil moisture
      double precision :: sw33   ! first solution 33 kpa soil moisture
      double precision :: sws33  ! first solution SAT-33 kpa soil moisture
      double precision :: s33    ! moisture SAT-33 kpa, normal density
      double precision :: spaen  ! first solution air entry tension, kpa

      do i = 1, nsl
         ! eqation 1 
         sw1500 = - 0.024d0*sand(i) &
                + 0.487d0*clay(i) &
                + 0.006d0*orgmat(i) &
                + 0.005d0*sand(i)*orgmat(i) &
                - 0.013d0*clay(i)*orgmat(i) &
                + 0.068d0*sand(i)*clay(i) &
                + 0.031d0

          saxwp(i) = max( 1.0e-5, (sw1500 + 0.14d0*sw1500 - 0.02d0) )

         ! equation 2
         sw33 = - 0.251d0*sand(i) &
              + 0.195d0*clay(i) &
              + 0.011d0*orgmat(i) &
              + 0.006d0*sand(i)*orgmat(i) &
              - 0.027d0*clay(i)*orgmat(i) &
              + 0.452d0*sand(i)*clay(i) &
              + 0.299d0

          saxfc(i) = sw33 + 1.283d0*sw33**2 - 0.374d0*sw33 - 0.015d0

         ! equation 3
         sws33 = 0.278d0*sand(i) &
               + 0.034d0*clay(i) &
               + 0.022d0*orgmat(i) &
               - 0.018d0*sand(i)*orgmat(i) &
               - 0.027d0*clay(i)*orgmat(i) &
               - 0.584d0*sand(i)*clay(i) &
               + 0.078d0

          s33 = sws33 + 0.636d0*sws33 - 0.107d0

          ! eqation 4
          spaen = - 21.67d0*sand(i) &
                - 27.93d0*clay(i) &
                - 81.97d0*s33 &
                + 71.12d0*sand(i)*s33 &
                +  8.29d0*clay(i)*s33 &
                + 14.05d0*sand(i)*clay(i) &
                + 27.16d0

          saxenp(i) = spaen + 0.02d0*spaen**2 - 0.113d0*spaen - 0.70d0

          ! equation 5
          saxpor(i) = saxfc(i) + s33 - 0.097d0*sand(i) + 0.043d0

          ! eqation 14
          saxB(i) = (log(1500.0d0) - log(33.0d0))/ &
                    (log(dble(saxfc(i))) - log(dble(saxwp(i))))
          ! eqation 15
          saxA(i) = exp (log(33.0d0) + saxB(i)*log(dble(saxfc(i))))

          ! equation 16
          ! The unit of the original saxton ans Rawls is mm/hr.
          ! The factor 1./3.6e+6 converts mm/hr to m/s
          saxks(i) = 1930.0d0*(saxpor(i) &
                   - saxfc(i))**(3.0d0 - 1.0d0/saxB(i))*1.0d0/3.6E+6
      end do

    end subroutine saxpar

    subroutine usdatx( sand, clay, class)
      integer class
      real sand, clay

! Determines the usda textural class from the sand and clay fractions.
! Original code included for reference below, was modified to use
! fractions instead of percent and modified to return a class number,
! also defined below, instead of returning the string shown in the comment
! after the line where class number is set.

      if (clay .gt. 0.40) then
         class = 12 !'c   '
         if (sand .gt. 0.45) class = 10 !'sc  '
         if ((sand+clay) .lt. 0.60) class = 11 !'sic '
      else
         if (clay .gt. 0.27) then
            class = 9 !'sicl'
            if (sand .gt. 0.20) class = 8 !'cl  '
            if (sand .gt. 0.45) then
               class = 7 !'scl '
               if (clay .gt. 0.35) class = 10 !'sc  '
            end if
         else
            if ((sand+clay) .lt. 0.50) class = 5 !'sil '
            if ((sand+clay) .lt. 0.20 .and.clay .lt. 0.12) class = 6 !'si  '
            if ((sand+clay) .ge. 0.50) class = 3 !'sl  '
            if (((sand+clay) .ge. 0.50) .and. ((sand+clay) .lt. 0.72)   &
     &         .and. (clay .gt. 0.7) .and. (sand .lt. 0.52)) class = 4 !'l   '
            if (((sand+clay).ge. 0.72).and.(clay .gt. 0.20)) class = 7 !'scl '
            if ((sand-clay) .gt. 0.70) class = 2 !'ls  '
            if ((sand-0.5*clay) .gt. 0.85) class = 1 !'s   '
         end if
      end if
      return
    end subroutine usdatx

!      PARTSIZE 4 DETERMINES THE USDA TEXTURAL CLASS FROM THE SAND AND
!      CLAY FRACTIONS

!	 Written by J. E. Hook, Univ. of Georgia, March, 1981.
!        Coastal Plain Exp Stn P.O. Box 748  Tifton, GA 31793-0748
!        Internet: jimhook@tifton.cpes.peachnet.edu
!        Voice: (912) 386-3182 Fax: (912) 386-7293     

!      SUBROUTINE USDATX(SAND,CLAY,CLASS)
!      CHARACTER*4 CLASS

!      IF (CLAY.GT.40) THEN
!         CLASS='C   '
!         IF (SAND.GT.45) CLASS='SC  '
!         IF ((SAND+CLAY).LT.60) CLASS='SIC '
!      ELSE
!         IF (CLAY.GT.27) THEN
!            CLASS='SICL'
!            IF (SAND.GT.20) CLASS='CL  '
!            IF (SAND.GT.45) THEN
!               CLASS='SCL '
!               IF (CLAY.GT.35) CLASS='SC  '
!            END IF
!         ELSE
!            IF ((SAND+CLAY).LT.50) CLASS='SIL '
!            IF ((SAND+CLAY).LT.20 .AND.CLAY.LT.12)CLASS='SI  '
!            IF ((SAND+CLAY).GE.50) CLASS='SL  '
!            IF (((SAND+CLAY).GE.50) .AND. ((SAND+CLAY).LT.72)
!     +          .AND. (CLAY.GT.7) .AND. (SAND.LT.52)) CLASS='L   '
!            IF (((SAND+CLAY).GE.72) .AND. (CLAY.GT.20)) CLASS='SCL '
!            IF ((SAND-CLAY).GT.70) CLASS='LS  '
!            IF ((SAND-0.5*CLAY).GT.85) CLASS='S   '
!         END IF
!      END IF
!      RETURN
!      END

!     TEXTURE       CODE
!     __________________
!     SAND           1    
!     LOAMY SAND     2    
!     SANDY LOAM     3    
!     LOAM           4    
!     SILT LOAM      5    
!     SILT           6    
!     S. CLAY LOAM   7    
!     CLAY LOAM      8    
!     SL. CLAY LOAM  9    
!     SANDY CLAY     10    
!     SILTY CLAY     11    
!     CLAY           12    

end module hydro_wepp_util_mod
