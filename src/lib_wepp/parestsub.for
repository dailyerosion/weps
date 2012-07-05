!$Author$
!$Date$
!$Revision$
!$HeadURL$

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
      END

