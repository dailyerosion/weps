!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'm1subr.inc'

!     This common block contains the state variables relating to
!     the subregion parameters for WERM.

!     Parameter include files:c:\main\inc\p1werm.inc


!     + + + VARIABLE DECLARATIONS + + +

      integer          nsubr
      integer          am0csr
      integer          amnryr(mnsub)
      integer          amnrotcycle(mnsub)
      real             amrslp(mnsub)
      real             SFCov(mnsub)
      real             WaterErosion(mnsub)
      real             SoilRockFragments(mnsub)
      character*160    am0sid(mnsub)
      character*80     am0tax(mnsub)
      character*80     am0localphase(mnsub)
      character*20     SoilLossTol(mnsub)

      common   / m1subr /                                               &
     &                   nsubr,                                         &
     &                   am0csr,                                        &
     &                   amnryr,                                        &
     &                   amnrotcycle,                                   &
     &                   amrslp,                                        &
     &                   SFCov,                                         &
     &                   WaterErosion,                                  & 
     &                   SoilRockFragments,                             & 
     &                   am0sid,                                        &
     &                   am0tax,                                        &
     &                   am0localphase,                                 &
     &                   SoilLossTol

      save :: /m1subr/

!     + + + VARIABLE DEFINITIONS + + +

!     am0csr - Current subregion

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            1            4
!              typical          1            1
!              ------------   ---------  ---------

!            - This variable contains the simulation current
!              subregion.

!     amnryr - Counter for current rotation year

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            1            10
!              typical          1            1
!              ------------   ---------  ---------

!            - This variable keeps track of the current rotation
!              year for each subregion.

!     amnrotcycle - Counter for current "rotation" cycle

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            1            100+
!              typical          1            1
!              ------------   ---------  ---------

!            - This variable keeps track of the current rotation
!              cycle for each subregion.

!     amrslp - Average subregion slope (m/m)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the average value for the slope
!              gradient for each subregion.

!     nsubr  - Number of subregions

!              range values      min        max
!              ------------   ---------  ---------
!              fixed              1          4
!              typical            1          4
!              ------------   ---------  ---------
!
!            - This variable contains the number of subregions for
!              a simulation run.

!    am0sid  - soil identification

!              range values      min        max
!              ------------   ---------  ---------
!              fixed
!              typical
!              ------------   ---------  ---------
!
!            - This character variable contains the soil identification
!              for each subregion.

!    am0tax  - soil taxonomic order

!              range values      min        max
!              ------------   ---------  ---------
!              fixed
!              typical
!              ------------   ---------  ---------
!
!            - This character variable contains the soil taxonomy
!              classification at the order level for each subregion.

!    am0localphase  - soil local phase

!              range values      min        max
!              ------------   ---------  ---------
!              fixed
!              typical
!              ------------   ---------  ---------
!
!            - This character variable contains the soil taxonomy
!              "local phase" soil classification for each subregion.

!    SoilLossTol  - NRCS assigned "soil loss tolerance" value

!              range values      min        max
!              ------------   ---------  ---------
!              fixed
!              typical
!              ------------   ---------  ---------

!    SFCov - NRCS "Surface Fragment Cover" or "Surface Layer Fragment" fraction (%)

!              range values      min        max
!              ------------   ---------  ---------
!              fixed
!              typical
!              ------------   ---------  ---------
!     ------------------------------------------------------------------

