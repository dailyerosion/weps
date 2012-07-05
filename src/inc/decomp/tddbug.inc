!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the variables used for keeping track of
!     time and subregions in the decomp debug output routine.

!     + + + VARIABLE DECLARATIONS + + +

      integer          tisr
      integer          tday
      integer          tmo
      integer          tyr

      common / tddbug /                                                 &
     &                 tisr, tday, tmo, tyr

      save :: /tddbug/

!     + + + VARIABLE DEFINITIONS + + +

!   tday      - The last accessed day of simulation month.
!   tmo       - The last accessed month of simulation year.
!   tyr       - The last accessed year of simulation run.
!   tisr      - The last accessed subregion index.

