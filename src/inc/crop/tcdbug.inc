!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     This common block contains the variables used for keeping track of
!     time and subregions in the crop debug output routine.

!     + + + VARIABLE DECLARATIONS + + +

      integer          tisr
      integer          tday
      integer          tmo
      integer          tyr

      common / tcdbug /                                                 &
     &                 tisr, tday, tmo, tyr

      save :: /tcdbug/

!     + + + VARIABLE DEFINITIONS + + +

!   tday      - The last accessed day of simulation month.
!   tmo       - The last accessed month of simulation year.
!   tyr       - The last accessed year of simulation run.
!   tisr      - The last accessed subregion index.

