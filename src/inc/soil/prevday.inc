!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     defines for soil conditions that are retained from the previous day
      real bhtmx0(mnsz)
      real bhrwc0(mnsz)
      real bszrh0
      real bszrr0

      common / prevday /                                                &
     & bhtmx0, bhrwc0, bszrh0, bszrr0

      save :: /prevday/

!   bhtmx0    - layer maximum temperature of yesterday. in C
!   bhrwc0    - soil water content for yesterday. mass basis kg/kg.
!   bszrh0    - prior day ridge height, mm
!   bszrr0    - prior day random roughness, mm
