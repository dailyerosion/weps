!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------
!     file 'plot.inc'

!     This block common is contains local variables used to store
!     plot variables for a WEPS simulation run.

!     + + + VARIABLE DECLARATIONS + + +

      real             total
      real             suspen
      real             pmten
      character*80     operat
      character*80     crname

      common / plot /                                                   &

     &                 total, suspen, pmten, operat, crname

      save :: /plot/

!     + + + VARIABLE DEFINITIONS + + +

!    total   - total average soil loss from the accounting
!              region (kg / m^2)

!    suspen   - total average suspension soil loss from the
!              accounting region (kg / m^2)

!    pmten   - total average PM10 soil loss from the
!              accounting region (kg / m^2)

!    operat  - operation name

!    ac0nam(mnsub)  - crop name
!    crname  - crop name

