!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 's2surf.inc'

!     This block common is contains variables used for a simulation run
!     for EROSION.

!     + + + VARIABLE DECLARATIONS + + +

      real             szcr (0:mngdpt, 0:mngdpt)
      real             sfcr (0:mngdpt, 0:mngdpt)
      real             smlos(0:mngdpt, 0:mngdpt)
      real             sflos(0:mngdpt, 0:mngdpt)
      real             smaglos (0:mngdpt, 0:mngdpt)
      real             dmlos (0:mngdpt, 0:mngdpt)
      real             smaglosmx (0:mngdpt, 0:mngdpt)


      common /s2urf/                                                    &
     &                 szcr,  sfcr,                                     &
     &                 smlos, sflos,                                    &
     &                 smaglos, dmlos, smaglosmx

      save :: /s2urf/

!     + + + VARIABLE DEFINITIONS + + +

!     szcr   - Consolidated (crust) thickness (mm)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        50
!              typical            0        20
!              ------------   ---------  ---------

!     sfcr   - soil fraction with crust cover (decimal)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0.0       1.0
!              typical            0.1       1.0
!              ------------   ---------  ---------

!     smlos  - mass of loose erodible soil on crust (kg/m^2)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              0          10
!              typical            0.2         2
!              ------------   ---------  ---------

!     sflos  - soil fraction covered with loose erodible soil
!              on the crusted area

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               0         1.0
!              typical             0         sfcr
!              ------------   ---------  ---------

!     smaglos- mobile soil mass removable from aggregated surface
!                by u* (kg/m^2).

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               0         60
!              typical             0          5
!              ------------   ---------  ---------

!     dmlos- mobile soil mass change from erosion of aggregated
!               surface (kg/m^2) (+)= gain, (-)= loss from surface.

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               0         60
!              typical             0          5
!              ------------   ---------  ---------
