!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     -----------------------------------------------------------------
!     file 'm1sim.inc'

!     This block common is contains variables used for a simulation run
!     for MAIN.

!     + + + VARIABLE DECLARATIONS + + +

      integer          ntstep
      integer          am0jd
      integer          erod_interval
      real             amalat
      real             amalon
      real             amzele

      common /m1sim/                                                    &
     &                 am0jd, ntstep, erod_interval,                    &
     &                 amalat, amalon, amzele

      save :: /m1sim/

!     + + + VARIABLE DEFINITIONS + + +

!     ntstep  - Number of timesteps per day for erosion.

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              1        96
!              typical           24        24
!              ------------   ---------  ---------

!            - This variable contains the number of timesteps
!              per day for which erosion is calculated.

!     erod_interval - surface updating interval within erosion.

!              range values      min       max
!              ------------   ---------  ---------
!              fixed             60        --
!              typical           60        3600
!              ------------   ---------  ---------

!            - This variable contains the number of seconds
!              the surface is updated within the erosion submodel.
!              (currently settable as a commandline option within
!              the standalone version of the erosion submodel)

!     am0jd  - Current julian day of simulation

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              1      variable
!              typical         variable  variable
!              ------------   ---------  ---------

!            - This variable contains the current julian day
!              the simulation run.

!     amalat - Latitude of simulation site (degrees)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.00      90.00
!              typical         15.00      75.00
!              ------------   ---------  ---------

!            - This variable contains the latitude of
!              simulation site.

!     amalon - Longitude of simulation site (degrees)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.00      360.00
!              typical         70.00      170.00
!              ------------   ---------  ---------

!            - This variable contains the longitude
!              of simulation site.

!     amzele - Average site elevation (m)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the average value of elevation
!              relative to sea level for the site.

!-----------------------------------------------------------------------
