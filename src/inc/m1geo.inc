!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'm1geo.inc'

!     This block common is contains variables used to describe the
!     geometrical configuration of a simulation area.

!     Parameter include file: c:\main\inc\p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer          nacctr
      integer          nbr
      real             sim_area
      real             amasim
      real             ampbr(mnbr)
      real             amxar(mnarpt,mnarpt,mnar)
      real             amxbr(mnbpt,mnbpt,mnbr)
      real             amxbrw(mnbr)
      real             amxsim(mnspt,mnspt)
      real             amxsr(mnsrpt,mnsrpt,mnsub)
      real             amzbr(mnbr)
      character*80     amzbt(mnbr)

      common / m1geo /                                                  &
     &                 nacctr, nbr,                                     &
     &                 sim_area,                                        &
     &                 amasim, ampbr,                                   &
     &                 amxar, amxbr, amxbrw,                            &
     &                 amxsim, amxsr, amzbr,                            &
     &                 amzbt

      save :: /m1geo/

!     + + + VARIABLE DEFINITIONS + + +

!     sim_area - Simulation Region area (m^2)

!     amasim - Field angle (degrees)

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              0        360
!              typical            0        360
!              ------------   ---------  ---------

!            - This variable contains the angle of the simulation
!              region boundary relative to north.

!     ampbr  - Porosity of each barrier

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the porosity as fraction
!              of the outline of each barrier.
!              (ie., 1 - silhouette area).

!   amxar(xy,pt,ar)
!             - Coordinates of the accounting region where:
!                xy = 1 for x coordinate
!                xy = 2 for y coordinate
!                pt = 1 for first point, ie. lower left corner of region
!                pt = 2 for second point, ie. upper right corner of region
!                ar = number of the accounting region
!                     eg. ar = 1 if only one acciunting region
!              Note that two numbers (ie x,y) for amxar are needed to
!                   describe each point if the accounting region

!              range values      min       max
!              ------------   ---------  ---------
!              fixed              ?         ?
!              typical            ?         ?
!              ------------   ---------  ---------

!            - This variable contains the coordinates of two
!              opposite points for a rectangular accounting region.

!     amxsim - Coordinates of simulation region

!              range values      min       max
!              ------------   ---------  ---------
!              fixed             ?          ?
!              typical           ?          ?
!              ------------   ---------  ---------

!            - This variable contains the coordinates of two
!              opposite points for a rectangular simulation region.

!     amxsr  - Coordinates of the subregions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the coordinates of two
!              opposite points for each rectangular subregion.

!     amxbr  - Coordinates of barriers

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the coordinates of two
!              endpoints of a linear barrier.

!     amxbrw - Width of each barrier

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the width of each barrier in meters.

!     amzbr  - Height of each barrier

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the height of each barrier in meters.

!    nacctr  - Number of accounting regions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the number of accounting
!              regions for the simulation region.

!    nbr     - Number of barriers within the simulation region

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               ?          ?
!              typical             ?          ?
!              ------------   ---------  ---------

!            - This variable contains the number of barriers
!              for the simulation region.

!    amzbt   - Barrier type
!-----------------------------------------------------------------------
