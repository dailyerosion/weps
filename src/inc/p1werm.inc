!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'p1werm.inc'

!     These parameter variables are used to size certain global arrays
!     at compile time for WERM.  They may also be consulted from within
!     sections of the WERM code if this file has been included.

      integer          mnar
      integer          mnarpt
      integer          mnbr
      integer          mnbpt
      integer          mncz
      integer          mnspt
      integer          mnsrpt
      integer          mnsub
      integer          mnsz
      integer          mntime
      integer          mnbpls
      integer          mndk
      integer          mngdpt
      integer          mnrbc

      integer          mnhhrs

      integer          mnryr
      integer          mnmon
      integer          mnopdat

      integer          mndayr

!     + + + VARIABLE DECLARATIONS + + +

      parameter (mnar = 4)
      parameter (mnarpt = 2)
      parameter (mnbr = 500)  !increased from 4 for standalone erosion use
      parameter (mnbpt = 2)
      parameter (mncz = 5)
      parameter (mnspt = 2)
      parameter (mnsrpt = 2)
      parameter (mnsub = 4)
      parameter (mnsz = 100)
      parameter (mntime = 1440)
      parameter (mnbpls = 5)
      parameter (mndk = 5)
      parameter (mngdpt = 501) !increased from 60 for standalone erosion testing
      parameter (mnrbc = 5)

      parameter (mnhhrs = 24)

      parameter (mnryr = 16)
      parameter (mnmon = 13)
      parameter (mnopdat = 32)

      parameter (mndayr = 367)

!     + + + VARIABLE DEFINITIONS + + +

!     mnar   - Maximum number of accounting regions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          10
!              ------------   ---------  ---------
!
!            - The maximum number of accounting regions to be handled
!              by any compiled version of WERM is specified by this
!              parameter variable.

!     mnarpt - Maximum number of accounting region points
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of accounting region points to be
!              handled by any compiled version of WERM is specified
!              by this parameter variable.

!     mnbr  - Maximum number of barriers within subregions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          10
!              ------------   ---------  ---------

!            - The maximum number of barriers within subregions to be
!              handled by any compiled version of WERM is specified
!              by this parameter variable.

!     mnbpt  - Maximum number of barrier points

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of points which designate a barrier
!              shape and location within a subregion.

!     mncz   - Maximum number of crop height segments

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        5          5
!              ------------   ---------  ---------

!            - The number of crop height segments to be handled by any
!              compiled version of WERM is specified by this parameter
!              variable.  (Probably should not be modified since some
!              crop parameters assume there will be 5 crop height
!              segments).

!     mnspt  - Maximum number of simulation region points

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of simulation region points to be
!              handled by any compiled version of WERM is specified
!              by this parameter variable.

!     mnsrpt - Maximum number of subregion points

!              range values       min        max
!              ------------   ---------  ---------
!              fixed               2          ?
!              typical             2          2
!              ------------   ---------  ---------

!            - The number of subregion points to be handled
!              by any compiled version of WERM is specified
!              by this parameter variable.

!     mnsub  - Maximum number of subregions

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          10
!              ------------   ---------  ---------

!            - The maximum number of subregions to be handled by any
!              compiled version of WERM is specified by this parameter
!              variable.

!     mnsz   - Maximum number of soil layers

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          memory dependent
!              typical        1          100
!              ------------   ---------  ---------

!            - The maximum number of soil layers to be handled by any
!              compiled version of WERM is specified by this parameter
!              variable.

!     mntime - Maximum number of timesteps per day

!              range values       min        max
!              ------------   ---------  ---------
!              fixed             1          1440
!              typical           24         24
!              ------------   ---------  ---------

!            - The number of time steps to be handled by
!              any compiled version of WERM is specified
!              by this parameter variable.

!     mnbpls - Maximum number of biomass pools

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1              ?
!              typical        1              ?
!              ------------   ---------  ---------

!            - The maximum number of biomass pools for each subregion

!     mndk   - maximum number of decay coefficients (st,fl,bg,rt,stem no)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1              3
!              typical        1              3
!              ------------   ---------  ---------

!            - The number of biomass decay coefficients for each subregion
!              (standing mass, flat mass, buried mass, root mass, & stem_no)

!     mngdpt - Maximum number of grid points in a given direction.

!              range values       min        max
!              ------------   ---------  ---------
!              fixed              1          30
!              typical            ?          30
!              ------------   ---------  ---------

!            - The maximum number of grid points in a given direction

!     mnrbc  - The number of residue burial classes

!     mnryr  - Maximum number of rotation years in a management file.

!              range values       min        max

!              ------------   ---------  ---------
!              fixed              1          16
!              typical            2           5
!              ------------   ---------  ---------

!            - The maximum number of rotation years in a management file.
!              The sixteenth year is used to store the monthly output
!              regardless of rotation year.

!     mnmon  - Maximum number of months in an output file.

!              range values       min        max

!              ------------   ---------  ---------
!              fixed              1          13
!              typical            1          13
!              ------------   ---------  ---------

!            - The maximum number of months in an output file.
!              The thirteenth month is used to store the annual output
!              for the rotation year.

!     mnopdat- Maximum number of operation dates in an output file.

!              range values       min        max

!              ------------   ---------  ---------
!              fixed              1          32
!              typical            1          32
!              ------------   ---------  ---------

!            - The maximum number of operation dates per month in an
!              output file.
!              The thirtysecond day is used to store the annual output
!              for the current rotation year.

!     mndayr- Maximum number of days read from cligen, wingen files at one time, 367

!     ------------------------------------------------------------------
