!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbdirini - initializes grid/wind direction relationships
!**********************************************************************

      subroutine sbdirini(wind_dir, prev_dir)

!     +++ purpose +++
!     Calc. wind angle on the sim. region
!     Calc. sweep sequence for update of grid cells
!     calc. ridge spacing parallel the wind
!
!     +++ ARGUMENT DECLARATIONS +++
      real wind_dir
      real prev_dir
!
!     +++ ARGUMENT DEFINITIONS +++
!     wind_dir - direction of the wind in degrees from north
!     prev_dir - previously computed direction of the wind
!
!     +++ PARAMETER +++
      real  pid180
      parameter(pid180 = 3.14159/180.)
!
!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'm1geo.inc'
      include 'm1subr.inc'
      include 's1sgeo.inc'
      include 'w1wind.inc'
!
!     + + +  LOCAL COMMON BLOCKS + + +
      include 'erosion/m2geo.inc'
      include 'erosion/e3grid.inc'
      include 'erosion/s2sgeo.inc'
!
!     + + + LOCAL VARIABLES + + +
      integer  icsr, i, j, ke
      real sfd1(mnsub), sfd10(mnsub), sfd84(mnsub), sfd200(mnsub)
     
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     icsr  = index of current subregion
!
!     + + + SUBROUTINES CALLED + + +
!     sbsfdi
!
!     + + + END SPECIFICATION + + +
!
!     check and do not calculate if done on last entry
      if( wind_dir.eq.prev_dir ) return
      prev_dir = wind_dir

   
!
!     calc wind angle relative to the field Y-axis (+, - 45 deg. range)
      awa = wind_dir - amasim
      if (awa .lt. 0.0 ) awa = awa + 360.0
      if (awa .gt. 360.0) awa = awa - 360.0

      sin_awa = sin(awa*pid180)
      cos_awa = cos(awa*pid180)
      tan_awa = tan(awa*pid180)
!
!     find wind quadrant relative to sim region & select sweep sequence
!
      If (awa .ge. 0.0 .and. awa .lt. 90.0) then
        i1 = imax - 1
        i2 = 1
        i3 = -1
        i4 = jmax - 1
        i5 = 1
        i6 = -1
        ke = 1
!
      elseif (awa .ge. 90.0 .and. awa .lt. 180.0) then
        i1 = imax - 1
        i2 = 1
        i3 = -1
        i4 = 1
        i5 = jmax - 1
        i6 = 1
        ke = 1
!
      elseif (awa .ge. 180.0 .and. awa .lt. 270.0) then
        i1 = 1
        i2 = imax - 1
        i3 = 1
        i4 = 1
        i5 = jmax - 1
        i6 = 1
        ke = 1
!

      else
        i1 = 1
        i2 = imax - 1
        i3 = 1
        i4 = jmax - 1
        i5 = 1
        i6 = -1
        ke = 1
      endif
!
!     determine barrier influence direction index (kbr)
!
      if (wind_dir .ge. 337.5 .or. wind_dir .lt. 22.5) then
        kbr = 1
      elseif (wind_dir .lt. 67.5) then
        kbr = 2
      elseif (wind_dir.lt. 112.5) then
        kbr = 3
      elseif (wind_dir .lt. 157.5) then
        kbr = 4
      elseif (wind_dir .lt. 202.5) then
        kbr = 5
      elseif (wind_dir.lt. 247.5) then
        kbr = 6
      elseif (wind_dir .lt. 292.5) then
        kbr = 7
      else
        kbr = 8
      endif
!
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
