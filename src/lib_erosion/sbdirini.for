!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbdirini - initializes grid/wind direction relationships
!**********************************************************************

      subroutine sbdirini(wind_dir, prev_dir, cellstate)

!     +++ purpose +++
!     Calc. wind angle on the sim. region
!     Calc. sweep sequence for update of grid cells
!     calc. ridge spacing parallel the wind

      use weps_interface_defs
      use grid_geo_def, only: imax, jmax, i1, i2, i3, i4, i5, i6,       &
     &                       kbr, awa, sin_awa, cos_awa, tan_awa, amasim
      use erosion_data_struct_defs, only: cellsurfacestate
      use p1unconv_mod, only: degtorad

!     +++ ARGUMENT DECLARATIONS +++
      real wind_dir  ! direction of the wind in degrees from north
      real prev_dir  ! previously computed direction of the wind
      type(cellsurfacestate),dimension(0:,0:),intent(inout) :: cellstate     ! grid cell state for sbbr

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'

!     + + + END SPECIFICATION + + +

!     check and do not calculate if done on last entry
      if( wind_dir .eq. prev_dir ) return
      prev_dir = wind_dir

!     calc wind angle relative to the field Y-axis (+, - 45 deg. range)
      awa = wind_dir - amasim
      if (awa .lt. 0.0 ) awa = awa + 360.0
      if (awa .gt. 360.0) awa = awa - 360.0

      sin_awa = sin(awa*degtorad)
      cos_awa = cos(awa*degtorad)
      tan_awa = tan(awa*degtorad)

!     find wind quadrant relative to sim region & select sweep sequence

      If (awa .ge. 0.0 .and. awa .lt. 90.0) then
        i1 = imax - 1
        i2 = 1
        i3 = -1
        i4 = jmax - 1
        i5 = 1
        i6 = -1

      elseif (awa .ge. 90.0 .and. awa .lt. 180.0) then
        i1 = imax - 1
        i2 = 1
        i3 = -1
        i4 = 1
        i5 = jmax - 1
        i6 = 1

      elseif (awa .ge. 180.0 .and. awa .lt. 270.0) then
        i1 = 1
        i2 = imax - 1
        i3 = 1
        i4 = 1
        i5 = jmax - 1
        i6 = 1

      else
        i1 = 1
        i2 = imax - 1
        i3 = 1
        i4 = jmax - 1
        i5 = 1
        i6 = -1

      endif

!     determine barrier influence
      call sbbr( awa, cellstate )

!     determine barrier influence direction index (kbr)
!      if (wind_dir .ge. 337.5 .or. wind_dir .lt. 22.5) then
!        kbr = 1
!      elseif (wind_dir .lt. 67.5) then
!        kbr = 2
!      elseif (wind_dir.lt. 112.5) then
!        kbr = 3
!      elseif (wind_dir .lt. 157.5) then
!        kbr = 4
!      elseif (wind_dir .lt. 202.5) then
!        kbr = 5
!      elseif (wind_dir.lt. 247.5) then
!        kbr = 6
!      elseif (wind_dir .lt. 292.5) then
!        kbr = 7
!      else
!        kbr = 8
!      endif

      return
      end

