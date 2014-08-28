!$Author$
!$Date$
!$Revision$
!$HeadURL$

module grid_mod
    use Points_Mod, only: point
    implicit none

    integer :: i1, i2, i3  ! do loop parameters defining grid update sequence, i1..i3 defines second update directions
    integer :: i4, i5, i6  ! do loop parameters defining grid update sequence, i4..i6 defines first update direction

    real :: awa            ! wind angle across simulation region relative to Y-axis
    real :: sin_awa        ! precomputed sin of awa angle
    real :: cos_awa        ! precomputed cos of awa angle
    real :: tan_awa        ! precomputed tan of awa angle

    integer :: imax             ! Number of grid intervals in x direction on EROSION grid
    integer :: jmax             ! Number of grid intervals in y direction on EROSION grid
    real :: ix                  ! grid interval in x-direction (m) (range from 7.0 to 3000)
    real :: jy                  ! grid interval in y-direction (m) (range from 7.0 to 3000)

    ! added for command line or alternate specification of grid dimensions
    integer :: xgdpt            ! specified # of grid data points in the x-dir
    integer :: ygdpt            ! specified # of grid data points in the y-dir

    ! original Hagen grid dimensions
    integer, parameter :: N_G_DPT = 30      ! # of grid data points under no barrier cond.
    integer, parameter :: B_G_DPT = 60      ! # of grid data points to use if barrier exists
    real, parameter :: MIN_GRID_SP = 7.0    ! minimum targeted grid spacing (m)

    type(point), dimension(2) :: amxsim ! Coordinates of two diagonally opposite points for a rectangular simulation region.
    real :: sim_area            ! sim_area - Simulation Region area (m^2)
    real :: amasim              ! Field angle (degrees) (0 to 360) the angle of the simulation region boundary relative to north.

  contains

    subroutine sbgrid( minht )

!     +++ PURPOSE +++
!     to calculate grid size and spacing for EROSION.
!     grid size assumes outer points are outside field boundary a distance ix/2
!     to calculate number of grid points for EROSION.
!     A max 'interior' square grid of 29X29 is assigned-no barriers
!     A max 'interior' rectangular grid of 59X59 is assigned barriers

!     +++ ARGUMENTS +++
      real :: minht    ! minimum height of barrier along it's length.

!     +++ LOCAL VARIABLES +++
      integer :: ngdpt ! number of grid points along a direction
      real :: dxmin    ! minimum grid interval (m)
      real :: lx, ly   ! x-axis and y-axis lengths of simulation region
      logical :: bar_exist  ! .true if barriers exist

!     +++ END SPECIFICATIONS +++

       ! set min grid spacing
       dxmin = MIN_GRID_SP

       ! set max no. of grid points with no barrier
       ngdpt = N_G_DPT

       if( minht > 0.0 ) then    !Check for zero height barriers
          ! at least one barrier exists
          dxmin = min(dxmin, 5.0*minht)
          ngdpt = B_G_DPT  !default to this value if a barrier exists
          bar_exist = .true.
       else
          bar_exist = .false.
       endif

!     calculate max grid intervals
!       calc. lx and ly sides of field
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y

      !write(*,*) 'SBGRID: lx, ly: ', lx, ly

!     increase grid points on large field
!      if((lx .gt. 200) .or. (ly  .gt. 200)) then
!         ngdpt = B_G_DPT
!      endif

      ! case where lx > ly
      if ( lx .gt. ly)then
        imax  = int ( lx / dxmin)
        imax = min(imax,ngdpt)
        imax = max(imax,2)
        ! calculate spacing for square or with barriers a rectangular grid
        ix  = lx / (imax - 1)

         if( bar_exist ) then
           jmax  = int (ly / dxmin)
           jmax  = min(jmax, ngdpt)
         else
           jmax = nint(ly/ix) + 1
         endif

        jmax = max(jmax,2)
        jy   = ly / (jmax - 1)

      ! case where lx = ly or lx < ly
      else
        jmax  = int (ly / dxmin)
        jmax = min(jmax,ngdpt)
        jmax = max(jmax,2)
        jy   = ly / (jmax - 1)

        if( bar_exist ) then
           imax  = int (lx / dxmin)
           imax  = min(imax,ngdpt)
        else
           imax = nint(lx/jy) + 1
        endif
        imax = max(imax,2)
        ix = lx/(imax-1)

      endif

    end subroutine sbgrid

    subroutine sbigrd( cellstate )

!     + + + PURPOSE + + +
!     To set the grid output arrays to zero

      use erosion_data_struct_defs, only: cellsurfacestate

!     +++ ARGUMENT DECLARATIONS +++
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + LOCAL VARIABLES + + +
      integer i, j

!     + + + END SPECIFICATIONS + + +

      do j = 0, jmax
         do i = 0, imax
            cellstate(i,j)%egt = 0.0
            cellstate(i,j)%egtcs = 0.0
            cellstate(i,j)%egtss = 0.0
            cellstate(i,j)%egt10 = 0.0
            cellstate(i,j)%egt2_5 = 0.0
         end do
      end do
      
    end subroutine sbigrd     

    subroutine init_regions_grid( cellstate )

!     +++ PURPOSE +++
!     Set the subregion and accounting region associations with each grid cell

!     + + + Modules Used + + +
      use pnpoly_mod, only: pnpoly
      use subregions_mod, only: subr_poly, acct_poly
      use erosion_data_struct_defs, only: cellsurfacestate

!     +++ ARGUMENT DECLARATIONS +++
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ LOCAL VARIABLES +++
      integer i, j, sr
      type(point) :: centroid

!     +++ END SPECIFICATIONS +++

      ! assign subregion number to each grid cell
      ! code lifted from sbgrid because it is initialized there - LEW
      do j = 1, jmax-1
        do i = 1, imax-1
          ! The grid cell is assumed rectangular. Use centroid of grid cell
          ! with subregion polygon to select grid cell subregion
          centroid%x = 0.5 * (i-1+i) * ix
          centroid%y = 0.5 * (j-1+j) * jy
          do sr = 1, size(subr_poly)
            ! Check if it is inside subregion polygon
            if( pnpoly(centroid, subr_poly(sr)) .ge. 0) then
               ! centroid of grid cell is inside or on edge of subregion polygon
               ! set subregion index
               cellstate(i,j)%csr = sr
               ! default to first polygon if on edge by exiting the subregion do loop
               exit
            end if
          end do
          ! check final status
          if( cellstate(i,j)%csr .eq. 0 ) then
              ! this grid cell not assigned to a subregion
              write(*,*) 'ERROR: no subregion for grid cell ',i,':',j
              write(*,*) 'Subregion coverage is not complete'
              stop
          end if
          ! do same assignment check for accounting regions
          do sr = 1, size(acct_poly)
            ! Check if it is inside subregion polygon
            if( pnpoly(centroid, acct_poly(sr)) .ge. 0) then
               ! centroid of grid cell is inside or on edge of subregion polygon
               ! set accounting region index
               cellstate(i,j)%car = sr
               ! default to first polygon if on edge by exiting the accounting region do loop
               exit
            end if
          end do
        end do          
      end do

    end subroutine init_regions_grid

    subroutine sbdirini(wind_dir, cellstate)

!     +++ purpose +++
!     Calc. wind angle on the sim. region
!     Calc. sweep sequence for update of grid cells

      use erosion_data_struct_defs, only: cellsurfacestate
      use p1unconv_mod, only: degtorad

!     +++ ARGUMENT DECLARATIONS +++
      real wind_dir  ! direction of the wind in degrees from north
      type(cellsurfacestate),dimension(0:,0:),intent(inout) :: cellstate     ! grid cell state for sbbr

!     + + + END SPECIFICATION + + +

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

    end subroutine sbdirini

end module grid_mod
