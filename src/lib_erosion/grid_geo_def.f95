!$Author$
!$Date$
!$Revision$
!$HeadURL$

module grid_geo_def
    use Points_Mod
    implicit none

    integer :: kbr         ! wind quadrant key relative to simulation region for barrier effect (from 1 to 8 )
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

end module grid_geo_def
