!$Author$
!$Date$
!$Revision$
!$HeadURL$

! A polyline is simply an ordered set of points.

module barriers_mod
  use Points_Mod, only: point, slen
  implicit none
 
  type barrier_params
    real :: amzbr          ! Height in meters.
    real :: amxbrw         ! Width in meters.
    real :: ampbr          ! Porosity as fraction of the outline of each barrier, silhouette area
  end type barrier_params

  type barrier_data
     character*80 :: amzbt  ! Barrier type
     integer :: np   ! number of points in barrier_params and polyline point array
     type(point), dimension(:), allocatable :: points  ! the polyline points
     type( barrier_params), dimension(:), allocatable :: param
  end type barrier_data

  type barrier_seasonal
     character*80 :: amzbt  ! Barrier type
     integer :: nper ! number of periods in barrier
     integer :: np   ! number of points in barrier_params and polyline point array
     type(point), dimension(:), allocatable :: points  ! the polyline points
     type( barrier_params), dimension(:), allocatable :: param
  end type barrier_seasonal

  type(barrier_data), dimension(:), allocatable :: barrier

contains
 
  ! allocates a barrier_data structure which can contain np points
  function create_barrier(nump) result(barr)
    integer, intent(in) :: nump  ! number of points in barrier_params and polyline created
    type(barrier_data) :: barr

    ! local variable
    integer :: sum_stat
    integer :: alloc_stat

    sum_stat = 0
    allocate(barr%points(nump), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    allocate(barr%param(nump), stat=alloc_stat)
    sum_stat = sum_stat + alloc_stat
    if( sum_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to allocate memory for barrier"
      barr%np = 0
    else
      barr%np = nump
    end if 
  end function create_barrier
 
  ! deallocates a barrier_data structure
  subroutine destroy_barrier(barr)
    type(barrier_data), intent(inout) :: barr

    ! local variable
    integer :: sum_stat
    integer :: dealloc_stat

    sum_stat = 0
    deallocate(barr%points, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    deallocate(barr%param, stat=dealloc_stat)
    sum_stat = sum_stat + dealloc_stat
    if( sum_stat .gt. 0 ) then
      ! deallocation failed
      write(*,*) "ERROR: unable to deallocate memory for Polygon"
    end if
  end subroutine destroy_barrier

  subroutine sbbr( cellstate )

!     + + + PURPOSE + + +
!     to calculate the fraction of open field friction velocity
!     from up wind and down wind sources of shelter at all interior nodes

      use erosion_data_struct_defs, only: cellsurfacestate
      use grid_mod, only: imax, jmax, ix, jy, amxsim, awa
      use p1unconv_mod, only: pi
      use Points_Mod, only: point
      use pnt_polyline_mod, only: location_intersect, pl_intersect
      use lin_interp_mod, only: lin_interp

!     + + + ARGUMENT DECLARATIONS + + +
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + LOCAL VARIABLES + + +
      integer i, j, n   ! do-loop indices
      integer :: nbr    ! number of barriers
      type(point) ::  pnt_grid  ! point form of grid coordinate
      type(location_intersect) ::  loc_intersect  ! point where upwind direction meets barrier, index in polyline and fraction distance between indexes
      real :: dist   ! distance from grid cell centroid to barrier
      real :: w0br_min   ! minimum value of sheltering effect (fraction of open field fric. vel) for this point
      integer :: npt     ! number of points along the barrier
      real :: zbr_interp  ! value of barrier height interpolated along barrier
      real :: pbr_interp  ! value of barrier porosity interpolated along barrier
      real :: xbrw_interp  ! value of barrier width interpolated along barrier

!     + + + END SPECIFICATIONS + + +

      ! discover number of barriers
      nbr = size(barrier)

      ! update interior nodes
      do i = 1, imax-1
        do j = 1, jmax-1
          ! calculate distance to middle of grid cell (maybe offset from origin)
          pnt_grid%x = (i-0.5)*ix + amxsim(1)%x
          pnt_grid%y = (j-0.5)*jy + amxsim(1)%y

          ! barrier sweep
          w0br_min = 1.0   ! maximum value for parameter
          do n = 1, nbr
            ! find number of points in barrier for interpolations
            npt = size(barrier(n)%param)

            ! look for barrier up wind
            if( pl_intersect( pnt_grid, awa, barrier(n)%points, loc_intersect ) ) then
              ! intersection point found (it is minimum distance for this barrier)
              dist = slen(pnt_grid, loc_intersect%pnt)

              ! barrier influence calculated down wind of barrier
              ! interpolate height along barrier segment
              zbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amzbr )
              if (dist .le. 35*zbr_interp) then
                ! distance is close enough for effect
                ! interpolate parameters along barrier segment
                pbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%ampbr )
                xbrw_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amxbrw )
                ! find shelter effect
                w0br_min = min(w0br_min, fu( dist, zbr_interp, pbr_interp, xbrw_interp ) )
              end if
            end if

            ! look for barrier down wind
            if( pl_intersect( pnt_grid, awa-180.0, barrier(n)%points, loc_intersect ) ) then
              ! intersection point found (it is minimum distance for this barrier)
              dist = slen(pnt_grid, loc_intersect%pnt)

              ! barrier influence calculated down wind of barrier
              ! interpolate height along barrier segment
              zbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amzbr )
              if (dist .lt. 5*zbr_interp) then
                ! distance is close enough for effect
                ! interpolate parameters along barrier segment
                pbr_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%ampbr )
                xbrw_interp = lin_interp( loc_intersect%low_index, loc_intersect%dist_frac, barrier(n)%param(1:npt)%amxbrw )

                ! find shelter effect (on upwind side of barrier use negative distance for correct function value)
                w0br_min = min(w0br_min, fu( -dist, zbr_interp, pbr_interp, xbrw_interp ) )
              end if
            end if
          end do

          ! assign minimum value to grid cell
          cellstate(i,j)%w0br = w0br_min

        end do
      end do

  end subroutine sbbr

  function fu (xh, zbr, pbr, xbrw) result(fufv)
      real, intent(in) :: xh     ! distance from barrier (range: -5*zbr to 50*zbr)
      real, intent(in) :: zbr    ! barrier height
      real, intent(in) :: pbr    ! barrier optical porosity (through entire width) (range: 0 to 0.9)
      real, intent(in) :: xbrw   ! barrier width

      real :: fufv  ! fraction of upwind fric. velocity near the  barrier

      ! local variables
      real a, b, c, d    ! intermediate result values
      real :: x, xw, pb  ! scaled values for distance, width and porosity

      ! scale distance & width by barrier height
      x = xh/zbr
      xw = xbrw/zbr

      ! increase effective porosity with barrier width
      pb = pbr + (1 - exp(-0.5*xw))*0.3*(1-pbr)

      ! calculate coef. as fn of porosity
      a = 0.008-0.17*pb+0.17*pb**1.05
      b = 1.35*exp(-0.5*pb**0.2)
      c = 10*(1-0.5*pb)
      d = 3 - pb

      ! calc. frac. of fric. vel.
      fufv = 1 - exp(-a*x**2) + b*exp(-0.003*(x+c)**d)

      ! Cap fu at 1.0
      if (fufv > 1.0) then
          fufv = 1.0
      endif

  end function fu

  function minht_barriers() result( minht )

    real :: minht  ! minimum barrier height

    integer :: i   ! do loop index

    ! default no barriers, so set height to zero
    minht = 0.0
    ! when barriers exist, find shortest barrier height
    do i = 1, size(barrier)
       minht = minval(barrier(i)%param(1:size(barrier(i)%param))%amzbr)
    end do

  end function minht_barriers

end module barriers_mod
