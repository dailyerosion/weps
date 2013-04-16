!$Author$
!$Date$
!$Revision$
!$HeadURL$

! A polyline is simply an ordered set of points.

module barriers_mod
  use Points_Mod
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

end module barriers_mod
