!$Author$
!$Date$
!$Revision$
!$HeadURL$

! started with code from:
! http://rosettacode.org/wiki/Ray-casting_algorithm#Fortran

! heavily modified to eliminate vertex index functionality.
! A polygon is now simply and ordered set of points.
! Routines define whether the last point must be joined to
! the first point in order to close the polygon


module Polygons_Mod
  use Points_Mod
  implicit none
 
  type polygon
     integer :: np   ! number of points in polygon point array
     real :: area    ! area of polygon in whatever unit the coordinates are in.
     type(point), dimension(:), allocatable :: points  ! the polygon points
  end type polygon
 
contains
 
  ! allocates a polygon structure which can contain np points
  function create_polygon(nump) result(ppol)
    integer, intent(in) :: nump  ! number of points in polygon created
    type(polygon) :: ppol

    ! local variable
    integer :: alloc_stat

    allocate(ppol%points(nump), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to allocate memory for Polygon"
      ppol%np = 0
    else
      ppol%np = nump
    end if 
  end function create_polygon
 
  ! deallocates a polygon structure
  subroutine destroy_polygon(pol)
    type(polygon), intent(inout) :: pol

    ! local variable
    integer :: dealloc_stat

    deallocate(pol%points, stat=dealloc_stat)
    if( dealloc_stat .gt. 0 ) then
      ! allocation failed
      write(*,*) "ERROR: unable to deallocate memory for Polygon"
    end if
  end subroutine destroy_polygon

  subroutine set_area_polygon( ppol )
    type(polygon), intent(inout) :: ppol

    real :: area
    integer :: idx
    integer :: np         ! number of points in input polygon
    integer :: start_np   ! starting point of closed polygon segment
    integer :: count_np   ! number of points to form closed polygon
    type(point), dimension(:), allocatable :: points
    integer :: alloc_stat

    area = 0        ! Accumulates area in the loop
    ! count and check for polygon closure and multisection polygon
    np = size(ppol%points)
    start_np = 1
    count_np = count_closure( ppol%points )
    do while( (count_np .gt. 0) .and. (start_np .lt. np) )
          area = area + area_point_array( ppol%points(start_np:start_np+count_np-1) )
          start_np = start_np + count_np
          count_np = count_closure( ppol%points(start_np:) )
          if( (count_np .eq. 0) .and. (start_np .lt. np) ) then
             ! a second polygon did not close back to first one
             ! check for self closure with later point
             start_np = start_np + 1
          else
             ! a second polygon never closed
             write(*,*) 'ERROR: mult section polygon does not close'
             stop 
          end if
    end do

    if( start_np .eq. 1 ) then
       ! single section non-closed polygon
       ! create close polygon array and find area
       allocate( points(np+1), stat=alloc_stat )
       if( alloc_stat .gt. 0) then
          write(*,*) 'ERROR: unable to allocate array in set_are_polygon'
       else
          do idx = 1, np
             points(idx) = ppol%points(idx)
          end do
          points(np+1) = ppol%points(1) ! using points(1) gives access violation in lahey
          ppol%area = area_point_array( points )
          deallocate( points, stat=alloc_stat )
          if( alloc_stat .gt. 0) then
             write(*,*) 'ERROR: unable to deallocate array in set_are_polygon'
          end if
       end if
    else
       ! last point closed polygon so report area
       ppol%area = area
    end if
    
  end subroutine set_area_polygon

  function area_point_array( points ) result( area )
    type(point), dimension(:), intent(in) :: points

    ! taken from http://arachnoid.com/area_irregular_polygon/
    ! this requires a closed point array with the first and last points the same.
    real :: area
    integer :: idx
    real :: ox, oy

    area = 0
    ox = points(1)%x
    oy = points(1)%y
    do idx = 2, size(points)
       area = area + ( points(idx)%x * oy - points(idx)%y * ox )
       ox = points(idx)%x
       oy = points(idx)%y
    end do
    area = abs(area / 2.0 )
  end function area_point_array

  function count_closure( points ) result( np )
    type(point), dimension(:), intent(in) :: points
    integer :: np

    ! If polygon closes, return number of points or 0 if not closed
    integer :: idx

    np = 0
    do idx = 2, size(points)
       if( points(idx) .eq. points(1) ) then
          np = idx
       end if
    end do
    
  end function count_closure

end module Polygons_Mod
