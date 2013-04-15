!$Author$
!$Date$
!$Revision$
!$HeadURL$

! methods to find the intersection (or not) of two polylines
! they may be specified by arrays of points
! or by a point and an angle (ie. a vector)

Module pnt_polyline_mod
  use Points_Mod

  type location_intersect
    type(point) :: pnt
    integer :: low_index
    real :: dist_frac
  end type location_intersect

  interface pl_intersect
    module procedure intersect_2_poly
    module procedure intersect_direction_poly
    module procedure intersect_4_points
  end interface pl_intersect

contains

  function intersect_2_poly(pline1, pline2, pnt_intersect, cnt_intersect) result( did_intersect)
    ! arguments
    type(point), dimension(:), intent(in) :: pline1  ! polyline array (at least 2 points)
    type(point), dimension(:), intent(in) :: pline2  ! polyline array (at least 2 points)
    type(point), intent(out) :: pnt_intersect  ! point of intersection
    integer, intent(inout) :: cnt_intersect    ! if less than or equal to zero on call, return count of number of intersections and the last intersection point.
                                               ! if greater than zero, return intersection point at that count.
    logical :: did_intersect    ! did the two polylines have an intersection point

    ! local variables
    integer :: idx1  ! loop index for pline1
    integer :: idx2  ! loop index for pline2
    integer :: cnt   ! count of number of intersections
    integer :: stat  ! status return from the intersection routine

    cnt = 0
    do idx1 = 2, size(pline1)
      do idx2 = 2, size(pline2)
        stat = intersect_4_points( pline1(idx1-1), pline1(idx1), pline2(idx2-1), pline2(idx2), pnt_intersect )
        if( abs(stat) .eq. 1 ) then
          ! intersection found, or common end point
          cnt = cnt + 1
          if( (cnt_intersect .gt. 0) .and. (cnt_intersect .eq. cnt) ) then
            ! specific point requested so return it
            did_intersect = .true.
            return
          end if
        end if
      end do
    end do

    ! check count and return accordingly
    if( cnt .lt. cnt_intersect ) then
      ! unable to find enough intersections, return did not intersect
      did_intersect = .false.
    else if( cnt .gt. 0 ) then
      ! intersections found and not equal to cnt_intersect so set and return
      cnt_intersect = cnt
      did_intersect = .true.
    else  ! cnt .eq. 0
      did_intersect = .false.
    end if

    return
  end function intersect_2_poly

  function intersect_direction_poly(pnt, angle, pline, loc_intersect) result(did_intersect)
    ! returns the intersection of the directed semi-infinite line segment with any segment of the polyline pline
    ! In the case of multiple intersections, the closest one is returned.

    ! arguments
    type(point), intent(in) :: pnt   ! starting point of semi-infinite line segment
    real, intent(in) :: angle        ! direction of semi-infinite line segment (degrees from North, clockwise is positive)
    type(point), dimension(:), intent(in) :: pline  ! polyline
    type(location_intersect), intent(out) :: loc_intersect  ! point of intersection, lower index and fraction of distance between indexes
    logical :: did_intersect                        ! .true. indicates intersection, .false. indicates no intersection

    ! local variables
    integer :: idx
    type(point), dimension(1:size(pline)) :: pline_tr  ! working array for polyline
    type(point) :: inv_pnt  ! point used to translate points back to original position
    real :: rot_ang   ! angle to rotate the polyline
    real :: dist      ! distance from pnt to intersection
    real :: min_dist  ! minimum distance from pnt to intersection

    ! find translation inverse
    inv_pnt%x = 0.0
    inv_pnt%y = 0.0
    inv_pnt = translate( pnt, inv_pnt )

    ! translate and rotate all points in pline so pnt is at (0,0) and angle is postive x-axis
    pline_tr = translate( pnt, pline ) 

    ! rotate all points in pline as if angle is postive x-axis (90 degrees from North
    ! NOTE: rotate routine considers counter-clockwise the positive direction
    rot_ang = (angle - 90.0)
    pline_tr = rotate( rot_ang, pline_tr )

    min_dist = huge(min_dist)
    ! check all line segments in polyline
    do idx = 2, size(pline_tr)

      ! check end points
      if( pline_tr(idx-1)%y .eq. 0.0 ) then
        ! first segment end point intersects
        ! check for minimum distance
        dist = slen(pline_tr(idx-1))
        if( dist .lt. min_dist ) then
          min_dist = dist
          loc_intersect%pnt = pline(idx-1)
          loc_intersect%low_index = idx - 1
          loc_intersect%dist_frac = 0.0
        end if
        if( pline_tr(idx)%y .eq. 0.0 ) then
          ! second segment end point intersects
          ! check for minimum distance
          dist = slen(pline_tr(idx))
          if( dist .lt. min_dist ) then
            min_dist = dist
            loc_intersect%pnt = pline(idx)
            loc_intersect%low_index = idx - 1
            loc_intersect%dist_frac = 1.0
          end if
        end if
        did_intersect = .true.
      else
        ! first segment end point does not intesect
        if( pline_tr(idx)%y .eq. 0.0 ) then
          ! only second segment end point intersects
          dist = slen(pline_tr(idx))
          if( dist .lt. min_dist ) then
            min_dist = dist
            loc_intersect%pnt = pline(idx)
            loc_intersect%low_index = idx - 1
            loc_intersect%dist_frac = 1.0
          end if
          did_intersect = .true.
        end if
      end if

      ! There is intersection if segment crosses positive x-axis
      if( nint(sign(1.0,pline_tr(idx-1)%y)) .ne. nint(sign(1.0,pline_tr(idx)%y)) ) then
        ! One is positive and the other negative, they cross the axis

        ! (3) Discover the position of the intersection point on the x-axis.
        dist = pline_tr(idx)%x + (pline_tr(idx-1)%x - pline_tr(idx)%x) * pline_tr(idx)%y / (pline_tr(idx)%y - pline_tr(idx-1)%y);

        if( dist .lt. min_dist ) then
          ! This intersection is closer, so make this the intersection point
          ! (4) rotate and translate back to the original coordinate system.
          min_dist = dist
          loc_intersect%pnt%x = dist
          loc_intersect%pnt%y = 0.0
          loc_intersect%pnt = rotate( -rot_ang,  loc_intersect%pnt )
          loc_intersect%pnt = translate( inv_pnt, loc_intersect%pnt )
          loc_intersect%low_index = idx - 1
          loc_intersect%dist_frac = (dist - pline_tr(idx-1)%x) / (pline_tr(idx)%x - pline_tr(idx-1)%x)
        end if
        did_intersect = .true.
      end if
    end do

  end function intersect_direction_poly

  function intersect_4_points( pnt1, pnt2, pnt3, pnt4, pnt_intersect ) result(type_intersect)

    ! given line segements (pnt1-pnt2) and (pnt3-pnt4), determine whether they intersect
    ! and if they do, the point of intersection. Also return indication of degenerate cases,
    ! such as zero length segments, end point on segment and coinciding line segments.
    ! return intersection point will be modified for case 1 and -1

    ! heavily modified from http://alienryderflex.com/intersect/
    ! public domain function by Darel Rex Finley, 2006

    ! arguments
    type(point), intent(in) :: pnt1, pnt2, pnt3, pnt4
    type(point), intent(out) :: pnt_intersect
    integer :: type_intersect    ! -3 - zero length segment
                                 ! -2 - coincident
                                 ! -1 - shared end point or coincident
                                 !  0 - no intersection
                                 !  1 - intersection

    ! local variables
    type(point) :: pnt2t, pnt3t, pnt4t
    real :: dist_1_2, theCos, theSin, newX, pos_1_2 ;

    ! either line segment is zero-length.
    if( ((pnt1%x .eq. pnt2%x) .and. (pnt1%y .eq. pnt2%y)) .or. ((pnt3%x .eq. pnt4%x) .and. (pnt3%y .eq. pnt4%y)) ) then
      type_intersect = -2
      return
    end if

    ! segments share an end-point.
    if( ((pnt1%x .eq. pnt3%x) .and. (pnt1%y .eq. pnt3%y)) .or. ((pnt2%x .eq. pnt3%x) .and. (pnt2%y .eq. pnt3%y)) ) then
      ! return this end point
      pnt_intersect = pnt3
      type_intersect = -1
      return
    end if
  
    if( ((pnt1%x .eq. pnt4%x) .and. (pnt1%y .eq. pnt4%y)) .or. ((pnt2%x .eq. pnt4%x) .and. (pnt2%y .eq. pnt4%y)) ) then
      ! return this end point
      pnt_intersect = pnt4
      type_intersect = -1
      return
    end if

    ! (1) Translate the system so that point pnt1 is on the origin.
    pnt2t = pnt2 - pnt1
    pnt3t = pnt3 - pnt1
    pnt4t = pnt4 - pnt1

    ! Discover the length of segment 1-2.
    dist_1_2 = slen(pnt2t)

    ! (2) Rotate the system so that point 2 is on the positive X axis.
    theCos = pnt2t%x / dist_1_2;
    theSin = pnt2t%y / dist_1_2;
    ! rotate point 3
    newX = pnt3t%x*theCos + pnt3t%y*theSin
    pnt3t%y = pnt3t%y*theCos - pnt3t%x*theSin
    pnt3t%x = newX
    ! rotate point 4
    newX = pnt4t%x*theCos + pnt4t%y*theSin
    pnt4t%y = pnt4t%y*theCos - pnt4t%x*theSin
    pnt4t%x = newX

    ! check if lines coincide (not testing segment overlap at this point)
    if( (pnt3t%y .eq. 0.0) .and. (pnt4t%y .eq. 0.0) ) then
      type_intersect = -2
      return
    end if

    ! No intersection if segment 3-4 doesn't cross line 1-2.
    if( ((pnt3t%y .lt. 0.0) .and. (pnt4t%y .lt. 0.0)) .or. ((pnt3t%y .gt. 0.0) .and. (pnt4t%y .gt. 0.0)) ) then
      type_intersect = 0
      return
    end if

    ! (3) Discover the position of the intersection point along line A-B.
    pos_1_2 = pnt4t%x + (pnt3t%x - pnt4t%x) * pnt4t%y / (pnt4t%y - pnt3t%y);

    ! No intersection if segment 3-4 crosses line 1-2 outside of segment 1-2.
    if( (pos_1_2 .lt. 0.0) .or. (pos_1_2 .gt. dist_1_2) ) then
      type_intersect = 0
      return
    end if

    ! (4) Apply the discovered position to line 1-2 in the original coordinate system.
    pnt_intersect%x = pnt1%x + pos_1_2*theCos
    pnt_intersect%y = pnt1%y + pos_1_2*theSin
    type_intersect = 1
    return

  end function intersect_4_points

end module pnt_polyline_mod

