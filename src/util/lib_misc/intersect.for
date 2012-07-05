!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function intersect( begind_a, endd_a, begind_b, endd_b )

!     + + + PURPOSE + + +
!     returns the intersection interval "distance" of two intervals
!     each defined by a greater and lesser value
!     Obviously the units must be consistent.

!     + + + ARGUMENT DECLARATIONS + + +
      real begind_a, endd_a, begind_b, endd_b

!     + + + ARGUMENT DEFINITIONS + + +
!     begind_a - lesser value of interval a
!     endd_a   - greater value of interval a
!     begind_b - lesser value of interval b
!     endd_b   - greater value of interval b

!     + + + END SPECIFICATIONS + + +

      if( (endd_a .gt. begind_b) .and. (begind_a .lt. endd_b) ) then
          ! some part of interval b intersects interval a
          if( (begind_a .le. begind_b) .and. (endd_a .ge. endd_b) ) then
              ! interval a completely surrounds interval b
              intersect = endd_b - begind_b
          else if( begind_a .le. begind_b ) then
              ! top part of interval b intersects farther end of interval a
              intersect = endd_a - begind_b
          else if( endd_a .ge. endd_b ) then
              ! farther end of interval b intersects nearer end of interval a
              intersect = endd_b - begind_a
          else
              ! interval b completely surrounds interval a
              intersect = endd_a - begind_a
          end if
      else
          intersect = 0.0
      end if

      return
      end
