!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function cross(x1,y1,x2,y2)
      implicit none
!
!     determines the point where two lines cross
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine erod
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: x1, y1, x2, y2
!
!     + + + argument definitions + + +
!
!     x1 -  point 1 x value
!     y1 -  point 1 y value
!     x2 -  point 2 x value
!     y2 -  point 2 y value
!
!     + + + local variables + + +
!
      real slope
!
!     + + + local variable definitions + + +
!
!     slope - slope of line segment
!
!     begin function cross
!
      if (x1.ne.x2) then
         slope = (y2-y1) / (x2-x1)
      else
         slope = 1.0e6
      end if
!     
      cross = -y1 / slope + x1
!     
      return
      end
