      subroutine root(a,b,c,x1,x2)
      
      use wepp_interface_defs
      
      implicit none
!
!     subroutine root finds roots for the equation y=a*x**2+b*x+c
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine xcrit
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date modified:  9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: a, b
      double precision, intent(in) :: c
      double precision, intent(out) :: x1, x2
!
!     + + + argument definitions + + +
!
!     a  - coefficient for x^2 in quadratic equation
!     b  - coefficient for x in quadratic equation
!     c  - constant in quadratic equation
!     x1 - one of two root solutions for quadratic equation
!     x2 - one of two root solutions for quadratic equation
!
!     + + + local variables + + +
!
      double precision cc, part
      real b1, tmpvr1
!
!     cc - 
!     part - 
!     b1 - 
!     tmpvr1 - 
!
!     + + + local variable definitions + + +
!
!     begin subroutine root
!
      b1 = -b
      tmpvr1 = 2.0 * a
      cc = -c
      part = sqrt(b**2-4.0*a*cc)
      x1 = (b1-part) / tmpvr1
      x2 = (b1+part) / tmpvr1
!     
      if (x1.gt.x2) then
         part = x2
         x2 = x1
         x1 = part
      end if
!     
      return
      END
