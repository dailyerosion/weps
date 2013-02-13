!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function shear(a,b,c,x)
      implicit none
!
!     function shear calculates nondimensional shear stress
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine xcrit (see equation 11.4.1 in nserl report #10)
!
!     author(s): d.c flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: a, b, c, x
!
!     + + + argument definitions + + +
!
!     a - shear stress coefficient for current slope section
!     b - shear stress coefficient for current slope section
!     c - shear stress coefficient for current slope section
!     x - nondimensional distance at which shear stress is to be 
!         calculated
!
!     + + + local variable declarations + + +
!
      real value
!
!     + + + local variable definitions + + +
!
!     value - intermediate value in calculating shear stress
!
!     begin function shear
!
      value = a * x ** 2 + b * x + c
      if (value.lt.0.0) value = 0.0
      shear = value ** 0.66666667
      if (shear.le.0.0) shear = 0.0001
!     
      return
      end
