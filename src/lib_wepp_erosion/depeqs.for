!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine depeqs(xu,cdep,a,b,phi,theta,x,depeq,ktrato,qostar)
      
      use wepp_interface_defs
      
      implicit none
!
!     solves the deposition equation.
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine depos
!
!     author(s): d.c flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: xu, cdep, a, b, phi, theta,ktrato,qostar
      real, intent(inout) :: x
      real, intent(out) :: depeq
!
!     + + + argument definitions + + +
!
!     xu     - nondimensional distance on an ofe where current
!              deposition region begins
!     cdep   -
!     a      - shear stress coefficient for current slope section
!     b      - shear stress coefficient for current slope section
!     phi    - nondimensional deposition parameter for current ofe
!     theta  - nondimensional interrill detachment parameter for
!              current ofe
!     x      -
!     depeq  -
!     ktrato - nondimensional sediment transport eqn. coefficient
!     qostar - nondimensional flow discharge onto top of ofe.
!
!     + + + local variables + + +
!
      real ratio, expon, tmpvr1
!
!     + + + local variable definitions + + +
!
!     ratio  - temp variable to store parts of the depeq equation
!     expon  - temp variable to store parts of the depeq equation
!
!     + + + subroutines called + + +
!
!     undflo
!
!     begin subroutine depeqs
!
      if (abs(qostar+x).lt.10e-8) x = -qostar - 0.000001
      ratio = (xu+qostar) / (x+qostar)
      if (qostar.ge.0.0.and.ratio.gt.1.0) ratio = 1.0
!     
      expon = 1.0 + phi
      call undflo(ratio,expon)
!     
      tmpvr1 = 2.0 * a * ktrato
!     
      depeq = (tmpvr1*phi*(x+qostar)/(2.0+phi)) + (phi/(1.0+phi)) * (b* &
     &    ktrato-theta-(tmpvr1*qostar)) + cdep * ratio ** expon
!     
      return
      end
