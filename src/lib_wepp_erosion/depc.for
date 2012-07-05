      real function depc(xu,a,b,phi,theta,du,ktrato,qostar)
      implicit none
!
!     function depc computes a portion of the deposition equation
!
!     module adapted from wepp version 2004.7 and called from
!     subroutine route in two locations
!
!     author(s): d.c flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) ::  xu, a, b, phi, theta, du, ktrato, qostar
!
!     + + + argument definitions + + +
!
!     xu     - nondimensional distance on an plane where current
!              deposition region begins
!     a      - "a" shear stress coefficient for current slope
!               section
!     b      - "b" shear stress coefficient for current slope
!              section
!     phi    - nondimensional deposition parameter for current plane
!     theta  - nondimensional interrill detachment parameter for
!              current plane
!     du     - deposition rate at xu
!     ktrato - nondimensional transport capacity coefficient
!     qostar - nondimensional flow discharge onto an plane
!
!     + + + local variables + + +
!     + + + local variable definitions + + +
!
!     begin function depc
!
      if (abs(qostar+xu).ge.10e-8) then
         depc = du - (a*ktrato*phi*2.0*(qostar+xu)/(phi+2.0)) - ((b*    &
     &       ktrato-2.0*a*ktrato*qostar-theta)*phi/(phi+1.0))
      else
         depc = 0.0
      end if
!     
      return
      end
