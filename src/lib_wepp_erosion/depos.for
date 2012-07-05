      subroutine depos(xb,xe,cdep,a,b,c,phi,theta,ilast,dl,ldlast,      &
     &    xinput,ktrato,detach,load,tc,qostar)
     
      use wepp_interface_defs
      
      implicit none
!
!     calculates deposition in each segment of the hillslope
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine route
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 4-1-2005
!
!     + + + argument declarations + + +
!
      real, intent(in) :: xb, cdep, phi, theta, ktrato, qostar
	real, intent(in) :: a, b, c
	real, intent(inout) :: xe, xinput(101), load(101)
      real, intent(out) :: dl, ldlast, detach(101)
      real, intent(out) :: tc(101)
      integer, intent(inout) :: ilast
	   
    
!
!     + + + argument definitions + + +
!
!     xb     - n.d. distance where deposition begins
!     xe     - n.d. distance where deposition ends
!     cdep   - portion of solution to deposition equation
!     a      - shear stress equation coefficient
!     b      - shear stress equation coefficient
!     c      - shear stress equation coefficient
!     phi    - n.d. deposition parameter
!     theta  - n.d. interill detachment parameter
!     ilast  - index counter at last point where load computed
!     dl     - n.d. deposition rate at distance x=xe
!     ldlast - n.d. sediment load at distance x=xe
!     xterm  -
!     xinput -
!     ktrato - n.d. sediment transport equation coefficient
!     detach - n.d. detachment for each point down ofe
!     load   - n.d. sediment load at a point
!     tc     - sediment transport capacity at each point (kg/s/m)
!     qostar - nondimensional flow discharge onto ofe
!
!     + + + local variables + + +
!
      integer ibeg, i, loopfg, xterm
      real tclast
!
!     + + + local variable definitions + + +
!
!     ibeg   - counter variable value at first deposition point
!     tclast - n.d. transport capacity at distance x=xe
!     loopfg - flag.  1 = exit l3 loop.
!
!     + + + subroutines called + + +
!
!     depeqs
!
!     begin subroutine depos
!
      ibeg = ilast + 1
!     
      if (ibeg.lt.102) then
!        
         if (xinput(ibeg).gt.xe) then
!           
            if (qostar.le.-1.0.or.qostar.ge.0.0.or.xe.le.-qostar) then
               call depeqs(xb,cdep,a,b,phi,theta,xe,dl,ktrato,qostar)
               xterm = a * xe ** 2 + b * xe + c
               tclast = xterm * ktrato
               if (tclast.le.0.0) tclast = 0.0
               ldlast = tclast - dl * (xe+qostar) / phi
            else
               tclast = 0.0
               ldlast = 0.0
            end if
         else
!           
            i = ilast
            loopfg = 0
!           
   10       continue
!           
            i = i + 1
!           
            if (xinput(i).le.xe) then
!              
!              check if point is past end of runoff on a case 4 plane
!              
               if (qostar.le.-1.0.or.qostar.ge.0.0.or.xinput(i)         &
     &             .le.-qostar) then
!                 
                  call depeqs(xb,cdep,a,b,phi,theta,xinput(i),          &
     &                detach(i),ktrato,qostar)
                  xterm = a * xinput(i) ** 2 + b *                      &
     &                xinput(i) + c
                  tc(i) = xterm * ktrato
                  if (tc(i).lt.0.0) tc(i) = 0.0
!                 
                  load(i) = tc(i) - detach(i)*(xinput(i)+qostar) / phi
!                 
!                 added to prevent erroneous calculation of detachment 
!                 by deposition equation (for case 4 plane)
!                 
                  if (theta.le.0.0.and.i.gt.1.and.load(i).gt.load(i-1)) &
     &                load(i) = load(i-1)
               else
                  load(i) = 0.0
                  tc(i) = 0.0
               end if
!              
               if (load(i).lt.0.0) load(i) = 0.0
               ilast = i
               if (xinput(i).ge.1.0) loopfg = 1
            else
               loopfg = 1
            end if
!           
            if (i.lt.101.and.loopfg.eq.0) go to 10
!           
!           corrections made to prevent bombing for a case 4 
!           plane where xe is greater than -qostar
!           
!           case 1 - plane where flow does not end
!           
            if (qostar.ge.0.0.or.qostar.le.-1.0) then
               call depeqs(xb,cdep,a,b,phi,theta,xe,dl,ktrato,qostar)
               xterm = a * xe ** 2 + b * xe + c
               tclast = xterm * ktrato
               if (tclast.lt.0.0) tclast = 0.0
               ldlast = tclast - dl * (xe+qostar) / phi
            else
!              
!              case 2 - plane where flow ends but on the current 
!              slope segment
!              
               if (xe.lt.-qostar) then
                  call depeqs(xb,cdep,a,b,phi,theta,xe,dl,ktrato,qostar)
                  xterm = a * xe ** 2 + b * xe + c
                  tclast = xterm * ktrato
                  if (tclast.lt.0.0) tclast = 0.0
                  ldlast = tclast - dl * (xe+qostar) / phi
               else
!                 
!                 case 3 - plane where flow ends on the current slope 
!                 segment
!                 
                  tclast = 0.0
                  ldlast = 0.0
                  dl = 0.0
!              
               end if
            end if
!           
            if (ldlast.lt.0.0) ldlast = 0.0
            if (tclast.lt.0.0) tclast = 0.0
!        
         end if ! if (xinput(ibeg).gt.xe) then
      end if    ! if (ibeg.lt.102) then
!     
      return
      end
