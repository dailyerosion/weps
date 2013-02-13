!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine erod(xb,xe,a,b,c,atc,btc,ctc,eata,tauc,theta,phi,ilast,&
     &    dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,detach,qostar)
     
      use wepp_interface_defs
      
      implicit none
!
!     subroutine erod calculates soil particle detachment in each 
!     slope segment on flow plane 
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine route
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 4-1-2005
!
!     + + + argument declarations + + +
!
      real, intent(in) :: xb, xe, a, b, c, eata, tauc, theta
	  real, intent(inout) :: xdbeg
      real, intent(in) ::  atc, btc, ctc, phi, qostar
      real, intent(in) :: xinput(101), ktrato
	  real, intent(inout) :: detach(101)
      integer, intent(inout) ::  ilast
	  integer, intent(out) :: ndep
	  real, intent(inout) :: ldlast, tc(101), load(101), dl
!
!     + + + argument definitions + + +
!
!     xb     - distance at beginning of detachment region
!     xe     - distance at end of detatchment region
!     a      - shear stress equation coefficient
!     b      - shear stress equation coefficient
!     c      - shear stress equation coefficient
!     atc    - transport eq. coef
!     btc    - transport eq. coef
!     ctc    - transport eq. coef
!     eata   - n.d. rill erodibility parameter
!     tauc   - n.d. critical shear stress parameter
!     theta  - n.d. interrill erodibility parameter
!     phi    - n.d. deposition parameter
!     ilast  - counter value for last point where load computed
!     dl     - n.d. deposition rate
!     ldlast - n.d. sediment load calculated at i=ilast
!     xdbeg  - n.d. distance where deposition begins
!     ndep   - flag indicating if deposition begins in
!              detachment segment
!     xinput - distance on ofe where detachment is to be calculated
!     ktrato - n.d. sediment transport eqution coefficient
!     load   - n.d. sediment load at a point
!     tc     - sediment transport capacity at a point (kg/s/m)
!     detach - detachment at a point on ofe
!     qostar - n.d. flow discharge onto top of an ofe.
!
!     + + + local variables + + +
!
      real ldtry, ldrat, dx, xlast, tcap, xtry, xfrt, detfrt, dettry,   &
     &    detlst, ldrat2, tclast, xterm, xtrmtc, xx
      real dcap,shr,taucx,eatax
      integer ibeg, kflag, i, loopfg, currpt
      data xx /0.0/
!
!     + + + local variable definitions + + +
!
!     ldtry  - n.d. load returned from runge kutta procedure - it
!              is used as long as load is less than tcap
!     ibeg   - beginning point counter
!     dx     - delta x value sent to runge kutta procedure
!     xlast  - n.d. distance at last point where load was computed
!     dcap   - detachment capacity at a point
!     tcap   - transport capacity at a point
!     xtry   - iterative solution for distance to find point
!              where deposition begins in a detachment segment
!     xfrt   - n.d. distance at front point where deposition is
!              predicted to occur
!     detfrt - ratio at front point used to determine if detach.
!              or deposition conditions occur at x=xfrt
!     dettry - ratio relating sediment load and transport cap.
!              at x=xtry
!     detlst - ratio relating sediment load and transport cap.
!              at last point (x=xlast)
!     ldrat  - ratio relating sed. load and trans. cap. at point
!              used when tran. cap. > 0, and sed. load = 0
!     ldrat2 - ratio relating sed. load and trans. cap. at point
!              used when sediment load is greater than 0
!     tclast - transport capacity at x=xlast
!     kflag  - flag indicating type of hydrologic plane and
!              which ratio type to use
!     loopfg - flag. 1 = exit the l2 & n2 loops.
!     currpt - number of the current point when l2 loop exited.
!
!     + + + saves + + +
!     saves are not allowed!! these are globals disguised as locals
!
!      save xfrt, detlst, detfrt
!      save xx, eatax, taucx, shr, dcap
!
!     + + + subroutines called + + +
!
!     runge
!
!     + + + function declarations + + +
!
!      real cross
!
!     begin subroutine erod
!
      ldrat = 0.0
      ldrat2 = 0.0
      ndep = 0
      ibeg = ilast + 1
!     
!     verify that the beginning point has not exceeded the end of
!     the slope segment
!     
      if (ibeg.lt.102) then
         if (xinput(ibeg).le.xe) then
!           
!           loopfg is set to 1 to force an exit from the loop
!           
            loopfg = 0
            i = ilast
!           
   10       continue
!           
            i = i + 1
!           
            if (xinput(i).le.xe) then
!              
!              determine the nondimensional horizontal distance between
!              the beginning of the current slope segment and next of 
!              the 101 points of the plane
!              
!              also determine transport capacity at the beginning of 
!              the slope segment [equation 11.4.6]
!              
               if (i.le.ibeg) then
                  dx = xinput(i) - xb
                  xlast = xb
                  xterm = a * xb ** 2 + b * xb + c
                  xtrmtc = atc * xb ** 2 + btc * xb + ctc
                  tclast = xtrmtc * ktrato
                  if (tclast.lt.0.0) tclast = 0.0
!              
!              for all other points in the detachment segment, set 
!              the nondimensional horizontal distance increment to 
!              the default of 0.01 
!              
!              set distance and obtain load and transport capacity 
!              for the previous point
               else
                  dx = 0.01
                  xlast = xinput(i-1)
                  ldlast = load(i-1)
                  tclast = tc(i-1)
               end if
!              
!              calculate dimensionless shear stress at the current
!              point  [equation 11.4.1]
!              
               xterm = a * xinput(i) ** 2 + b * xinput(i) + c
               xtrmtc = atc * xinput(i) ** 2 + btc * xinput(i) + ctc
!              
               if (xterm.ne.xx) then
                  if (xterm.gt.0.0) then
                     shr = exp(0.666667*log(xterm))
                  else
                     shr = 0.0
                  end if
!                 
                  xx = xterm
!                 
!                 calculate detachment capacity at the current point
!                 [equations 11.2.3, 11.3.7, 11.3.8, and 11.4.1]
!                 
                  dcap = eata * (shr-tauc)
                  if (dcap.lt.0.0) dcap = 0.0
                  eatax = eata
                  taucx = tauc
               else if (eatax.ne.eata.or.taucx.ne.tauc) then
                  dcap = eata * (shr-tauc)
                  if (dcap.lt.0.0) dcap = 0.0
                  eatax = eata
                  taucx = tauc
               end if
!              
!              calculate dimensionless transport capacity at the current
!              point [equation 11.4.6]
!              
               tcap = xtrmtc * ktrato
               if (tcap.lt.0.0) tcap = 0.0
               tc(i) = tcap
!              
!              check whether on a case 4 plane past where runoff ends
!              if past point, set load equal to zero, and flag the 
!              plane (kflag = 4)
!              
               if (qostar.gt.-1.0.and.qostar.lt.0.0.and.                &
     &             xinput(i).gt.-qostar) then
                  load(i) = 0.0
                  kflag = 4
                  ndep = 0
!              
!              use runge kutta numerical procedure to solve for 
!              sediment load at the current point 
!              [nserl report 10; equation 11.3.13]
!              
               else
!                 
                  call runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,xlast,&
     &                ldlast,load(i),xx,eatax,taucx,shr,dcap,ktrato)
!                 
!                 if transport capacity at the current point is greater
!                 than zero, calculate ratio values used to test if
!                 current point is in deposition
!                 
!                 kflg = 1  indicates  tc > zero
!                 kflg = 2  indicates  load > zero
!                 
                  if (tcap.gt.0.0) then
                     ldrat = 1.0 - (load(i)/tcap)
                     kflag = 1
                     detach(i) = dcap * ldrat
!                    
                     if (load(i).gt.0.0) then
                        ldrat2 = (tcap/load(i)) - 1.0
                        kflag = 2
                     end if
!                 
!                 when transport capacity at the current point <= 
!                 zero - if load exceeds zero must use ldrat2 ratio
!                 
                  else
                     if (load(i).gt.0.0) then
                        ldrat2 = (tcap/load(i)) - 1.0
                        kflag = 2
!                    
!                    when both load and transport capacity at point 
!                    are zero (kflg = 3)
                     else
                        load(i) = 0.0
                        kflag = 3
                     end if
                  end if        ! if (tcap.gt.0.0) then
               end if   ! if (qostar.gt.-1.0.and.qostar.lt.0.0.and...)
!              
!              if deposition is predicted at current point, set flag
!              ndep = 1 and exit detachment calculations
!              
               if ((kflag.eq.2.and.ldrat2.lt.0.0).or.(kflag.eq.1.and.   &
     &             ldrat.lt.0.0)) then
                  ndep = 1
                  loopfg = 1
               else
                  ilast = i
               end if
!              
               if (xinput(i).ge.1.0) loopfg = 1
!           
            else
               loopfg = 1
            end if     ! if (xinput(i).le.xe) then
!           
!           if end of segment has not been reached and have not
!           encountered deposition then go back through the 
!           "if (xinput(ibeg.le.xe) then" loop
!           
            if (loopfg.eq.0.and.i.lt.102) go to 10
!           
!           remember number of current point
!           
            currpt = i
!           
!           on the last of the 101 ofe points in this segment, if
!           deposition is not occurring, compute load at the end of
!           the segment
!           
            if (ndep.eq.0) then
!              
!              on a segment where flow is present (not a case 4),
!              use runge kutta solution [page 11.6, section 11.3.5]
!              
               if (kflag.ne.4) then
                  if (xe.ne.xinput(ilast)) then
                     dx = xe - xinput(ilast)
                     call runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,   &
     &                   xinput(ilast),load(ilast),ldlast,xx,           &
     &                   eatax,taucx,shr,dcap,ktrato)
                     xlast = xe
                  else
                     ldlast = load(ilast)
                     xlast = xinput(ilast)
                  end if
!              
!              on a segment where flow is not present (case 4),
!              past where runoff ends, set load to zero and return 
!              to route
               else
                  ldlast = 0.0
                  xlast = xe
                  dl = 0.0
                  return
               end if
!              
               xterm = a * xlast ** 2 + b * xlast + c
               xtrmtc = atc * xlast ** 2 + btc * xlast + ctc
!              
               if (xterm.ne.xx) then
!                 
!                 calculate shear stress at end of segment (x=xe)
!                 [equation 11.4.1]
!                 
                  if (xterm.gt.0.0) then
                     shr = exp(0.666667*log(xterm))
                  else
                     shr = 0.0
                  end if
!                 
                  xx = xterm
!                 
!                 calculate detachment capacity at end of segment 
!                 (x=xe) [equation 11.2.3, and others]
!                 
                  dcap = eata * (shr-tauc)
                  if (dcap.lt.0.0) dcap = 0.0
                  eatax = eata
                  taucx = tauc
               else if (eatax.ne.eata.or.taucx.ne.tauc) then
                  dcap = eata * (shr-tauc)
                  if (dcap.lt.0.0) dcap = 0.0
                  eatax = eata
                  taucx = tauc
               end if
!              
!              calculate transport capacity at end of segment (x=xe)
!              [equation 11.4.6]
!              
               tcap = xtrmtc * ktrato
               if (tcap.lt.0.0) tcap = 0.0
!              
!              transport capacity greater than zero
!              
               if (tcap.gt.0.0) then
                  ldrat = 1.0 - (ldlast/tcap)
                  dl = dcap * ldrat
                  kflag = 1
!                 
!                 if load is less than transport capacity at end of 
!                 segment (still in detachment condition) then return 
!                 to route
!                 
                  if (ldrat.ge.0.0) return
!              
!              transport capacity is zero
!              
               else
!                 
!                 if load at end of segment is also zero then return 
!                 to route
!                 
                  if (ldlast.le.0.0) then
                     ldlast = 0.0
                     dl = 0.0
                     return
                  end if
!              
               end if
!              
!              set up the last point (xlast) and front point (xfrt) 
!              ratios used to determine where deposition begins 
!              (x=xdbeg)
!              
               ldrat2 = (tcap/ldlast) - 1.0
               kflag = 2
               detfrt = ldrat2
               if (load(ilast).gt.0.0) detlst = (tc(ilast)/load(ilast)) &
     &              - 1.0
               ndep = 1
               xfrt = xlast
!              
               if (xinput(ilast).eq.xfrt) then
                  xlast = xinput(ilast-1)
                  if (detfrt.eq.ldrat2) then
                     if (load(ilast-1).gt.0.0) detlst = (tc(ilast-1)/   &
     &                   load(ilast-1)) - 1.0
                  else
                     if (tc(ilast-1).gt.0.0) detlst = 1.0 - (           &
     &                   load(ilast-1)/tc(ilast-1))
                  end if
               else
                  xlast = xinput(ilast)
               end if
!           
            else
!              
!              on the last of the 101 points on this segment, 
!              deposition is occurring - compute where deposition 
!              begins (x=xdbeg)
!              
               xfrt = xinput(currpt)
!              
               if (xlast.le.0.0.and.tclast.le.0.0.and.ldlast.le.0.0)    &
     &            then
                  kflag = 5
                  detlst = dl
                  detfrt = (phi/(phi+1.0)) * (ktrato*(atc*xfrt*xfrt+btc*&
     &               xfrt+ctc)-theta)
               end if
!              
               if (kflag.eq.1) then
                  detfrt = ldrat
                  if (tclast.gt.0.0) then
                     detlst = 1.0 - (ldlast/tclast)
                  else
                     detlst = 0.0
                  end if
               else if (kflag.eq.2) then
                  detfrt = ldrat2
                  if (ldlast.gt.0.0) then
                     detlst = (tclast/ldlast) - 1.0
                  else
                     detlst = 0.0
                  end if
               end if
!              
!              if at top of flow plane and ratios are not positive at 
!              both the beginning and end of the segment, assume
!              deposition begins at top of the plane
!              
               if (detfrt.le.0.0.and.detlst.le.0.0.and.xlast.le.0.0)    &
     &            then
                  xdbeg = 0.0
                  return
               end if
!              
!              prevent a negative value for detachment at the last point
!              
               if (detlst.lt.0.0) detlst = 0.0
!           
            end if      ! if (ndep.eq.0) then
!           
!           iterative proceedure to find point where deposition begins
!           (x=xdbeg)
!           
            i = 0
!           
   20       i = i + 1
!           
!           use cross function to solve for point (x=xtry) where
!           tcap = ldtry, or where test ratios equal zero
!           
            xtry = cross(xlast,detlst,xfrt,detfrt)
            dx = xtry - xlast
!           
!           use runge kutta procedure to estimate load at x=xtry
!           
            call runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,xlast,      &
     &          ldlast,ldtry,xx,eatax,taucx,shr,dcap,ktrato)
!           
            tcap = (atc*xtry**2+btc*xtry+ctc) * ktrato
            if (tcap.lt.0.0) tcap = 0.0
!           
            loopfg = 0
!           
            if (kflag.eq.2) then
               if (ldtry.le.0.0) ldtry = 0.00001
               if (abs((tcap-ldtry)/ldtry).lt.0.001) then
                  loopfg = 1
               else
                  dettry = (tcap/ldtry) - 1.0
               end if
            else if (kflag.eq.1) then
               if (tcap.le.0.0) tcap = 0.00001
               if (abs((ldtry-tcap)/tcap).lt.0.001) then
                  loopfg = 1
               else
                  dettry = 1.0 - (ldtry/tcap)
               end if
            else if (kflag.eq.5) then
               dettry = (phi/(phi+1.0)) * (tcap-theta)
            end if
!           
            if (loopfg.eq.0) then
               if (dettry.le.0.0) then
                  detfrt = dettry
                  xfrt = xtry
               else
                  xlast = xtry
                  detlst = dettry
                  ldlast = ldtry
               end if
            end if
!           
            if (i.lt.10.and.loopfg.eq.0) go to 20
!           
!           
            xdbeg = xtry
            dl = 0.0
            ldlast = ldtry
!        
         end if ! if (xinput(ibeg).le.xe) then
      end if    ! if (ibeg.lt.102) then
!     
      return
      end
