      subroutine runge(a,b,c,atc,btc,ctc,eata,tauc,theta,dx,x,ldold,    &
     &    ldnew,xx,eatax,taucx,shr,dcap,ktrato)
     
      use wepp_interface_defs
      
      implicit none
!
!     subroutine runge performs runge-kutta iteration
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine erod
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: atc, btc, ctc, a, b, c, ktrato
	  real, intent(in) :: eata, tauc, theta, dx, ldold, x
      real, intent(out) :: dcap, ldnew 
      real, intent(inout) ::  xx, eatax, taucx, shr
     
!
!     + + + argument definitions + + +
!
!     a      - shear stress equation coefficient
!     b      - shear stress equation coefficient
!     c      - shear stress equation coefficient
!     eata   - n.d. rill erodibility parameter
!     tauc   - n.d. critical shear stress parameter
!     theta  - n.d. interrill erodibility parameter
!     dx     -
!     x      -
!     ldold  - n.d. sediment load calculated at i=ilast
!              detachment segment
!     ldnew  -
!     xx     - n.d. distance at last point calculated
!     eatax  - eata value at last point
!     taucx  - tauc value at last point
!     shr    - n.d. shear stress value at last point 
!     dcap   - n.d. detachment capacity value at last point
!     ktrato - dimensionless sed. transport equation coefficient
!
!
!     + + + saves + + +
!
!     save xx,eatax,taucx,shr,dcap
!
!     + + + local variables + + +
!
      real ldrk, k1, k2, k3, k4, tmpvr, tcap, xrk, xterm, xtrmtc
      real ldtest
!
!     + + + local variable definitions + + +
!
!     xrk   -
!     ldrk  -
!     tcap  - uniform slope
!     k1    - \
!     k2    -  \
!     k3    -   > constants used to compute ldnew
!     k4    -  /
!
!     begin subroutine runge
!
!     compute k1 constant
!
      xrk = x
      ldrk = ldold
!     
      xterm = a * xrk ** 2 + b * xrk + c
      xtrmtc = atc * xrk ** 2 + btc * xrk + ctc
!     
      if (xterm.ne.xx) then
!        
!        update shr and save xterm
!        
         if (xterm.gt.0.0) then
            shr = exp(0.666667*log(xterm))
         else
            shr = 0.0
         end if
!        
         xx = xterm
!        
!        update dcap and save eata and tauc
!        
         dcap = eata * (shr-tauc)
         if (dcap.lt.0.0) dcap = 0.0
         eatax = eata
         taucx = tauc
!     
      else if (eatax.ne.eata.or.taucx.ne.tauc) then
!        
!        update dcap and save eata and tauc
!        
         dcap = eata * (shr-tauc)
         if (dcap.lt.0.0) dcap = 0.0
         eatax = eata
         taucx = tauc
      end if
!     
      tcap = xtrmtc * ktrato
      if (tcap.lt.0.0) tcap = 0.0
!     
      if (tcap.gt.0.0) then
         tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
         tmpvr = theta
      end if
!     
      k1 = dx * tmpvr
!     
!     compute k2 constant
!     
      xrk = x + dx / 2.0
      ldrk = ldold + 0.5 * k1
!     
      xterm = a * xrk ** 2 + b * xrk + c
      xtrmtc = atc * xrk ** 2 + btc * xrk + ctc
!     
      if (xterm.ne.xx) then
!        
!        update shr and save xterm
!        
         if (xterm.gt.0.0) then
            shr = exp(0.666667*log(xterm))
         else
            shr = 0.0
         end if
!        
         xx = xterm
!        
!        update dcap and save eata and tauc
!        
         dcap = eata * (shr-tauc)
         if (dcap.lt.0.0) dcap = 0.0
         eatax = eata
         taucx = tauc
!     
      else if (eatax.ne.eata.or.taucx.ne.tauc) then
!        
!        update dcap and save eata and tauc
!        
         dcap = eata * (shr-tauc)
         if (dcap.lt.0.0) dcap = 0.0
         eatax = eata
         taucx = tauc
      end if
!     
      tcap = xtrmtc * ktrato
      if (tcap.lt.0.0) tcap = 0.0
!     
      if (tcap.gt.0.0) then
         tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
         tmpvr = theta
      end if
!     
      k2 = dx * tmpvr
!     
!     compute k3 constant
!     
      ldrk = ldold + 0.5 * k2
!     
      if (tcap.gt.0.0) then
         tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
         tmpvr = theta
      end if
!     
      k3 = dx * tmpvr
!     
!     compute k4 constant
!     
      xrk = x + dx
      ldrk = ldold + k3
!     
      xterm = a * xrk ** 2 + b * xrk + c
      xtrmtc = atc * xrk ** 2 + btc * xrk + ctc
!     
      if (xterm.ne.xx) then
!        
!        update shr and save xterm
!        
         if (xterm.gt.0.0) then
            shr = exp(0.666667*log(xterm))
         else
            shr = 0.0
         end if
!        
         xx = xterm
!        
!        update dcap and save eata and tauc
!        
         dcap = eata * (shr-tauc)
         if (dcap.lt.0.0) dcap = 0.0
         eatax = eata
         taucx = tauc
!     
      else if (eatax.ne.eata.or.taucx.ne.tauc) then
!        
!        update dcap and save eata and tauc
!        
         dcap = eata * (shr-tauc)
         if (dcap.lt.0.0) dcap = 0.0
         eatax = eata
         taucx = tauc
      end if
!     
      tcap = xtrmtc * ktrato
!     
      if (tcap.gt.0.0) then
         tmpvr = dcap * ((tcap-ldrk)/tcap) + theta
      else
         tmpvr = theta
      end if
!     
      k4 = dx * tmpvr
!     
      ldnew = ldold + (k1+2.0*k2+2.0*k3+k4) / 6.0
!     
!     add check to prevent use of negative sediment loads if
!     they are predicted by runge-kutta method because the
!     step size (x dimension) is too large
!     
!     a better solution is needed in which the routine will check 
!     and redo the computations using a smaller "dx" value should 
!     negative values of the "k1", "k2", "k3", and "k4" test loads be
!     computed
!     
!     for now, check to see if the new load predicted is less than 
!     the old load plus the interrill contribution across the "dx" 
!     distance - if it is then set the new load equal to this sum
!     
      ldtest = ldold + theta * dx
!     
      if (ldnew.lt.ldtest) then
         ldnew = ldtest
      end if
!     
      return
      end
