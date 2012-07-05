      real function depend(xu,xl,a,b,cdep,phi,theta,ktrato,qostar)
      implicit none
!
!     function depend (for a segment of a flow plane that has 
!     deposition at the beginning) uses the analytic solution to the 
!     deposition equation to calculate where deposition ends
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine route
!
!     author(s): d.c flanagan and j.c. ascough ii
!     date last modified: 9-30-2004
!
!     + + + argument declarations + + +
!
      real, intent(in) :: xu, xl, a, b, cdep, phi, theta, ktrato, qostar
!
!     + + + argument definitions + + +
!
!     xu     - n.d. distance at top of slope segment
!     xl     - n.d. distance at end of slope segment
!     a      - shear stress equation coefficient
!     b      - shear stress equation coefficient
!     cdep   - portion of the solution to deposition equation
!     phi    - n.d. deposition parameter
!     theta  - n.d. interrill detachment parameter
!     ktrato - dimensionless transport coefficient
!     qostar - dimensionless flow discharge onto top of ofe
!
!     + + + local variables + + +
!
      real xdend, r1, r2, ratio, expon, f, df, xmin, tmpvr1
      integer j, kkkk, itmpv1, loopfg
!
!     + + + local variable definitions + + +
!
!     xdend  - current trial value for n.d. distance where
!              deposition ends
!     r1     - portion of solution to deposition equation solved
!              for conditions of rate equal to 0 and beginning
!              point equal to x=xu
!     r2     -   "       "          "          "        "
!     ratio  -   "       "          "          "        "
!     expon  -   "       "          "          "        "
!     f      - current solution for deposition rate at x=xdend
!     df     - current solution for derivative of deposition rate
!     j      - counter variable - in loop to only allow a finite
!              number of iterations to determine where dep. ends
!     loopfg - flag. 1 = exit l1 loop
!
!     + + + subroutines called + + +
!
!     undflo
!
!     begin subroutine depend
!
!     solve for first term of deposition equation
!
      tmpvr1 = 2.0 * a * ktrato
!     
      r1 = (phi/(1.0+phi)) * (b*ktrato-theta-(tmpvr1*qostar))
!     
!     solve for second term of deposition equation
!     
      r2 = tmpvr1 * phi / (2.0+phi)
!     
!     if flow is increasing down the ofe, check whether deposition
!     at the end of the current segment (uniform or convex shape)
!     
      if (qostar.ge.0.0) then
         xdend = xl
         ratio = (xu+qostar) / (xdend+qostar)
         expon = 1.0 + phi
!        
         call undflo(ratio,expon)
!        
!        determine deposition rate at x = xl (end of segment) 
!        return if a negative value deposition occurs from xu to xl
!        
         f = r1 + r2 * (xdend+qostar) + cdep * ratio ** expon
         if (f.lt.0.0) then
            depend = xdend
            return
         end if
!        
!        if deposition not occurring at x=xl, set beginning point for
!        iterative solution for xdend close to x=xu
!        
         xdend = xu + 0.01
         if (xdend.gt.xl) xdend = (xu+xl) / 2.0
!     
!     flow is decreasing down the segment - first check 
!     to determine that deposition does not immediately end
!     past the initial point (x=xu) and then set the
!     beginning point for the iterative solution for xdend
!     close to x = xu
!     
      else
!        
         if (abs(xu+qostar).le.0.0001) then
            depend = -qostar
            return
         end if
!        
         xdend = xu + 0.0001
         if (xdend.gt.xl) xdend = (xu+xl) / 2.0
         ratio = (xu+qostar) / (xdend+qostar)
         expon = 1.0 + phi
         call undflo(ratio,expon)
!        
!        determine deposition at x = xdend, and return if the value
!        is positive or zero
!        
         f = r1 + r2 * (xdend+qostar) + cdep * ratio ** expon
         if (f.ge.0.0) then
            depend = xdend
            return
         end if
!     
      end if    ! if (qostar.ge.0.0) then
!     
      loopfg = 0
      j = 0
      xmin = xl
      kkkk = 0
!     
   10 continue
!     
      j = j + 1
!     
!     iterative solution for xdend
!     
!     
!     solve for portions of the deposition equation
!     at the trial value of xdend
!     
      tmpvr1 = xdend + qostar
!     
      if (abs(tmpvr1).gt.0.0) then
         itmpv1 = 1
      else
         itmpv1 = 0
      end if
!     
      if (itmpv1.ne.0) then
         ratio = (xu+qostar) / tmpvr1
      else
         ratio = 1.0
      end if
!     
      if (ratio.lt.0.0) ratio = 1.0
      expon = 1.0 + phi
!     
      call undflo(ratio,expon)
!     
!     solve the deposition equation at trial value of xdend
!     
      f = r1 + r2 * (xdend+qostar) + cdep * ratio ** expon
!     
!     check added to flag if any positive values of f have been 
!     calculated between xu and xl, and then to record the point on 
!     slope where this was found
!     
      if (f.gt.0.0.and.qostar.lt.0.0) then
         kkkk = kkkk + 1
         if (xdend.lt.xmin) xmin = xdend
      end if
!     
!     if the nondimensional deposition rate is less than 0.0001 then
!     jump out of do loop and use the current value of xdend as the 
!     point where deposition is predicted to end
!     
      if (abs(f).gt.0.0001) then
!        
         if (itmpv1.ne.0) then
!           
!           solve for the derivative of the deposition function
!           
            df = r2 - (1.0+phi) * cdep * (ratio**expon) / tmpvr1
            if (abs(df).gt.0.0) then
!              
!              use the derivative to obtain a new trial value for xdend
!              
               xdend = xdend - f / df
!              
               if (qostar.lt.0.0) then
                  if (xdend.lt.xu) xdend = xu + 0.0001
                  if (xdend.gt.-qostar) xdend = -qostar - 0.0001
                  if (xdend.gt.xl) xdend = xl
               end if
            else
!              
!              if the derivative is zero at the point,
!              restart the iterations with xdend close to xu
!              
               xdend = xu + 0.0001
            end if
         end if
!        
         if (xdend.lt.xu) xdend = xu + 0.0001
      else
         loopfg = 1
      end if
!     
      if (j.lt.10.and.loopfg.eq.0) go to 10
!     
!     check if solution has not converged for a plane on which
!     flow is decreasing - if it has not and none of the trial
!     values of xdend between xu and xl (or -qostar) have produced
!     non-negative results then set xdend equal to xl
!     
      if (loopfg.eq.0.and.qostar.lt.0.0) then
         if (kkkk.eq.0) then
            xdend = xl
         else
            xdend = xmin
         end if
      end if
!     
      depend = xdend
!     
      return
      end
