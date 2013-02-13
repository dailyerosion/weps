!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine enrich(kk,xtop,xbot,xdetst,ldtop,ldbot,lddend,theta,   &
     &    iendfg,slplen,ktrato,qin,qout,qostar,ainftc,binftc,cinftc,    &
     &    npart,frac,fall,frcly,frslt,frsnd,frorg,sand,silt,clay,orgmat,&
     &    fidel,tcf1,frcflw,enrato)

      implicit none 
!
!     subroutine enrich computes the new particle size distribution 
!     of sediment in runoff following routing through a deposition 
!     region - the equation is an analytical solution of the 
!     nondimensional sediment load equation for a depositional region
!
!     module adapted from wepp version 2004.7 and called from 
!     subroutine route
!
!     author(s): d.c. flanagan and j.c. ascough ii
!     date last modified: 4-1-2005
!
!     + + + parameter declarations + + +
!
      include 'wepp_erosion.inc'
!
!     + + + argument declarations + + +
!
      integer, intent(in) :: kk, iendfg, npart
      real, intent(in) :: xtop, xbot, xdetst, theta, ldtop,ldbot,lddend,&
     &     slplen, ktrato, qin, qout, qostar, ainftc(mxslp),            & 
     &     binftc(mxslp), cinftc(mxslp), frac(mxpart), fall(mxpart),    &
     &     frcly(mxpart), frslt(mxpart), frsnd(mxpart), frorg(mxpart),  &
     &     fidel(mxpart), tcf1(mxpart),                                 &
     &     sand(mxnsl), silt(mxnsl), clay(mxnsl), orgmat(mxnsl)  
	  real, intent(inout) :: frcflw(mxpart)    
	  real, intent(out) ::  enrato
!
!     + + + argument definitions + + +
!
!     kk     - slope segment number
!     xtop   - nondimensional horizontal distance where
!              deposition begins
!     xbot   - nondimensional horizontal distance where
!              deposition ends
!     xdetst - normalized distance at top of detachment region
!     ldtop  - nondimensional sediment load at xtop
!     ldbot  - nondimensional sediment load at xbot
!     lddend - nomalized sediment load at top of detachment
!              region
!     theta  - nondimensional interrill detachment parameter
!     iendfg - flag to indicate last call to enrich at the end
!              of a flow plane
!     slplen - length of each ofe (m)
!     ktrato - dimensionless sed. tranport eqn. coefficient
!     qin    - flow discharge per unit width (m^3/m*s) at top of ofe
!     qout   - flow discharge per unit width (m^3/m*s) at end of ofe
!     qostar - nondimensional discharge of water onto an ofe
!     ainftc - n.d. sediment transport coefficient
!     binftc - n.d. sediment transport coefficient
!     cinftc - n.d. sediment transport coefficient
!     npart  - number of sediment particle classes
!     frac   - fraction of sediment in each size class at the point
!     fall   - sediment fall velocity for each size class (m/s)
!     frcly  - fraction of clay in each size class
!     frslt  - fraction of silt in each size class
!     frsnd  - fraction of sand in each size class
!     frorg  - fraction of organic matter in each size class
!     fidel  - fraction of each particle class after interrill sorting
!     tcf1   - fraction of total sed. transport capacity for a class
!     frcflw - fraction of sediment in each size class in flow,
!              usually reported as output for the end of the hillslope
!     enrato - sediment enrichment ratio calculated for the hillslope
!
!
!     + + + local variables + + +
!
      real beta, ratio, expon, ratio2, coef1, coef2, term1, term2,      &
     &    term3, term4a, term4b, ratbot, gadd, aa, bb, cc, pkro,        &
     &    gu(mxpart), gend(mxpart), phi, ssasol, sumg, ssasnd, ssaslt,  & 
     &    ssacly, ssaorg, sedmax(mxpart), ssased, sumssa,               &
     &    ftheta(mxpart), intlod, rillod, tmpvr1, tmpvr2, tmpvr3,       &
     &    tmpvr4, tmpvr5
      integer i, iiflag, phiflg
!
!     + + + local variable definitions + + +
!
!     beta   - raindrop induced turbulence coefficient
!              (equation 11.2.4)
!     sumssa - variable used to sum ssa for sediment
!              (equation 11.5.5)
!     sumg   - variable used to sum sediment loads predicted
!              by equation 11.5.1
!     ssasnd - specific surface area of sand in m**2/g
!     ssaslt - specific surface area of silt in m**2/g
!     ssacly - specific surface area of clay in m**2/g
!     ssaorg - specific surface area of organic carbon in
!              m**2/g
!     aa     - adjusted a transport coefficient for particle
!              class i
!     bb     - adjusted b transport coefficient for particle
!              class i
!     cc     - adjusted c transport coefficient for particle
!              class i
!     ftheta - fraction of interrill detachment attributed to
!              particle class i
!     gu     - nondimensional sediment load for particle
!              class i at x=xtop
!     intlod - interrill load contribution from detachment region
!              above deposition region
!     ratio  - ratio of x terms in the last component of
!              equation 11.5.1
!     phi    - nondimensional deposition parameter for a
!              particle class i (equation 11.3.11)
!     expon  - phi correction variable to prevent machine
!              overflow
!     ratio2 - ratio correction variable to prevent machine
!              overflow
!     coef1  - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     coef2  - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     term1  - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     term2  - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     term3  - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     term4a - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     term4b - coefficients and terms of equations 11.5.1 and
!              11.5.2
!     gend(mxpart)   : predicted nondimensional sediment load for the
!                      particle class i at x=xbot
!     sedmax(mxpart) : maximum possible sediment load in a particle
!                      class i at x=xbot
!     ratbot - running sum of sediment in classes that have
!              not exceeded sedmax
!     iiflag - flag to indicate if reproportioning among
!              classes should be calculated
!     gadd   - additional sediment load added to particle
!              class i during reporportioning
!     ssased - specific surface area for particle class i in
!              the sediment
!     ssasol - total specific surface area for the insitu
!              soil on current plane
!     i      - counter variable used to indicate particle
!              class
!
!     + + + subroutines called + + +
!
!     undflo
!
!     begin subroutine enrich
!
!     change to computation of beta made to be consistent with similar 
!     computation in sr param
!
!     set beta value to 0.5 - assume these computations are always
!     under rainfall and shallow flow conditions
!
      beta = 0.5
!     
      sumssa = 0.0
      sumg = 0.0
!     
!     the specific surface area values for sand, silt, clay,
!     and organic carbon are those used in the creams model,
!     the values given below have units of (m^2)/g
!     
      ssasnd = 0.05
      ssaslt = 4.0
      ssacly = 20.0
      ssaorg = 1000.0
!     
!     at the beginning of a depositional section (or the last time
!     through at the end of a plane), compute the fraction of each
!     particle type using a weighted average of sediment inputs
!     from last depositional region and the erosional region between
!     
      if (ldtop.gt.0.00001.and.qout.gt.0.0) then
         intlod = theta * (xtop-xdetst)
         rillod = ldtop - lddend - intlod
         if (rillod.lt.0.0) rillod = 0.0
!        
         do i = 1, npart
            frcflw(i) = (frcflw(i)*lddend+frac(i)*                      &
     &          rillod+fidel(i)*intlod) / ldtop
         end do
      end if
!     
!     determine if this is the last call at the end of a plane 
!     (iendfg=1)
!     
      if (iendfg.ne.0) then
!        
         do i = 1, npart
            if (qout.le.0.0) frcflw(i) = 0.0
!           
!           calculate the specific surface area of the sediment
!           
            ssased = frcflw(i) * ((frsnd(i)*ssasnd +                    &
     &          frslt(i)*ssaslt + frcly(i)*ssacly) / (1.0 + frorg(i))   &
     &          + frorg(i)*ssaorg/1.73)
            sumssa = sumssa + ssased
         end do
!        
!        calculate the specific surface area of the surface soil
!        for the current plane
!        
         ssasol = (orgmat(1)*ssaorg/1.73) + (sand(1)*ssasnd +           &
     &            silt(1)*ssaslt + clay(1)*ssacly) / (1.0 + orgmat(1))
!        
!        calculate an enrichment ratio of the specific surface area
!        
         enrato = sumssa / ssasol + 0.005
!     
      else
!        
!        if there is water flowing off of the overland flow elements,
!        then calculate the particle sorting and enrichment ratio
!        (case 2 and 3 hydrologic planes)
!        
         if (qout.gt.0.0) then
            pkro = (qout-qin) / slplen
!           
!           proportion transport capacity to the individual size 
!           classes and calculate the sediment load at the end of the 
!           depositional region for each size class
!           
            tmpvr2 = xbot + qostar
            tmpvr3 = xtop + qostar
            tmpvr4 = tmpvr2 ** 2
            tmpvr5 = tmpvr3 ** 2
!           
            if (abs(pkro).gt.1e-15) then
               phiflg = 1
            else if (qostar.ge.0.0) then
               phiflg = 2
            else
               phiflg = 3
            end if
!           
            do i = 1, npart
               tmpvr1 = ktrato * tcf1(i)
               aa = tmpvr1 * ainftc(kk)
               bb = tmpvr1 * binftc(kk)
               cc = tmpvr1 * cinftc(kk)
               ftheta(i) = fidel(i) * theta
               gu(i) = frcflw(i) * ldtop
!              
               if (phiflg.eq.1) then
                  phi = beta * fall(i) / pkro
                  if (phi.gt.100000.) phi = 100000.
                  if (phi.lt.-100000.) phi = -100000.
               else if (phiflg.eq.2) then
                  phi = 100000.
               else
                  phi = -100000.
               end if
!              
               ratio = tmpvr3 / tmpvr2
!              
               if (qostar.ge.0.0.and.ratio.gt.1.0) ratio = 1.0
               expon = phi
               ratio2 = ratio
               call undflo(ratio2,expon)
!              
               coef1 = phi * aa / (phi+2.0)
               coef2 = (phi*bb+ftheta(i)-2.0*aa*phi*qostar) / (1.0+phi)
               term1 = coef1 * tmpvr4
               term2 = coef2 * tmpvr2
               term3 = aa * qostar ** 2 - bb * qostar + cc
!              
               term4a = ratio2 ** expon
               if (term4a.lt.1.0e-08) term4a = 0.0
               term4b = gu(i) - coef1 * tmpvr5 - coef2 * tmpvr3 - term3
               gend(i) = term1 + term2 + term3 + term4a * term4b
               if (gend(i).lt.0.0) gend(i) = 0.0
               sumg = sumg + gend(i)
!           
            end do
!           
!           correction added if enrichment routine calculates that 
!           there is no load at all at end of region - must still 
!           provide a size distribution in order to agree with other 
!           part of model calculations that predict a sediment load
!           
!           this likely will only happen when transport capacity
!           is zero and there is no interrill detachment on the plane -
!           in this case return to sr route and assume that the 
!           particle size distribution exiting the deposition region 
!           is the same that entered
!           
            if (sumg.gt.0.0) then
!              
!              adjust the individual fraction sediment loads at
!              the end of the section so that they total to the
!              load computed in the deposition routines elsewhere
!              in the model
!              
!              also, compute the maximum possible sediment load in a 
!              particle class which is the sum of the sediment 
!              entering at the top (gu(i)) plus the interrill 
!              contribution (ftheta(i)*(xbot-xtop))
!              
               do i = 1, npart
                  gend(i) = gend(i) * ldbot / sumg
                  sedmax(i) = gu(i) + ftheta(i) * (xbot-xtop)
                  if (gend(i).lt.1.0e-15) gend(i) = 1.0e-15
               end do
!              
!              check that the mass of sediment in any particle class 
!              does not exceed the total possible mass in that class 
!              which is the sum of the sediment entering the segment 
!              at the top (gu(i)) plus the detached interrill sediment
!              (ftheta(i)*(xbot - xtop)) - if it does then set the mass
!              in the class to sedmax, and then reproportion the
!              leftover mass to the remaining particle classes
!              
   10          ratbot = 0.0
               sumg = 0.0
               iiflag = 0
!              
               do i = 1, npart
                  if (gend(i).gt.sedmax(i)) then
                     gend(i) = sedmax(i)
                     iiflag = 1
                  else if (gend(i).lt.sedmax(i)) then
                     ratbot = ratbot + gend(i)
                  end if
                  sumg = sumg + gend(i)
               end do
!              
!              if at least one of the particle classes has been
!              set with gend(i) = sedmax(i) then the other classes that
!              have not been set gend(i) = sedmax(i) are reproportioned
!              
               if (iiflag.ne.0) then
                  do i = 1, npart
                     if (gend(i).lt.sedmax(i)) then
                        gadd = (ldbot-sumg) * gend(i) / ratbot
                        gend(i) = gend(i) + gadd
                     end if
                  end do
               end if
!              
               if (iiflag.ne.0) go to 10
!              
!              compute fraction at the end of the depositional region
!              
               if (sumg.gt.0.0) then
                  do i = 1, npart
                     frcflw(i) = gend(i) / sumg
                  end do
               else
                  do i = 1, npart
                     frcflw(i) = 0.0
                  end do
               end if
!           
            end if
!        
!        if flow ends on the plane (no outflow), set all fractions 
!        to zero since no sediment is leaving (case 4 hydrologic 
!        plane)
!        
         else
            do i = 1, npart
               frcflw(i) = 0.0
            end do
!        
         end if ! if (qout.gt.0.0) then
!     
      end if    ! if (iendfg.ne.0) then 
!     
      return
      end
