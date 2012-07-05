      subroutine route(qin,qout,qostar,strldn,ktrato,ainf,binf,         &
     &    cinf,ainftc,binftc,cinftc,npart,frac,frcly,frslt,frsnd,frorg, &
     &    fall,frcflw,nslpts,xinput,xu,xl,load,enrato,tcf1,fidel,sand,  &
     &    silt,clay,orgmat,eata,tauc,theta,phi,slplen)
     
      use wepp_interface_defs
!
!     + + + purpose + + +
!     calculates detachment (when shear stress exceeds critical shear
!     stress) or deposition at the upper end of the slope segment and
!     routes sediment through the hillslope profile.
!
!     called from subroutine main
!     author(s): d. flanagan, m. nearing, g. foster
!
!     version: this module taken largely from wepp v2004.7 code
!     date last modified:  4-1-2005
!     coded by: d. flanagan
!
!     + + + parameter declarations + + +
!
      include 'wepp_erosion.inc'
!
!     + + + argument declarations + + +
      real, intent(in):: qin, qout, qostar,strldn,ktrato
      real, intent(in):: ainf(mxslp), binf(mxslp), cinf(mxslp)

     
      real, intent(in) :: ainftc(mxslp), binftc(mxslp), cinftc(mxslp)
      real, intent(in) :: frac(mxpart), frcly(mxpart), frslt(mxpart)
	  real, intent(in) :: frsnd(mxpart)
      real, intent(in) :: frorg(mxpart), fall(mxpart)
	  real, intent(in) :: fidel(mxpart)
      real, intent(inout) :: xinput(101)
	  real, intent(out) :: enrato
      real, intent(in) :: sand(mxnsl), silt(mxnsl), clay(mxnsl)
	  real, intent(in) :: orgmat(mxnsl)
      real, intent(in) :: eata, tauc, theta, phi, slplen, tcf1(mxpart)
	  real, intent(out) ::  load(101), frcflw(mxpart)
	  real, intent(inout) :: xu(mxslp), xl(mxslp)
      integer, intent(in) :: npart, nslpts
!
!     + + + argument definitions + + +
!
!     qin     - flow discharge per unit width (m^3/s*m) at ofe top
!     qout    - flow discharge per unit width (m^3/s*m) at ofe bottom
!     qostar  - nondimensional influx of water onto top of ofe
!     strldn  - nondimensional sediment load at top of current ofe
!     ktrato  - nondimensional sediment transport capacity ratio
!               (from nserl report 11 - equation 11.3.6)
!     ainf    - nondimensional shear stress coefficient
!     binf    - nondimensional shear stress coefficient
!     cinf    - nondimensional shear stress coefficient
!     ainftc  - nondimensional transport coefficient
!     binftc  - nondimensional transport coefficient
!     cinftc  - nondimensional transport coefficient
!     frac    - fraction of sediment in each size class at the
!               point of initial detachment
!     frcly   - fraction of clay in each sediment size class
!     frslt   - fraction of silt in each sediment size class
!     frsnd   - fraction of sand in each sediment size class
!     frorg   - fraction of organic matter in each sediment size class
!     fall    - fall velocity of each sediment size class (m/s)
!     frcflw  - fraction of sediment in each size class at some
!               point in the rill flow (reported in output at end
!               of hillslope.
!     xinput  - dimensionless distance at points down slope
!     xu      - dimensionless distance at upper end of current section
!     xl      - dimensionless distance at lower end of current section
!     load    - nondimensional sediment load at each point down ofe
!     fidel   - fraction of each particle size class after interrill
!               sorting - calculated in param.for
!     tcf1    - fraction of sediment transport capacity for each
!               particle size class (computed in yalin.for)
!     enrato  - enrichment ratio of the specific surface area of the
!               sediment.  a value of enrato is computed at the end
!               of every ofe for each storm event.
!     npart   - number of particle size classes
!     
!
!     + + + local variables + + +
      real ldlast, loadup, lddend, xdend, xdbeg, xdetst, cdep, dl, du,  &
     &    xc1, xc2, tc(101), detach(101)
      integer i, iendfg, ilast, jj, k, mshear, ndep
!
!     ldlast - n.d. sediment load calculated at last point
!     loadup - n.d. load computed entering deposition region - value
!              is sent and used in subroutine enrich.f
!     lddend - n.d. load computed at end of previous deposition
!              region or overland flow element (i.e., load at xdetst)
!     xdend  - n.d. distance where deposition computed to end
!     xdbeg  - n.d. distance where deposition begins on a segment
!     xdetst - n.d. distance at start of previous detachment area
!     ilast  - counter index variable at last point
!     iendfg - flag to subroutine enrich.f indicating last call at
!              end of an overland flow element
!     dl     - n.d. deposition rate
!     du     - n.d. deposition rate at the top of a segment
!     cdep   - portion of solution to deposition equation
!     erd    - n.d. rill erodibility parameter sent to erod.f
!              if shear < critical erd=0, otherwise erd=eata
!     mshear - flag indicating what shear conditions exist on segment
!
!     + + + subroutines called + + +
!     xcrit
!     depos
!     erod
!
!     + + + function declarations + + +
!      real depc
!      real depend
!
!     + + + end specifications + + +
!
!
!*********************************************************************
!
! ... initialize erosion variables at the top of an ofe.
!
      lddend = strldn
      ldlast = strldn
      ilast = 1
      ndep = 0
      iendfg = 0
      xdbeg = 0.0
      xdetst = 0.0
!     
!     ... set sediment loads and transport capacity at each point,
!     equal to zero.
!     
      do 10 jj = 1, 101
         load(jj) = 0.0
         tc(jj) = 0.0
   10 continue
!     
!     ... set distance, transport capacity, and sediment load at
!     first point on ofe.
!     
      xu(2) = 0.0
      tc(1) = cinftc(2) * ktrato
      load(1) = strldn
!     
!     ... initialize particle fractions in the flow at top of ofe
!     with no inflow from previous ofe.
!     
      if (qout.gt.0.0) then
         if (qin.le.0.0) then
            do 20 i = 1, npart
               frcflw(i) = frac(i)
   20       continue
         else
!           
!           initialize particle fractions in the flow for ofe's
!           with inflow from previous ofe.
!           note - changing to a single flow element requires
!           the fractions in flow from previous element to be
!           read in as input.  dcf - 4/1/2005
!
!            this needs to be passed as parms or something else, for
!            weps we only have 1 ofe so don't worry about it
!            read(7,*) (frcflw(i),i = 1, npart)
         end if
      else
         do 40 i = 1, npart
            frcflw(i) = 0.0
   40    continue
      end if
!     
!******************************************************************
!     upper boundary condition for overland flow elements
!     
!     determine if deposition is occuring at x=0 on ofe.
!     if qostar = 0.0 (no inflow) estimate dl from deposition equation
!     else, estimate dl from the incoming sediment load and flow rate
!     
      if (abs(qostar).lt..0011) then
         dl = phi / (phi+1.0) * (ktrato*binftc(2)-theta)
      else
         dl = phi / qostar * (ktrato*cinftc(2)-ldlast)
      end if
!     
!     
!     **************************************************************
!     *** for each slope segment within an overland flow element ***
!     **************************************************************
!     perform detachment and deposition calculations.
!     
!     *** start of big do-loop ***
!     
      do 170 k = 2, nslpts
!        
!        for a case 4 plane - bypass all calculations if flow
!        has ended before current segment
!        *** start of big if ***
         if (qout.gt.0.0.or.xu(k).lt.-qostar) then
!           
!           if a case 2 or 3 plane, or a case 4 plane segment
!           on which runoff does not end.
            if (qout.gt.0.0.or.(qout.le.0.0.and.xl(k).lt.-qostar))then
!              
!              calculate shear conditions in segment, then
!              find where shear equals critical shear for segment.
               call xcrit(ainf(k),binf(k),cinf(k),tauc,xu(k),           &
     &              xl(k),xc1,xc2,mshear)
!           
!           else for a case 4 plane segment on which the flow ends
            else
               call xcrit(ainf(k),binf(k),cinf(k),tauc,xu(k),           &
     &             -qostar,xc1,xc2,mshear)
            end if
!           
!           determine if there is deposition at the beginning of the
!           segment - if there is - calculate where deposition ends
!           
            du = dl
!           
!           *** l1 if ***
            if (du.lt.0.) then
!              
!              deposition at upper end of segment
!              
               cdep = depc(xu(k),ainftc(k),binftc(k),phi,theta,         &
     &             du,ktrato,qostar)
!              
!              check for deposition ending within segment
!              
               xdend = depend(xu(k),xl(k),ainftc(k),                    &
     &            binftc(k),cdep,phi,theta,ktrato,qostar)
!              
!              deposition does not end
!              
!              *** l2 if ***
               if (xdend.ge.xl(k)) then
                  xdend = xl(k)
!                 
                  loadup = ldlast
                  call depos(xu(k),xdend,cdep,ainftc(k),                &
     &                binftc(k),cinftc(k),phi,theta,ilast,dl,ldlast,    &
     &                xinput,ktrato,detach,load,tc,qostar)
                  ndep = 0
                  if (ldlast.gt.0.0.and.qout.gt.0.0) then
                     call enrich(k,xu(k),xdend,xdetst,loadup,           &
     &                   ldlast,lddend,theta,iendfg,slplen,ktrato,qin,  &
     &                   qout,qostar,ainftc,binftc,cinftc,npart,frac,   &
     &                   fall,frcly,frslt,frsnd,frorg,sand,silt,clay,   &
     &                   orgmat,fidel,tcf1,frcflw,enrato)
                     lddend = ldlast
                     xdetst = xdend
                  end if
!              *** l2 else ***
               else
!                 
!                 deposition ends in segment
!                 
                  loadup = ldlast
                  call depos(xu(k),xdend,cdep,ainftc(k),                &
     &                binftc(k),cinftc(k),phi,theta,ilast,dl,ldlast,    &
     &                xinput,ktrato,detach,load,tc,qostar)
                  ndep = 0
                  if (ldlast.gt.0.0.and.qout.gt.0.0) then
                     call enrich(k,xu(k),xdend,xdetst,loadup,           &
     &                   ldlast,lddend,theta,iendfg,slplen,ktrato,qin,  &
     &                   qout,qostar,ainftc,binftc,cinftc,npart,frac,   &
     &                   fall,frcly,frslt,frsnd,frorg,sand,silt,clay,   &
     &                   orgmat,fidel,tcf1,frcflw,enrato)
                     lddend = ldlast
                     xdetst = xdend
                  end if
!                 
!                 
!                 detachment after deposition
!                 
!                 
                  go to (50,60,70,80,90)mshear
!                 
!                 shear below critical in entire segment.
!                 ** mshear = 1 **
   50             continue
                  call erod(xdend,xl(k),ainf(k),binf(k),cinf(k),        &
     &                ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi, &
     &                ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc, &
     &                detach,qostar)
                  go to 100
!                 
!                 shear exceeds critical in entire segment.
!                 ** mshear = 2 **
   60             continue
                  call erod(xdend,xl(k),ainf(k),binf(k),cinf(k),        &
     &                ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,&
     &                ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc, &
     &                detach,qostar)
                  go to 100
!                 
!                 shear increases downslope and exceeds critical at x=xc1.
!                 ** mshear = 3 **
   70             continue
                  if (xdend.le.xc1) then
                     call erod(xdend,xc1,ainf(k),binf(k),cinf(k),       &
     &                   ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,  &
     &                   phi,ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,  &
     &                   load,tc,detach,qostar)
                     if (ndep.eq.0) call erod(xc1,xl(k),ainf(k),        &
     &                   binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k), &
     &                   eata,tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep,&
     &                   xinput,ktrato,load,tc,detach,qostar)
                  else
                     call erod(xdend,xl(k),ainf(k),binf(k),             &
     &                   cinf(k),ainftc(k),binftc(k),cinftc(k),eata,    &
     &                   tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep,     &
     &                   xinput,ktrato,load,tc,detach,qostar)
                  end if
                  go to 100
!                 
!                 shear decreases downslope and drops below
!                 critical at x=xc1.
!                 ** mshear = 4 **
   80             continue
                  if (xdend.le.xc1) then
                     call erod(xdend,xc1,ainf(k),binf(k),cinf(k),       &
     &                   ainftc(k),binftc(k),cinftc(k),eata,tauc,theta, &
     &                   phi,ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,  &
     &                   load,tc,detach,qostar)
                     if (ndep.eq.0) call erod(xc1,xl(k),ainf(k),        &
     &                   binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k), &
     &                   0.0,tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep, &
     &                   xinput,ktrato,load,tc,detach,qostar)
                  else
                     call erod(xdend,xl(k),ainf(k),binf(k),             &
     &                   cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,tauc,&
     &                   theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput,   &
     &                   ktrato,load,tc,detach,qostar)
                  end if
                  go to 100
!                 
!                 shear increases down slope -- exceeds critical at x=xc1,
!                 then decreases from xc1 to xc2, and drops below critical
!                 at xc2.
!                 ** mshear = 5 **
   90             continue
                  if (xdend.le.xc1) then
                     call erod(xdend,xc1,ainf(k),binf(k),cinf(k),       &
     &                   ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,  &
     &                   phi,ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,  &
     &                   load,tc,detach,qostar)
                     if (ndep.eq.0) then
                        call erod(xc1,xc2,ainf(k),binf(k),cinf(k),      &
     &                      ainftc(k),binftc(k),cinftc(k),eata,tauc,    &
     &                      theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput,&
     &                      ktrato,load,tc,detach,qostar)
                        if (ndep.eq.0) call erod(xc2,xl(k),             &
     &                      ainf(k),binf(k),cinf(k),ainftc(k),          &
     &                      binftc(k),cinftc(k),0.0,tauc,theta,phi,     &
     &                      ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,   &
     &                      load,tc,detach,qostar)
                     end if
                  else if (xdend.gt.xc2) then
                     call erod(xdend,xl(k),ainf(k),binf(k),             &
     &                   cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,tauc,&
     &                   theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput,   &
     &                   ktrato,load,tc,detach,qostar)
                  else
                     call erod(xdend,xc2,ainf(k),binf(k),cinf(k),       &
     &                   ainftc(k),binftc(k),cinftc(k),eata,tauc,theta, &
     &                   phi,ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,  &
     &                   load,tc,detach,qostar)
                     if (ndep.eq.0) call erod(xc2,xl(k),ainf(k),        &
     &                   binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k), &
     &                   0.0,tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep, &
     &                   xinput,ktrato,load,tc,detach,qostar)
                  end if
  100             continue
!              *** l2 endif ***
               end if
!           
!           detachment at upper end of segment
!           
!           *** l1 else ***
            else
!              
               dl = 0.0
               du = 0.0
!              
               go to (110,120,130,140,150)mshear
!              
!              shear below critical in entire segment.
!              ** mshear = 1 **
  110          continue
               call erod(xu(k),xl(k),ainf(k),binf(k),                   &
     &             cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,&
     &             phi,ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,&
     &             detach,qostar)
               go to 160
!              
!              shear exceeds critical in entire segment.
!              ** mshear = 2 **
  120          continue
               call erod(xu(k),xl(k),ainf(k),binf(k),                   &
     &             cinf(k),ainftc(k),binftc(k),cinftc(k),eata,tauc,     &
     &             theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,  &
     &             load,tc,detach,qostar)
               go to 160
!              
!              shear increases downslope and exceeds critical at x=xc1.
!              ** mshear = 3 **
  130          continue
               call erod(xu(k),xc1,ainf(k),binf(k),cinf(k),             &
     &             ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,    &
     &             ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,    &
     &             detach,qostar)
               if (ndep.eq.0) call erod(xc1,xl(k),ainf(k),              &
     &             binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),eata,  &
     &             tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput,    &
     &             ktrato,load,tc,detach,qostar)
               go to 160
!              
!              shear decreases downslope and drops below critical at x=xc1.
!              ** mshear = 4 **
  140          continue
               call erod(xu(k),xc1,ainf(k),binf(k),cinf(k),             &
     &             ainftc(k),binftc(k),cinftc(k),eata,tauc,theta,phi,   &
     &             ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,    &
     &             detach,qostar)
               if (ndep.eq.0) call erod(xc1,xl(k),ainf(k),              &
     &             binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,   &
     &             tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput,    &
     &             ktrato,load,tc,detach,qostar)
               go to 160
!              
!              shear increases down slope -- exceeds critical at x=xc1,
!              then decreases from xc1 to xc2, and drops below critical
!              at xc2.
!              ** mshear = 5 **
  150          continue
               call erod(xu(k),xc1,ainf(k),binf(k),cinf(k),             &
     &             ainftc(k),binftc(k),cinftc(k),0.0,tauc,theta,phi,    &
     &             ilast,dl,ldlast,xdbeg,ndep,xinput,ktrato,load,tc,    &
     &             detach,qostar)
               if (ndep.eq.0) then
                  call erod(xc1,xc2,ainf(k),binf(k),cinf(k),ainftc(k),  &
     &                binftc(k),cinftc(k),eata,tauc,theta,phi,ilast,dl, &
     &                ldlast,xdbeg,ndep,xinput,ktrato,load,tc,detach,   &
     &                qostar)
                  if (ndep.eq.0) call erod(xc2,xl(k),ainf(k),           &
     &                binf(k),cinf(k),ainftc(k),binftc(k),cinftc(k),0.0,&
     &                tauc,theta,phi,ilast,dl,ldlast,xdbeg,ndep,xinput, &
     &                ktrato,load,tc,detach,qostar)
               end if
!           
!           *** l1 endif ***
            end if
!           
  160       continue
!           
!           if this was a detachment section on the segment of the
!           ofe which went into deposition (ndep = 1) then call
!           the deposition routine from the point where load equals
!           transport capacity to the end of the segment.
!           
            if (ndep.ne.0) then
               if (ilast.lt.102) then
                  dl = 0.0
                  du = 0.0
                  cdep = depc(xdbeg,ainftc(k),binftc(k),phi,theta,du,   &
     &                ktrato,qostar)
                  loadup = ldlast
                  if (loadup.lt.lddend) loadup = lddend
                  call depos(xdbeg,xl(k),cdep,ainftc(k),                &
     &                binftc(k),cinftc(k),phi,theta,ilast,dl,ldlast,    &
     &                xinput,ktrato,detach,load,tc,qostar)
                  ndep = 0
                  if (ldlast.gt.0.0.and.qout.gt.0.0) then
                     call enrich(k,xdbeg,xl(k),xdetst,loadup,           &
     &                   ldlast,lddend,theta,iendfg,slplen,ktrato,qin,  &
     &                   qout,qostar,ainftc,binftc,cinftc,npart,frac,   &
     &                   fall,frcly,frslt,frsnd,frorg,sand,silt,clay,   &
     &                   orgmat,fidel,tcf1,frcflw,enrato)
                     lddend = ldlast
                     xdetst = xl(k)
                  end if
               end if
            end if
!        
!        end of big if
         end if
!     
!     end of big do-loop
  170 continue
!     
!     compute enrichment ratio at the end of each
!     overland flow element.
!     
      iendfg = 1
!     
      call enrich(k,1.0,1.0,xdetst,ldlast,ldlast,lddend,theta,iendfg,   &
     &    slplen,ktrato,qin,qout,qostar,ainftc,binftc,cinftc,npart,frac,&
     &    fall,frcly,frslt,frsnd,frorg,sand,silt,clay,orgmat,fidel,     &
     &    tcf1,frcflw,enrato)
!     
      return
      end

