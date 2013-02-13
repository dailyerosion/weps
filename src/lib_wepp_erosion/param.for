!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine param(qin,qout,qostar,qshear,qsout,a,b,avgslp,         &
     &    width,rspace,ktrato,shrsol,tcend,frcsol,frctrl,rrc,npart,frac,&
     &    dia,spg,fall,runoff,effdrn,effint,effdrr,strldn,tcf1,fidel,   &
     &    eata,tauc,theta,phi,slpend,ainf,binf,cinf,ainftc,binftc,      &
     &    cinftc,sand,slplen,kiadj,kradj,shcrtadj,nslpts,efflen,        &
     &    anflst, bnflst, cnflst, atclst, btclst, ctclst,slpprv, wdhtop,&
     &    rwflag)
     
      use wepp_interface_defs
      
      implicit none
!
!     subroutine param finds dimensionless rill and interrill 
!     soil erosion parameters: one for interrill erosion (theta), 
!     two for rill erosion (eata and tauc), and one for deposition (phi)
!     param calls functions shears and falvel, and subroutine trcoef
!
!     module adapted from wepp version 2004.7 and called from the
!     main program
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
      real, intent(out) :: ktrato, shrsol, tcend, strldn, eata, tauc
	real, intent(out) :: phi, tcf1(mxpart), theta, fidel(mxpart)
	real, intent(out) :: slpend
	real, intent(out) ::  ainftc(mxslp), binftc(mxslp),cinftc(mxslp)

	real, intent(inout) :: width,avgslp

	real, intent(in):: a(mxslp), b(mxslp),qin,qout,qostar
      real, intent(in):: qsout, qshear, rspace, frcsol, frctrl, rrc           
      real, intent(in):: frac(mxpart), dia(mxpart),spg(mxpart)
	real, intent(in):: fall(mxpart), runoff,effdrn, effint, effdrr
	real, intent(in):: sand(mxnsl), slplen, kiadj, kradj, shcrtadj
	real, intent(in) ::efflen

	real, intent(inout):: ainf(mxslp),binf(mxslp),cinf(mxslp)
  
      integer, intent(in):: npart, nslpts,rwflag
      real, intent(inout) :: anflst, bnflst, cnflst, atclst, btclst
      real, intent(inout) :: ctclst, slpprv, wdhtop
!
!     + + + argument definitions + + + 
!
!
!     + + + local variables + + + 
!
!     save anflst, bnflst, cnflst, atclst, btclst, ctclst
!
      real kt, kt2, shrend, detinr, diaeff, spgeff, sumf, veleff, beta, &
     &    ktrprv, tcprev,                                               &
     &    sterm1, sterm2, spart1, tpart1, tterm1, tterm2, tprod,        &
     &    denom, shrspv, qi, rif, intdr, drinti(mxpart), az, bz, widprv,&
     &    qtop,slptop,shrtp1,ktop1,ktop2,ktop
     
      real trcoef,shrdum, shrati, tcrati, pkro
      !real shears, falvel
      integer i,k, iclass
!
!     + + + local variable definitions  + + + 
!
!     drinti - unitless interrill delivery ratio of each particle size
!              class (computed as function of rif)
!     intdr  - unitless weighted interrill sediment delivery ratio
!     kt2    - transport coefficient calculated using the average
!              of shrend and shrsol
!     npart  - number of particle types to use in calculating
!              effective particle parameters
!     shrend - shear stress calculated using actual slope at end
!              of ofe
!     shrsol - shear stress calculated using average slope of ofe
!     detinr - interrill detchment rate
!     diaeff - effective sediment diameter
!     fall   - fall velocity of each particle size of each ofe (m/s)
!     spgeff - effective sediment specific gravity
!     sumf   - sum of the mass fractions of npart fractions
!     veleff - effective particle full velocity
!     beta   - rainfall induced turbulence factor
!     pkro   - slope of the flow discharge line.  for 1-ofe hills
!              pkro is the flow discharge per unit width
!     qi     - average unit discharge of runoff from interrill
!              over time of excess rainfall
!     rif    - interrill roughness factor for calculating sediment
!              delivery (from foster, 1981 modeling chapter)
!
!     begin subroutine param
!       
!     compute actual slope gradient at the end of slope (slpend)
!
!     case of positive outflow
!
      if (qout.gt.0.0) slpend = (a(nslpts)*1.0 + b(nslpts)) * avgslp
     
!     
!     case of no outflow from plane
!     
      if (qout.le.0.0) slpend = b(2) * avgslp
!     
!     obtain variables needed for making shear stress and transport
!     capacity continuous at flow plane breaks
!     
      if (qin.gt.0.0 .and. qout.gt.0.0) then
         qtop = qin * rspace
         slptop = b(2) * avgslp
         call sheart(qtop,slptop,rspace,wdhtop,frcsol,frctrl,shrtp1)
         if (shrtp1.lt.0.000001) shrtp1 = 0.000001
         call sheart(qtop,slpprv,rspace,wdhtop,frcsol,frctrl,shrspv)
         if (shrspv.lt.0.000001) shrspv = 0.000001
         call trcoeff(trcoef,shrtp1,sand,dia,spg,tcf1,npart,frac)
         ktop1 = trcoef
         shrdum = (shrtp1+shrspv) / 2.0
         call trcoeff(trcoef,shrdum,sand,dia,spg,tcf1,npart,frac)
         ktop2 = trcoef
         ktop = ktop2 / ktop1
         tcprev = ktop1 * shrspv**1.5
         ktrprv = ktop
      end if
!     
!     compute shear stress at the end of slope (shrend) using actual
!     slope gradient
!     
      call shears(qshear,slpend,rspace,width,frcsol,frctrl,shrend,      &
     &    rwflag)
     
      if (shrend.lt.0.000001) shrend = 0.000001
!     
!     compute shear stress at the end of the slope (shrsol) using
!     average slope gradient (avgslp)
! 
		    
      call shears(qshear,avgslp,rspace,width,frcsol,frctrl,shrsol,      &
     &     rwflag)
!
      if (shrsol.lt.0.000001) shrsol = 0.000001
!     
!     compute the transport coefficient (kt) based on the average slope
!     for the purpose of normalizing
!     
      call trcoeff(trcoef,shrsol,sand,dia,spg,tcf1,npart,frac)
!
      kt = trcoef
!     
!     compute transport coefficient (kt2) using the average of
!     shrend and shrsol
!     
      shrdum = (shrend+shrsol) / 2.0
      call trcoeff(trcoef,shrdum,sand,dia,spg,tcf1,npart,frac)
!
      kt2 = trcoef
!     
!     compute the normalized transport coefficient (ktrato)
!     
      ktrato = kt2 / kt
!     
!     compute sediment transport capacity (tcend) at end of average
!     slope
!     
      tcend = kt * shrsol ** 1.5
!     
!     limit tcend so that it is a very small number, but never 0.0 so
!     that the code will not bomb for inputs of zero slopes
!     
      if (tcend.lt.1.0e-10) tcend = 1.0e-10
!     
!     compute the starting nondimensional sediment load at the top of
!     the current flow plane
!     
      if (qin.le.0.0 .or. qout.le.0.0)then
         strldn = qsout * rspace / tcend / width
      else
         strldn = qsout * rspace / tcend / wdhtop
      endif
!     
!     compute new values of shear stress (ainf,binf,cinf) coefficients
!     and new values of transport coefficients (ainftc,binftc,cinftc)
!     that make shear stress and transport capacity functions
!     continuous on adjacent planes

!     
      if (qout.gt.0.0.and.qin.gt.0.0) then
!      
!        check to see that the shear stress at the end of the previous
!        ofe was not zero - if it is zero the solutions for the new
!        shear stress and transport coefficients are not valid and must
!        use original values calculated in xinflo.for
!        
         spart1 = anflst + bnflst + cnflst
!        
         if (spart1.gt.1.0e-5.and.shrspv.gt.0.0) then
            sterm1 = b(2) / spart1
            sterm2 = (shrspv/shrsol) ** 1.5
            shrati = 1.0 / ((sterm1/sterm2)-1.0)
            tpart1 = atclst + btclst + ctclst
!           
            if (tpart1.gt.1.0e-5) then
               tterm1 = b(2) / tpart1
            else
               tterm1 = b(2) / 1.0e-5
            end if
!           
            tterm2 = ((tcend/tcprev)*(ktrato/ktrprv))
            tprod = (tterm1*tterm2) - 1.0
!           
            if (abs(tprod).gt.1.0e-5) then
               tcrati = 1.0 / tprod
            else
               if (tprod.ge.0.0) then
                  tcrati = 1.0 / 1.0e-5
               else
                  tcrati = -1.0 / 1.0e-5
               end if
            end if
!        
!        else  -  have zero transport capacity at beginning of ofe
!        due to a zero slope condition - if this is the case the best
!        solution is to use qostar in place of shrati and tcrati
!        since it will usually still have a reasonable number - and
!        when you have a zero slope at an ofe boundary the shear
!        and transport will still be continuous even when using
!        qostar
!        
         else
            shrati = qostar
            tcrati = qostar
         end if
!        
!        re-calculate the shear stress and transport coefficients
!        using the just calculated shear stress and transport ratios
!        
         do i = 2, nslpts
!           
            denom = (shrati+1.0)
!           
!           prevent the denominator term for the coefficients from
!           becoming zero, which will cause code to bomb in
!           following computations
!           
            if (abs(denom).lt.1.0e-3) then
               if (denom.ge.0.0) then
                  denom = 0.001
               else
                  denom = -0.001
               end if
            end if
!           
            ainf(i) = a(i) / denom
            binf(i) = (a(i)*shrati+b(i)) / denom
            cinf(i) = (b(i)*shrati/denom)
!           
            denom = (tcrati+1.0)
!           
!           prevent the denominator term for the coefficients from
!           becoming zero, which will cause the code to bomb in
!           following computations
!           
            if (abs(denom).lt.1.0e-3) then
               if (denom.ge.0.0) then
                  denom = 0.001
               else
                  denom = -0.001
               end if
            end if
!           
            ainftc(i) = a(i) / denom
            binftc(i) = (a(i)*tcrati+b(i)) / denom
            cinftc(i) = (b(i)*tcrati/denom)
!           
            anflst = ainf(i)
            bnflst = binf(i)
            cnflst = cinf(i)
!           
            atclst = ainftc(i)
            btclst = binftc(i)
            ctclst = cinftc(i)
!        
         end do
!     
      end if
!     
      if (qin.le.0.0) then
         do i = 2, nslpts
            anflst = ainf(i)
            bnflst = binf(i)
            cnflst = cinf(i)
!           
            atclst = ainf(i)
            btclst = binf(i)
            ctclst = cinf(i)
         end do
      end if
!     
!     compute dimensionless parameters for rill erosion (eata and tauc)
!     

      eata = slplen * kradj * shrsol / tcend
  
      tauc = shcrtadj / shrsol
! 
!     compute the interrill delivery
!     ratio as function of random roughness and particle size
!     distribution
!     
      rif = -23.0 * rrc + 1.14
!     
      if (rif.lt.0.0) rif = 0.0
      if (rif.gt.1.0) rif = 1.0
!     
      intdr = 0.0
!     
      do iclass = 1, npart
         if (fall(iclass).lt.0.01) then
            bz = 0.1286 + 2209.0 * fall(iclass)
            az = exp(0.0672+659.0*fall(iclass))
            drinti(iclass) = az * rif ** bz
         else
            drinti(iclass) = 2.5 * rif - 1.5
         end if
         if (drinti(iclass).gt.1.0) drinti(iclass) = 1.0
         if (drinti(iclass).lt.0.0) drinti(iclass) = 0.0
         intdr = intdr + frac(iclass) * drinti(iclass)
      end do
!     
      do iclass = 1, npart
!        
!        added check to prevent a divide by zero if no delivery of
!        interrill sediment
!        
!        this problem has been enhanced due to the new equation
!        added above to compute rif (now, for an "rrc" value of
!        0.05 meters (2 inches) or greater, the value of rif is
!        always zero, causing a bomb here)
!        
         if (intdr.gt.0.0) then
            fidel(iclass) = frac(iclass) * drinti(iclass) / intdr
         else
            fidel(iclass) = 0.0
         end if
      end do
!     

      if (effdrr.gt.0.0) then
         qi = runoff / effdrr
      else
         qi = 0.0
      end if
!     
    
      if (width.gt.0.0) then
         detinr = kiadj * effint * qi * intdr * rspace / width
      else
         detinr = 0.0
      end if
!     
!     compute dimensionless interill erosion parameter (theta)
!     
      theta = slplen * detinr / tcend
      if (qout.le.qin) then
!       for planes with no rainfall excess, set interrill detachment
!       parameter to 0.0
        theta = 0.0
      else
!       time correction for interrill detachment
        theta = theta * (effdrr/effdrn)
      endif
!     
!     compute the effective particle diameter (diaeff), effective
!     specific gravity (spgeff), and effective particle fall
!     velocity (veleff) using three size classes (primary clay,
!     silt and sand)
!     
      diaeff = 0.0
      spgeff = 0.0
      sumf = 0.0
!     
!     corrections need to be made here to compute the effective
!     diameter, specific gravity, and fall velocity when there are 
!     large amounts of large aggregates or sand present
!     
      do k = 1, 3
         diaeff = diaeff + frac(k) * alog(dia(k))
         spgeff = spgeff + frac(k) * alog(spg(k))
         sumf = sumf + frac(k)
      end do
!     
      if (sumf.gt.0.0) then
         diaeff = exp(diaeff/sumf)
         spgeff = exp(spgeff/sumf)
         veleff = falvel(spgeff,diaeff)
!        
!        set the turbulence factor beta to 0.5
!
         beta = 0.5
!     end if
!     
      end if
!     
!     compute the slope of the flow discharge line (pkro)
!     
      pkro = -1.0e-10
!     
      if (qout.gt.0.0) then
         pkro = (qout-qin) / slplen
      else
         if (qin.gt.0.0) then
            if (efflen.gt.1.e-10) then
               pkro = -qin / efflen
            else
               pkro = -1.0e-10
            end if
         end if
      end if
! 
!     compute demensionless deposition parameter (phi)
!    
      if (abs(pkro).ge.1.0e-15) then
         phi = beta * veleff / pkro
      else
!        
!        if slope of flow discharge line is almost 0.0 then
!        set maximum value of deposition parameter to +-100000
!        
         if (qostar.ge.0.0) then
            phi = 100000.
         else
            phi = -100000.
         end if
      end if
!     
!     limit value of deposition parameter to absolute value of 100000
!     
      if (phi.gt.100000.0) phi = 100000.0
      if (phi.lt.-100000.0) phi = -100000.0
!     
      return
      end
