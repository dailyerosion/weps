!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine yalin(effsh,tottc,sand,dia,spg,tcf1,npart,frac)
      
      use wepp_interface_defs, ignore_me=>yalin
      
      implicit none
!                                                                   
!      this routine is called from fn trcoef to compute sediment    
!      transport capacity using the yalin equation. it is called    
!      fn shield.                                                   
!                                                                   
!
!     + + + parameter declarations + + +
!
      include 'wepp_erosion.inc'

      real, intent(in) :: effsh
      real, intent(in) :: dia(mxpart), spg(mxpart), sand(mxnsl)
      real, intent(in) :: frac(mxpart)      
      integer, intent(in) ::  npart
      real, intent(out) :: tottc, tcf1(mxpart)
!                                                                   
!      arguments                                                    
!         effsh - effective sheer stress                            
!         tottc - total sediment transport capacity                 
!                                                                   
!    local variables:                                               *
!       ws     : sediment transport capacity for particle class k   *
!                (kg/m*s)                                           *
!       coef   : portion of yalin equation solution                 *
!       ycrit  : ordinate from shields diagram for dimensionless    *
!                critical shear for transport                       *
!       delta  : portion of yalin equation solution                 *
!       sigma  : portion of yalin equation solution                 *
!       p      : sediment transport capacity for particle class     *
!                k (nondimensional)                                 *
!       dltrat : portion of yalin equation solution                 *
!       tottc  : total sediment transport capacity (kg/m*s)         *
!                                                                   *
!********************************************************************
!
!      save
      real ws(mxpart), coef(mxpart), ycrit(mxpart), delta(mxpart),      &
     &    sigma(mxpart), p(mxpart), dltrat(mxpart), reyn,  t,           &
     &    vstar, yalcon, oldtot, adjtc, msdens, kinvis
      real accgav
      integer k
!
! initialization:
!
      yalcon = 0.635
      t = 0.0
      tottc = 0.0
!     
      msdens = 1000.0
      kinvis = 1.0e-06
      accgav = 9.807
!     
!     the constant 0.635 was derived empirically by yalin
!     
!     compute shear velocity (vstar):
!     
      vstar = sqrt(effsh/msdens)
!     
!     compute coefficient coef=vstar*msdens*dia*spg for each
!     particle classes:
!     
      coef(npart) = vstar * msdens
      do 10 k = 1, npart
         coef(k) = coef(npart) * dia(k) * spg(k)
   10 continue
!     
!     compute reynold's number (reyn), dimensionless critical shear
!     parameter from the shields diagram (ycrit), parameters delta and
!     sigma, and the dimensionless sediment transport capacity (p) for
!     each particle class:
!     
      do 20 k = 1, npart
         reyn = vstar * dia(k) / kinvis
         ycrit(k) = shield(reyn)
         delta(k) = (vstar**2/(spg(k)-1.0)/accgav/dia(k)/               &
     &               ycrit(k)) - 1.0
!        
         if (delta(k).gt.0.0) then
            sigma(k) = delta(k) * 2.45 * spg(k) ** (-0.4) *             &
     &          sqrt(ycrit(k))
            p(k) = yalcon * delta(k) * (1.0-1.0/sigma(k)*               &
     &          alog(1.0+sigma(k)))
            t = t + delta(k)
         else
            delta(k) = 0.0
            p(k) = 0.0
         end if
   20 continue  
!     
!     compute the transport capacity (mass per unit width per unit time)
!     ws for each particle class:
!     
      if (t.eq.0.0) t = 1000.0
      do 30 k = 1, npart
         dltrat(k) = delta(k) / t
         ws(k) = p(k) * dltrat(k) * coef(k)
!    
!        use a weighting scheme for transport capacity to account
!        for the amount of each sediment class being transported
!
!        should frac(k,iplane) or frcflw(k,iplane) be used for the weighting?        
         ws(k) = ws(k) * (frac(k)*float(npart))
!        
         tottc = tottc + ws(k)
   30 continue
!     
!     
!     add changes to include nearing alteration to tc that was
!     previously included in tcend calculation in param.for.  dcf
!
      oldtot = tottc
      if (sand(1).gt.0.5) then
         adjtc = 0.3 + 0.7 * exp(-12.52*(sand(1)-0.5))
         if (adjtc.lt.0.30) adjtc = 0.30
         tottc = tottc * adjtc
      end if
!     
      do 40 k = 1, npart
         if (oldtot.gt.0.0) ws(k) = (ws(k)/oldtot) * tottc
   40 continue
!     
      do 50 k = 1, npart
!        
!       code added to prevent divide by zero (if tottc is zero)
!        
         if (tottc.gt.0.0) then
            tcf1(k) = ws(k) / tottc
         else
            tcf1(k) = 0.0
         end if
   50 continue
      return
      end
