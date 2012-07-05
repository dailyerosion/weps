      subroutine soil_adj(ki,kr,shcrit,kiadj,kradj,shcrtadj,            &
     & rrc, canhgt,cancov,inrcov,rtm15,rtm,bconsd,daydis,rh,rspace,     &
     & avgslp,smrm,krcrat,tccrat,kicrat,dg,thetdr,st,thdp,frdp,ifrost,  &
     & thetfc,por,tens,cycle)

       use wepp_interface_defs
	  implicit none
!
!      soil_adj()
! 
!      This code is taken from the WEPP subroutine soil(), since WEPS already has a subroutine
!      named soil this is now called soil_adj. Updates soil parameters.
! 
 
! + + + argument declarations + + +	
	  real, intent(in):: canhgt,cancov,inrcov,rtm15,rtm(3)
	  real, intent(in):: bconsd,rh,rspace,avgslp
	  real, intent(in):: smrm(3),krcrat,tccrat,rrc,kicrat
	  real, intent(in):: dg(10), thetdr(10), st(10),thdp,frdp
	  real, intent(in):: thetfc(10), por(10)
	  integer, intent(in):: cycle, daydis
	  integer, intent(inout):: ifrost
	  real, intent(out):: tens, kiadj, kradj, shcrtadj
	  real, intent(in):: ki, kr, shcrit
	  
!     + + + argument definitions + + +

!   ki - initial interrill detachment parameter baseline interrill erodibility
!   kr -  initial rill detachment rate parameter (s/m)
!   shcrit - rill detachment threshold parameter,or critical shear stress
!   kiadj - adjusted ki
!   kradj - adjusted kr
!   shcrtadj - adjusted shcrit
!   rrc - random roughness coefficent
!   canhgt - canopy height
!   cancov - canopy cover
!   inrcov - interrill cover
!   rtm15 - root mass at 15 cm
!   rtm - on living root mass
!   bconsd - consolidation decay coefficient
!   daydis - days since previous disturbance
!   rh - ridge height coefficient
!   rspace - rill spacing
!   avgslp - average slope 
!   smrm - submerged residue mass today
!   krcrat - ratio of freshly tilled to fully consolidated
!                      rill erodibility (nondimensional)
!   tccrat - ratio of freshly tilled to fully consolidated
!                      critical shear stress (nondimensional)
!   kicrat - ratio of freshly tilled to fully consolidated  interrill erodibility
!   dg - depth of each soil layer in meters
!   thetdr - 15-bar soil water content (wilting point)
!   st - current available water content per soil layer
!   thdp - Thaw depth of the frozen-layer system 
!   frdp -  Depth of the frost layer
!   ifrost - flag for frost adjustment to ki, kr, tauc (ifrost)
!                      0 - no adjustment
!                      1 - soil is frozen to surface
!                      2 - soil has thawed on surface, adjust ki, kr, tauc
!   thetfc - 1/3-bar soil water content (field capacity)
!   por - porosity for each soil
!   tens - Water tension value to be used in QWET calcs.  (m)
!   cycle -	Counter of # of freeze/thaw cycles occuring.

!    --------- Local variables ----------
      real ckiacc,ckiagc,ckialr,ckiadr,denom,produc,ckiasc
	real ckiasa,ckiawc,ckrbgb,ckradr, ckralr,ctcarr
	real ckrasc, ctcasc,ckrawc,ckiaft,pwater, ckraft, tcaft
      real slo,tenkpa,acyc,kiadjf,kradjf,tcadjf

!
!-----------------------------------------------------------------------
!
!     FREEZE AND THAW ADJUSTMENTS TO ERODIBILITY VALUES (Ki,Kr,Tauc)
!
!     7/9/91 add adjustment based on effective matric potential (sm)
!     Improved equations from Bob Young added 2/13/92 by dcf
!     Corrections added 4/21/93 to compute the correct soil matric
!     potential needed in these calculations (dcf and savabi)
!
      pwater = (st(1)+thetdr(1)*dg(1)) / dg(1)
 
!
      if (frdp.gt.0.0.and.thdp.le.0.0) then
        ifrost = 1
        ckiaft = 0.0
        ckraft = 0.0
        tcaft = 1.0
      else if (pwater.le.thetfc(1)) then
        ifrost = 0
        ckiaft = 1.0
        ckraft = 1.0
        tcaft = 1.0
      else if (ifrost.gt.0) then
        ifrost = 2
      else
        ckiaft = 1.0
        ckraft = 1.0
        tcaft = 1.0
      end if
!
!     ---- Once thawing of a frozen soil is detected....
      if (ifrost.eq.2) then
!
!       compute the soil matric potential in kiloPascals.  The
!       following equation is an approximation valid only between
!       the range of saturation and field capacity. (from Savabi)
!
!       NOTE replaced Savabi code of setting te33=33 and t33=log10(te33)
!       with the constant value 1.518514 in equation for slo
!       dcf 5/14/93
!
        slo = (100.*(thetfc(1)-por(1))) / 1.518514
!
!       if the water present is less than the porosity compute
!       the matric potential in kilopascals
!
        if (pwater.lt.por(1)) then
          tenkpa = 10. ** ((100.*(por(1)-pwater))/abs(slo))
!
!       else if the water present is greater than or equal to the
!       porosity set the matric potential to 1.0 kilopascal
!
        else
          tenkpa = 1.0
        end if
!
!   The WINTER model needs to store water tension in units of m.
!
        tens = tenkpa / 10.
!
!       The main constant for the interrill erodibility adjustment
!       due to frost and thaw is based upon the number of freeze-thaw
!       cycles - once reach 10 cycles - set equal to 1.31
!
        if (cycle.lt.11) then
          acyc = 1.0 + 0.0586 * float(cycle) - 0.0027 *                 &
     &        float(cycle**2)
        else
          acyc = 1.31
        end if
!
!       ------ compute interrill erodibility adjustment due to freeze-thaw
        ckiaft = acyc * exp((-alog(acyc))*tenkpa/33.0)
!
!       once 1/10 bar effective matric potential is reached the
!       adjustments for rill erodibility and critical shear are stopped
!
!       ------ if soil matric potential > 1/10-bar....  (10 kiloPascals)
        if (tenkpa.gt.10.0) then
          ckraft = 1.0
          tcaft = 1.0
!
!       ------ if soil matric potential <= 1/10-bar....  (10 kiloPascals)
        else
          ckraft = 2.0 * (0.933) ** tenkpa
          tcaft = 0.875 + 0.0543 * alog(tenkpa)
        end if
!
      end if
 
!-----------------------------------------------------------------
!       Interrill erodibility (Ki) adjustments for cropland
!       requires: canhgt, cancov, inrcov, rtm15, rtm, bconsd,
!                 daydis, kicrat, rh, rspace, avgslp, 
!-----------------------------------------------------------------
!       ------ canopy effects  (Laflen equation  4/13/93  dcf )
!
        if (canhgt.gt.0.0) then
          ckiacc = 1.0 - (2.941*cancov/canhgt) *                        &
     &        (1.0-exp(-0.34*canhgt))
        else
          ckiacc = 1.0 - cancov
        end if
!
!       ------ ground cover effects (originally in function INRDET)
!
        ckiagc = exp(-2.5*inrcov)
!
!       ------ live root biomass (WEPP Equation 6.11.2)
        ckialr = exp(-0.56*rtm15)
!
!       ------ dead root biomass (WEPP Equation 6.11.1)
        ckiadr = exp(-0.56*(rtm(1)+rtm(2)+rtm(3)))
!
!
!       ------ sealing and crusting
!
!       Variable produc is used in a trap to prevent numeric underflows
!       It is also used below in Kr and Tauc consolidation equations
!
        produc = bconsd * daydis
!
        if (produc.lt.10.0) then
          ckiasc = kicrat + (1-kicrat) * exp(-produc)
        else
          ckiasc = kicrat
        end if
!
!       Interrill slope adjustment (0 < denom < .707)
!       ------ equivalent of "sin(S)", assuming the furrows are triangular.
!
!       Change suggested by Nearing 11/22/93 to use OFE average slope
!       if it is steeper than row sideslopes for slope factor
!       adjustment.   dcf   11/22/93
!
        if ((rh/(rspace/2.)).gt.avgslp) then
          denom = rh / sqrt((rspace/2.)**2+rh**2)

        else
!
!         Logic added 3/4/97 to prevent a decrease in predicted
!         interrill detachment for extremely steep slopes (300%)
!         dcf
          if(avgslp .lt. 0.7854)then
            denom = sin(avgslp)
          else
            denom = 0.707
          end if
        end if
!
!       ------ constrain row sideslope to <= 45 degrees
        if (denom.gt..707) denom = .707
!       ------ (Equation 3 -- NSERL Report No. 3)
        ckiasa = 1.05 - .85 * exp(-4.*denom)
!
!       ------ wheel compaction
        ckiawc = 1.0

!       ------ Total Ki adjustment factor
        kiadjf = ckiacc * ckiagc * ckialr * ckiadr * ckiasc *           &
     &      ckiaft * ckiasa * ckiawc
        if (kiadjf.lt.0.03) kiadjf=0.03



!-------------------------------------------------------------
!       cropland rill erodibility (Kr) and critical shear stress
!       requires: smrm, rtm, rtm15, rrc, krcrat, tccrat, 
!-------------------------------------------------------------
!
!       ------ incorporated residue and roots
!       (WEPP Eq. 6.14.1, adapted to include 3 types of residue)
!
!       ... adjustment of kr to buried residue
        ckrbgb = exp(-.40*(smrm(1)+smrm(2)+smrm(3)))
!
!       ... adjustment of kr to dead root
        ckradr = exp(-2.2 * (rtm(1)+rtm(2)+rtm(3)))
!       ... adjustment of kr to live root
        ckralr = exp(-3.5*rtm15)
!
!       ... adjustment of tc to random roughness
        ctcarr = 1. + 8.0 * (rrc-0.006)

        if (produc.lt.10.0) then
          ckrasc = krcrat + (1.0 - krcrat) * exp(-produc)
          ctcasc = tccrat - (tccrat-1.0) * exp(-produc)
        else
          ckrasc = krcrat
          ctcasc = tccrat
        end if
!
!       ------ wheel compaction
        ckrawc = 1.0

!       ------ Total Kr adjustment factor
        kradjf = ckrbgb * ckrasc * ckraft * ckrawc *                    &
     &  ckradr * ckralr
        if (kradjf.lt.0.03) kradjf=0.03

!       ------ Total critical shear stress adjustment factor
        tcadjf = tcaft * ctcasc * ctcarr
        if (tcadjf.gt.2.0) tcadjf=2.0

!       apply the adjustment factors to original values

        kiadj = ki * kiadjf
		kradj = kr * kradjf
		shcrtadj = shcrit * tcadjf

		return

		end