!$Author$
!$Date$
!$Revision$
!$HeadURL$

      SUBROUTINE water_erosion(isr, cd, cm, cy, soil, restot, croptot)
      
      use wepp_interface_defs, ignore_me=>water_erosion
      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biototal
      use file_io_mod, only: luowepperod, luoweppsum

!----------------------------------------------------------------------
!     water_erosion()
!
!     This is the entry point for setting up the WEPP erosion
!     routines. Any wind erosion processing is done before this
!     subroutine. Also, this runs after any submodels have been
!     run. See the main weps.for file where this is called from.
!
!     This is run on a daily timestep.
!
!     Jim Frankenberger
!     November 7, 2008
!-----------------------------------------------------------------------
	include 'wepp_erosion.inc'

      integer, intent(in):: isr,cd,cm,cy
      type(soil_def), intent(in) :: soil  ! soil for this subregion
      type(biototal), intent(in) :: restot, croptot

!     Local Variables
!
	  integer j,iyear,noout

	  real kiadj,kradj,shcrtadj
	  real ainf(mxslp),binf(mxslp),cinf(mxslp)
	  real ainftc(mxslp), binftc(mxslp), cinftc(mxslp)
	  real sand(mxnsl), silt(mxnsl), clay(mxnsl), orgmat(mxnsl)
	  real thetdr(mxnsl), tens, rh, qshear
	  real rrc,qostar, dg(mxnsl), st(mxnsl), thdp, frdp, phi
	  real theta, tauc, eata, fidel(mxpart), tcf1(mxpart)
	  real strldn, slpend, ktrato
	  real frctrl, frcsol, tcend
	  real shrsol,load(101)
	  real avsole, smrm
	  integer ifrost,ndetach,ndepos
	  real FRCFLW(MXPART), thetfc(mxnsl), por(mxnsl),prcp
        real dslost(101),dstot(1000),stdist(1000),ysdist(1000)
        real avedet,maxdet,ptdet
        real avedep,maxdep,ptdep
        real detpt1(100), detpt2(100), dtavls(100)
        real detstd(100), detmax(100), pdtmax(100)
        real detmin(100), deppt1(100), deppt2(100)
        real dpavls(100), depstd(100), depmax(100)
        real pdpmax(100), depmin(100), pdpmin(100)
        real pdtmin(100), enrato
        real anflst,bnflst,cnflst,atclst, btclst,ctclst,slpprv,wdhtop
!     
!     Local variable definitions
!
!     ainf(mxslp) - nondimensional shear stress coefficient, computed by xinflo
!     binf(mxslp) - nondimensional shear stress coefficient, computed by xinflo
!     cinf(mxslp) - nondimensional shear stress coefficient, computed by xinflo
!     ainftc(mxslp) - nondimensional transport coefficient, computed by xinflo
!     binftc(mxslp) - nondimensional transport coefficient, computed by xinflo
!     cinftc(mxslp) - nondimensional transport coefficient, computed by xinflo
!     kiadj - adjusted ki after factor applied, computed by soil_adj
!     kradj - adjusted kr after factor applied, computed by soil_adj
!     shcrtadj - adjusted shcrit after factor applied, computed by soil_adj
!     thetdr - 15-bar soil water content (wilting point), computed by getfromweps
!     sand - sand content, computed by getfromweps
!     silt - silt content, computed by getfromweps
!     clay - clay content, computed by getfromweps
!     orgmat - organic matter, computed by getfromweps
!     rrc - random roughness coefficient (m)
!     qostar - non-dimensional discharge out of strips, computed by xinflo
!     qshear - peak flow discharge (m^3/s), computed by xinflo
!     dg(mxnsl) - depth of each soil layer (m)
!     st(mxnsl) - current available water content per soil layer (m)
!     thdp - Thaw depth of the frozen-layer system
!     frdp - depth of the frost layer
!     por(mxnsl) - porosity for each soil layer(m**3/m**3)
!     rh - ridge height coefficent
!     thetfc(mxnsl) - 1/3-bar soil water content (field capacity)
!     phi - 
!     theta - 
!     tauc - 
!     eata - 
!     fidel(mxpart) -
!     tcf1(mxpart) - 
!     strldn - 
!     slpend -  
!     ktrato - 
!     effint -   
!     frctrl - 
!     frcsol - 
!     tcend - 
!     shrsol -  
!     load(101) - 
!     avsole - 

!     other
!     wp_inrcov - interrill cover (0-1)
!     wp_cycle - counter of # of freeze/thaw cycles occuring

      
!      write(*,*) 'Compute WEPP EROSION : ', cm, '/', cd, '/', cy
      iyear = 0
      noout = 2
      wp_inrcov = 0.0
	wp_bconsd = 0.02
	
      do j = 2, 101
         dslost(j-1) = 0.0
      end do
      
      do j = 1, wp_npart
        frcflw(j) = 0.0
      end do
      
      wp_irdgdx = 0.0
!     wp_enrato = 0.0
      avsole = 0.0
      wp_qsout = 0.0
      enrato = 0.0
      anflst = 0.0
      bnflst = 0.0
      cnflst = 0.0
      atclst = 0.0
      btclst = 0.0
      ctclst = 0.0
      slpprv = 0.0
      wdhtop = 0.0
!
!     copy any weps variables into WEPP names
!     variables that begin with wp_ are stored in the WEPP
!     common area and don't have direct WEPS equivalent. 
!     For those variables that have a WEPS counterpart we can grab
!     them now.
!

      call getfromweps(isr,sand,silt,clay,orgmat,                       &
     &  thetdr,rrc,dg,st,thdp,frdp, thetfc, por, rh,                    &
     &  frctrl, frcsol, prcp, soil)

      call soil_adj(wp_ki,wp_kr,wp_shcrit,kiadj,kradj,shcrtadj,rrc,     &
     &   croptot%zht_ave, croptot%ftcancov,                             &
     &   wp_inrcov, croptot%mrttotto15, restot%mrttot,wp_bconsd,        &
     &   wp_daydis,rh, wp_rspace,                                       &
     &   wp_avgslp,restot%mbgtot,wp_krcrat,wp_tccrat,wp_kicrat,dg,      &
     &   thetdr,st, thdp,frdp,                                          &
     &   ifrost,                                                        &
     &   thetfc,por,tens,wp_cycle)
      
  
      call xinflo(wp_x,wp_efflen,wp_slplen,wp_a,wp_b,wp_qin,            &
     &   wp_qout,wp_peakro,qostar,                                      &
     &            ainf,binf,cinf,ainftc,binftc,cinftc,qshear,wp_rspace, &
     &            wp_nslpts)

	
!        
!     call param subroutine to calculate the nondimensional parameter
!     values to use in the erosion calculations.
!        
      call param(wp_qin,wp_qout,qostar,qshear,wp_qsout,wp_a,wp_b,       &
     &       wp_avgslp,wp_width,wp_rspace,                              &
     &       ktrato,shrsol,tcend,frcsol,frctrl,rrc,wp_npart,            &
     &       wp_frac,wp_dia,wp_spg,                                     &
     &       wp_fall,wp_runoff,wp_effdrn,wp_effint,wp_effdrr,strldn,    &
     &       tcf1,fidel,eata,                                           &
     &       tauc,theta,phi,slpend,ainf,binf,cinf,ainftc,binftc,        &
     &       cinftc,                                                    &
     &       sand,wp_slplen,kiadj,kradj,shcrtadj,wp_nslpts,wp_efflen,   &
     &       anflst, bnflst, cnflst, atclst, btclst, ctclst,slpprv,     &
     &       wdhtop,wp_rwflag)   

      

!        
!     call the erosion routing subroutine route.
!     this is largely taken from wepp v2004.7  dcf
!     only do erosion calculations if there is runon or runoff 
!     from the hillslope element.
!
      if (wp_qin.gt.0.0 .or. wp_qout.gt.0.0)then
        call route(wp_qin,wp_qout,qostar,strldn,ktrato,ainf,binf,cinf,  &
     &             ainftc,binftc,cinftc,wp_npart,wp_frac,wp_frcly,      &
     &             wp_frslt,                                            &
     &             wp_frsnd,                                            &
     &             wp_frorg,wp_fall,frcflw,wp_nslpts,wp_x,wp_xu,        &
     &             wp_xl,load,                                          &
     &             enrato,                                              &
     &             tcf1,fidel,sand,silt,clay,orgmat,eata,tauc,theta,    &
     &             phi,wp_slplen)
!        
!       call the sloss subroutine to redimensionalize the
!       sediment load at point output from route, and 
!       compute the dg/dx values at the points and sediment
!       leaving the profile.
!        
        call sloss(load,tcend,wp_width,wp_rspace,wp_effdrn,theta,       &
     &      wp_slplen,wp_irdgdx,                                        &
     &      wp_qsout,dslost,wp_dsmon, wp_dsyear, wp_dsavg,              &
     &      avsole,wp_qout,frcflw,wp_npart,enrato)
!
!       following added to correct error at ofe break between
!       a case 4 ofe and a case 2 ofe.  dcf 1-21-2005
!

      call sedseg(dslost,luowepperod(isr),iyear,noout,dstot,stdist,     &
     & wp_irdgdx,                                                       &
     & ysdist,wp_avgslp,wp_slplen,wp_y,avedet,maxdet,ptdet,avedep,      &
     & maxdep,ptdep,detpt1, detpt2, dtavls, detstd, detmax, pdtmax,     &
     & detmin, pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,  &
     & depmin, pdpmin,ndetach,ndepos)
     
      if(wp_qout.le.0.0) wp_qsout = 0.0
 
!     write output to erosion events file 

      call write_event(luowepperod(isr),cd, cm, cy, prcp,wp_runoff*1000,&
     &     wp_irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,    &
     &     enrato)
 
     
!     if detailed output in main file write event output there also
!     but in a different format 
     
      if (wp_detailout .eq. 1) then   
         call write_main_event(luoweppsum(isr),cd, cm, cy, prcp,        &
     &     wp_runoff*1000,                                              &
     &     wp_irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,    &
     &        enrato,detpt1, detpt2, dtavls, detstd, detmax, pdtmax,    &
     & detmin, pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,  &
     &     depmin, pdpmin,ndetach,ndepos,wp_npart,wp_frac,frcflw,wp_dia,&
     &     wp_spg,wp_frsnd,wp_frslt, wp_frcly,wp_frorg,                 &
     &     wp_slplen,wp_fwidth,wp_avgslp,stdist,dslost,1,0)
       endif
         
!         call write_plot(luoweppsum(isr),stdist, ysdist, dstot)
     
!         call enrprt(luoweppsum(isr),wp_npart,wp_frac,frcflw,wp_dia,    &
!     &         wp_spg, wp_frsnd,wp_frslt, wp_frcly,wp_frorg,wp_enrato)

!      endif

      else
!       no runon or runoff for the ofe, then set soil loss,
!       sediment, particle and enrichment outputs to zero.
!
        do j = 2, 101
          dslost(j-1) = 0.0
        end do
        do j = 1, wp_npart
          frcflw(j) = 0.0
        end do
        wp_irdgdx = 0.0
!        wp_enrato = 0.0
        avsole = 0.0
        wp_qsout = 0.0
      endif  
	  
	! update running final, monthly and year totals
	wp_avsolf = wp_avsolf + avsole
	wp_avsolm = wp_avsolm + avsole
	wp_avsoly = wp_avsoly + avsole    

	  return
	  
1100  format (3(i5), 3x, f5.1, 3x, f5.1, f7.3,f7.2,f7.2,f7.1,           &
     &    f7.2,f7.2,f7.1,2x,f8.3,f7.2) 
	  
	  end
