!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine weppsum(isr, years, wp)
      
      use wepp_interface_defs
      use file_io_mod, only: luoweppplot, luoweppsum
      use wepp_param_mod, only: wepp_param

      implicit none

      include 'wepp_erosion.inc'
      
      integer, intent(in) :: isr, years
      type(wepp_param), intent(inout) :: wp

      integer i
      real enrato, avsole, irdgdx
      real FRCFLW(MXPART)
	
      real dstot(1000), stdist(1000), ysdist(1000)
      real avedet,maxdet,ptdet
      real avedep,maxdep,ptdep
      real detpt1(100), detpt2(100), dtavls(100)
      real detstd(100), detmax(100), pdtmax(100)
      real detmin(100), deppt1(100), deppt2(100)
      real dpavls(100), depstd(100), depmax(100)
      real pdpmax(100), depmin(100), pdpmin(100)
      real pdtmin(100), prcp
      integer ndetach, ndepos,noout

	avsole = wp_avsolf / years

      do i=1,mxeropts
        wp_dsavg(i) = wp_dsavg(i) / years
      end do

	do i=1, mxpart
        if (wp_frcff2(i).gt.0.0) then
          frcflw(i) = wp_frcff1(i) / wp_frcff2(i)
        else
          frcflw(i) = 0.0
        end if
      end do

	if (wp_enrff2.gt.0.) then
        enrato = wp_enrff1 / wp_enrff2
      else
        enrato = 0.0
      end if
                                    !
      call sedseg(wp_dsavg,luoweppsum(isr),years,noout,dstot,stdist,    &
     & wp_irdgdx,                                                       &
     & ysdist,wp_avgslp,wp_slplen,wp_y,                                 &
     & avedet,maxdet,ptdet,avedep,                                      &
     & maxdep,ptdep,detpt1, detpt2, dtavls, detstd, detmax, pdtmax,     &
     & detmin, pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,  &
     & depmin, pdpmin,ndetach,ndepos)
     
      call write_hydro_summary(luoweppsum(isr),wp%totalPrecip,          &
     &   wp%precipEvents,wp%totalRunoff,wp%runoffEvents,                &
     &   wp%totalSnowrunoff,wp%snowmeltEvents,years)
     
      call write_main_event(luoweppsum(isr),-1, -1, -1, prcp,           &
     &     wp_runoff*1000,                                              &
     &     wp_irdgdx,avedet,maxdet,ptdet,avedep,maxdep,ptdep,avsole,    &
     &     enrato,detpt1, detpt2, dtavls, detstd, detmax, pdtmax,       &
     & detmin, pdtmin, deppt1, deppt2, dpavls, depstd, depmax, pdpmax,  &
     &     depmin, pdpmin,ndetach,ndepos,wp_npart,wp_frac,frcflw,wp_dia,&
     &     wp_spg,wp_frsnd,wp_frslt, wp_frcly,wp_frorg,                 &
     &     wp_slplen,wp_fwidth,wp_avgslp,stdist,wp_dsavg,years,1)
     
      call write_plotfile(luoweppplot(isr),stdist,ysdist,wp_dsavg,100)

!      call sedout(luoweppsum(isr), luoweppplot(isr),wp_irdgdx,wp_dsavg, &
!     &    avsole, enrato,wp_npart,wp_frac,                              &
!     &    wp_dia,wp_spg,                                                &
!     &    wp_frcly,wp_frslt,wp_frsnd,wp_frorg,frcflw,                   &
!     &    wp_slplen,wp_fwidth,wp_avgslp,                                &
!     &    wp_y,wp_slplen, years)

      return
	  end

