!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine callsoil(daysim, isr, biotot)
! ***************************************************************** wjr
! Wrapper to call soil

      use weps_interface_defs
      use biomaterial, only: biototal
      use timer_def, only: TIMSOIL, TIMSTART, TIMSTOP

! Arguments
      integer daysim
      integer isr                   
      type(biototal), intent(in) :: biotot

! Includes
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1agg.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 's1sgeo.inc'
      include 's1surf.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'w1clig.inc'

      call timer(TIMSOIL,TIMSTART)      
!
            if (am0sdb .eq. 1) call sdbug(isr, nslay(isr), biotot)
            call soil (daysim, ahlocirr(isr), ahzirr(isr), ahzsmt(isr), &
     &                 ahtsmx(1,isr), ahtsmn(1,isr),                    &
     &                 ahrwc(1,isr), ahrwcdmx(1,isr), ahrwca(1,isr),    &
     &                 ahrwcw(1,isr), ahrwcs(1,isr),                    &
     &                 aszlyt(1,isr), nslay(isr),                       &
     &                 asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),     &
     &                 asfom(1,isr), asvroc(1,isr),                     &
     &                 asxrgs(isr), aszrgh(isr), aszrho(isr),           &
     &                 aslrr(isr), aslrro(isr),                         &
     &                 aszcr(isr), asfcr(isr), asecr(isr),              &
     &                 asdcr(isr), asmlos(isr), asflos(isr),            &
     &                 asdsblk(1,isr), asdwblk(1,isr),                  &
     &                 asdblk(0,isr), asdagd(0,isr),                    &
     &                 aslagm(0,isr), aslagn(0,isr),                    &
     &                 as0ags(0,isr), aslagx(0,isr), aseags(0,isr),     &
     &                 aseagm(1,isr), aseagmn(1,isr), aseagmx(1,isr),   &
     &                 ask4d(1,isr), aslmin(1,isr), aslmax(1,isr),      &
     &                 biotot%ffcvtot, biotot%fscvtot,                  &
     &                 asfcce(1,isr), asfcec(1,isr),                    &
     &                 ahzinf(isr), ahzwid(isr), awzdpt, awtdav         &
     &                )
            if (am0sdb .eq. 1) call sdbug(isr, nslay(isr), biotot)

      ! recalculate  depth to bottom of soil layer
      call depthini( nslay(isr), aszlyt(1,isr), aszlyd(1,isr) )

      call timer(TIMSOIL,TIMSTOP)      

!
      end               
