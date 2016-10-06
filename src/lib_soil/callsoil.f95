!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine callsoil(daysim, isr, croptot, biotot, h1et, subrsurf)
! ***************************************************************** wjr
! Wrapper to call soil

      use weps_interface_defs, ignore_me=>callsoil
      use biomaterial, only: biototal
      use timer_mod, only: timer, TIMSOIL, TIMSTART, TIMSTOP
      use soil_data_struct_defs, only: am0sdb
      use hydro_data_struct_defs, only: hydro_derived_et
      use erosion_data_struct_defs, only: subregionsurfacestate

! Arguments
      integer daysim
      integer isr                   
      type(biototal), intent(in) :: croptot, biotot
      type(hydro_derived_et), intent(inout) :: h1et
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions

! Includes
      include 'p1werm.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 's1agg.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'

      call timer(TIMSOIL,TIMSTART)      

            if (am0sdb(isr) .eq. 1) then
               call sdbug(isr, nslay(isr), croptot, biotot, h1et, subrsurf)
            end if
            call soil(isr,daysim,ahlocirr(isr),h1et%zirr, ahzsmt(isr),  &
     &                 ahtsmx(1,isr), ahtsmn(1,isr),                    &
     &                 ahrwc(1,isr), ahrwcdmx(1,isr), ahrwca(1,isr),    &
     &                 ahrwcw(1,isr), ahrwcs(1,isr),                    &
     &                 aszlyt(1,isr), nslay(isr),                       &
     &                 asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),     &
     &                 asfom(1,isr), asvroc(1,isr),                     &
     &                 asdsblk(1,isr), asdwblk(1,isr),                  &
     &                 asdblk(0,isr), asdagd(0,isr),                    &
     &                 aslagm(0,isr), aslagn(0,isr),                    &
     &                 as0ags(0,isr), aslagx(0,isr), aseags(0,isr),     &
     &                 aseagm(1,isr), aseagmn(1,isr), aseagmx(1,isr),   &
     &                 ask4d(1,isr), aslmin(1,isr), aslmax(1,isr),      &
     &                 biotot%ffcvtot, biotot%fscvtot,                  &
     &                 asfcce(1,isr), asfcec(1,isr),                    &
     &                 ahzinf(isr), ahzwid(isr), subrsurf)
            if (am0sdb(isr) .eq. 1) then
               call sdbug(isr, nslay(isr), croptot, biotot, h1et, subrsurf)
            end if

      ! recalculate  depth to bottom of soil layer
      call depthini( nslay(isr), aszlyt(1,isr), aszlyd(1,isr) )

      call timer(TIMSOIL,TIMSTOP)      

!
      end               
