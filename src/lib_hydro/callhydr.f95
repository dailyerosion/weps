!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine callhydr(daysim, isr, crop, restot, biotot, h1et, wp, subrsurf)

! ***************************************************************** wjr
! Wrapper to call hydro

      use weps_interface_defs, only: hdbug, hydro
      use biomaterial, only: biototal, biomatter
      use timer_mod, only: timer, TIMHYDR, TIMSTART, TIMSTOP
      use erosion_data_struct_defs, only: awudav
      use hydro_data_struct_defs, only: am0hdb, hydro_derived_et
      use wepp_param_mod, only: wepp_param
      use erosion_data_struct_defs, only: subregionsurfacestate

!     + + + ARGUMENT DECLARATIONS + + +
      integer daysim
      integer isr                   
      type(biomatter), intent(in) :: crop
      type(biototal), intent(in) :: restot
      type(biototal), intent(in) :: biotot
      type(hydro_derived_et), intent(inout) :: h1et
      type(wepp_param), intent(inout) :: wp
      type(subregionsurfacestate), intent(in) :: subrsurf  ! subregion surface conditions

!     + + + ARGUMENT DEFINITIONS + + +
!     restot          - structure array containing summary residue pool amounts for all subregions

! Includes
      include 'p1werm.inc'
      include 'c1gen.inc'
      include 'm1sim.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'h1scs.inc'
      include 'h1balance.inc'

      call timer(TIMHYDR,TIMSTART)      

      if (am0hdb(isr) .eq. 1) then
         call hdbug(isr, nslay(isr), crop, restot, h1et, subrsurf)
      end if

      call hydro( isr, nslay(isr), amrslp(isr), biotot%zht_ave,         &
     &            crop%deriv%rlai, crop%deriv%rsai, crop%geometry%zht, crop%growth%dayap,   &
     &       acxrow(isr), ac0rg(isr), biotot%ftcancov, crop%growth%fliveleaf,&
     &         biotot%mftot, biotot%evapredu, crop%geometry%zrtd, ahfwsf(isr), &
     &            aszlyd(1, isr), asdblk(1, isr), asdblk0(1,isr),       &
     &            asdpart(1, isr), asdwblk(1, isr), ahrwc(1, isr),      &
     &            ahrwcdmx(1, isr), ahrwcs(1, isr), ahrwcf(1, isr),     &
     &            ahrwcw(1, isr), ahrwcr(1,isr), ahrwca(1,isr),         &
     &            ah0cb(1,isr), aheaep(1,isr), ahfredsat(1,isr),        &
     &            asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),          &
     &            asvroc(1,isr), asfom(1,isr), asfcec(1,isr),           &
     &            ahtsav(1,isr), biotot%dstmtot, biotot%ffcvtot,        &
     &            subrsurf%asxrgs, subrsurf%aszrgh, subrsurf%asfcr, &
                  subrsurf%aslrro, subrsurf%aslrr, amzele, &
     &            ahzdmaxirr(isr), ahratirr(isr), ahdurirr(isr),        &
     &            ahlocirr(isr), ahminirr(isr), am0monirr(isr),         &
     &            ahmadirr(isr), ahndayirr(isr), ahmintirr(isr),        &
     &            ahzoutflow(isr), ahzinf(isr),            &
     &            ahzsno(isr), ahtsno(isr), ahfsnfrz(isr), &
     &            ahzsmt(isr), ahfice(1, isr), ahrsk(1, isr),           &
     &            ahtsmx(1, isr), ahtsmn(1, isr),                       &
     &            ahrwc0(1, isr), daysim,                               &
     &            subrsurf%asfald, subrsurf%asfalw, aszlyt(1,isr), &
     &            awudav, ahzwid(isr),                  &
     &            ahzeasurf(isr),                                       &
     &            cumprecip(isr), cumirrig(isr), &
     &            cumrunoff(isr), cumevap(isr),         &
     &            cumtrans(isr), cumdrain(isr),                         &
     &            presswc(isr), pressnow(isr), presday(isr),            &
     &            ahztranspdepth(isr), restot, h1et, wp )

! removed from call: ah0cng(isr), ah0cnp(isr), 
!                 initswc(isr), initsnow(isr), initday(isr)

      if (am0hdb(isr) .eq. 1) then
         call hdbug(isr, nslay(isr), crop, restot, h1et, subrsurf)
      end if
      call timer(TIMHYDR,TIMSTOP)      

      end
