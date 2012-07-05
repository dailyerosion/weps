!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine callhydr(daysim, isr)

! ***************************************************************** wjr
! Wrapper to call hydro
!
! Arguments
      integer daysim
      integer isr                   
!
! Includes
      include 'p1werm.inc'
      include 'b1glob.inc'
      include 'c1glob.inc'
      include 'c1gen.inc'
      include 'd1glob.inc'
      include 'm1sim.inc'
      include 'm1subr.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 's1sgeo.inc'
      include 's1surf.inc'
      include 'h1hydro.inc'
      include 'h1temp.inc'
      include 'h1db1.inc'
      include 'h1scs.inc'
      include 'h1balance.inc'
      include 'w1wind.inc'
      include 'w1clig.inc'
      include 'timer.inc'

      call timer(TIMHYDR,TIMSTART)      

      if (am0hdb .eq. 1) call hdbug(isr, nslay(isr))

      call hydro( nslay(isr), amrslp(isr),                              &
     &            adrlaitot(isr), adrsaitot(isr), abzht(isr),           &
     &            acrlai(isr), acrsai(isr), aczht(isr), acdayap(isr),   &
     &        acxrow(isr), ac0rg(isr), abfcancov(isr), acfliveleaf(isr),&
     &            abmf(isr), abevapredu(isr), aczrtd(isr), ahfwsf(isr), &
     &            aszlyd(1, isr), asdblk(1, isr), asdblk0(1,isr),       &
     &            asdpart(1, isr), asdwblk(1, isr), ahrwc(1, isr),      &
     &            ahrwcdmx(1, isr), ahrwcs(1, isr), ahrwcf(1, isr),     &
     &            ahrwcw(1, isr), ahrwcr(1,isr), ahrwca(1,isr),         &
     &            ah0cb(1,isr), aheaep(1,isr), ahfredsat(1,isr),        &
     &            asfsan(1,isr), asfsil(1,isr), asfcla(1,isr),          &
     &            asvroc(1,isr), asfom(1,isr), asfcec(1,isr),           &
     &            ahtsav(1,isr), abdstm(isr), abffcv(isr),              &
     &            asxrgs(isr), aszrgh(isr),                             &
     &            aslrro(isr), aslrr(isr), amzele,                      &
     &            ah0cng(isr), ah0cnp(isr), ahzper(isr),                &
     &       ahzirr(isr), ahzdmaxirr(isr), ahratirr(isr), ahdurirr(isr),&
     &            ahlocirr(isr), ahminirr(isr), am0monirr(isr),         &
     &            ahmadirr(isr), ahndayirr(isr), ahmintirr(isr),        &
     &            ahzoutflow(isr), ahzrun(isr), ahzinf(isr),            &
     &            ahzsno(isr), ahtsno(isr), ahfsnfrz(isr), ahzsnd(isr), &
     &            ahzsmt(isr), ahfice(1, isr), ahrsk(1, isr),           &
     &            ahtsmx(1, isr), ahtsmn(1, isr),                       &
     &            ahrwc0(1, isr), daysim,                               &
     &            asfald(isr), asfalw(isr), aszlyt(1,isr),              &
     &            awzdpt, awdurpt, awpeaktpt, awpeakipt,                &
     &            awtdmxprev, awtdmn, awtdmx, awtdmnnext,               &
     &            awtdav, awtyav, awrrh,                                &
     &            awtdpt, aweirr, awudav, ahzwid(isr),                  &
     &            ahzeasurf(isr),                                       &
     &            cumprecip(isr), cumrunoff(isr), cumevap(isr),         &
     &            cumtrans(isr), cumdrain(isr),                         &
     &            initswc(isr), initsnow(isr), initday(isr),            &
     &            presswc(isr), pressnow(isr), presday(isr),            &
     &            ahztranspdepth(isr) )

      if (am0hdb .eq. 1) call hdbug(isr, nslay(isr))
      call timer(TIMHYDR,TIMSTOP)      

      end
