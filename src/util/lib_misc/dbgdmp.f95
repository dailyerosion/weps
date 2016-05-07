!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine dbgdmp(day, sr, crop, residue, croptot, biotot, h1et)
! ****************************************************************** wjr
!     The dumps variables that have gone out of range

!       EDIT HISTORY
!       01-Mar-99       wjr     original coding

      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: awdair, awadir, awhrmx, awudmx, awudmn, awudav, subday, ntstep
      use climate_input_mod, only: cli_today, cli_tyav
      use hydro_data_struct_defs, only: hydro_derived_et

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: day
      integer, intent(in) :: sr
      type(biomatter), intent(in) :: crop
      type(biomatter), dimension(:), intent(in) :: residue
      type(biototal), intent(in) :: croptot
      type(biototal), intent(in) :: biotot
      type(hydro_derived_et), intent(inout) :: h1et

!     + + + GLOBAL COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1flag.inc'
      include 's1layr.inc'
      include 's1surf.inc'
      include 's1phys.inc'
      include 's1agg.inc'
      include 's1dbh.inc'
      include 's1dbc.inc'
      include 's1sgeo.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'h1hydro.inc'
      include 'h1scs.inc'
      include 'h1db1.inc'
      include 'h1temp.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'manage/man.inc'

      integer  idx,jdx
!
      real     tstmin
      parameter (tstmin=1e-10)
!      
      real     tstmax
      parameter (tstmax=1e10)
!      
      logical  dmpflg
      data dmpflg /.true./
! prototype, remove before compiling
!      if (#(sr).lt.tstmin.or.#(sr).gt.tstmax)
!     &  write(*,*) 'day ',day,' # ', #(sr)


! s1surf

      if (dmpflg) write(*,*) 's1surf'
!
      if (aszcr(sr).lt.0.0.or.aszcr(sr).gt.23.0)                        &
     &  write(*,*) 'day ',day,' aszcr ', aszcr(sr)
!
      if (asfcr(sr).lt.0.0.or.asfcr(sr).gt.1.0)                         &
     &  write(*,*) 'day ',day,' asfcr ', asfcr(sr)
!
      if (asmlos(sr).lt.0.0.or.asmlos(sr).gt.2.0)                       &
     &  write(*,*) 'day ',day,' asmlos ', asmlos(sr)
!
      if (asflos(sr).lt.0.0.or.asflos(sr).gt.1.0)                       &
     &  write(*,*) 'day ',day,' asflos ', asflos(sr)
!
! wjr,  test values based on definition
      if (asdcr(sr).lt.0.6.or.asdcr(sr).gt.2.0)                         &
     &  write(*,*) 'day ',day,' asdcr ', asdcr(sr)
!
      if (asecr(sr).lt.0.1.or.asecr(sr).gt.7.0)                         &
     &  write(*,*) 'day ',day,' asecr ', asecr(sr)
!
      if (asfald(sr).lt.0.05.or.asfald(sr).gt.0.25)                     &
     &  write(*,*) 'day ',day,' asfald ', asfald(sr)
!
      if (asfalw(sr).lt.0.05.or.asfalw(sr).gt.0.2)                      &
     &  write(*,*) 'day ',day,' asfalw ', asfalw(sr)
!
! w1info
!     
! ***      if (aw0cln.lt.70.0.or.aw0cln.gt.170.0)
! ***     *  write(*,*) 'day ',day,' aw0cln ', aw0cln
!
! ***      if (aw0clt.lt.15.0.or.aw0clt.gt.75.0)
! ***     *  write(*,*) 'day ',day,' aw0clt ', aw0clt
!
! ***      if (aw0wln.lt.70.0.or.aw0wln.gt.170.0)
! ***     *  write(*,*) 'day ',day,' aw0wln ', aw0wln
!
! ***      if (aw0wlt.lt.15.0.or.aw0wlt.gt.75.0)
! ***     *  write(*,*) 'day ',day,' aw0wlt ', aw0wlt
!
! s1sgeo
!
      if (dmpflg) write(*,*) 's1sgeo'
!      
      if (aszrgh(sr).lt.0.0.or.aszrgh(sr).gt.500.0)                     &
     &  write(*,*) 'day ',day,' aszrgh ', aszrgh(sr)
!
      if (asxrgw(sr).lt.10.0.or.asxrgw(sr).gt.4000.0)                   &
     &  write(*,*) 'day ',day,' asxrgw ', asxrgw(sr)
!
      if (asxrgs(sr).lt.10.0.or.asxrgs(sr).gt.2000.0)                   &
     &  write(*,*) 'day ',day,' asxrgs ', asxrgs(sr)
!
      if (asargo(sr).lt.0.0.or.asargo(sr).gt.179.0)                     &
     &  write(*,*) 'day ',day,' asargo ', asargo(sr)
!
! wjr,  test values based on definition
      if (asxdks(sr).lt.0.0.or.asxdks(sr).gt.1000.0)                    &
     &  write(*,*) 'day ',day,' asxdks ', asxdks(sr)
!
! wjr,  test values based on definition
      if (asxdkh(sr).lt.0.0.or.asxdkh(sr).gt.1000.0)                    &
     &  write(*,*) 'day ',day,' asxdkh ', asxdkh(sr)
!
      if (as0rrk(sr).lt.tstmin.or.as0rrk(sr).gt.tstmax)                 &
     &  write(*,*) 'day ',day,' as0rrk ', as0rrk(sr)
!
      if (aslrrc(sr).lt.tstmin.or.aslrrc(sr).gt.tstmax)                 &
     &  write(*,*) 'day ',day,' aslrrc ', aslrrc(sr)
!
      if (aslrr(sr).lt.1.0.or.aslrr(sr).gt.30.0)                        &
     &  write(*,*) 'day ',day,' aslrr ', aslrr(sr)
!
! w1wind
!
      if (dmpflg) write(*,*) 'w1wind'
!      
! wjr,  test values based on definition
      if (awadir.lt.0.0.or.awadir.gt.360.0)                             &
     &  write(*,*) 'day ',day,' awadir ', awadir
!
      if (awhrmx.lt.1.0.or.awhrmx.gt.24.0)                              &
     &  write(*,*) 'day ',day,' awhrmx ', awhrmx
!
! wjr,  test values based on definition
      if (awudmx.lt.0.0.or.awudmx.gt.50.0)                              &
     &  write(*,*) 'day ',day,' awudmx ', awudmx
!
! wjr,  test values based on definition
      if (awudmn.lt.0.0.or.awudmn.gt.25.0)                              &
     &  write(*,*) 'day ',day,' awudmn ', awudmn
!
! wjr,  test values based on definition
      if (awudav.lt.0.0.or.awudav.gt.35.0)                              &
     &  write(*,*) 'day ',day,' awudav ', awudav
!
      do 10 idx=1,mntime
! wjr,  test values based on definition
        if( subday(idx)%awu .lt. 0.0 .or. subday(idx)%awu .gt. 35.0 )   &
     &    write(*,*) 'day ',day,' awu(',idx,') ',  subday(idx)%awu
10    continue     
!
! w1pagv
!
! wjr,  test values based on definition
      if (awdair.lt.0.0.or.awdair.gt.tstmax)                            &
     &  write(*,*) 'day ',day,' awdair ', awdair
!
! b1geom
!
      if (dmpflg) write(*,*) 'b1geom'
!      
! wjr,  test values based on definition
      if (biotot%rsaitot .lt. 0.0 .or. biotot%rsaitot .gt. 1.0)  &
     &  write(*,*) 'day ',day,' biotot%rsaitot ', biotot%rsaitot
!
! wjr,  test values based on definition
      if (biotot%rlaitot .lt. 0.0 .or. biotot%rlaitot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%rlaitot ', biotot%rlaitot
!
      do 20 idx=1,mncz
!      
! wjr,  test values based on definition
      if (biotot%rsaz(idx) .lt. 0.0 .or. biotot%rsaz(idx) .gt. tstmax) &
     &  write(*,*) 'day ',day,' biotot%rsaz(',idx,') ', biotot%rsaz(idx)
!
! wjr,  test values based on definition
      if (biotot%rlaz(idx) .lt. 0.0 .or. biotot%rlaz(idx) .gt. tstmax) &
     &  write(*,*) 'day ',day,' biotot%rlaz(',idx,') ', biotot%rlaz(idx)
!
   20 continue
!
! wjr,  test values based on definition
      if (biotot%ffcvtot .lt. 0.0 .or. biotot%ffcvtot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%ffcvtot ', biotot%ffcvtot
!
! wjr,  test values based on definition
      if (biotot%fscvtot .lt. 0.0 .or. biotot%fscvtot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%fscvtot ', biotot%fscvtot
!
! wjr,  test values based on definition
      if (biotot%ftcvtot .lt. 0.0 .or. biotot%ftcvtot .gt. 1.0) &
     &  write(*,*) 'day ',day,' biotot%ftcvtot ', biotot%ftcvtot
!
! w1clig
!
      if (dmpflg) write(*,*) 'w1clig'
!      
! wjr,  test values based on definition
      if (cli_today%tdav.lt.-20.0.or.cli_today%tdav.gt.50.0)     &
     &  write(*,*) 'day ',day,' cli_today%tdav ', cli_today%tdav
!
! wjr,  test values based on definition
      if (cli_tyav.lt.0.0.or.cli_tyav.gt.30.0)      &
     &  write(*,*) 'day ',day,' cli_tyav ', cli_tyav
!
! wjr,  test values based on definition
      if (cli_today%tdmx.lt.0.0.or.cli_today%tdmx.gt.50.0)      &
     &  write(*,*) 'day ',day,' cli_today%tdmx ', cli_today%tdmx
!
! wjr,  test values based on definition
      if (cli_today%tdmn.lt.-20.0.or.cli_today%tdmn.gt.40.0)    &
     &  write(*,*) 'day ',day,' cli_today%tdmn ', cli_today%tdmn
!
! wjr,  test values based on definition
      if (cli_today%tdpt.lt.0.0.or.cli_today%tdpt.gt.40.0)      &
     &  write(*,*) 'day ',day,' cli_today%tdpt ', cli_today%tdpt
!
! wjr,  test values based on definition
      if (cli_today%zdpt.lt.0.0.or.cli_today%zdpt.gt.1000.0)    &
     &  write(*,*) 'day ',day,' cli_today%zdpt ', cli_today%zdpt
!
! wjr,  test values based on definition
      if (cli_today%eirr.lt.0.0.or.cli_today%eirr.gt.tstmax)    &
     &  write(*,*) 'day ',day,' cli_today%eirr ', cli_today%eirr
!
! s1psd
!
! ***      do 40 idx=1,mnsz
! ***      if (aslsgm(idx, sr).lt.0.0.or.aslsgm(idx, sr).gt.10.0)
! ***     *  write(*,*) 'day ',day,' aslsgm ', aslsgm(idx, sr)
!
! ***      if (as0sgs(idx, sr).lt.0.0.or.as0sgs(idx, sr).gt.10.0)
! ***     *  write(*,*) 'day ',day,' as0sgs ', as0sgs(idx, sr)
! ***   40 continue
!
! s1layd
!
      if (dmpflg) write(*,*) 's1layd'
!      
      do 50 idx=1,nslay(sr)
      if (asdsblk(idx, sr).lt.tstmin.or.asdsblk(idx, sr).gt.tstmax)     &
     &  write(*,*) 'day ',day,' asdsblk(',idx,') ', asdsblk(idx, sr)
!
      if (aszlyd(idx, sr).lt.tstmin.or.aszlyd(idx, sr).gt.tstmax)       &
     &  write(*,*) 'day ',day,' aszlyd(',idx,') ', aszlyd(idx, sr)
!
! ***      if (aszlym(idx, sr).lt.tstmin.or.aszlym(idx, sr).gt.tstmax)
! ***     *  write(*,*) 'day ',day,' aszlym(',idx,') ', aszlym(idx, sr)
!
! ***      if (aszmpt(idx, sr).lt.tstmin.or.aszmpt(idx, sr).gt.tstmax)
! ***     *  write(*,*) 'day ',day,' aszmpt(',idx,') ', aszmpt(idx, sr)
   50 continue
!
! s1layr
!
      if (dmpflg) write(*,*) 's1layr'
!      
      if (nslay(sr).lt.1.or.nslay(sr).gt.10)                            &
     &  write(*,*) 'day ',day,' nslay ', nslay(sr)
!
      if (aszlyt(1, sr).lt.10.0.or.aszlyt(1, sr).gt.10.0)               &
     &  write(*,*) 'day ',day,' aszlyt(1) ', aszlyt(1, sr)
!
      if (nslay(sr).gt.1.and.                                           &
     & (aszlyt(2, sr).lt.40.0.or.aszlyt(2, sr).gt.40.0))                &
     &  write(*,*) 'day ',day,' aszlyt(2) ', aszlyt(2, sr)
!
      if (nslay(sr).gt.2.and.                                           &
     & (aszlyt(3, sr).lt.50.0.or.aszlyt(3, sr).gt.100.0))               &
     &  write(*,*) 'day ',day,' aszlyt(3) ', aszlyt(3, sr)
!
      if (nslay(sr).gt.3.and.                                           &
     & (aszlyt(4, sr).lt.50.0.or.aszlyt(4, sr).gt.100.0))               &
     &  write(*,*) 'day ',day,' aszlyt(4) ', aszlyt(4, sr)
!
      do 60 idx=5,mnsz+1
      if (nslay(sr).ge.idx.and.                                         &
     & (aszlyt(idx, sr).lt.1.0.or.aszlyt(idx, sr).gt.1000.0))           &
     &  write(*,*) 'day ',day,' aszlyt(',idx,') ', aszlyt(idx, sr)
   60 continue     
!
! s1phys
!
      if (dmpflg) write(*,*) 's1phys'
!      
      do 70 idx=0,mnsz
      if (asdblk(idx, sr).lt.0.50.or.asdblk(idx, sr).gt.2.5)            &
     &  write(*,*) 'day ',day,' asdblk(',idx,') ', asdblk(idx, sr)
  70  continue
!
! s1dbh
!
      if (dmpflg) write(*,*) 's1dbh'
!      
      do 80 idx=0,mnsz
      if (asfsan(idx, sr).lt.0.0.or.asfsan(idx, sr).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfsan(',idx,') ', asfsan(idx, sr)
!
      if (asfsil(idx, sr).lt.0.0.or.asfsil(idx, sr).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfsil(',idx,') ', asfsil(idx, sr)
!
      if (asfcla(idx, sr).lt.0.0.or.asfcla(idx, sr).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfcla(',idx,') ', asfcla(idx, sr)
!
      if (asvroc(idx, sr).lt.0.0.or.asvroc(idx, sr).gt.1.0)             &
     &  write(*,*) 'day ',day,' asvroc(',idx,') ', asvroc(idx, sr)
   80 continue     
!
! s1agg
!
      if (dmpflg) write(*,*) 's1agg'
!      
      do 90 idx=0,mnsz
      if (asdagd(idx, sr).lt.0.6.or.asdagd(idx, sr).gt.2.5)             &
     &  write(*,*) 'day ',day,' asdagd(',idx,') ', asdagd(idx, sr)
!
      if (aseags(idx, sr).lt.0.1.or.aseags(idx, sr).gt.7.0)             &
     &  write(*,*) 'day ',day,' aseags(',idx,') ', aseags(idx, sr)
!
      if (aslagm(idx, sr).lt.0.03.or.aslagm(idx, sr).gt.30.0)           &
     &  write(*,*) 'day ',day,' aslagm(',idx,') ', aslagm(idx, sr)
!
      if (aslagn(idx, sr).lt.0.001.or.aslagn(idx, sr).gt.5.0)           &
     &  write(*,*) 'day ',day,' aslagn(',idx,') ', aslagn(idx, sr)
!
      if (aslagx(idx, sr).lt.1.0.or.aslagx(idx, sr).gt.1000.0)          &
     &  write(*,*) 'day ',day,' aslagx(',idx,') ', aslagx(idx, sr)
!
      if (as0ags(idx, sr).lt.1.0.or.as0ags(idx, sr).gt.20.0)            &
     &  write(*,*) 'day ',day,' as0ags(',idx,') ', as0ags(idx, sr)
   90 continue     
!
! s1dbc
!
      if (dmpflg) write(*,*) 's1dbc'
!      
      if (asfom(0, sr).lt.tstmin.or.asfom(0, sr).gt.tstmax)             &
     &  write(*,*) 'day ',day,' asfom(0) ', asfom(0, sr)
!
      do 100 idx=1,mnsz
      if (as0ph(idx, sr).lt.0.0.or.as0ph(idx, sr).gt.14.0)              &
     &  write(*,*) 'day ',day,' as0ph(',idx,') ', as0ph(idx, sr)
!
      if (ascmg(idx, sr).lt.0.0.or.ascmg(idx, sr).gt.tstmax)            &
     &  write(*,*) 'day ',day,' ascmg(',idx,') ', ascmg(idx, sr)
!
      if (ascna(idx, sr).lt.0.0.or.ascna(idx, sr).gt.tstmax)            &
     &  write(*,*) 'day ',day,' ascna(',idx,') ', ascna(idx, sr)
!
      if (asfcce(idx, sr).lt.0.0.or.asfcce(idx, sr).gt.100.0)           &
     &  write(*,*) 'day ',day,' asfcce(',idx,') ', asfcce(idx, sr)
!
      if (asfcec(idx, sr).lt.0.0.or.asfcec(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asfcec(',idx,') ', asfcec(idx, sr)
!
      if (asfesp(idx, sr).lt.0.0.or.asfesp(idx, sr).gt.100.0)           &
     &  write(*,*) 'day ',day,' asfesp(',idx,') ', asfesp(idx, sr)
!
      if (asfom(idx, sr).lt.0.0.or.asfom(idx, sr).gt.tstmax)            &
     &  write(*,*) 'day ',day,' asfom(',idx,') ', asfom(idx, sr)
!
      if (asfnoh(idx, sr).lt.0.0.or.asfnoh(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asfnoh(',idx,') ', asfnoh(idx, sr)
!
      if (asfpoh(idx, sr).lt.0.0.or.asfpoh(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asfpoh(',idx,') ', asfpoh(idx, sr)
!
      if (asfpsp(idx, sr).lt.0.0.or.asfpsp(idx, sr).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfpsp(',idx,') ', asfpsp(idx, sr)
!
      if (asfsmb(idx, sr).lt.0.0.or.asfsmb(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asfsmb(',idx,') ', asfsmb(idx, sr)
!
      if (asftap(idx, sr).lt.0.0.or.asftap(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asftap(',idx,') ', asftap(idx, sr)
!
      if (asftan(idx, sr).lt.0.0.or.asftan(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asftan(',idx,') ', asftan(idx, sr)
  100 continue
!
      if (asmno3(sr).lt.0.0.or.asmno3(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' asmno3 ', asmno3(sr)
!
! m1sim
!
      if (dmpflg) write(*,*) 'm1sim'
!      
      if (ntstep.lt.1.or.ntstep.gt.96)                                  &
     &  write(*,*) 'day ',day,' ntstep ', ntstep
!
      if (am0jd.ne.day)                                                 &
     &  write(*,*) 'day ',day,' am0jd ', am0jd
!
      if (amalat.lt.15.0.or.amalat.gt.75.0)                             &
     &  write(*,*) 'day ',day,' amalat ', amalat
!
      if (amalon.lt.70.0.or.amalon.gt.170.0)                            &
     &  write(*,*) 'day ',day,' amalon ', amalon
!
      if (amzele.lt.0.0.or.amzele.gt.2500.0)                            &
     &  write(*,*) 'day ',day,' amzele ', amzele
!
! m1subr
!
      if (dmpflg) write(*,*) 'm1subr'
!
       if (amnryr(sr).lt.1.or.amnryr(sr).gt.10)                          &
     &  write(*,*) 'day ',day,' amnryr ', amnryr(sr)
!
      if (amrslp(sr).lt.0.0.or.amrslp(sr).gt.1.0)                       &
     &  write(*,*) 'day ',day,' amrslp ', amrslp(sr)
!
! h1temp
!
      if (dmpflg) write(*,*) 'h1temp'
!      
      do 110 idx=1,mnsz
      if (ahtsav(idx, sr).lt.-20.0.or.ahtsav(idx, sr).gt.50.0)          &
     &  write(*,*) 'day ',day,' ahtsav(',idx,') ', ahtsav(idx, sr)
!
      if (ahtsmx(idx, sr).lt.-20.0.or.ahtsmx(idx, sr).gt.50.0)          &
     &  write(*,*) 'day ',day,' ahtsmx(',idx,') ', ahtsmx(idx, sr)
!
      if (ahtsmn(idx, sr).lt.-20.0.or.ahtsmn(idx, sr).gt.50.0)          &
     &  write(*,*) 'day ',day,' ahtsmn(',idx,') ', ahtsmn(idx, sr)
  110 continue     
!
! h1et
!
! ***      if (ahzea.lt.0.0.or.ahzea.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzea ', ahzea
!
! ***      if (ahzep.lt.0.0.or.ahzep.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzep ', ahzep
!
! ***      if (ahzeta.lt.0.0.or.ahzeta.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzeta ', ahzeta
!
! ***      if (ahzetp.lt.0.0.or.ahzetp.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzetp ', ahzetp
!
! ***      if (ahzpta.lt.0.0.or.ahzpta.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzpta ', ahzpta
!
! ***      if (ahzptp.lt.0.0.or.ahzptp.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzptp ', ahzptp
!
! ***      if (ah0drat.lt.0.0.or.ah0drat.gt.1.0)
! ***     *  write(*,*) 'day ',day,' ah0drat ', ah0drat
!
! h1hydro
!
      if (dmpflg) write(*,*) 'h1hydro'
!      
      do 120 idx=1,mnsz
      if (ahrwc(idx, sr).lt.0.011.or.ahrwc(idx, sr).gt.0.379)           &
     &  write(*,*) 'day ',day,' ahrwc(',idx,') ', ahrwc(idx, sr)
!
      if (aheaep(idx, sr).lt.-17.91.or.aheaep(idx, sr).gt.0.0)          &
     &  write(*,*) 'day ',day,' aheaep(',idx,') ', aheaep(idx, sr)
!
      if (ahrsk(idx, sr).lt.0.0.or.ahrsk(idx, sr).gt.0.001)             &
     &  write(*,*) 'day ',day,' ahrsk(',idx,') ', ahrsk(idx, sr)
!
      if (ah0cb(idx, sr).lt.0.917.or.ah0cb(idx, sr).gt.27.927)          &
     &  write(*,*) 'day ',day,' ah0cb(',idx,') ', ah0cb(idx, sr)
  120 continue
!
      if (ahfwsf(sr).lt.tstmin.or.ahfwsf(sr).gt.tstmax)                 &
     &  write(*,*) 'day ',day,' ahfwsf ', ahfwsf(sr)
!
      if (ahzsno(sr).lt.0.0.or.ahzsno(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ahzsno ', ahzsno(sr)
!
      if (h1et%zirr.lt.0.0.or.h1et%zirr.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' h1et%zirr ', h1et%zirr
!
      if (h1et%zper.lt.0.0.or.h1et%zper.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' h1et%zper ', h1et%zper
!
      if (h1et%zrun.lt.0.0.or.h1et%zrun.gt.tstmax)                    &
     &  write(*,*) 'day ',day,' h1et%zrun ', h1et%zrun
!
      if (ahzsmt(sr).lt.0.0.or.ahzsmt(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ahzsmt ', ahzsmt(sr)
!
! h1scs
!
      if (dmpflg) write(*,*) 'h1scs'
!      
      if (ah0cng(sr).lt.6.0.or.ah0cng(sr).gt.91.0)                      &
     &  write(*,*) 'day ',day,' ah0cng ', ah0cng(sr)
!
      if (ah0cnp(sr).lt.45.0.or.ah0cnp(sr).gt.94.0)                     &
     &  write(*,*) 'day ',day,' ah0cnp ', ah0cnp(sr)
!
! h1db1
!
      if (dmpflg) write(*,*) 'h1db1'
!      
      do 130 idx=1,mnsz
      if (ahrwcw(idx, sr).lt.0.005.or.ahrwcw(idx, sr).gt.0.242)         &
     &  write(*,*) 'day ',day,' ahrwcw(',idx,') ', ahrwcw(idx, sr)
!
      if (ahrwcf(idx, sr).lt.0.012.or.ahrwcf(idx, sr).gt.0.335)         &
     &  write(*,*) 'day ',day,' ahrwcf(',idx,') ', ahrwcf(idx, sr)
!
      if (ahrwcs(idx, sr).lt.0.208.or.ahrwcs(idx, sr).gt.0.440)         &
     &  write(*,*) 'day ',day,' ahrwcs(',idx,') ', ahrwcs(idx, sr)
!
      if (ahrwca(idx, sr).lt.0.0.or.ahrwca(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ahrwca(',idx,') ', ahrwca(idx, sr)
  130 continue     
!
      do 140 idx=1,mnhhrs
      if (ahrwc0(idx, sr).lt.0.0.or.ahrwc0(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ahrwc0(',idx,') ', ahrwc0(idx, sr)
  140 continue     
!
! h1et
!      if (ahzsnd(sr).lt.0.0.or.ahzsnd(sr).gt.tstmax)                    &
!     &  write(*,*) 'day ',day,' ahzsnd ', ahzsnd(sr)

! p1werm

   do idx=1,mnbpls
      if (residue(idx)%deriv%mf .lt. 0.0 .or. residue(idx)%deriv%mf .gt. tstmax) &
         write(*,*) 'day ',day,' residue(',idx,')%deriv%mf', residue(idx)%deriv%mf
   end do

   do idx=1,mnsz
      do jdx=1,mnbpls
         if (residue(jdx)%deriv%mbgz(idx) .lt. 0.0 .or. residue(jdx)%deriv%mbgz(idx) .gt. tstmax) &
            write(*,*) 'day ',day,' residue(',jdx,')%deriv%mbgz(',idx,') ', residue(jdx)%deriv%mbgz(idx)
!
         if (residue(jdx)%deriv%mrtz(idx) .lt. 0.0 .or. residue(jdx)%deriv%mrtz(idx) .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',jdx,')%deriv%mrtz(',idx,') ', residue(jdx)%deriv%mrtz(idx)
      end do
   end do

! d1glob

      if (dmpflg) write(*,*) 'd1glob'
!      
   do idx=1,mnbpls
      if (residue(idx)%geometry%zht .lt. tstmin .or. residue(idx)%geometry%zht .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%geometry%zht ', residue(idx)%geometry%zht

      if (residue(idx)%deriv%m .lt. 0.0 .or. residue(idx)%deriv%m .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%deriv%m ', residue(idx)%deriv%m

      if (residue(idx)%deriv%mst .lt. 0.0 .or. residue(idx)%deriv%mst .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%deriv%mst ', residue(idx)%deriv%mst

      if (residue(idx)%deriv%mf .lt. 0.0 .or. residue(idx)%deriv%mf .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%deriv%mf ', residue(idx)%deriv%mf

      if (residue(idx)%deriv%mbg .lt. 0.0 .or. residue(idx)%deriv%mbg .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%deriv%mbg ', residue(idx)%deriv%mbg

      if (residue(idx)%deriv%mrt .lt. 0.0 .or. residue(idx)%deriv%mrt .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%deriv%mrt ', residue(idx)%deriv%mrt

      if (residue(idx)%geometry%dstm .lt. 0.0 .or. residue(idx)%geometry%dstm .gt. tstmax) &
     &  write(*,*) 'day ',day,' residue(',idx,')%geometry%dstm ', residue(idx)%geometry%dstm
   end do

! c1gen
!
      if (dmpflg) write(*,*) 'c1gen'
!      
      if (ac0rg(sr).lt.0.or.ac0rg(sr).gt.1)                             &
     &  write(*,*) 'day ',day,' ac0rg ', ac0rg(sr)
!
      if (acdpop(sr).lt.0.0.or.acdpop(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' acdpop ', acdpop(sr)
!
      if (acxrow(sr).lt.0.0.or.acxrow(sr).gt.1.0)                       &
     &  write(*,*) 'day ',day,' acxrow ', acxrow(sr)

      if (croptot%rsaitot.lt.0.0.or.croptot%rsaitot.gt.tstmax) &
          write(*,*) 'day ',day,' croptot%rsaitot ', croptot%rsaitot
!
      if (croptot%rlaitot.lt.0.0.or.croptot%rlaitot.gt.tstmax) &
          write(*,*) 'day ',day,' croptot%rlaitot ', croptot%rlaitot
!
      do 191 idx=1,mncz
      if (croptot%rsaz(idx).lt.0.0.or.croptot%rsaz(idx).gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rsaz(',idx,') ', croptot%rsaz(idx)
!
      if (croptot%rlaz(idx).lt.0.0.or.croptot%rlaz(idx).gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rlaz(',idx,') ', croptot%rlaz(idx)
191   continue
!
      if (croptot%ffcvtot.lt.0.0.or.croptot%ffcvtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%ffcvtot ', croptot%ffcvtot
!
      if (croptot%fscvtot.lt.0.0.or.croptot%fscvtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%fscvtot ', croptot%fscvtot
!
      if (croptot%ftcvtot.lt.0.0.or.croptot%ftcvtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%ftcvtot ', croptot%ftcvtot
!
! c1glob
!
      if (dmpflg) write(*,*) 'c1glob'
!      
      if (croptot%zht_ave.lt.0.0.or.croptot%zht_ave.gt.3.0) &
         write(*,*) 'day ',day,' croptot%zht_ave ', croptot%zht_ave

      if (croptot%mtot.lt.0.0.or.croptot%mtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%mtot ', croptot%mtot
!
      if (croptot%msttot.lt.0.0.or.croptot%msttot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%msttot ', croptot%msttot
!
      if (croptot%mrttot.lt.0.0.or.croptot%mrttot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%mrttot ', croptot%mrttot
!
      do 2000 idx = 1, nslay(sr)
        if (croptot%mrtz(idx).lt.0.0.or.croptot%mrtz(idx).gt.tstmax) &
           write(*,*) 'day ',day,' croptot%mrtz ', croptot%mrtz(idx)
2000  continue
!
      if (croptot%rsaitot.lt.0.0.or.croptot%rsaitot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rsaitot ', croptot%rsaitot
!
      if (croptot%rlaitot.lt.0.0.or.croptot%rlaitot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%rlaitot ', croptot%rlaitot
!
      do 2100 idx = 1, mncz 
        if (croptot%rsaz(idx).lt.0.0.or.croptot%rsaz(idx).gt.tstmax) &
           write(*,*) 'day ',day,' croptot%rsaz ', croptot%rsaz(idx)
        if (croptot%rlaz(idx).lt.0.0.or.croptot%rlaz(idx).gt.tstmax) &
           write(*,*) 'day ',day,' croptot%rlaz ', croptot%rlaz(idx)
2100  continue
!
      if (croptot%ffcvtot.lt.0.0.or.croptot%ffcvtot.gt. 1.0) &
         write(*,*) 'day ',day,' croptot%ffcvtot ', croptot%ffcvtot
!
      if (croptot%fscvtot.lt.0.0.or.croptot%fscvtot.gt. 1.0) &
         write(*,*) 'day ',day,' croptot%fscvtot ', croptot%fscvtot
!
      if (croptot%ftcvtot.lt.0.0.or.croptot%ftcvtot.gt. 1.0) &
         write(*,*) 'day ',day,' croptot%ftcvtot ', croptot%ftcvtot
!
      if (croptot%dstmtot.lt.0.0.or.croptot%dstmtot.gt.tstmax) &
         write(*,*) 'day ',day,' croptot%dstmtot ', croptot%dstmtot

! cldb2

      if (dmpflg) write(*,*) 'c1db2'
      
      if (actopt(sr).lt.0.0.or.actopt(sr).gt.40.0)                      &
     &  write(*,*) 'day ',day,' actopt ', actopt(sr)

      if (actmin(sr).lt.0.0.or.actmin(sr).gt.20.0)                      &
     &  write(*,*) 'day ',day,' actmin ', actmin(sr)

      do 200 idx=1,2

      if (ac0fd1(idx, sr).lt.0.0.or.ac0fd1(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ac0fd1(',idx,') ', ac0fd1(idx, sr)

      if (ac0fd2(idx, sr).lt.0.0.or.ac0fd2(idx, sr).gt.1.0)             &
     &  write(*,*) 'day ',day,' ac0fd2(',idx,') ', ac0fd2(idx, sr)
  200 continue     

      if (crop%database%ck.lt.0.0.or.crop%database%ck.gt.1.0)                         &
     &  write(*,*) 'day ',day,' ac0ck ', crop%database%ck

! c1db1

      if (dmpflg) write(*,*) 'c1db1'
      
      if (acrcn(sr).lt.0.0.or.acrcn(sr).gt.tstmax)                      &
     &  write(*,*) 'day ',day,' acrcn ', acrcn(sr)

      if (actdtm(sr).lt.0.0.or.actdtm(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' actdtm ', actdtm(sr)
!
      if (aczmrt(sr).lt.0.0.or.aczmrt(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' aczmrt ', aczmrt(sr)
!
      if (aczmxc(sr).lt.0.0.or.aczmxc(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' aczmxc ', aczmxc(sr)
!
      if (acrbe(sr).lt.0.0.or.acrbe(sr).gt.tstmax)                      &
     &  write(*,*) 'day ',day,' acrbe ', acrbe(sr)
!
      if (acrbed(sr).lt.0.0.or.acrbed(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' acrbed ', acrbed(sr)
!
      do 220 idx=1,mncz
      if (ac0lad(idx, sr).lt.0.0.or.ac0lad(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ac0lad(',idx,') ', ac0lad(idx, sr)
!
      if (ac0sad(idx, sr).lt.0.0.or.ac0sad(idx, sr).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ac0sad(',idx,') ', ac0sad(idx, sr)
  220 continue     
!
      if (acehu0(sr).lt.0.0.or.acehu0(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' acehu0 ', acehu0(sr)
!
      if (ac0alf(sr).lt.0.0.or.ac0alf(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0alf ', ac0alf(sr)
!
      if (ac0blf(sr).lt.0.0.or.ac0blf(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0blf ', ac0blf(sr)
!
      if (ac0clf(sr).lt.0.0.or.ac0clf(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0clf ', ac0clf(sr)
!
      if (ac0dlf(sr).lt.0.0.or.ac0dlf(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0dlf ', ac0dlf(sr)
!
      if (ac0arp(sr).lt.0.0.or.ac0arp(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0arp ', ac0arp(sr)
!
      if (ac0brp(sr).lt.0.0.or.ac0brp(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0brp ', ac0brp(sr)
!
      if (ac0crp(sr).lt.0.0.or.ac0crp(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0crp ', ac0crp(sr)
!
      if (ac0drp(sr).lt.0.0.or.ac0drp(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0drp ', ac0drp(sr)
!
      if (ac0aht(sr).lt.0.0.or.ac0aht(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0aht ', ac0aht(sr)
!
      if (ac0bht(sr).lt.0.0.or.ac0bht(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0bht ', ac0bht(sr)
!
      if (ac0ssa(sr).lt.0.0.or.ac0ssa(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0ssa ', ac0ssa(sr)
!
      if (ac0ssb(sr).lt.0.0.or.ac0ssb(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0ssb ', ac0ssb(sr)

      if (crop%database%sla.lt.0.0.or.crop%database%sla.gt.tstmax) &
        write(*,*) 'day ',day,' ac0sla ', crop%database%sla

      if (ac0hue(sr).lt.0.0.or.ac0hue(sr).gt.tstmax)                    &
     &  write(*,*) 'day ',day,' ac0hue ', ac0hue(sr)
!
      if (dmpflg) write(*,*) 'end dbgdmp'
!      
      end
