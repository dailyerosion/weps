!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine dbgdmp(day, sr, soil, crop, residue, croptot, biotot, h1et)
! ****************************************************************** wjr
!     The dumps variables that have gone out of range

!       EDIT HISTORY
!       01-Mar-99       wjr     original coding

      use soil_data_struct_defs, only: soil_def
      use biomaterial, only: biomatter, biototal
      use erosion_data_struct_defs, only: awdair, awadir, awhrmx, awudmx, awudmn, awudav, subday, ntstep
      use climate_input_mod, only: cli_today, cli_tyav
      use hydro_data_struct_defs, only: hydro_derived_et
      use erosion_data_struct_defs, only: subregionsurfacestate

!     + + + ARGUMENT DECLARATIONS + + +
      integer, intent(in) :: day
      integer, intent(in) :: sr
      type(soil_def), intent(in) :: soil  ! soil for this subregion
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

      if (soil%aszcr.lt.0.0.or.soil%aszcr.gt.23.0) &
        write(*,*) 'day ',day,' aszcr ', soil%aszcr

      if (soil%asfcr.lt.0.0.or.soil%asfcr.gt.1.0) &
        write(*,*) 'day ',day,' asfcr ', soil%asfcr

      if (soil%asmlos.lt.0.0.or.soil%asmlos.gt.2.0) &
        write(*,*) 'day ',day,' asmlos ', soil%asmlos

      if (soil%asflos.lt.0.0.or.soil%asflos.gt.1.0) &
        write(*,*) 'day ',day,' asflos ', soil%asflos

! wjr,  test values based on definition
      if (soil%asdcr.lt.0.6.or.soil%asdcr.gt.2.0) &
        write(*,*) 'day ',day,' asdcr ', soil%asdcr

      if (soil%asecr.lt.0.1.or.soil%asecr.gt.7.0) &
        write(*,*) 'day ',day,' asecr ', soil%asecr

      if (soil%asfald.lt.0.05.or.soil%asfald.gt.0.25) &
        write(*,*) 'day ',day,' asfald ', soil%asfald

      if (soil%asfalw.lt.0.05.or.soil%asfalw.gt.0.2) &
        write(*,*) 'day ',day,' asfalw ', soil%asfalw
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
      if (soil%aszrgh.lt.0.0.or.soil%aszrgh.gt.500.0) &
     &  write(*,*) 'day ',day,' aszrgh ', soil%aszrgh
!
      if (soil%asxrgw.lt.10.0.or.soil%asxrgw.gt.4000.0) &
     &  write(*,*) 'day ',day,' asxrgw ', soil%asxrgw
!
      if (soil%asxrgs.lt.10.0.or.soil%asxrgs.gt.2000.0)                   &
     &  write(*,*) 'day ',day,' asxrgs ', soil%asxrgs
!
      if (soil%asargo.lt.0.0.or.soil%asargo.gt.179.0)                     &
     &  write(*,*) 'day ',day,' asargo ', soil%asargo
!
! wjr,  test values based on definition
      if (soil%asxdks.lt.0.0.or.soil%asxdks.gt.1000.0)                    &
     &  write(*,*) 'day ',day,' asxdks ', soil%asxdks
!
! wjr,  test values based on definition
      if (soil%asxdkh.lt.0.0.or.soil%asxdkh.gt.1000.0)                    &
     &  write(*,*) 'day ',day,' asxdkh ', soil%asxdkh
!
      if (soil%aslrr.lt.1.0.or.soil%aslrr.gt.30.0) &
     &  write(*,*) 'day ',day,' aslrr ', soil%aslrr
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
      do 10 idx=1,size(subday)
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
      do 50 idx=1,soil%nslay
      if (soil%asdsblk(idx).lt.tstmin.or.soil%asdsblk(idx).gt.tstmax)     &
     &  write(*,*) 'day ',day,' asdsblk(',idx,') ', soil%asdsblk(idx)
!
      if (soil%aszlyd(idx).lt.tstmin.or.soil%aszlyd(idx).gt.tstmax)       &
     &  write(*,*) 'day ',day,' aszlyd(',idx,') ', soil%aszlyd(idx)
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
      if (soil%nslay.lt.1.or.soil%nslay.gt.10)                            &
     &  write(*,*) 'day ',day,' nslay ', soil%nslay
!
      if (soil%aszlyt(1).lt.10.0.or.soil%aszlyt(1).gt.10.0)               &
     &  write(*,*) 'day ',day,' aszlyt(1) ', soil%aszlyt(1)
!
      if (soil%nslay.gt.1.and.                                           &
     & (soil%aszlyt(2).lt.40.0.or.soil%aszlyt(2).gt.40.0))                &
     &  write(*,*) 'day ',day,' aszlyt(2) ', soil%aszlyt(2)
!
      if (soil%nslay.gt.2.and.                                           &
     & (soil%aszlyt(3).lt.50.0.or.soil%aszlyt(3).gt.100.0))               &
     &  write(*,*) 'day ',day,' aszlyt(3) ', soil%aszlyt(3)
!
      if (soil%nslay.gt.3.and.                                           &
     & (soil%aszlyt(4).lt.50.0.or.soil%aszlyt(4).gt.100.0))               &
     &  write(*,*) 'day ',day,' aszlyt(4) ', soil%aszlyt(4)
!
      do 60 idx=5,soil%nslay
      if (soil%nslay.ge.idx.and.                                         &
     & (soil%aszlyt(idx).lt.1.0.or.soil%aszlyt(idx).gt.1000.0))           &
     &  write(*,*) 'day ',day,' aszlyt(',idx,') ', soil%aszlyt(idx)
   60 continue     
!
! s1phys
!
      if (dmpflg) write(*,*) 's1phys'
!      
      do 70 idx=1, soil%nslay
      if (soil%asdblk(idx).lt.0.50.or.soil%asdblk(idx).gt.2.5)            &
     &  write(*,*) 'day ',day,' asdblk(',idx,') ', soil%asdblk(idx)
  70  continue
!
! s1dbh
!
      if (dmpflg) write(*,*) 's1dbh'
!      
      do 80 idx=1,soil%nslay
      if (soil%asfsan(idx).lt.0.0.or.soil%asfsan(idx).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfsan(',idx,') ', soil%asfsan(idx)
!
      if (soil%asfsil(idx).lt.0.0.or.soil%asfsil(idx).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfsil(',idx,') ', soil%asfsil(idx)
!
      if (soil%asfcla(idx).lt.0.0.or.soil%asfcla(idx).gt.1.0)             &
     &  write(*,*) 'day ',day,' asfcla(',idx,') ', soil%asfcla(idx)
!
      if (soil%asvroc(idx).lt.0.0.or.soil%asvroc(idx).gt.1.0)             &
     &  write(*,*) 'day ',day,' asvroc(',idx,') ', soil%asvroc(idx)
   80 continue     
!
! s1agg
!
      if (dmpflg) write(*,*) 's1agg'
!      
      do 90 idx=1, soil%nslay
      if (soil%asdagd(idx).lt.0.6.or.soil%asdagd(idx).gt.2.5)             &
     &  write(*,*) 'day ',day,' asdagd(',idx,') ', soil%asdagd(idx)
!
      if (soil%aseags(idx).lt.0.1.or.soil%aseags(idx).gt.7.0)             &
     &  write(*,*) 'day ',day,' aseags(',idx,') ', soil%aseags(idx)
!
      if (soil%aslagm(idx).lt.0.03.or.soil%aslagm(idx).gt.30.0)           &
     &  write(*,*) 'day ',day,' aslagm(',idx,') ', soil%aslagm(idx)
!
      if (soil%aslagn(idx).lt.0.001.or.soil%aslagn(idx).gt.5.0)           &
     &  write(*,*) 'day ',day,' aslagn(',idx,') ', soil%aslagn(idx)
!
      if (soil%aslagx(idx).lt.1.0.or.soil%aslagx(idx).gt.1000.0)          &
     &  write(*,*) 'day ',day,' aslagx(',idx,') ', soil%aslagx(idx)
!
      if (soil%as0ags(idx).lt.1.0.or.soil%as0ags(idx).gt.20.0)            &
     &  write(*,*) 'day ',day,' as0ags(',idx,') ', soil%as0ags(idx)
   90 continue     
!
! s1dbc
!
      if (dmpflg) write(*,*) 's1dbc'
      
      do 100 idx=1, soil%nslay
      if (soil%as0ph(idx).lt.0.0.or.soil%as0ph(idx).gt.14.0)              &
     &  write(*,*) 'day ',day,' as0ph(',idx,') ', soil%as0ph(idx)

      if (soil%asfcce(idx).lt.0.0.or.soil%asfcce(idx).gt.100.0)           &
     &  write(*,*) 'day ',day,' asfcce(',idx,') ', soil%asfcce(idx)

      if (soil%asfcec(idx).lt.0.0.or.soil%asfcec(idx).gt.tstmax)          &
     &  write(*,*) 'day ',day,' asfcec(',idx,') ', soil%asfcec(idx)

      if (soil%asfom(idx).lt.0.0.or.soil%asfom(idx).gt.tstmax)            &
     &  write(*,*) 'day ',day,' asfom(',idx,') ', soil%asfom(idx)
  100 continue

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
      do 120 idx=1, soil%nslay
      if (soil%ahrwc(idx).lt.0.011.or.soil%ahrwc(idx).gt.0.379)           &
     &  write(*,*) 'day ',day,' ahrwc(',idx,') ', soil%ahrwc(idx)
!
      if (soil%aheaep(idx).lt.-17.91.or.soil%aheaep(idx).gt.0.0)          &
     &  write(*,*) 'day ',day,' aheaep(',idx,') ', soil%aheaep(idx)
!
      if (soil%ahrsk(idx).lt.0.0.or.soil%ahrsk(idx).gt.0.001)             &
     &  write(*,*) 'day ',day,' ahrsk(',idx,') ', soil%ahrsk(idx)
!
      if (soil%ah0cb(idx).lt.0.917.or.soil%ah0cb(idx).gt.27.927)          &
     &  write(*,*) 'day ',day,' ah0cb(',idx,') ', soil%ah0cb(idx)
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
      do 130 idx=1, soil%nslay
      if (soil%ahrwcw(idx).lt.0.005.or.soil%ahrwcw(idx).gt.0.242)         &
     &  write(*,*) 'day ',day,' ahrwcw(',idx,') ', soil%ahrwcw(idx)
!
      if (soil%ahrwcf(idx).lt.0.012.or.soil%ahrwcf(idx).gt.0.335)         &
     &  write(*,*) 'day ',day,' ahrwcf(',idx,') ', soil%ahrwcf(idx)
!
      if (soil%ahrwcs(idx).lt.0.208.or.soil%ahrwcs(idx).gt.0.440)         &
     &  write(*,*) 'day ',day,' ahrwcs(',idx,') ', soil%ahrwcs(idx)
!
      if (soil%ahrwca(idx).lt.0.0.or.soil%ahrwca(idx).gt.tstmax)          &
     &  write(*,*) 'day ',day,' ahrwca(',idx,') ', soil%ahrwca(idx)
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

   do idx=1, soil%nslay
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
      do 2000 idx = 1, soil%nslay
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
