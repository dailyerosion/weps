!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine callcrop(daysim, sr, residue, restot, croptot)
! ***************************************************************** wjr
! Wrapper to call crop

      use weps_interface_defs
      use biomaterial, only: biomatter, biototal

!     + + +   ARGUMENT DECLARATIONS + + +
      integer daysim
      integer sr
      type(biomatter), dimension(:), intent(inout) :: residue
      type(biototal), intent(in) :: restot
      type(biototal), intent(inout) :: croptot

! Includes
      include 'p1werm.inc'
      include 'c1db1.inc'
      include 'c1db2.inc'
      include 'c1db3.inc'
      include 'c1info.inc'
      include 'c1glob.inc'
      include 'c1gen.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1dbc.inc'
      include 's1dbh.inc'
      include 's1phys.inc'
      include 's1sgeo.inc'    ! Contains required variables for biodrag()
      include 'h1hydro.inc'
      include 'h1et.inc'
      include 'h1temp.inc'
      include 'w1clig.inc'
      include 'timer.inc'
      include 'crop/prevstate.inc'
      include 'crop/gcrop.inc'

! Local Variables
      integer lay

!     + + + END OF SPECIFICATIONS + + +

      call timer(TIMCROP,TIMSTART)

! Note that crop "may" really require (admbgz + admrtz) in place of admbgz
! because crop wants to know the amount of biomass in each soil layer
! for nutrient cycling.  However, since the nutrient cycling is supposed
! to be disabled, we won't worry about it right now.  LEW - 04/23/99

      ! check for a valid growing crop
      if( (ac0shoot(sr) .le. 0.0) .or. (acdpop(sr) .le. 0.0) ) then
          am0cgf = .false.
      end if

!     only continue if crop is growing
      if( am0cgf ) then

         if (am0cdb.eq.1) call cdbug(sr, nslay(sr), restot)

         call cropgrow(nslay(sr),                                       &
     &   aszlyt(1,sr), aszlyd(1,sr), asdblk(1,sr),                      &
     &   asfcce(1,sr), asfom(1,sr), asfcec(1,sr), asfsmb(1,sr),         &
     &   asfcla(1,sr), as0ph(1,sr), asftan(1,sr), asftap(1,sr),         &
     &   asmno3(sr),                                                    &
     &   ac0bn1(sr), ac0bn2(sr), ac0bn3(sr),                            &
     &   ac0bp1(sr), ac0bp2(sr), ac0bp3(sr),                            &
     &   ac0ck(sr), acgrf(sr), acehu0(sr), aczmxc(sr),                  &
     &   ac0nam(sr),ac0idc(sr), acxrow(sr),                             &
     &   actdtm(sr), aczmrt(sr), actmin(sr), actopt(sr),                &
     &   ac0fd1(1,sr), ac0fd2(1,sr), ac0fd1(2,sr), ac0fd2(2,sr),        &
     &   ac0bceff(sr),                                                  &
     &   ac0alf(sr), ac0blf(sr), ac0clf(sr),                            &
     &   ac0dlf(sr), ac0arp(sr), ac0brp(sr), ac0crp(sr),                &
     &   ac0drp(sr), ac0aht(sr), ac0bht(sr),                            &
     &   ac0sla(sr), ac0hue(sr),  actverndel(sr),                       &
     &   aweirr, awtdmx, awtdmn, awzdpt,                                &
     &   ahtsmx(1,sr), ahtsmn(1,sr),                                    &
     &   ahzpta, ahzeta, ahzptp, ahfwsf(sr),                            &
     &   am0cif, am0cgf,                                                &
     &   acthudf(sr), acbaflg(sr), acbaf(sr), acyraf(sr),               &
     &   achyfg(sr), acthum(sr), acdpop(sr), acdmaxshoot(sr),           &
     &   ac0transf(sr), ac0storeinit(sr), acfshoot(sr),                 &
     &   ac0growdepth(sr), acfleafstem(sr), ac0shoot(sr),               &
     &   ac0diammax(sr), ac0ssa(sr), ac0ssb(sr),                        &
     &   acfleaf2stor(sr), acfstem2stor(sr), acfstor2stor(sr),          &
     &   acyld_coef(sr), acresid_int(sr), acxstm(sr),                   &
     &   acmstandstem(sr), acmstandleaf(sr), acmstandstore(sr),         &
     &   acmflatstem(sr), acmflatleaf(sr), acmflatstore(sr),            &
     &   acmshoot(sr), acmtotshoot(sr), acmbgstemz(1,sr),               &
     &   acmrootstorez(1,sr), acmrootfiberz(1,sr),                      &
     &   aczht(sr), aczshoot(sr), acdstm(sr), aczrtd(sr),               &
     &   acdayap(sr), acdayam(sr), acthucum(sr), actrthucum(sr),        &
     &   acgrainf(sr), aczgrowpt(sr), acfliveleaf(sr),                  &
     &   acleafareatrend(sr), acstemmasstrend(sr), actwarmdays(sr),     &
     &   actchillucum(sr), acthardnx(sr), acthu_shoot_beg(sr),          &
     &   acthu_shoot_end(sr), acxstmrep(sr),                            &
     &   prevstandstem(sr), prevstandleaf(sr), prevstandstore(sr),      &
     &   prevflatstem(sr), prevflatleaf(sr), prevflatstore(sr),         &
     &   prevmshoot(sr), prevmtotshoot(sr), prevbgstemz(1,sr),          &
     &   prevrootstorez(1,sr), prevrootfiberz(1,sr),                    &
     &   prevht(sr), prevzshoot(sr), prevstm(sr), prevrtd(sr),          &
     &   prevdayap(sr), prevhucum(sr), prevrthucum(sr),                 &
     &   prevgrainf(sr), prevchillucum(sr), prevliveleaf(sr),           &
     &   prevdayspring(sr), daysim, acdayspring(sr), aczloc_regrow(sr), &
     &   agmstandstem(sr), agmstandleaf(sr), agmstandstore(sr),         &
     &   agmflatstem(sr), agmflatleaf(sr), agmflatstore(sr),            &
     &   agmbgstemz(1,sr),                                              &
     &   agzht(sr), agdstm(sr), agxstmrep(sr), aggrainf(sr) )

         if (am0cdb.eq.1) call cdbug(sr, nslay(sr), restot)
      end if

      ! check for abandoned stems in crop regrowth
      if( ( agmstandstem(sr) + agmstandleaf(sr) + agmstandstore(sr)     &
     &     + agmflatstem(sr) + agmflatleaf(sr) + agmflatstore(sr) )     &
     &    .gt. 0.0 ) then
          ! zero out residue pools which crop is not transferring
          agmflatrootstore(sr) = 0.0
          agmflatrootfiber(sr) = 0.0
          do lay = 1, nslay(sr)
              agmbgleafz(lay,sr) = 0.0
              agmbgstorez(lay,sr) = 0.0
              agmbgrootstorez(lay,sr) = 0.0
              agmbgrootfiberz(lay,sr) = 0.0
          end do
          call trans(                                                   &
     &      agmstandstem(sr), agmstandleaf(sr), agmstandstore(sr),      &
     &      agmflatstem(sr), agmflatleaf(sr), agmflatstore(sr),         &
     &      agmflatrootstore(sr), agmflatrootfiber(sr),                 &
     &      agmbgstemz(1,sr), agmbgleafz(1,sr), agmbgstorez(1,sr),      &
     &      agmbgrootstorez(1,sr), agmbgrootfiberz(1,sr),               &
     &      agzht(sr), agdstm(sr), agxstmrep(sr), aggrainf(sr),         &
     &      ac0nam(sr), acxstm(sr), acrbc(sr), ac0sla(sr), ac0ck(sr),   &
     &      acdkrate(1,sr), accovfact(sr), acddsthrsh(sr), achyfg(sr),  &
     &      acresevapa(sr), acresevapb(sr),                             &
     &      nslay(sr), residue)
      end if

      ! update all derived globals for crop global variables
      call cropupdate(                                                  &
     &      acmstandstem(sr), acmstandleaf(sr), acmstandstore(sr),      &
     &      acmflatstem(sr), acmflatleaf(sr), acmflatstore(sr),         &
     &      acmbgstemz(1,sr),                                           &
     &      acmrootstorez(1,sr),acmrootfiberz(1,sr),                    &
     &      aczht(sr), acdstm(sr), aczrtd(sr),                          &
     &      acmbgstem(sr),                                              &
     &      acmrootstore(sr), acmrootfiber(sr), acxstmrep(sr),          &
     &      acm(sr), acmst(sr), acmf(sr), acmrt(sr), acmrtz(1,sr),      &
     &      acrcd(sr), aszrgh(sr), aszlyd(1,sr),                        &
     &      acrsai(sr), acrlai(sr), acrsaz(1,sr), acrlaz(1,sr),         &
     &      acffcv(sr), acfscv(sr), acftcv(sr), acfcancov(sr),          &
     &      ac0rg(sr), acxrow(sr),                                      &
     &      nslay(sr), ac0ssa(sr), ac0ssb(sr), ac0sla(sr),              &
     &      accovfact(sr), ac0ck(sr), acxstm(sr), acdpop(sr),           &
     &      ahztranspdepth(sr), ahzfurcut(sr),                          &
     &      ahztransprtmin(sr), ahztransprtmax(sr), croptot  )

      ! dependent variables have been updated
      am0cropupfl = 0

      call timer(TIMCROP,TIMSTOP)

      end
