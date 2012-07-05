!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sberod
!**********************************************************************
      subroutine sberod (time,flg)

!     To calc loss/dep of saltation/creep, susp. and PM-10 at cells
!     To call sbqout to calc. qo, qsso, q10o for each cell
!     To calc. deposition in the boundary cells of sim. region
!     To update the threshold friction velocity as the loose material
!         depletes upwind and increases downwind
!
!     +++ ARGUMENT DECLARATIONS +++

      real      time
      integer   flg    !Surface update flag (1=on, 0=off)
!
!     +++ ARGUMENT DEFINITIONS +++
!     time     = time interval (seconds)

!     + + + GLOBAL COMMON BLOCKS + + +

      include  'p1werm.inc'
      include  's1agg.inc'
      include  's1sgeo.inc'
      include  's1surf.inc'
      include  's1dbh.inc'
      include  'b1glob.inc'
      include  'm1sim.inc'
      include  'h1db1.inc'
      include  'timer.inc'
      include  'w1clig.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
      include  'erosion/m2geo.inc'
      include  'erosion/e2grid.inc'
      include  'erosion/e3grid.inc'
      include  'erosion/s2agg.inc'
      include  'erosion/s2surf.inc'
      include  'erosion/s2sgeo.inc'
      include  'erosion/e2erod.inc'
      include  'erosion/w2wind.inc'
!
!     +++ PARAMETERS +++
!
!     +++ LOCAL VARIABLES +++
      integer i, j, icsr
!*
      real lx,aa,bb,dd,la,lb,ld,ly
!
      real  qi, qssi, q10i, qo, qsso, q10o, eg, egss, eg10
      real qx(0:mngdpt, 0:mngdpt)
      real qy(0:mngdpt, 0:mngdpt)
      real qssx(0:mngdpt, 0:mngdpt), qssy(0:mngdpt, 0:mngdpt)
      real q10x(0:mngdpt, 0:mngdpt), q10y(0:mngdpt, 0:mngdpt)
!     c wzzo,
!     c slagm(0:mngdpt, 0:mngdpt), s0ags(0:mngdpt, 0:mngdpt)
!
!     +++ LOCAL VARIABLE DEFINITIONS +++
!     cc      =
!     i, j    =
!     qi,qssi, q10i =
!     qo,qsso, q10o =
!     eg      =
!     egss    =
!     eg10    =
!     egt     =
!     egtss   =
!     egt10   =
!     qx      =
!     qy      =
!
!     +++ END SPECIFICATIONS +++
!
!     set initial conditions to zero
      do 50 j = 0, jmax
      do 45 i = 0, imax
        qx(i,j)    = 0.
        qy(i,j)    = 0.
        qssx(i,j)  = 0.
        qssy(i,j)  = 0.
        q10x(i,j)  = 0.
        q10y(i,j)  = 0.
   45 continue
   50 continue
!
!     set a correction term
!      cc = (jy - ix)/ix
!*    set field length
!      lx = ix/(abs(sin_awa)+0.001)
!      if (lx .gt. max(ix,jy))then
!
!          lx = max(ix,jy)
!      endif
!     grid length (lx): revised by LH 9-22-00
      if (abs(tan_awa) .le. (ix/jy)) then
         la = jy
         lb = abs(tan_awa*jy)
         ld = abs(jy/cos_awa)
      else
         ld = abs(ix/sin_awa)
         lb = ix
         la = sqrt(ld*ld - lb*lb)
      endif
       lx = ld*(1.0 - 0.292893*la*lb/(ix*jy))
       ly = ix*jy/lx
! ^^^ tmp out
!      write (*,*) ' output from sberod'
!      write (*,*) 'la=', la, 'lb=', lb,'ld=', ld
!      write (*,*) 'lx=', lx,'ly=',ly,'ix=',ix,'jy=', jy
!      write (*,*) '-----------------------------'
!
!     update interior grid cells:
      do  110  i = i1, i2, i3
      do  100  j = i4, i5, i6

!     if(ke .eq. 1) then
!       calculate input discharge
      qi   = (qx(i-i3,j)*jy   + qy(i,j-i6)*ix)/ly
      qssi = (qssx(i-i3,j)*jy + qssy(i,j-i6)*ix)/ly
      q10i = (q10x(i-i3,j)*jy + q10y(i,j-i6)*ix)/ly

!       calc. output discharge
      icsr = csr(i,j)
!^^^ tmp out
!      if (j .eq. 1 .and. i .eq. 1) then
!        write (*,*) 'out from sberod line 131'
!        write (*,*)  'imax   jmax  icsr  sxprg'
!        write (*,*)  imax, jmax, icsr, sxprg(icsr)
!        write (*,*) 'wus    wust     wusp   sf1  sf10   sf84'
!        write (*,300) wus(i,j),wust(i,j), wusp(i,j), sf1(i,j),
!     &         sf10(i,j), sf84(i,j)
!        write (*,*)
!  300   format(1x, 10f8.3)
!      endif
!^^^ end tmp out

      call timer(TIMSBEROD,TIMSTOP)
      call timer(TIMSBQOUT,TIMSTART)

       call sbqout (flg,                                                &
     & wus(i,j), wust(i,j), wusp(i,j), sf10(i,j), sf84(i,j),            &
     & sf200(i,j), szcr(i,j), sfcr(i,j), sflos(i,j), smlos(i,j),        &
     & szrgh(i,j), asxrgs(icsr), sxprg(icsr), slrr(i,j),                &
     & asfcla(1,icsr), asfsan(1,icsr),                                  &
     & asfvfs(1,icsr),svroc(i,j), abrsai(icsr), abzht(icsr),            &  !edit ljh 1-22-05  
     & abffcv(icsr), time,                                              &
     & acanag(icsr), acancr(icsr),asf10an(icsr),                        &
     & asf10en(icsr), asf10bk(icsr),                                    &
     & lx, qi, qssi, q10i, i, j, imax, jmax,                            &
     & smaglos(i,j), dmlos(i,j), sf84mn(i,j), sf84ic, sf10ic,           &  !edit ljh 1-22-05
     & asvroc(1,icsr), smaglosmx(i,j),                                  &
     & qo, qsso, q10o )

      call timer(TIMSBQOUT,TIMSTOP)
      call timer(TIMSBEROD,TIMSTART)
!
!       update output accumulation arrays
!       soil loss is negative:
        eg =   -time*(qo - qi)/lx
        egss = -time*(qsso - qssi)/lx
        eg10 = -time*(q10o - q10i)/lx
        egt(i,j)   = egt (i,j) + eg + egss
        egtss(i,j) = egtss(i,j) + egss
        egt10(i,j) = egt10(i,j) + eg10
!
!*       update discharge scalars
        aa = abs(-ix*cos_awa)
        bb = abs(-jy*sin_awa)
        dd = abs(aa)+abs(bb)
!
        qx(i,j)   = qo*ly*bb/(jy*dd)
        qy(i,j)   = qo*ly*aa/(ix*dd)
        qssx(i,j) = qsso*ly*bb/(jy*dd)
        qssy(i,j) = qsso*ly*aa/(ix*dd)
        q10x(i,j) = q10o*ly*bb/(jy*dd)
        q10y(i,j) = q10o*ly*aa/(ix*dd)
! ^^^tmp
!        if (i .eq. 1 .and. qy(i,j) .gt. 0.00001) then
!           write (*,*) 'tmp out sberod line 172'
!           write (*,*) 'i=',i,'j=',j,'aa=',aa, 'bb=',bb,'qy=',qy(i,j)
!           write (*,*) 'i2=',i2,'i3=', i3,'i5=', i5, 'i6=', i6
!       endif
!*
!        qx(i,j)   = -qo*sin_awa
!        qy(i,j)   = -(qo + (qo - qi)*cc)*cos_awa
!        qssx(i,j) = -qsso*sin_awa
!        qssy(i,j) = -(qsso + (qsso -qssi)*cc)*cos_awa
!        q10x(i,j) = -q10o*sin_awa
!        q10y(i,j) = -(q10o + (q10o - q10i)*cc)*cos_awa
!
!       update salt/creep, suspension & pm-10 crossing boundary
!       note the units are kg/m and different than interior cells and
!       the meaning also differs.
!       egt = salt/creep discharge (not total)
!       egtss = suspension discharge
!       egt10 = pm-10 discharge
!
!       calculate scalar discharge crossing borders
!
       if (i .eq. i2) then
         if (qx(i,j) .gt. 1.0e-10) then
           egt(i2+i3, j) = egt(i2+i3,j) + time*qx(i2, j)
         endif
         if (qssx(i,j) .gt. 1.0e-10) then
           egtss(i2+i3,j) = egtss(i2+i3,j) + time*qssx(i2, j)
           egt10(i2+i3, j) = egt10(i2+i3,j) + time*q10x(i2, j)
         endif
       endif
       if (j .eq. i5) then
         if (qy(i,j) .gt. 1.0e-10) then
           egt(i, i5+i6) = egt(i,i5+i6) + time*qy(i,i5)
         endif
         if (qssy(i,j) .gt. 1.0e-10) then
           egtss(i,i5+i6) = egtss(i,i5+i6) + time*qssy(i,i5)
           egt10(i,i5+i6) = egt10(i,i5+i6) + time*q10y(i,i5)
         endif
       endif
!

! ^^^ tmp output
!     if (i .eq. 1   .and. j .eq. 1) then
!        write (*,*)
!        write (*,*) 'out at sberod 1,1 call sbwust sf84=', sf84(i,j)
!
!        write (*,*)
!     i slagm(i,j), s0ags(i,j), aslagn(1,icsr), aslagx(1,icsr),
!     i ' slagm, s0ags, aslagn,aslagx'
!        write(*,*)
!     i asdagd(1,icsr), sfcr(i,j), smlos(i,j), sflos(i,j),
!     i  ' asdagd, sfcr, smlos, sflos'
!        Write (*,*)
!     i abffcv(icsr), wzzo, ahrwc0(12,icsr), ahrwcw(1,icsr),
!     i ' abffcv, wzzo, ahrwc0, ahrwcw'
!        write (*,*)
!     i szrgh(i,j), slrr(i,j), wust(i,j), wusp(i,j),
!     i ' szrgh, slrr, wust, wusp'
!
!      endif
!
!
!      if (i .eq. (imax-1)/2   .and. j .eq. 1) then
!       write (*,*) 'out at sberod (imax-1)/2,1 call
!     i             sbwust sf84=', sf84((imax-1)/2,j)
!         write (*,*)
!     i slagm(i,j), s0ags(i,j), aslagn(1,icsr), aslagx(1,icsr),
!     i ' slagm, s0ags, aslagn,aslagx'
!        write(*,*)
!     i asdagd(1,icsr), sfcr(i,j), smlos(i,j), sflos(i,j),
!     i  ' asdagd, sfcr, smlos, sflos'
!        Write (*,*)
!     i abffcv(icsr), wzzo, ahrwc0(12,icsr), ahrwcw(1,icsr),
!     i ' abffcv, wzzo, ahrwc0, ahrwcw'
!        write (*,*)
!     i szrgh(i,j), slrr(i,j),wust(i,j), wusp(i,j),
!     i  ' szrgh, slrr, wust, wusp'
!
!      endif
!      if (i .eq. imax-1   .and. j .eq.1) then
!      write (*,*) 'out at sberod imax-1,1 call sbwust sf84=',
!     i      sf84(imax-1,j)
!        write (*,*)
!     i slagm(i,j), s0ags(i,j), aslagn(1,icsr), aslagx(1,icsr),
!     i ' slagm, s0ags, aslagn,aslagx'
!        write(*,*)
!     i asdagd(1,icsr), sfcr(i,j), smlos(i,j), sflos(i,j),
!     i  ' asdagd, sfcr, smlos, sflos'
!        Write (*,*)
!     i abffcv(icsr), wzzo, ahrwc0(12,icsr), ahrwcw(1,icsr),
!     i ' abffcv, wzzo, ahrwc0, ahrwcw'
!        write (*,*)
!     i  szrgh(i,J), slrr(i,j), wust(i,j), wusp(i,j),
!     i ' szrgh, slrr, wust, wusp'
!
!      endif

!^^^ end tmp out



  100 continue
!

!
  110 continue
!
!     temp code for output c
  210 continue

      return
      end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






