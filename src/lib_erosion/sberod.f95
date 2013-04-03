!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sberod
!**********************************************************************
      subroutine sberod (time, SURF_UPD_FLG, subrsurf, cellstate)

!     To calc loss/dep of saltation/creep, susp. and PM-10 at cells
!     To call sbqout to calc. qo, qsso, q10o for each cell
!     To calc. deposition in the boundary cells of sim. region
!     To update the threshold friction velocity as the loose material
!         depletes upwind and increases downwind

      use weps_interface_defs
      use erosion_data_struct_defs
      use grid_geo_def, only: i1, i2, i3, i4, i5, i6, sin_awa, cos_awa, tan_awa, imax, jmax, ix, jy
      use timer_def, only: TIMSBEROD, TIMSBQOUT, TIMSTART, TIMSTOP

!     +++ ARGUMENT DECLARATIONS +++
      real      time            ! time interval (seconds)
      integer   SURF_UPD_FLG    ! Surface update flag (1=on, 0=off)
      type(subregionsurfacestate), dimension(:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ LOCAL VARIABLES +++
      integer i, j, icsr

      real lx,aa,bb,dd,la,lb,ld,ly

      real  qi, qssi, q10i, qo, qsso, q10o, eg, egss, eg10
      real qx(0:imax, 0:jmax)
      real qy(0:imax, 0:jmax)
      real qssx(0:imax, 0:jmax), qssy(0:imax, 0:jmax)
      real q10x(0:imax, 0:jmax), q10y(0:imax, 0:jmax)

!     +++ LOCAL VARIABLE DEFINITIONS +++
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

!     +++ END SPECIFICATIONS +++

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
      icsr = cellstate(i,j)%csr
!^^^ tmp out
!      if (j .eq. 1 .and. i .eq. 1) then
!        write (*,*) 'out from sberod line 131'
!        write (*,*)  'imax   jmax  icsr  sxprg'
!        write (*,*)  imax, jmax, icsr, sxprg(icsr)
!        write (*,*) 'wus    wust     wusp   sf1  sf10   sf84'
!        write (*,300) cellstate(i,j)%wus,cellstate(i,j)%wust, cellstate(i,j)%wusp, cellstate(i,j)%sf1, &
!     &         cellstate(i,j)%sf10, cellstate(i,j)%sf84
!        write (*,*)
!  300   format(1x, 10f8.3)
!      endif
!^^^ end tmp out

      call timer(TIMSBEROD,TIMSTOP)
      call timer(TIMSBQOUT,TIMSTART)

       call sbqout (SURF_UPD_FLG, &
       cellstate(i,j)%wus, cellstate(i,j)%wust, cellstate(i,j)%wusp, cellstate(i,j)%sf10, cellstate(i,j)%sf84, &
       cellstate(i,j)%sf200, cellstate(i,j)%szcr, cellstate(i,j)%sfcr, cellstate(i,j)%sflos, cellstate(i,j)%smlos, &
       cellstate(i,j)%szrgh, subrsurf(icsr)%asxrgs, subrsurf(icsr)%sxprg, cellstate(i,j)%slrr, &
       subrsurf(icsr)%bsl(1)%asfcla, subrsurf(icsr)%bsl(1)%asfsan, &
       subrsurf(icsr)%bsl(1)%asfvfs, cellstate(i,j)%svroc, subrsurf(icsr)%abrsai, subrsurf(icsr)%abzht, &  !edit ljh 1-22-05  
       subrsurf(icsr)%abffcv, time, &
       subrsurf(icsr)%acanag, subrsurf(icsr)%acancr, subrsurf(icsr)%asf10an, &
       subrsurf(icsr)%asf10en, subrsurf(icsr)%asf10bk, &
       lx, qi, qssi, q10i, &
       cellstate(i,j)%dmlos, cellstate(i,j)%sf84mn, subrsurf(icsr)%sf84ic, subrsurf(icsr)%sf10ic, &  !edit ljh 1-22-05
       subrsurf(icsr)%bsl(1)%asvroc, cellstate(i,j)%smaglosmx, &
       qo, qsso, q10o )

      call timer(TIMSBQOUT,TIMSTOP)
      call timer(TIMSBEROD,TIMSTART)
!
!       update output accumulation arrays
!       soil loss is negative:
        eg =   -time*(qo - qi)/lx
        egss = -time*(qsso - qssi)/lx
        eg10 = -time*(q10o - q10i)/lx
        cellstate(i,j)%egt   = cellstate(i,j)%egt + eg + egss
        cellstate(i,j)%egtss = cellstate(i,j)%egtss + egss
        cellstate(i,j)%egt10 = cellstate(i,j)%egt10 + eg10
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
           cellstate(i2+i3,j)%egt = cellstate(i2+i3,j)%egt + time*qx(i2, j)
         endif
         if (qssx(i,j) .gt. 1.0e-10) then
           cellstate(i2+i3,j)%egtss = cellstate(i2+i3,j)%egtss + time*qssx(i2, j)
           cellstate(i2+i3,j)%egt10 = cellstate(i2+i3,j)%egt10 + time*q10x(i2, j)
         endif
       endif
       if (j .eq. i5) then
         if (qy(i,j) .gt. 1.0e-10) then
           cellstate(i,i5+i6)%egt = cellstate(i,i5+i6)%egt + time*qy(i,i5)
         endif
         if (qssy(i,j) .gt. 1.0e-10) then
           cellstate(i,i5+i6)%egtss = cellstate(i,i5+i6)%egtss + time*qssy(i,i5)
           cellstate(i,i5+i6)%egt10 = cellstate(i,i5+i6)%egt10 + time*q10y(i,i5)
         endif
       endif
!

! ^^^ tmp output
!     if (i .eq. 1   .and. j .eq. 1) then
!        write (*,*)
!        write (*,*) 'out at sberod 1,1 call sbwust sf84=', cellstate(i,j)%sf84
!
!        write (*,*)
!     i slagm(i,j), s0ags(i,j), aslagn(1,icsr), aslagx(1,icsr),
!     i ' slagm, s0ags, aslagn,aslagx'
!        write(*,*)
!     i asdagd(1,icsr), cellstate(i,j)%sfcr, cellstate(i,j)%smlos, cellstate(i,j)%sflos,
!     i  ' asdagd, sfcr, smlos, sflos'
!        Write (*,*)
!     i abffcv(icsr), wzzo, ahrwc0(12,icsr), ahrwcw(1,icsr),
!     i ' abffcv, wzzo, ahrwc0, ahrwcw'
!        write (*,*)
!     i cellstate(i,j)%szrgh, cellstate(i,j)%slrr, cellstate(i,j)%wust, cellstate(i,j)%wusp,
!     i ' szrgh, slrr, wust, wusp'
!
!      endif
!
!
!      if (i .eq. (imax-1)/2   .and. j .eq. 1) then
!       write (*,*) 'out at sberod (imax-1)/2,1 call
!     i             sbwust sf84=', cellstate(i,j)%sf84
!         write (*,*)
!     i cellstate(i,j)%slagm, cellstate(i,j)%s0ags, aslagn(1,icsr), aslagx(1,icsr),
!     i ' slagm, s0ags, aslagn,aslagx'
!        write(*,*)
!     i asdagd(1,icsr), cellstate(i,j)%sfcr, cellstate(i,j)%smlos, cellstate(i,j)%sflos,
!     i  ' asdagd, sfcr, smlos, sflos'
!        Write (*,*)
!     i abffcv(icsr), wzzo, ahrwc0(12,icsr), ahrwcw(1,icsr),
!     i ' abffcv, wzzo, ahrwc0, ahrwcw'
!        write (*,*)
!     i cellstate(i,j)%szrgh, cellstate(i,j)%slrr,cellstate(i,j)%wust, cellstate(i,j)%wusp,
!     i  ' szrgh, slrr, wust, wusp'
!
!      endif
!      if (i .eq. imax-1   .and. j .eq.1) then
!      write (*,*) 'out at sberod imax-1,1 call sbwust sf84=',
!     i      cellstate(i,j)%sf84
!        write (*,*)
!     i cellstate(i,j)%slagm, cellstate(i,j)%s0ags, aslagn(1,icsr), aslagx(1,icsr),
!     i ' slagm, s0ags, aslagn,aslagx'
!        write(*,*)
!     i asdagd(1,icsr), cellstate(i,j)%sfcr, cellstate(i,j)%smlos, cellstate(i,j)%sflos,
!     i  ' asdagd, sfcr, smlos, sflos'
!        Write (*,*)
!     i abffcv(icsr), wzzo, ahrwc0(12,icsr), ahrwcw(1,icsr),
!     i ' abffcv, wzzo, ahrwc0, ahrwcw'
!        write (*,*)
!     i  cellstate(i,j)%szrgh, cellstate(i,j)%slrr, cellstate(i,j)%wust, cellstate(i,j)%wusp,
!     i ' szrgh, slrr, wust, wusp'
!
!      endif

!^^^ end tmp out

  100 continue

  110 continue

      return
      end

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






