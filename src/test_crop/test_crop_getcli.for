!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!NOTE:  Taken from main/getcli.for on 2/9/05
!       so one didn't have to include the "main"
!       subdirectory in the test_crop build - LEW

! Modified to assume that we are only reading the
! newest version of cligen files (Forest Service version)

      subroutine test_crop_getcli(ccd, ccm, ccy)
! ***************************************************************** wjr
! reads cligen file into common blocks and supplies cligen data to main
!
!     Edit History
!     09-Mar-99   wjr   created
!
      include 'p1werm.inc'
      include 'file.inc'
      include 'w1clig.inc'
      include 'm1sim.inc'
      include 'w1pavg.inc'
      include 'm1flag.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/w1cli.inc'
!
!     + + + Arguments + + +
      integer ccd,ccm,ccy
!
!     + + + LOCAL VARIABLES + + +
      integer     dayidx
      character   header*80
      logical     wrnflg
      integer     ioc
      real        dummy

!   wgrad      - Global radiation (ly/day) as read in from CLIGEN.
!
!     + + + FUNCTION DECLARATIONS + + +
!
      logical isleap
      integer dayear, lstday, maxday22
      data dayidx /0/
      data wrnflg /.true./

! This code added to re-initialize the reading of a cligen file
! following the "initialization" phase.  It is triggered if the
! day (ccd) passed to the subroutine is set to zero - LEW
      if (ccd == 0) then
         dayidx = 0
         rewind luicli
         return
      endif

! skip header
      if (dayidx .ne. 0) goto 40
      rewind luicli
      n_header = 15  !We are assuming that we are only working with latest cligen (Forest Service)

   10 do 20 dayidx=1,n_header
        read(luicli, 1010, err=9000) header
 1010   format (a80)
   20 continue

! load data buffers if it is the first day of a year
   40 if ((ccd .eq. 1) .and. (ccm .eq. 1)) then
        maxday2 = 365
        if ( isleap(ccy) ) maxday2=366
        do 30 dayidx=1,maxday2
          ioc=0
          read(luicli, *, iostat=ioc)                                   &
     &      wcd(dayidx), wcm(dayidx),wcy(dayidx),                       &
     &      wwzdpt(dayidx), wwdurpt(dayidx),                            &
     &      wwpeaktpt(dayidx), wwpeakipt(dayidx),                       &
     &      wwtdmx(dayidx),wwtdmn(dayidx),wgrad(dayidx),dummy,dummy,    &
     &      wwtdpt(dayidx)
! 1030     format (2(2x,i2),1x,i4,1x,2f6.2,f5.2,1x,f6.2,3f7.2,f6.2,
!     &      2f7.2)
          !write(*,*) 'dayidx,maxday2,ccy: ',dayidx,maxday2,ccy

          if (ioc .eq. -1) then  ! We have a failure reading the file

            !check if only one 365 day year in cligen file
            if (dayidx == 366) then
               !Since we are short, we will set the 366th day values
               !to the last day values read in
               wcd(dayidx) = wcd(dayidx-1)
               wcm(dayidx) = wcm(dayidx-1)
               wcy(dayidx) = wcy(dayidx-1)
               wwzdpt(dayidx) = wwzdpt(dayidx-1)
               wwdurpt(dayidx) = wwdurpt(dayidx-1)
               wwpeaktpt(dayidx) = wwpeaktpt(dayidx-1)
               wwpeakipt(dayidx) = wwpeakipt(dayidx-1)
               wwtdmx(dayidx) = wwtdmx(dayidx-1)
               wwtdmn(dayidx) = wwtdmn(dayidx-1)
               wgrad(dayidx) = wgrad(dayidx-1)
               wwtdpt(dayidx) = wwtdpt(dayidx-1)
               write(6,*)                                               &
     &           'WEPS thinks it is a leap year and Cligen does not.'
               write(6,*)                                               &
     &           'So we just reuse day 365 out of the Cligen file.'
!              write(6,2030) !print heading
!              write(6,2040) !print added Cligen day
!    &           dayidx, wcd(dayidx), wcm(dayidx), wcy(dayidx)
!              write(6,2050) !print WEPS date
!    &           dayear(lstday(ccm,ccy),12,ccy),lstday(ccm,ccy),12,ccy
            else if((ccd.eq.1).and.(ccm.eq.1).and.(ccy.ne.1)) then
              rewind luicli
              write(6,2030) !print heading
              !print Cligen date
              write(6,2040)                                             &
     &           dayidx, wcd(dayidx), wcm(dayidx), wcy(dayidx)
              write(6,2060) !print end of CLIGEN file message
              goto 10
            else
              goto 9001
            endif
          endif
   30   continue
        dayidx = 1
      endif
!
      !Hmm, Bill has this being done only once when it gets triggered.
      if (wrnflg) then
        if ((wcd(dayidx) .ne. ccd) .or. (wcm(dayidx) .ne. ccm)          &
     &         .or. (wcy(dayidx) .ne. ccy)) then
          write (*,2010)
 2010     format (' warning !',28x,' day       month       year')
          write (6,2020) ccd, ccm, ccy,                                 &
     &                   wcd(dayidx), wcm(dayidx), wcy(dayidx)
 2020     format (' current simulation date -              ',i2,9x,i2,  &
     &      8x,i4,/,' does not match current CLIGEN date -   ',i2,9x,   &
     &      i2,8x,i4,/)
          wrnflg = .false.
        endif
      endif

 2030         format (' warning !',12x,                                 &
     &                ' day-of-year day       month       year')
 2040         format (' current CLIGEN date - ',                        &
     &                i3,9x,i2,9x,i2,8x,i4)
 2050         format (' current WEPS   date - ',                        &
     &                i3,9x,i2,9x,i2,8x,i4)
 2060         format ( ' is beyond the end of file - ',                 &
     &                'rewinding to top of CLIGEN file',/)

      awzdpt = wwzdpt(dayidx)
      awdurpt = wwdurpt(dayidx)
      awpeaktpt = wwpeaktpt(dayidx)
      awpeakipt = wwpeakipt(dayidx)
      if( dayidx.gt.1 ) then
          awtdmxprev = wwtdmx(dayidx-1)
      else
          awtdmxprev = wwtdmx(dayidx)
      end if
      awtdmn = wwtdmn(dayidx)
      awtdmx = wwtdmx(dayidx)
      if( dayidx.lt.maxday2 ) then
          awtdmnnext = wwtdmn(dayidx+1)
      else
          awtdmnnext = wwtdmn(dayidx)
      endif
      awtdpt = wwtdpt(dayidx)
      aweirr = wgrad(dayidx) * 0.04186
      dayidx = dayidx + 1

! calculate air density from temperature and pressure
      awtdav = (awtdmx + awtdmn) / 2.
      awdair = 348.56 * (1.013-0.1183*(amzele/1000.)                    &
     &       + 0.0048 * (amzele/1000.)**2.) / (awtdav + 273.1)

      return
!
! error returns and stops
!
9000  write(0,*) 'Unexpected error in cligen header'
      call exit(1)
9001  write(0,*) 'Unexpected error reading cligen file day ', dayidx
      call exit(1)
      end
