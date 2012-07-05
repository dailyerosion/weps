!$Author: jhudd@weru.ksu.edu $
!$Date: 2008-08-27 09:26:46 -0500 (Wed, 27 Aug 2008) $
!$Revision: 2009-07-31 by Jin Gao
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/main/getcli.for $
      subroutine getcli(ccd, ccm, ccy)
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
      integer dayear, lstday
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
      if (cli_gen_fmt_flag == 3) then
         n_header = 15
      else if (cli_gen_fmt_flag == 2) then
         n_header = 14
      else
         n_header = 8 
      endif
   10 do 20 dayidx=1,n_header
        read(luicli, 1010, err=9000) header
 1010   format (a80)
   20 continue
!

! load data buffers if it is the first day of a year
!   40 if ((ccd .eq. 1) .and. (ccm .eq. 1)) then
!        maxday = 365
!        if ( isleap(ccy) ) maxday=366
!        do 30 dayidx=1,maxday
          ioc=0
  40      read(luicli, *, iostat=ioc)                                   &
     &      wcd(dayidx), wcm(dayidx),wcy(dayidx),                       &
     &      wwzdpt(dayidx), wwdurpt(dayidx),                            &
     &      wwpeaktpt(dayidx), wwpeakipt(dayidx),                       &
     &      wwtdmx(dayidx),wwtdmn(dayidx),wgrad(dayidx),dummy,dummy,    &
     &      wwtdpt(dayidx)
! check the loading date
        if ((wcd(dayidx) .ne. ccd) .or. (wcm(dayidx) .ne. ccm)          &
     &         .or. (wcy(dayidx) .ne. ccy)) then
           goto 40
        
! 1030     format (2(2x,i2),1x,i4,1x,2f6.2,f5.2,1x,f6.2,3f7.2,f6.2,
!     &      2f7.2)
!          write(*,*) 'dayidx,maxday,ccy: ', dayidx, maxday, ccy

          if (ioc .eq. -1) then  ! We have a failure reading the file

            !check if only one 365 day year in cligen file
              goto 9001
          endif
        
 !       dayidx = 1
      endif
!
!      write(*,*)'Date:',ccd,ccm,ccy,wcd(dayidx),wcm(dayidx),wcy(dayidx) 
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
      if( dayidx.lt.maxday ) then
          awtdmnnext = wwtdmn(dayidx+1)
      else
          awtdmnnext = wwtdmn(dayidx)
      endif
      awtdpt = wwtdpt(dayidx)
      aweirr = wgrad(dayidx) * 0.04186
!      dayidx = dayidx + 1   not necessary to increment

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
