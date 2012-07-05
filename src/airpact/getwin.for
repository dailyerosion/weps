!
!$Author: jhudd@weru.ksu.edu $
!$Date: 2008-08-27 09:26:46 -0500 (Wed, 27 Aug 2008) $
!$Revision: 2009-7-24 by Jin Gao
!$HeadURL: https://svn.weru.ksu.edu/weru/weps1/trunk/weps.src/main/getwin.for $
!
      subroutine getwin(cwd,cwm,cwy)
! ***************************************************************** wjr
! reads wingen file into common blocks and supplies wingen data to main
!
!     Edit History
!     09-Mar-99   wjr   created

      include 'p1werm.inc'
      include 'file.inc'
      include 'w1wind.inc'
      include 'm1flag.inc'
      include 'command.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/w1win.inc'

!     + + + Arguments + + +
      integer cwd,cwm,cwy

!     + + + LOCAL VARIABLES + + +
      integer     dayidx
      integer     maxday
      character   header*80
      logical     wrnflg
      integer     ioc
      integer     i                          !local loop index
      integer, dimension(1) :: tmp_hrmax     ! tmp array
      real        tmp_array(24)              ! tmp array for hrly ws
      character winfil *128

!     + + + FUNCTION DECLARATIONS + + +

      logical isleap
      integer dayear, lstday
      data dayidx /0/
      data wrnflg /.true./

      winfil = 'c:/usr/test_erosion/test1/win_gen.win'
      call fopenk(luiwin, winfil, 'old')    ! open wingen file
! This code added to re-initialize the reading of a windgen file
! following the "initialization" phase.  It is triggered if the
! day (cwd) passed to the subroutine is set to zero - LEW
      if (cwd == 0) then
         dayidx = 0
         rewind luiwin
         return
      endif

! skip header
      if (dayidx.ne.0) goto 40
      rewind luiwin
   10 do 20 dayidx=1,7
        read(luiwin,*,err=9000) header
! 1010   format (a80)
   20 continue
   
! read data from win_gen.win file 
             ! Check wind_gen_fmt_flag (determine input data file format)
   40    if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
            read(luiwin, *, iostat=ioc) wwd(dayidx), wwm(dayidx),       &
     &        wwy(dayidx), wwadir(dayidx), wwudmx(dayidx),              &
     &        wwudmn(dayidx), wwhrmx(dayidx)
!           write(6,*)  wwd(dayidx), wwm(dayidx),
!    &        wwy(dayidx), wwadir(dayidx), wwudmx(dayidx),
!    &        wwudmn(dayidx), wwhrmx(dayidx)
          else                               ! wind_gen2 file format
            read(luiwin, *, iostat=ioc) wwd(dayidx), wwm(dayidx),       &
     &        wwy(dayidx), wwadir(dayidx),                              &
     &        (wawu(i,dayidx), i=1,24)
         end if
!           write(6,*)  wwd(dayidx), wwm(dayidx),                       &
!    &        wwy(dayidx), wwadir(dayidx),                              &
!    &        (wawu(i,dayidx), i=1,24)
!   reading line by line to check the matched date 
          if ((wwd(dayidx) .ne. cwd) .or. (wwm(dayidx) .ne. cwm) .or.   &
     &    (wwy(dayidx) .ne. cwy)) then
           goto 40
          else 
          if (wind_gen_fmt_flag .ne. 1) then 
              if (wind_max_flag == 1) then   ! Cap winds greater than specified maximum
               do i = 1, 24
                  if (wawu(i,dayidx) > wind_max_value) then
                     wawu(i,dayidx) = wind_max_value
                  end if
                end do
              end if
                       !compute the total wind energy for the day
            twe(dayidx) = 0.0
            do i = 1, 24
                twe(dayidx) = twe(dayidx) +                             &
     &                  0.5*(wawu(i,dayidx)**3.0)*3600.0/1000.0
            end do
      
            !compute the average wind speed to generate
            ! the total wind energy for the day
            ! (this is not the same as daily average wind speed)
            wewudav(dayidx) = (twe(dayidx)/24.0) * 2.0 * 1000.0/3600.0
            wewudav(dayidx) = (wewudav(dayidx))**(1.0/3.0)
!           write(6,*)  'ave wind speed (for twe):', wewudav(dayidx)

            !populate the global subdaily (hrly for now) wind speed array
            do i = 1, 24
                awu(i) = wawu(i,dayidx)
            end do
           

!           Determine the "old" variable values needed within the model
!           Some of these may not be 100% correct, but they are the best
!           have come up with for the time being.
            do i = 1, 24
                tmp_array(i) = wawu(i,dayidx)
            end do
!           write(6,*) 'tmp_array() array: ',(tmp_array(i), i=1,24)
            tmp_hrmax = maxloc(tmp_array)
            wwhrmx(dayidx) = tmp_hrmax(1) 
            wawudav(dayidx) = sum(tmp_array)/24
            wwudmx(dayidx) = maxval(tmp_array)
            wwudmn(dayidx) = wwudmx(dayidx) - sum(tmp_array)/24
           end if
        
           ioc=0
! Check wind_gen_fmt_flag (determine input data file format)
!           write(6,*)  wwd(dayidx), wwm(dayidx),                       &
!    &        wwy(dayidx), wwadir(dayidx),                              &
!    &        (wawu(i,dayidx), i=1,24)

            if (wind_max_flag == 1) then   ! Cap winds greater than specified maximum
               do i = 1, 24
                  if (wawu(i,dayidx) > wind_max_value) then
                     wawu(i,dayidx) = wind_max_value
                  end if
               end do
            end if

            !compute the total wind energy for the day
            twe(dayidx) = 0.0
            do i = 1, 24
                twe(dayidx) = twe(dayidx) +                             &
     &                  0.5*(wawu(i,dayidx)**3.0)*3600.0/1000.0
            end do

            !compute the average wind speed to generate
            ! the total wind energy for the day
            ! (this is not the same as daily average wind speed)
            wewudav(dayidx) = (twe(dayidx)/24.0) * 2.0 * 1000.0/3600.0
            wewudav(dayidx) = (wewudav(dayidx))**(1.0/3.0)
!           write(6,*)  'ave wind speed (for twe):', wewudav(dayidx)

            !populate the global subdaily (hrly for now) wind speed array
            do i = 1, 24
                awu(i) = wawu(i,dayidx)
            end do
         
!           Determine the "old" variable values needed within the model
!           Some of these may not be 100% correct, but they are the best
!           have come up with for the time being.
            do i = 1, 24
                tmp_array(i) = wawu(i,dayidx)
            end do

            dayidx = 1
          end if           
            
  
!           write(6,*) 'tmp_array() array: ',(tmp_array(i), i=1,24)
            tmp_hrmax = maxloc(tmp_array)
            wwhrmx(dayidx) = tmp_hrmax(1) 
            wawudav(dayidx) = sum(tmp_array)/24
            wwudmx(dayidx) = maxval(tmp_array)
            wwudmn(dayidx) = wwudmx(dayidx) - sum(tmp_array)/24

          if (ioc .eq. -1) then   !We have a failure reading the file

            !check if only one 365 day year in wind_gen file, etc.
            if (dayidx == 366) then
               !Since we are short, we will set the 366th day values
               !to the last day values read in
               wwd(dayidx) = wwd(dayidx-1) !This should let us know what we did
               wwm(dayidx) = wwm(dayidx-1)
               wwy(dayidx) = wwy(dayidx-1)
               wwadir(dayidx) = wwadir(dayidx-1)
               wwudmx(dayidx) = wwudmx(dayidx-1)
               wwudmn(dayidx) = wwudmn(dayidx-1)
               wwhrmx(dayidx) = wwhrmx(dayidx-1)
               write(6,*)                                               &
     &           'WEPS thinks it is a leap year and Windgen does not.'
               write(6,*)                                               &
     &           'So we just reuse day 365 out of the Windgen file.'
!              write(6,2030) !print heading
!              write(6,2040) !print added Windgen day
!    &           dayidx, wmd(dayidx), wmm(dayidx), wmy(dayidx)
!              write(6,2050) !print WEPS date
!    &           dayear(lstday(ccm,ccy),12,ccy),lstday(ccm,ccy),12,ccy
            else if ((cwd .eq. 1) .and. (cwm .eq. 1)) then
              rewind luiwin
              write(6,2030) !print heading
              !print added Windgen day
              write(6,2040)                                             &
     &           dayidx, wwd(dayidx), wwm(dayidx), wwy(dayidx)
              goto 10
            else
              goto 9001
            endif
          endif

   30   continue
!        dayidx = 1   

      if (wrnflg) then
        if ((wwd(dayidx) .ne. cwd) .or. (wwm(dayidx) .ne. cwm) .or.     &
     &    (wwy(dayidx) .ne. cwy)) then
          write (*,2010)
 2010     format (' warning !',28x,' day       month       year')
          write (6,2020) cwd, cwm, cwy,                                 &
     &                   wwd(dayidx), wwm(dayidx), wwy(dayidx)
 2020     format (' current simulation date -              ',i2,9x,i2,  &
     &      8x,i4,/,' does not match current WINDGEN date -  ',i2,9x,   &
     &      i2,8x,i4,/)
          wrnflg = .false.
        endif
      endif

 2030         format (' warning !',12x,                                 &
     &                ' day-of-year day       month       year')
 2040         format (' current WINDGEN date - ',                       &
     &                i3,9x,i2,9x,i2,8x,i4)
 2050         format (' current WEPS   date - ',                        &
     &                i3,9x,i2,9x,i2,8x,i4)
 2060         format ( ' is beyond the end of file - ',                 &
     &                'rewinding to top of WINDGEN file',/)
      awadir = wwadir(dayidx)
      awudmx = wwudmx(dayidx)
      awudmn = wwudmn(dayidx)
      awhrmx = wwhrmx(dayidx)
   
      if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
         awudav = (awudmx + awudmn) / 2.
      else                               ! wind_gen2 file format
         awudav = wawudav(dayidx)
         do i = 1,24
            awu(i) = wawu(i,dayidx)
         end do
      endif
      dayidx = dayidx + 1
      write(*,*)'Wind info', awadir,awudmx,awudmn,awhrmx,awudav
      return
!
! error returns and stops
!
9000  write(0,*) 'Unexpected error in wingen header'
      call exit(1)
9001  write(0,*) 'Unexpected error reading wingen file day ', dayidx
      call exit(1)
      end
