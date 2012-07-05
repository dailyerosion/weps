!$Author$
!$Date$
!$Revision$
!$HeadURL$
      subroutine getcli(ccd, ccm, ccy)
! ***************************************************************** wjr
! reads cligen file into common blocks and supplies cligen data to main

!     Edit History
!     09-Mar-99   wjr   created

      include 'p1werm.inc'
      include 'file.inc'
      include 'w1clig.inc'
      include 'm1sim.inc'
      include 'w1pavg.inc'
      include 'm1flag.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/w1cli.inc'

!     + + + Arguments + + +
      integer ccd,ccm,ccy

!     + + + LOCAL VARIABLES + + +
      character   header*80
      character   line*256
      integer     ioc
      real        dummy

!     + + + FUNCTION DECLARATIONS + + +

      logical isleap

! This code added to re-initialize the reading of a cligen file
! following the "initialization" phase.  It is triggered if the
! day (ccd) passed to the subroutine is set to zero - LEW
      if (ccd == 0) then
        daycdx = 0
        rewind luicli
        return
      endif

      if (daycdx .eq. 0) then
        rewind luicli
        if (cli_gen_fmt_flag == 3) then
           n_header = 15
        else if (cli_gen_fmt_flag == 2) then
           n_header = 14
        else
           n_header = 8 
        endif
        ! read through header lines when at beginning of file
        do daycdx=1,n_header
          read(luicli, '(a80)', err=9000) header
        end do
        newyrcdx = 0
      end if

      ! load data buffers if it is the first day of a year
   40 if ((ccd .eq. 1) .and. (ccm .eq. 1)) then
        ! first day of year requested, start reading first day of year
        daycdx = 1
        ioc=0
        if( newyrcdx .gt. 0 ) then
          ! first line of this year has been read, move value from end of array to beginning
          wcd(daycdx) = wcd(newyrcdx)
          wcm(daycdx) = wcm(newyrcdx)
          wcy(daycdx) = wcy(newyrcdx)
          wwzdpt(daycdx) = wwzdpt(newyrcdx)
          wwdurpt(daycdx) = wwdurpt(newyrcdx)
          wwpeaktpt(daycdx) = wwpeaktpt(newyrcdx)
          wwpeakipt(daycdx) = wwpeakipt(newyrcdx)
          wwtdmx(daycdx) = wwtdmx(newyrcdx)
          wwtdmn(daycdx) = wwtdmn(newyrcdx)
          wgrad(daycdx) = wgrad(newyrcdx)
          wwtdpt(daycdx) = wwtdpt(newyrcdx)
          newyrcdx = 0
        else

          ! reading first line of year
          read (luicli,'(a)',iostat=ioc) line

          if (ioc .eq. -1) then  ! We have a failure reading the file
            ! no more values will be read so first day of next year has not been read
            newyrcdx = 0
            if(ccy.ne.1) then
              ! partial years should not be used, so reread previous good year
              rewind luicli
              ! read through header lines when at beginning of file
              do daycdx=1,n_header
                read(luicli, '(a80)', err=9000) header
              end do
              ! write warning message
              write(6,*) 'Warning, CLIGEN file ended on day of year ',  &
     &                   daycdx, ' with dd/mm/yyyy:', wcd(daycdx), '/', &
     &                   wcm(daycdx), '/', wcy(daycdx), ' Rewound.'
              goto 40
            else
              goto 9001
            endif
          else
            ! parse the line read from file
            read(line, *, iostat=ioc)                                   &
     &        wcd(daycdx), wcm(daycdx),wcy(daycdx),                     &
     &        wwzdpt(daycdx), wwdurpt(daycdx),                          &
     &        wwpeaktpt(daycdx), wwpeakipt(daycdx),                     &
     &        wwtdmx(daycdx),wwtdmn(daycdx),wgrad(daycdx),dummy,dummy,  &
     &        wwtdpt(daycdx)
            if (ioc .eq. -1) then  ! We have a failure parsing the line
              ! Failure reading one file line
              if( daycdx .gt. 1 ) then
                ! index will give correct value
                write (0,*)                                             &
     &            'ERROR: CLIGEN read failed on line after dd/mm/yy: ', &
     &            wcd(daycdx-1),'/',wcm(daycdx-1),'/',wcy(daycdx-1),    &
     &            '. Check file format.'
              else
                ! error trying to read first day of year
                write (0,*)                                             &
     &            'ERROR: CLIGEN read failed on line after dd/mm/yy: ', &
     &            ccd,'/',ccm,'/',ccy, '. Check file format.'
              end if 
              call exit(1)
            end if
          endif
        end if

        if( wcd(daycdx).eq.1 .and. wcm(daycdx).eq.1 ) then
          ! data record is at the first day of the year
          ! read in the remainder of the year in the record
          do while( wcy(daycdx) .eq. wcy(1) )
            daycdx = daycdx + 1
            ioc=0
            ! don't exceed the array length
            if( daycdx .gt. mndayr ) then
              ! past end of array, too many values in one year, error
              write(*,*) 'ERROR: Cligen file year ', wcy(1), 'has ',    &
     &                    mndayr,' days'
              call exit(1)
            else
              ! still within array, read line
              read (luicli,'(a)',iostat=ioc) line
            end if
      
            if (ioc .eq. -1) then  ! We have a failure reading the file
              ! no more values will be read so first day of next year has not been read
              newyrcdx = 0
              if( daycdx .gt. 365 ) then
                ! full year read, reached end of cligen file normally
                ! failed to read record, so back index down and exit loop
                daycdx = daycdx - 1
                exit
              else if((ccd.eq.1).and.(ccm.eq.1).and.(ccy.ne.1)) then
                ! partial years should not be used, so reread previous good year
                rewind luicli
                ! read through header lines when at beginning of file
                do daycdx=1,n_header
                  read(luicli, '(a80)', err=9000) header
                end do
              ! write warning message
                write(6,*) 'Warning, CLIGEN file ended on day of year ',&
     &                   daycdx, ' with dd/mm/yyyy:', wcd(daycdx), '/', &
     &                   wcm(daycdx), '/', wcy(daycdx), ' Rewound.'
                goto 40
              else
                goto 9001
              endif
            else
              ! parse the line read from file
              read(line, *, iostat=ioc)                                 &
     &          wcd(daycdx), wcm(daycdx),wcy(daycdx),                   &
     &          wwzdpt(daycdx), wwdurpt(daycdx),                        &
     &          wwpeaktpt(daycdx), wwpeakipt(daycdx),                   &
     &          wwtdmx(daycdx),wwtdmn(daycdx),wgrad(daycdx),dummy,dummy,&
     &          wwtdpt(daycdx)
              if (ioc .eq. -1) then  ! We have a failure parsing the line
                ! Failure reading one file line
                ! no more values will be read so first day of next year has not been read
                newyrcdx = 0
                if( daycdx .gt. 365 ) then
                  ! full year read, reached end of cligen file normally
                  ! failed to read record, so back index down and exit loop
                  daycdx = daycdx - 1
                  exit
                else if( daycdx .gt. 1 ) then
                  ! index will give correct value
                  write (0,*)                                           &
     &              'ERROR: CLIGEN read failed on line after dd/mm/yy:',&
     &              wcd(daycdx-1),'/',wcm(daycdx-1),'/',wcy(daycdx-1),  &
     &              '. Check file format.'
                else
                  ! error trying to read first day of year
                  write (0,*)                                           &
     &              'ERROR: CLIGEN read failed on line after dd/mm/yy:',&
     &              ccd,'/',ccm,'/',ccy, '. Check file format.'
                end if 
                call exit(1)
              end if
            endif
          end do

          if( wcy(daycdx) .ne. wcy(1) ) then
            ! last record is for different year
            ! record index so can be moved to beginning when needed
            newyrcdx = daycdx
          end if

          ! fill in any missing days so the arrays are populated
          do while( daycdx .lt. 366 )
            daycdx = daycdx + 1
            ! Since we are short, set this day to previous day values
            wcd(daycdx) = wcd(daycdx-1)
            wcm(daycdx) = wcm(daycdx-1)
            wcy(daycdx) = wcy(daycdx-1)
            wwzdpt(daycdx) = wwzdpt(daycdx-1)
            wwdurpt(daycdx) = wwdurpt(daycdx-1)
            wwpeaktpt(daycdx) = wwpeaktpt(daycdx-1)
            wwpeakipt(daycdx) = wwpeakipt(daycdx-1)
            wwtdmx(daycdx) = wwtdmx(daycdx-1)
            wwtdmn(daycdx) = wwtdmn(daycdx-1)
            wgrad(daycdx) = wgrad(daycdx-1)
            wwtdpt(daycdx) = wwtdpt(daycdx-1)
          end do
        else
          ! record read was not the first day of the year
          ! this must be a bad cligen file - hard error
          write(*,*) 'ERROR: Cligen file does not have 1/1/', wcy(1),   &
     &               ' Day is ', wcd(1), 'Month is ', wcm(1)
          call exit(1)
        end if
        daycdx = 1
      endif

      !Hmm, Bill has this being done only once when it gets triggered.
      if ((wcd(daycdx) .ne. ccd) .or. (wcm(daycdx) .ne. ccm)            &
     &         .or. (wcy(daycdx) .ne. ccy)) then
        if( (isleap(ccy) .eqv. .false.) .and. (wcd(daycdx) .eq. 29)     &
     &      .and. (wcm(daycdx) .eq. 2) ) then
          daycdx = daycdx + 1
        elseif (cwrnflg) then
          write (*,*) 'Warning, Simulation date: ',                     &
     &                 ccd, '/', ccm, '/', ccy,                         &
     &                 ' does not match CLIGEN date: ',                 &
     &                   wcd(daycdx), '/', wcm(daycdx), '/', wcy(daycdx)
!          cwrnflg = .false.
        endif
      endif

      awzdpt = wwzdpt(daycdx)
      awdurpt = wwdurpt(daycdx)
      awpeaktpt = wwpeaktpt(daycdx)
      awpeakipt = wwpeakipt(daycdx)
      if( daycdx.gt.1 ) then
          awtdmxprev = wwtdmx(daycdx-1)
      else
          awtdmxprev = wwtdmx(daycdx)
      end if
      awtdmn = wwtdmn(daycdx)
      awtdmx = wwtdmx(daycdx)
      if( daycdx.lt.maxday ) then
          awtdmnnext = wwtdmn(daycdx+1)
      else
          awtdmnnext = wwtdmn(daycdx)
      endif
      awtdpt = wwtdpt(daycdx)
      aweirr = wgrad(daycdx) * 0.04186
      daycdx = daycdx + 1

! calculate air density from temperature and pressure
      awtdav = (awtdmx + awtdmn) / 2.
      awdair = 348.56 * (1.013-0.1183*(amzele/1000.)                    &
     &       + 0.0048 * (amzele/1000.)**2.) / (awtdav + 273.1)

      return

! error returns and stops

9000  write(0,*) 'Unexpected error in cligen header'
      call exit(1)
9001  write(0,*) 'Unexpected error reading cligen file day ', daycdx
      call exit(1)
      end
