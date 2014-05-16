!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine getwin(cwd,cwm,cwy)
! ***************************************************************** wjr
! reads wingen file into common blocks and supplies wingen data to main
!
!     Edit History
!     09-Mar-99   wjr   created

      use weps_interface_defs
      use datetime_mod, only: isleap
      use file_io_mod, only: luiwin
      use erosion_data_struct_defs, only: awadir, awhrmx, awudmx,       &
     &                                    awudmn, awudav, subday, ntstep
      use climate_input_mod, only: wind_gen_fmt_flag, wwrnflg

      include 'p1werm.inc'
      include 'm1flag.inc'
      include 'command.inc'

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/w1win.inc'

!     + + + Arguments + + +
      integer cwd,cwm,cwy

!     + + + LOCAL VARIABLES + + +
      character   header*80
      integer     ioc
      integer     i                          !local loop index

! This code added to re-initialize the reading of a windgen file
! following the "initialization" phase.  It is triggered if the
! day (cwd) passed to the subroutine is set to zero - LEW
      if (cwd == 0) then
         daywdx = 0
         rewind luiwin
         return
      endif

! skip header
      if (daywdx.eq.0) then
        rewind luiwin
        do daywdx=1,7
          read(luiwin,*,err=9000) header
        end do
        newyrwdx = 0
      end if

! load data buffers if it is the first day of a year
   40 if ((cwd .eq. 1) .and. (cwm.eq.1)) then
        ! first day of year requested, start reading first day of year
        daywdx = 1
        ioc=0
        if( newyrwdx .gt. 0 ) then
          ! first line of this year has been read, move value from end of array to beginning
          wwd(daywdx) = wwd(newyrwdx)
          wwm(daywdx) = wwm(newyrwdx)
          wwy(daywdx) = wwy(newyrwdx)
          wwadir(daywdx) = wwadir(newyrwdx)
          wwudmx(daywdx) = wwudmx(newyrwdx)
          wwudmn(daywdx) = wwudmn(newyrwdx)
          wwhrmx(daywdx) = wwhrmx(newyrwdx)
          do i = 1, ntstep
            wawu(i,daywdx) = wawu(i,newyrwdx)
          end do
          twe(daywdx) = twe(newyrwdx)
          wewudav(daywdx) = wewudav(newyrwdx)
          wawudav(daywdx) = wawudav(newyrwdx)
          newyrwdx = 0
        else
          call readwinline(luiwin, ioc, wind_gen_fmt_flag,              &
     &         wind_max_flag, wind_max_value,                           &
     &         wwd(daywdx), wwm(daywdx), wwy(daywdx), wwadir(daywdx),   &
     &         wwudmx(daywdx), wwudmn(daywdx), wwhrmx(daywdx),          &
     &         wawu(1,(daywdx)), twe(daywdx), wewudav(daywdx),          &
     &         wawudav(daywdx))
          if (ioc .eq. -1) then   !We have a failure reading the file
            ! no more values will be read so first day of next year has not been read
            newyrwdx = 0
            if (cwy .ne. 1) then
              ! partial years should not be used, so reread previous good year
              rewind luiwin
              ! read through header lines when at beginning of file
              do daywdx=1,7
                read(luiwin,*,err=9000) header
              end do
              ! write warning message
              write(6,*) 'Warning, WINDGEN file ended on day of year ', &
     &                   daywdx, ' with dd/mm/yyyy:', wwd(daywdx), '/', &
     &                   wwm(daywdx), '/', wwy(daywdx), ' Rewound.'
              goto 40
            else
              goto 9001
            endif
          else if (ioc .eq. 1) then
              ! Failure reading one file line
              if( daywdx .gt. 1 ) then
                ! index will give correct value
                write (0,*)                                             &
     &            'ERROR: WINDGEN read failed on line after dd/mm/yy: ',&
     &            wwd(daywdx-1),'/',wwm(daywdx-1),'/',wwy(daywdx-1),    &
     &            '. Check file format.'
              else
                ! must be first day of year
                write (0,*)                                             &
     &            'ERROR: WINDGEN read failed on line after dd/mm/yy: ',&
     &            cwd,'/',cwm,'/',cwy, '. Check file format.'
              end if 
              call exit(1)
          end if
        end if

        if( wwd(daywdx).eq.1 .and. wwm(daywdx).eq.1 ) then
          ! data record is at the first day of the year
          ! read in the remainder of the year in the record
          do while( wwy(daywdx) .eq. wwy(1) )
            daywdx = daywdx + 1
            ioc=0
            ! don't exceed the array length
            if( daywdx .gt. mndayr ) then
              ! past end of array, too many values in one year, error
              write(0,*) 'ERROR: WINDGEN file year ', wwy(1), 'has ',   &
     &                    mndayr,' days'
              call exit(1)
            else
              ! still within array
              call readwinline(luiwin, ioc, wind_gen_fmt_flag,          &
     &             wind_max_flag, wind_max_value,                       &
     &             wwd(daywdx), wwm(daywdx), wwy(daywdx),wwadir(daywdx),&
     &             wwudmx(daywdx), wwudmn(daywdx), wwhrmx(daywdx),      &
     &             wawu(1,(daywdx)), twe(daywdx), wewudav(daywdx),      &
     &             wawudav(daywdx))
            endif
      
            if (ioc .eq. -1) then   !We have a failure reading the file
              ! no more values will be read so first day of next year has not been read
              newyrwdx = 0

              if( daywdx .gt. 365 ) then
                ! full year read, reached end of windgen file normally
                ! failed to read record, so back index down and exit loop
                daywdx = daywdx - 1
                exit
              else if (cwy .ne. 1) then
                rewind luiwin
                do daywdx=1,7
                  read(luiwin,*,err=9000) header
                end do
                ! write warning message
                write(6,*)'Warning, WINDGEN file ended on day of year ',&
     &                    daywdx, ' with dd/mm/yyyy:', wwd(daywdx), '/',&
     &                    wwm(daywdx), '/', wwy(daywdx), ' Rewound.'
                goto 40
              else
                goto 9001
              endif
            else if (ioc .eq. 1) then
              ! Failure reading one file line
              ! no more values will be read so first day of next year has not been read
              newyrwdx = 0
              if( daywdx .gt. 365 ) then
                ! full year read, reached end of windgen file normally
                ! failed to read record, so back index down and exit loop
                daywdx = daywdx - 1
                exit
              else if( daywdx .gt. 1 ) then
                ! index will give correct value
                write (0,*)                                             &
     &            'ERROR: WINDGEN read failed on line after dd/mm/yy: ',&
     &            wwd(daywdx-1),'/',wwm(daywdx-1),'/',wwy(daywdx-1),    &
     &            '. Check file format.'
              else
                ! error trying to read first day of year
                write (0,*)                                             &
     &            'ERROR: WINDGEN read failed on line after dd/mm/yy: ',&
     &            cwd,'/',cwm,'/',cwy, '. Check file format.'
              end if 
              call exit(1)
            end if
          end do

          if( wwy(daywdx) .ne. wwy(1) ) then
            ! last record is for different year
            ! record index so can be moved to beginning when needed
            newyrwdx = daywdx
          end if

          ! fill in any missing days so the arrays are populated
          do while( daywdx .lt. 366 )
            daywdx = daywdx + 1
            ! Since we are short, set this day to previous day values
            wwd(daywdx) = wwd(daywdx-1)
            wwm(daywdx) = wwm(daywdx-1)
            wwy(daywdx) = wwy(daywdx-1)
            wwadir(daywdx) = wwadir(daywdx-1)
            wwudmx(daywdx) = wwudmx(daywdx-1)
            wwudmn(daywdx) = wwudmn(daywdx-1)
            wwhrmx(daywdx) = wwhrmx(daywdx-1)
            do i = 1, ntstep
              wawu(i,daywdx) = wawu(i,daywdx-1)
            end do
            twe(daywdx) = twe(daywdx-1)
            wewudav(daywdx) = wewudav(daywdx-1)
            wawudav(daywdx) = wawudav(daywdx-1)
          end do

        else
          ! record read was not the first day of the year
          ! this must be a bad windgen file - hard error
          write(0,*) 'ERROR: WINDGEN file does not have 1/1/', wwy(1),  &
     &               ' Day is ', wwd(1), 'Month is ', wwm(1)
          call exit(1)
        end if
        daywdx = 1
      endif

      if ((wwd(daywdx) .ne. cwd) .or. (wwm(daywdx) .ne. cwm)            &
     &         .or. (wwy(daywdx) .ne. cwy)) then
        if( (isleap(cwy) .eqv. .false.) .and. (wwd(daywdx) .eq. 29)     &
     &      .and. (wwm(daywdx) .eq. 2) ) then
          daywdx = daywdx + 1
        elseif (wwrnflg.gt.0) then
          write (*,*) 'Warning, Simulation date: ',                     &
     &                 cwd, '/', cwm, '/', cwy,                         &
     &                 ' does not match WINDGEN date: ',                &
     &                   wwd(daywdx), '/', wwm(daywdx), '/', wwy(daywdx)
!          wwrnflg = .false.
        endif
      endif

      awadir = wwadir(daywdx)
      awudmx = wwudmx(daywdx)
      awudmn = wwudmn(daywdx)
      awhrmx = wwhrmx(daywdx)
      if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
         awudav = (awudmx + awudmn) / 2.
      else                               ! wind_gen2 file format
         awudav = wawudav(daywdx)
         do i = 1,ntstep
            subday(i)%awu = wawu(i,daywdx)
         end do
      endif
      daywdx = daywdx + 1

      return

! error returns and stops

9000  write(0,*) 'Unexpected error in wingen header'
      call exit(1)
9001  write(0,*) 'Unexpected error reading wingen file day ', daywdx
      call exit(1)
      end

! subroutine to read in one day from windgen file (windgen format 2)
      subroutine readwinline(luiwin, ioc, formatflg, maxflg, wind_max,  &
     &                        wwd, wwm, wwy, wwadir,                    &
     &                        wwudmx, wwudmn, wwhrmx,                   &
     &                        wawu, twe, wewudav, wawudav)

      use erosion_data_struct_defs, only: ntstep

!     + + + Arguments + + +
      integer luiwin, ioc, formatflg, maxflg, wwd, wwm, wwy
      real wind_max, wwadir
      real wwudmx, wwudmn, wwhrmx
      real wawu(ntstep), twe, wewudav, wawudav

      character line*1024
      integer i
      integer, dimension(1) :: tmp_hrmax     ! tmp array for hour of max wind speed
      real tmp_array(ntstep)                     ! tmp array for hrly wind speed

      ! read single line from file
      read (luiwin,'(a)',iostat=ioc) line

      if (formatflg == 1) then   ! original wind_gen file format
        if( ioc .ne. 0 ) then
          ! error reading line from file, set return values to null values
          wwd = -1
          wwm = -1
          wwy = -1
          wwadir = -1
          wwudmx = 0.0
          wwudmn = 0.0
          wwhrmx = 0.0
        else
          read(line, *, iostat=ioc) wwd, wwm, wwy, wwadir,              &
     &                              wwudmx, wwudmn, wwhrmx
          if( ioc .ne. 0 ) then
            ! error reading individual line
            ioc = 1
          end if
        end if
      else                               ! wind_gen2 file format
        if( ioc .ne. 0 ) then
          ! error reading line from file, set return values to null values
          wwd = -1
          wwm = -1
          wwy = -1
          do i = 1, ntstep
            wawu(i) = 0.0
          end do
        else
          read(line, *, iostat=ioc) wwd, wwm, wwy, wwadir,              &
     &                             (wawu(i), i=1,ntstep)

          if( ioc .ne. 0 ) then
            ! error reading individual line
            ioc = 1
          end if
          if (maxflg == 1) then   ! Cap winds greater than specified maximum
            do i = 1, ntstep
              wawu(i) = min(wawu(i), wind_max)
            end do
          end if

          ! compute the total wind energy for the day
          twe = 0.0
          do i = 1, ntstep
            twe = twe + 0.5 * (wawu(i)**3.0) * (86400./ntstep) / 1000.0
          end do

          ! compute the average wind speed to generate
          ! the total wind energy for the day
          ! (this is not the same as daily average wind speed)
          wewudav = (twe/ntstep) * 2.0 * 1000.0/(86400./ntstep)
          wewudav = (wewudav)**(1.0/3.0)

          ! Determine the "old" variable values needed within the model
          ! Some of these may not be 100% correct, but they are the best
          ! have come up with for the time being.
          do i = 1, ntstep
            tmp_array(i) = wawu(i)
          end do
          tmp_hrmax = maxloc(tmp_array)
          wwhrmx = tmp_hrmax(1) 
          wawudav = sum(tmp_array)/ntstep
          wwudmx = maxval(tmp_array)
          wwudmn = wwudmx - sum(tmp_array)/ntstep
        end if
      end if

      return
      end 
