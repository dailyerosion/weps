!$Author$
!$Date$
!$Revision$
!$HeadURL$

module climate_input_mod
    implicit none

    ! data structure for average monthly data
    type cligen_monavg
        real :: tmx(12)    ! Average monthly maximum air temperature (deg C) obtained from the CLIGEN run file
        real :: tmn(12)    ! Average monthly minimum air temperature (deg C) obtained from the CLIGEN run file
        real :: zpt(12)    ! Average monthly total precipitation depth (mm)
    end type cligen_monavg

    ! data structure for cligen input state variables
    type cligen_state
        ! read from file
        integer :: day   ! day of month
        integer :: month ! month of year
        integer :: year  ! year
        real :: zdpt     ! Daily precipitation (mm)
        real :: durpt    ! Duration of Daily precipitation (hours)
        real :: peaktpt  ! Normalized time to peak of Daily precipitation (time to peak/duration)
        real :: peakipt  ! Normalized intensity of peak Daily precipitation (peak intensity/average intensity)
        real :: tdmx     ! Maximum daily air temperature (deg C)
        real :: tdmn     ! Minimum daily air temperature (deg C)
        real :: grad     ! Daily global radiation (langleys/day) (as in cligen file)
        real :: tdpt     ! Daily dew point air temperature (deg C)
        ! derived from file values
        integer :: jday  ! astronomical julian day
        integer :: doy   ! julian day of year
        real :: tdav     ! Mean daily air temperature (deg C)
        real :: eirr     ! Daily global radiation (MJ/m^2)
    end type cligen_state

    character(80) :: cligen_sname ! CLIGEN station name from WEPS.RUN file
    integer :: cli_gen_fmt_flag   ! flag indicating type of cli_gen daily input file
                                  ! = 1 --> 3.1 version cligen db format
                                  ! = 2 --> Forest Service cligen db format
                                  ! = 3 --> new WEPP compatible cli_gen file format

    integer :: wind_gen_fmt_flag  ! flag indicating type of wind_gen daily input file
                                  ! = 1 --> original max/min wind_gen file format
                                  ! = 2 --> new hourly wind_gen file format

    integer :: cwrnflg            ! binary indicator for cligen warnings
                                  ! 0000, 0 - file contains exact dates needed for simulation
                                  ! 0001, 1 - not all years contained in file, rewind and run in day of year mode
                                  ! 0010, 2 - partial year at end of file, rewind and run in day of year mode
                                  ! 0100, 4 - leap year feb 29 does not match, skip or duplicate as needed
    integer :: wwrnflg            ! flag to show or turn off date mismatch warnings
    integer :: n_header           ! number of lines in the cligen file header

    type(cligen_monavg) :: cli_mav

    type(cligen_state) :: cli_prev
    type(cligen_state) :: cli_today
    type(cligen_state) :: cli_next

    integer :: cli_first_jday

contains

    ! rewinds file to beginning reads headers in preparation for reading first data line
    subroutine cli_start()
        use file_io_mod, only: luicli     ! file unit number

        ! + + + LOCAL VARIABLES + + +
        character :: header*80
        integer :: idx
        
        rewind luicli
        ! read through header lines when at beginning of file
        do idx = 1, n_header
            read(luicli, '(a80)', err=9000) header
        end do
        return

9000    write(0,*) 'Unexpected error in cligen header'
        call exit(1)
    end subroutine cli_start

    ! reads the next data line from the cligen file
    function cli_read_next( cli_oneday ) result( errflg )

        use file_io_mod, only: luicli     ! file unit number
        use datetime_mod, only: julday, dayear

        type(cligen_state) :: cli_oneday  ! structure for cligen data line values
        integer :: errflg                 ! returns error result of read
                                          ! 0 - no error
                                          ! 1 - failure to read from file
                                          ! 2 - failure to parse line

        ! + + + LOCAL VARIABLES + + +
        character :: line*256             ! data line read from file
        integer :: ioc                    ! error return from read statement
        real :: dummy

        ! read line from file
        read (luicli,'(a)',iostat=ioc) line
        if (ioc .eq. -1) then
            ! failure reading data line from file
            errflg = 1
            return
        end if

        ! parse the line read from file
        read(line, *, iostat=ioc) cli_oneday%day, cli_oneday%month, cli_oneday%year, &
            cli_oneday%zdpt, cli_oneday%durpt, cli_oneday%peaktpt, cli_oneday%peakipt, &
            cli_oneday%tdmx, cli_oneday%tdmn, cli_oneday%grad, dummy, dummy, cli_oneday%tdpt
        if (ioc .eq. -1) then
            ! We have a failure parsing the data line
            errflg = 2
            return
        end if

        ! populate derived values in cligen data record
        cli_oneday%jday = julday(cli_oneday%day,cli_oneday%month,cli_oneday%year)
        cli_oneday%doy = dayear(cli_oneday%day,cli_oneday%month,cli_oneday%year)
        cli_oneday%tdav = (cli_oneday%tdmx + cli_oneday%tdmn) / 2.0
        cli_oneday%eirr = cli_oneday%grad * 0.04186
        errflg = 0
    end function cli_read_next

    subroutine getcli(ccd, ccm, ccy)

        use datetime_mod, only: isleap, julday, dayear
        use file_io_mod, only: luicli
        use erosion_data_struct_defs, only: awdair

        integer :: ccd    ! requested day of month
        integer :: ccm    ! requested month of year
        integer :: ccy    ! requested year

!      include 'p1werm.inc'
        include 'w1clig.inc'
        include 'm1sim.inc'  ! amzele
!      include 'w1pavg.inc'
!      include 'm1flag.inc'

!     + + + LOCAL COMMON BLOCKS + + +
!      include 'main/w1cli.inc'

        ! + + + LOCAL VARIABLES + + +
        integer     ioc
        integer :: jday    ! astronomical julian day for requested day
        integer :: doy     ! day of year for requested day

        ! This code added to re-initialize the reading of a cligen file
        ! following the "initialization" phase.  It is triggered if the
        ! day (ccd) passed to the subroutine is set to zero - LEW
        if (ccd == 0) then
            cli_today%day = 0
            rewind luicli
            return
        endif

        if (cli_today%day .eq. 0) then
            ! restarting at beginning of the file
            call cli_start()
            ! reading first data line of file
            ioc = cli_read_next(cli_today)
            if( ioc .eq. 1) then
                ! We have a failure reading the first data line in file
                write (0,*) 'ERROR: CLIGEN failed to read first data line. Check file.'
                call exit(1)
            else if(ioc .eq. 2) then
                ! We have a failure parsing the first data line in file
                write (0,*) 'ERROR: CLIGEN failed to parse first data line. Check file format.'
                call exit(1)
            end if

            if( julday(ccd, ccm, ccy) .lt. cli_today%jday ) then
                ! set error flag to indicate earlier date request, will now read by day of year.
                cwrnflg = ibset(cwrnflg,0)
            end if

            ! reading second line of file
            ioc = cli_read_next(cli_next)
            if (ioc .eq. 1) then
                ! We have a failure reading the second data line in file
                write (0,*) 'ERROR: CLIGEN failed to read second data line. Check file.'
                call exit(1)
            else if (ioc .eq. 2) then
                ! We have a failure parsing the second data line in file
                write (0,*) 'ERROR: CLIGEN failed to parse second data line. Check file format.'
                call exit(1)
            end if

            ! check that next day is one day after today
            if( cli_next%jday .ne. (cli_today%jday + 1) ) then
                ! first two days in file are not consecutive
                write (0,*) 'ERROR: CLIGEN days not consecutive in first two data lines. Check file.'
                call exit(1)
            end if

            ! set cli_prev to the same values as cli_today
            cli_prev = cli_today

        end if

        if( cwrnflg .eq. 0 ) then
            ! running in exact date mode

            ! astronomical julian day requested
            jday = julday( ccd, ccm, ccy)
        
            if( (jday .lt. cli_today%jday) .and. (jday .ge. cli_next%jday) ) then
                ! file reset to beginning, read next day
                ! set cli_prev to cli_today
                cli_prev = cli_today
                ! set cli_today to cli_next
                cli_today = cli_next
                ! retrieve line for cli_next
                ioc = cli_read_next(cli_next)
                if (ioc .gt. 0) then
                    write (0,*) 'ERROR: CLIGEN read failed following dd/mm/yy: ', &
                         cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                         ' Rewind to first day failed.'
                    call exit(1)
                end if
            end if

            do while( (jday .gt. cli_today%jday) .and. (cwrnflg .eq. 0 ) )
                ! day requested is later than position in file, scan forward

                ! set cli_prev to cli_today
                cli_prev = cli_today
                ! set cli_today to cli_next
                cli_today = cli_next
                ! retrieve line for cli_next
                ioc = cli_read_next(cli_next)
                if (ioc .gt. 0) then
                    ! We have a failure reading or parsing the next data line in file
                    if( (ccd .eq. 31) .and. (ccm .eq. 12) ) then
                        ! end of year, read next line from beginning of file
                        call cli_start()
                        ioc = cli_read_next(cli_next)
                        if (ioc .gt. 0) then
                            write (0,*) 'ERROR: CLIGEN read failed following dd/mm/yy: ', &
                                 cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                                 ' Rewind to first day failed.'
                            call exit(1)
                        end if
                    else
                        ! file did not end with complete year
                        ! set error flag to indicate incomplete year
                        cwrnflg = ibset(cwrnflg,1)
                        ! reset file to beginning
                        call cli_start()
                        write (0,*) 'WARNING: CLIGEN file ended on dd/mm/yy: ', &
                            cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                            ' searching for correct day of year from beginning of file.'
                    end if
                end if
            end do

            if( cwrnflg .eq. 0 ) then
                ! check result
                if( cli_today%jday .ne. (cli_next%jday-1) ) then
                    ! days in file are not consecutive
                    if( (ccd .eq. 31) .and. (ccm .eq. 12) ) then
                        ! end of last year, no action required
                    else if( (( (ccd .eq. 28) .or. (ccd .eq. 29) ) .and. (ccm .eq. 2)) &
                        .or. ((ccd .eq. 1) .and. (ccm .eq. 3)) ) then
                        ! leap year mismatch problem, set error flag
                        ! this should now run in day of year mode, agnostic to leap day
                        cwrnflg = ibset(cwrnflg,2)
                        write (0,*) 'WARNING: CLIGEN file leap year mismatch on dd/mm/yy: ', &
                            cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                            ' Using day of year and not searching for years.'
                    else
                        ! missing day in file
                        write (0,*) 'ERROR: CLIGEN read failed following dd/mm/yy: ', &
                             cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                             ' Please create file with no missing days.'
                            call exit(1)
                    end if
                end if
            end if
            
        end if
          
        if( cwrnflg .gt. 0 ) then
            ! running in day of year mode

            ! julian day of year
            doy = dayear( ccd, ccm, ccy)

            if( (doy .lt. cli_today%doy) .and. (cli_next%doy .eq. 1) ) then
                ! read next in new year
                ! set cli_prev to cli_today
                cli_prev = cli_today
                ! set cli_today to cli_next
                cli_today = cli_next
                ! retrieve line for cli_next
                ioc = cli_read_next(cli_next)
                if (ioc .gt. 0) then
                    ! We have a failure reading or parsing the next data line in file
                    write (0,*) 'ERROR: CLIGEN read failed following dd/mm/yy: ', &
                         cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                         ' Rewind to first day failed.'
                    call exit(1)
                end if
            end if

            do while( doy .gt. cli_today%doy )
                ! day requested is later than position in file, scan forward

                ! set cli_prev to cli_today
                cli_prev = cli_today
                ! set cli_today to cli_next
                cli_today = cli_next
                ! retrieve line for cli_next
                ioc = cli_read_next(cli_next)
                if (ioc .gt. 0) then
                    ! We have a failure reading or parsing the next data line in file
                    ! read next line from beginning of file
                    call cli_start()
                    ioc = cli_read_next(cli_next)
                    if (ioc .gt. 0) then
                        write (0,*) 'ERROR: CLIGEN read failed following dd/mm/yy: ', &
                             cli_today%day,'/',cli_today%month,'/',cli_today%year, &
                             ' Rewind to first day failed.'
                        call exit(1)
                    end if
                end if

                if( doy .eq. 366 ) then
                    if( (cli_next%doy .eq. 1) .and. (cli_today%doy .eq. 365) ) then
                        ! leap years out of sync, just reuse day 365
                        exit
                    end if
                end if

            end do
        end if

        ! set air density for today
        awdair = 348.56 * (1.013-0.1183*(amzele/1000.) + 0.0048 * (amzele/1000.)**2.) / (cli_today%tdav + 273.1)

       ! copy values into the common blocks include files (eliminate with modules)
       awzdpt = cli_today%zdpt
       awdurpt = cli_today%durpt
       awpeaktpt = cli_today%peaktpt
       awpeakipt = cli_today%peakipt
       awtdmxprev = cli_prev%tdmx
       awtdmn = cli_today%tdmn
       awtdmx = cli_today%tdmx
       awtdmnnext = cli_next%tdmn
       awtdpt = cli_today%tdpt
       aweirr = cli_today%eirr
       awtdav = cli_today%tdav

    end subroutine getcli

    subroutine cliginit
        use file_io_mod, only: luicli
        use erosion_data_struct_defs, only: awzypt
!      include 'p1werm.inc'
      include 'w1clig.inc'
!      include 'm1flag.inc'
!      include 'main/w1cli.inc'
!      include 'main/w1win.inc'

        integer idx
        character header*128
        ! real junk1, junk2

        rewind luicli

        if (cli_gen_fmt_flag == 3) then
            n_header = 15
        else if (cli_gen_fmt_flag == 2) then
            n_header = 14
        else
            n_header = 8
        endif

        if (cli_gen_fmt_flag == 3) then
            read(luicli,'(a128)') header
        end if

        if (cli_gen_fmt_flag >= 2) then  !Forest Service cligen db format

            do idx = 1, 5
                read(luicli,'(a128)') header
               ! write(6,*) 'header: ', header,':'
            end do

            ! read monthy average of daily maximum temperature
            read(luicli,*) (cli_mav%tmx(idx), idx = 1,12)
            ! write(6,*)  (awtmxav(idx), idx = 1,12)

            read(luicli,'(a128)') header
            ! write(6,*) 'header: ', header

            ! read monthy average of daily minimum temperature
            read(luicli,*) (cli_mav%tmn(idx), idx = 1,12)
            ! write(6,*)  (awtmnav(idx), idx = 1,12)

            ! find yearly average temperature
            awtyav = 0.0
            do idx = 1, 12
                ! average temperature is mean of maximum and minimum
                awtyav = awtyav + (cli_mav%tmn(idx) + cli_mav%tmx(idx)) / 2.0
            end do
            awtyav = awtyav/12.0

            ! read three lines to get to precipitation values
            do idx = 1, 3
                read(luicli,'(a128)') header
                ! write(6,*) 'header: ', header,':'
            end do

            ! read average monthy total precipitation
            read(luicli,*) (cli_mav%zpt(idx), idx = 1,12)
            ! write(6,*)  (awzmpt(idx), idx = 1,12)

            ! find average yearly total precipitation
            awzypt = 0.0
            do idx = 1, 12
                awzypt = awzypt + cli_mav%zpt(idx)
            end do

            ! copy average values into the common blocks include files (eliminate with modules)
            do idx = 1, 12
                awtmxav(idx) = cli_mav%tmx(idx)
                awtmnav(idx) = cli_mav%tmn(idx)
            end do

        else
 
            ! Old obsolete cligen file format (Canadians are still using it)
            write(*,*) 'Warning: Attempting to use old cligen format file'
            ! we really don't need to use anything from this header
            !do idx = 1, 3       ! Skip the first 3 lines
            !    read(luicli,'(a128)') header
            !end do
            !read(luicli,*) junk1, junk2, awtyav        ! skip lat/lon and get yearly ave temp
            !read(luicli,*) (awtmav(idx), idx = 1,12)   ! read 12 monthly ave temps
        endif

        rewind luicli
        cli_today%day = 0
        cwrnflg = 0

        ! this is a windgen intialization
        ! placed here rather than create a whole new file.
        wwrnflg = 0

    end subroutine cliginit

end module climate_input_mod
