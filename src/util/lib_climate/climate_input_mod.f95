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

    type windgen_state
        ! read from file
        integer :: day   ! day of month
        integer :: month ! month of year
        integer :: year  ! year
        real :: wwadir   ! Predominant daily wind direction (degrees)
        ! read in from original wind_gen file format
        ! also populated from wing_gen2 file format data
        real :: wwudmx   ! Maximum daily wind speed (m/s)
        real :: wwudmn   ! Minimum daily wind speed (m/s)
        real :: wwhrmx   ! Hour maximum daily wind speed occurs (hr)
        ! read in from wind_gen2 file format (note: use allocatable, NOT pointer to copy correctly)
        real, dimension(:), allocatable :: wawu   ! Subdaily wind speeds (m/s)
        ! derived values
        integer :: jday  ! astronomical julian day
        integer :: doy   ! julian day of year
        real :: twe      ! Daily total wind energy (KJ)
        real :: wewudav  ! Average daily wind speed (m/s) (to obtain the daily wind energy)
        real :: wawudav  ! Average daily wind speed (m/s)
    end type windgen_state

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

    real :: cli_tyav              ! Average yearly air temperature (deg C) from CLIGEN header monthly average temperaure

    type(cligen_monavg) :: cli_mav

    type(cligen_state) :: cli_prev
    type(cligen_state) :: cli_today
    type(cligen_state) :: cli_next

    type(windgen_state) :: wind_prev   ! previous wind data read from file
    type(windgen_state) :: wind_today  ! wind data for the requested day
    type(windgen_state) :: wind_next   ! last wind data read from file

   real :: amalat  ! site latitude (degrees)
   real :: amalon  ! site longitude (degrees)
   real :: amzele  ! site elevation (m)

contains

    subroutine cliginit
        use file_io_mod, only: luicli
        use erosion_data_struct_defs, only: awzypt

        integer idx
        character header*128
        integer :: ioc                    ! error return from read statement
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
            read(luicli,'(a128)', iostat=ioc) header
            if( ioc .ne. 0 ) then
                write(6,*) 'Error reading cligen header line 0'
            end if
        end if

        if (cli_gen_fmt_flag >= 2) then  !Forest Service cligen db format

            do idx = 1, 5
                read(luicli,'(a128)',iostat=ioc) header
                if( ioc .ne. 0 ) then
                    write(6,*) 'Error reading cligen header line ', idx
                end if
            end do

            ! read monthy average of daily maximum temperature
            read(luicli,*,iostat=ioc) (cli_mav%tmx(idx), idx = 1,12)
            if( ioc .ne. 0 ) then
                write(6,*) 'Error reading cligen average monthly max temperature values line.'
            end if

            read(luicli,'(a128)',iostat=ioc) header
            if( ioc .ne. 0 ) then
                write(6,*) 'Error reading cligen header line: '
                write(6,*) 'Line = ', header
            end if

            ! read monthy average of daily minimum temperature
            read(luicli,*,iostat=ioc) (cli_mav%tmn(idx), idx = 1,12)
            if( ioc .ne. 0 ) then
                write(6,*) 'Error reading cligen average monthly min temperature values line.'
            end if

            ! find yearly average temperature
            cli_tyav = 0.0
            do idx = 1, 12
                ! average temperature is mean of maximum and minimum
                cli_tyav = cli_tyav + (cli_mav%tmn(idx) + cli_mav%tmx(idx)) / 2.0
            end do
            cli_tyav = cli_tyav/12.0

            ! read three lines to get to precipitation values
            do idx = 1, 3
                read(luicli,'(a128)',iostat=ioc) header
                if( ioc .ne. 0 ) then
                    write(6,*) 'Error reading cligen header line: '
                    write(6,*) 'Line = ', header
                end if
            end do

            ! read average monthy total precipitation
            read(luicli,*,iostat=ioc) (cli_mav%zpt(idx), idx = 1,12)
            if( ioc .ne. 0 ) then
                write(6,*) 'Error reading cligen average monthly precipitation values line.'
            end if

            ! find average yearly total precipitation
            awzypt = 0.0
            do idx = 1, 12
                awzypt = awzypt + cli_mav%zpt(idx)
            end do

        else
 
            ! Old obsolete cligen file format (Canadians are still using it)
            write(*,*) 'Warning: Attempting to use old cligen format file'
            ! we really don't need to use anything from this header
            !do idx = 1, 3       ! Skip the first 3 lines
            !    read(luicli,'(a128)') header
            !end do
            !read(luicli,*) junk1, junk2, cli_tyav        ! skip lat/lon and get yearly ave temp
            !read(luicli,*) (awtmav(idx), idx = 1,12)   ! read 12 monthly ave temps
        endif

        rewind luicli
        cli_today%day = 0
        cwrnflg = 0

    end subroutine cliginit

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
        if (ioc .ne. 0) then
            ! failure reading data line from file
            errflg = 1
            return
        end if

        ! parse the line read from file
        read(line, *, iostat=ioc) cli_oneday%day, cli_oneday%month, cli_oneday%year, &
            cli_oneday%zdpt, cli_oneday%durpt, cli_oneday%peaktpt, cli_oneday%peakipt, &
            cli_oneday%tdmx, cli_oneday%tdmn, cli_oneday%grad, dummy, dummy, cli_oneday%tdpt
        if (ioc .ne. 0) then
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

        ! + + + LOCAL VARIABLES + + +
        integer :: ioc     ! file and string read error return code
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

    end subroutine getcli

    subroutine windinit
        use erosion_data_struct_defs, only: ntstep

        integer :: alloc_stat

        allocate(wind_prev%wawu(ntstep), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., WINDGEN wind speeds'
           stop 1
        end if

        allocate(wind_today%wawu(ntstep), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., WINDGEN wind speeds'
           stop 1
        end if

        allocate(wind_next%wawu(ntstep), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., WINDGEN wind speeds'
           stop 1
        end if

        ! this is a windgen intialization
        ! placed here rather than create a whole new file.
        wwrnflg = 0

    end subroutine windinit

    subroutine wind_start()
        use file_io_mod, only: luiwin     ! file unit number

        ! + + + LOCAL VARIABLES + + +
        character :: header*80
        integer :: idx

        rewind luiwin
        ! read through header lines when at beginning of file
        do idx = 1,7
            read(luiwin,*,err=9010) header
        end do
        return

9010    write(0,*) 'Unexpected error in windgen header'
        call exit(1)
    end subroutine wind_start

    ! subroutine to read in one day from windgen file
    function wind_read_next( wind_oneday ) result( errflg )

        use file_io_mod, only: luiwin     ! file unit number
        use erosion_data_struct_defs, only: ntstep
        use datetime_mod, only: julday, dayear
        use weps_main_mod, only: wind_max_flag, wind_max_value

        type(windgen_state) :: wind_oneday  ! structure for windgen data line values
        integer :: errflg                 ! returns error result of read
                                          ! 0 - no error
                                          ! 1 - failure to read from file
                                          ! 2 - failure to parse line

        ! + + + LOCAL VARIABLES + + +
        character :: line*1024            ! data line read from file
        integer :: ioc                    ! error return from read statement
        integer :: idx                    ! index for reading array values
        integer, dimension(1) :: tmp_hrmax   ! tmp array for hour of max wind speed
        real tmp_array(ntstep)               ! tmp array for hrly wind speed

        ! read single line from file
        read (luiwin,'(a)',iostat=ioc) line
        if (ioc .ne. 0) then
            ! failure reading data line from file
            errflg = 1
            return
        end if

        if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
            read(line, *, iostat=ioc) wind_oneday%day, wind_oneday%month, wind_oneday%year, wind_oneday%wwadir, &
                                      wind_oneday%wwudmx, wind_oneday%wwudmn, wind_oneday%wwhrmx
            if( ioc .ne. 0 ) then
                ! error reading individual line
                errflg = 2
                return
            end if
        else                               ! wind_gen2 file format
            read(line, *, iostat=ioc) wind_oneday%day, wind_oneday%month, wind_oneday%year, wind_oneday%wwadir, &
                                      (wind_oneday%wawu(idx), idx=1,ntstep)
            if( ioc .ne. 0 ) then
                ! error reading individual line
                errflg = 2
                return
            end if

            if (wind_max_flag == 1) then   ! Cap winds greater than specified maximum
                do idx = 1, ntstep
                    wind_oneday%wawu(idx) = min(wind_oneday%wawu(idx), wind_max_value)
                end do
            end if

            ! compute the total wind energy for the day
            wind_oneday%twe = 0.0
            do idx = 1, ntstep
                wind_oneday%twe = wind_oneday%twe + 0.5 * (wind_oneday%wawu(idx)**3.0) * (86400./ntstep) / 1000.0
            end do

            ! compute the average wind speed to generate the total wind energy for the day
            ! (this is not the same as daily average wind speed)
            wind_oneday%wewudav = (wind_oneday%twe/ntstep) * 2.0 * 1000.0/(86400./ntstep)
            wind_oneday%wewudav = (wind_oneday%wewudav)**(1.0/3.0)

            ! Determine the "old" variable values needed within the model
            ! Some of these may not be 100% correct, but they are the best
            ! have come up with for the time being. Modified to remove the ntstep=24 assumption
            do idx = 1, ntstep
                tmp_array(idx) = wind_oneday%wawu(idx)
            end do
            tmp_hrmax = maxloc(tmp_array)
            wind_oneday%wwhrmx = nint(tmp_hrmax(1)*24.0/ntstep)
            wind_oneday%wawudav = sum(tmp_array)/ntstep
            wind_oneday%wwudmx = maxval(tmp_array)
            wind_oneday%wwudmn = wind_oneday%wwudmx - wind_oneday%wawudav
        end if

        ! populate derived values in windgen data record
        wind_oneday%jday = julday(wind_oneday%day,wind_oneday%month,wind_oneday%year)
        wind_oneday%doy = dayear(wind_oneday%day,wind_oneday%month,wind_oneday%year)

        errflg = 0
        return
    end function wind_read_next

    subroutine wind_print(wind_oneday)
        use erosion_data_struct_defs, only: ntstep
        type(windgen_state) :: wind_oneday  ! structure for windgen data line values
        integer :: idx

        if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
            write(*,*) 'Wind jday, year, doy: ', wind_oneday%jday, wind_oneday%year, wind_oneday%doy
        else
            write(*,'(2i3,i5,f6.1,24f5.1)') wind_oneday%day, wind_oneday%month, wind_oneday%year, wind_oneday%wwadir, &
                                      (wind_oneday%wawu(idx), idx=1,ntstep)
        end if
    end subroutine wind_print

    subroutine getwin(cwd, cwm, cwy)

        use weps_interface_defs
        use datetime_mod, only: isleap
        use file_io_mod, only: luiwin
        use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn, awudav, subday, ntstep
        use datetime_mod, only: julday, dayear

        integer :: cwd    ! requested day of month
        integer :: cwm    ! requested month of year
        integer :: cwy    ! requested year

        ! + + + LOCAL VARIABLES + + +
        integer :: ioc     ! file and string read error return code
        integer :: jday    ! astronomical julian day for requested day
        integer :: doy     ! day of year for requested day
        integer :: idx     ! index for reading array values

        ! This code added to re-initialize the reading of a windgen file
        ! following the "initialization" phase.  It is triggered if the
        ! day (cwd) passed to the subroutine is set to zero - LEW
        if (cwd == 0) then
            wind_today%day = 0
            rewind luiwin
            return
        endif

        if (wind_today%day .eq. 0) then
            ! restarting at beginning of the file
            call wind_start()
            ! reading first data line of file
            ioc = wind_read_next(wind_today)
            if( ioc .eq. 1) then
                ! We have a failure reading the first data line in file
                write (0,*) 'ERROR: WINDGEN failed to read first data line. Check file.'
                call exit(1)
            else if(ioc .eq. 2) then
                ! We have a failure parsing the first data line in file
                write (0,*) 'ERROR: WINDGEN failed to parse first data line. Check file format.'
                call exit(1)
            end if
            if( julday(cwd, cwm, cwy) .lt. wind_today%jday ) then
                ! set error flag to indicate earlier date request, will now read by day of year.
                wwrnflg = ibset(wwrnflg,0)
                write(*,*) 'Warning: WINGEN file does not contain date requested, using Day of Year mode.'
            end if

            ! reading second line of file
            ioc = wind_read_next(wind_next)
            if (ioc .eq. 1) then
                ! We have a failure reading the second data line in file
                write (0,*) 'ERROR: WINDGEN failed to read second data line. Check file.'
                call exit(1)
            else if (ioc .eq. 2) then
                ! We have a failure parsing the second data line in file
                write (0,*) 'ERROR: WINDGEN failed to parse second data line. Check file format.'
                call exit(1)
            end if

            ! check that next day is one day after today
            if( wind_next%jday .ne. (wind_today%jday + 1) ) then
                ! first two days in file are not consecutive
                write (0,*) 'ERROR: WINDGEN days not consecutive in first two data lines. Check file.'
                call exit(1)
            end if

            ! set wind_prev to the same values as wind_today
            wind_prev = wind_today

        end if

        if( wwrnflg .eq. 0 ) then
            ! running in exact date mode

            ! astronomical julian day requested
            jday = julday( cwd, cwm, cwy)
        
            if( (jday .lt. wind_today%jday) .and. (jday .ge. wind_next%jday) ) then
                ! file reset to beginning, read next day
                ! set wind_prev to wind_today
                wind_prev = wind_today
                ! set wind_today to wind_next
                wind_today = wind_next
                ! retrieve line for wind_next
                ioc = wind_read_next(wind_next)
                if (ioc .gt. 0) then
                    write (0,*) 'ERROR: WINDGEN read failed following dd/mm/yy: ', &
                         wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                         ' Rewind to first day failed.'
                    call exit(1)
                end if
            end if

            do while( (jday .gt. wind_today%jday) .and. (wwrnflg .eq. 0 ) )
                ! day requested is later than position in file, scan forward

                ! set wind_prev to wind_today
                wind_prev = wind_today
                ! set wind_today to wind_next
                wind_today = wind_next
                ! retrieve line for wind_next
                ioc = wind_read_next(wind_next)
                if (ioc .gt. 0) then
                    ! We have a failure reading or parsing the next data line in file
                    if( (cwd .eq. 31) .and. (cwm .eq. 12) ) then
                        ! end of year, read next line from beginning of file
                        call wind_start()
                        ioc = wind_read_next(wind_next)
                        if (ioc .gt. 0) then
                            write (0,*) 'ERROR: WINDGEN read failed following dd/mm/yy: ', &
                                 wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                                 ' Rewind to first day failed.'
                            call exit(1)
                        end if
                    else
                        ! file did not end with complete year
                        ! set error flag to indicate incomplete year
                        wwrnflg = ibset(wwrnflg,1)
                        ! reset file to beginning
                        call wind_start()
                        write (0,*) 'WARNING: WINDGEN file ended on dd/mm/yy: ', &
                            wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                            ' searching for correct day of year from beginning of file.'
                    end if
                end if
            end do

            if( wwrnflg .eq. 0 ) then
                ! check result
                if( wind_today%jday .ne. (wind_next%jday-1) ) then
                    ! days in file are not consecutive
                    if( (cwd .eq. 31) .and. (cwm .eq. 12) ) then
                        ! end of last year, no action required
                    else if( (( (cwd .eq. 28) .or. (cwd .eq. 29) ) .and. (cwm .eq. 2)) &
                        .or. ((cwd .eq. 1) .and. (cwm .eq. 3)) ) then
                        ! leap year mismatch problem, set error flag
                        ! this should now run in day of year mode, agnostic to leap day
                        wwrnflg = ibset(wwrnflg,2)
                        write (0,*) 'WARNING: WINDGEN file leap year mismatch on dd/mm/yy: ', &
                            wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                            ' Using day of year and not searching for years.'
                    else
                        ! missing day in file
                        write (0,*) 'ERROR: WINDGEN read failed following dd/mm/yy: ', &
                             wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                             ' Please create file with no missing days.'
                            call exit(1)
                    end if
                end if
            end if

        end if

        if( wwrnflg .gt. 0 ) then
            ! running in day of year mode

            ! julian day of year
            doy = dayear( cwd, cwm, cwy)

            if( (doy .lt. wind_today%doy) .and. (wind_next%doy .eq. 1) ) then
                ! read next in new year
                ! set wind_prev to wind_today
                wind_prev = wind_today
                ! set wind_today to wind_next
                wind_today = wind_next
                ! retrieve line for wind_next
                ioc = wind_read_next(wind_next)
                if (ioc .gt. 0) then
                    ! We have a failure reading or parsing the next data line in file
                    write (0,*) 'ERROR: WINDGEN read failed following dd/mm/yy: ', &
                         wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                         ' Rewind to first day failed.'
                    call exit(1)
                end if
            end if

            do while( doy .gt. wind_today%doy )
                ! day requested is later than position in file, scan forward

                ! set wind_prev to wind_today
                wind_prev = wind_today
                ! set wind_today to wind_next
                wind_today = wind_next
                ! retrieve line for wind_next
                ioc = wind_read_next(wind_next)
                if (ioc .gt. 0) then
                    ! We have a failure reading or parsing the next data line in file
                    ! read next line from beginning of file
                    call wind_start()
                    ioc = wind_read_next(wind_next)
                    if (ioc .gt. 0) then
                        write (0,*) 'ERROR: WINDGEN read failed following dd/mm/yy: ', &
                             wind_today%day,'/',wind_today%month,'/',wind_today%year, &
                             ' Rewind to first day failed.'
                        call exit(1)
                    end if
                end if

                if( doy .eq. 366 ) then
                    if( (wind_next%doy .eq. 1) .and. (wind_today%doy .eq. 365) ) then
                        ! leap years out of sync, just reuse day 365
                        exit
                    end if
                end if

            end do
        end if

        awadir = wind_today%wwadir
        awudmx = wind_today%wwudmx
        awudmn = wind_today%wwudmn
        awhrmx = wind_today%wwhrmx
        if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
            awudav = (awudmx + awudmn) / 2.0
        else                               ! wind_gen2 file format
            awudav = wind_today%wawudav
            do idx = 1,ntstep
                subday(idx)%awu = wind_today%wawu(idx)
            end do
        endif

        !call wind_print(wind_today)

        return
    end subroutine getwin

end module climate_input_mod
