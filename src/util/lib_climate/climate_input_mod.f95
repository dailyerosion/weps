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
        real :: dair     ! Daily average air density (Kg/m^3)
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

    integer :: cli_gen_fmt_flag   ! flag indicating type of cli_gen daily input file
                                  ! = 1 --> 3.1 version cligen db format
                                  ! = 2 --> Forest Service cligen db format
                                  ! = 3 --> new WEPP compatible cli_gen file format

    integer :: wind_gen_fmt_flag  ! flag indicating type of wind_gen daily input file
                                  ! = 1 --> original max/min wind_gen file format
                                  ! = 2 --> new hourly wind_gen file format

    real :: wind_max_value    ! Cap wind speeds to specified value
                              ! wind_max_value = xx.x   ! specified wind speed
                              ! wind_max_value > 0.0    ! sets wind_max_flag to 1

    integer :: wind_max_flag  ! flag indicating whether input wind speeds should be capped
                              ! wind_max_flag = 1 --> cap wind speeds to specified max value

    integer :: cwrnflg            ! binary indicator for cligen warnings
                                  ! 0000, 0 - file contains exact dates needed for simulation
                                  ! 0001, 1 - not all years contained in file, rewind and run in day of year mode
                                  ! 0010, 2 - partial year at end of file, rewind and run in day of year mode
                                  ! 0100, 4 - leap year feb 29 does not match, skip or duplicate as needed
    integer :: wwrnflg            ! binary indicator for windgen warnings (same meaning as above)
    integer :: n_header           ! number of lines in the cligen file header

    real :: cli_tyav              ! Average yearly air temperature (deg C) from CLIGEN header monthly average temperaure

    type(cligen_monavg) :: cli_mav

    type(cligen_state) :: cli_prev
    type(cligen_state) :: cli_today
    type(cligen_state) :: cli_next
    
    type(cligen_state), dimension(:), allocatable :: cli_day

    type(windgen_state) :: wind_prev   ! previous wind data read from file
    type(windgen_state) :: wind_today  ! wind data for the requested day
    type(windgen_state) :: wind_next   ! last wind data read from file

    type(windgen_state), dimension(:), allocatable :: wind_day

   real :: amzele  ! site elevation (m)
   
   integer :: minjuld ! minimum julian day in cligen/windgen array
   integer :: maxjuld ! maximum julian day in cligen/windgen array
   
   logical :: cli_warn_only ! if true, CLIGEN file reading errors generate warning, not error and exit
   logical :: win_warn_only ! if true, WINDGEN file reading errors generate warning, not error and exit

contains

    subroutine set_cli_today( pjuld )
        use erosion_data_struct_defs, only: awdair
        integer, intent(in) :: pjuld   ! present julian day of the simulation run.

        cli_today = cli_day(pjuld)
        awdair = cli_today%dair
    end subroutine set_cli_today

    subroutine set_wind_today( pjuld )
        use erosion_data_struct_defs, only: awadir, awhrmx, awudmx, awudmn, awudav, subday, ntstep
        integer, intent(in) :: pjuld   ! present julian day of the simulation run.

        integer :: idx   ! loop index

        wind_today = wind_day(pjuld)

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

        ! call wind_print(wind_today)

    end subroutine set_wind_today

    subroutine create_cli_day( ijuld, ljuld )
    
        use datetime_mod, only: caldat

        integer, intent(in) :: ijuld   ! This variable contains the initial julian day of the simulation run.
        integer, intent(in) :: ljuld   ! This variable contains the last julian day of the simulation run.

        integer :: alloc_stat  ! allocation error return value
        integer :: idx         ! loop variable
        integer :: day         ! day of month
        integer :: mon         ! month on year (1-12)
        integer :: year        ! year

        ! set array bounds
        minjuld = ijuld
        maxjuld = ljuld

        ! allocate cli_day array
        allocate(cli_day(ijuld:ljuld), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
            write(*,*) 'ERROR: unable to allocate enough memory for cli_day array'
        end if

        call caldat( ijuld, day, mon, year )
        if( (day .eq. 1) .and. (mon .eq. 1) .and. (year .eq. 1) ) then
            ! likely running simulated data, not real data. Allow switch to day of year mode as needed (with warnings)
            cli_warn_only = .true.
        else
            cli_warn_only = .false.
        end if

        do idx = ijuld, ljuld
            call caldat( idx, day, mon, year )
            call getcli( day, mon, year )
            cli_day(idx) = cli_today
        end do

    end subroutine create_cli_day
    
    subroutine create_wind_day( ijuld, ljuld )
        use erosion_data_struct_defs, only: ntstep
        use datetime_mod, only: caldat

        integer, intent(in) :: ijuld   ! This variable contains the initial julian day of the simulation run.
        integer, intent(in) :: ljuld   ! This variable contains the last julian day of the simulation run.

        integer :: alloc_stat  ! allocation error return value
        integer :: idx         ! loop variable
        integer :: jdx         ! loop variable
        integer :: day         ! day of month
        integer :: mon         ! month on year (1-12)
        integer :: year        ! year

        ! allocate wind_day array
        allocate(wind_day(ijuld:ljuld), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
            write(*,*) 'ERROR: unable to allocate enough memory for wind_day array'
        end if

        call caldat( ijuld, day, mon, year )
        if( (day .eq. 1) .and. (mon .eq. 1) .and. (year .eq. 1) ) then
            ! likely running simulated data, not real data. Allow switch to day of year mode as needed (with warnings)
            win_warn_only = .true.
        else
            win_warn_only = .false.
        end if

        do idx = ijuld, ljuld
            ! allocate subdaily array for today
            allocate(wind_day(idx)%wawu(ntstep), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
               write(*,*) 'ERROR: memory alloc., WINDGEN wind speeds'
               stop 1
            end if

            call caldat( idx, day, mon, year )
            call getwin( day, mon, year )
            wind_day(idx) = wind_today
            do jdx = 1, ntstep
               wind_day(idx)%wawu(jdx) = wind_today%wawu(jdx)
            end do
        end do

    end subroutine create_wind_day

    subroutine cliginit( ijuld, ljuld )
        use file_io_mod, only: luicli
        use erosion_data_struct_defs, only: awzypt

        integer, intent(in) :: ijuld   ! This variable contains the initial julian day of the simulation run.
        integer, intent(in) :: ljuld   ! This variable contains the last julian day of the simulation run.

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
            !read(luicli,*) junk1, junk2, cli_tyav      ! skip lat/lon and get yearly ave temp
            !read(luicli,*) (awtmav(idx), idx = 1,12)   ! read 12 monthly ave temps
        endif

        ! triggers first day setup
        cli_today%day = 0
        ! error flag set to no errors
        cwrnflg = 0

        call create_cli_day( ijuld, ljuld )
        
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
    function cli_read_next( ) result( cli_oneday )

        use file_io_mod, only: luicli     ! file unit number
        use datetime_mod, only: julday, dayear, ckdate

        type(cligen_state) :: cli_oneday  ! structure for cligen data line values

        ! + + + LOCAL VARIABLES + + +
        character :: line*256             ! data line read from file
        integer :: ioc                    ! error return from read statement
        integer :: errflg                 ! 0 - no error
                                          ! 1 - read from file failed, restarted from beginning
        real :: dummy

        errflg = 0
        do while(.true.)
            ! look for a valid line in the file

            ! read line from file
            read (luicli,'(a)',iostat=ioc) line
            if (ioc .ne. 0) then
                ! failure reading data line from file
                errflg = 1
                ! try resetting to beginning of file
                call cli_start()
                read (luicli,'(a)',iostat=ioc) line
                if (ioc .ne. 0) then
                    write(0,*) 'Unable to read cligen file after rewind'
                    call exit(1)
                end if
            end if

            ! successfull read from file, parse the line
            read(line, *, iostat=ioc) cli_oneday%day, cli_oneday%month, cli_oneday%year, &
                cli_oneday%zdpt, cli_oneday%durpt, cli_oneday%peaktpt, cli_oneday%peakipt, &
                cli_oneday%tdmx, cli_oneday%tdmn, cli_oneday%grad, dummy, dummy, cli_oneday%tdpt
            if (ioc .eq. 0) then

                ! check that day, month, year values are a valid date
                if( ckdate( cli_oneday%day,cli_oneday%month,cli_oneday%year ) ) then
                    ! populate derived values in cligen data record
                    cli_oneday%jday = julday(cli_oneday%day,cli_oneday%month,cli_oneday%year)
                    cli_oneday%doy = dayear(cli_oneday%day,cli_oneday%month,cli_oneday%year)
                    cli_oneday%tdav = (cli_oneday%tdmx + cli_oneday%tdmn) / 2.0
                    cli_oneday%eirr = cli_oneday%grad * 0.04186
                    cli_oneday%dair = 348.56 * (1.013-0.1183*(amzele/1000.) + 0.0048 * (amzele/1000.)**2.) / (cli_oneday%tdav+273.1)
                    exit
                else
                    write(0,"(6(a,i0),a)") 'WARNING: Date in file: ', cli_oneday%day, '/', cli_oneday%month, '/', cli_oneday%year, &
                                ' Is not a valid date. This line is skipped.'
                end if
                !write(*,*) 'READ:   ', cli_oneday%doy, cli_oneday%day, cli_oneday%month, cli_oneday%year, cli_oneday%tdmx

            else
                errflg = errflg + 1
                if( errflg .gt. 2 ) then
                    write(0,*) 'Unable to parse cligen file line after rewind'
                    call exit(1)
                end if
            end if

        end do
        
    end function cli_read_next

    subroutine getcli(ccd, ccm, ccy)

        use datetime_mod, only: isleap, julday, dayear
        use file_io_mod, only: luicli

        integer, intent(in) :: ccd    ! requested day of month
        integer, intent(in) :: ccm    ! requested month of year
        integer, intent(in) :: ccy    ! requested year

        ! + + + LOCAL VARIABLES + + +
        integer :: jday    ! astronomical julian day for requested day
        integer :: doy     ! day of year for requested day

        if (cli_today%day .eq. 0) then
            ! starting at beginning of the file
            call cli_start()
            ! reading first data line of file
            cli_today = cli_read_next()

            if( julday(ccd, ccm, ccy) .lt. cli_today%jday ) then
                if( cli_warn_only ) then
                    ! set error flag to indicate earlier date request, will now read by day of year.
                    cwrnflg = ibset(cwrnflg,0)
                    write (0,"(6(a,i0),a)") 'WARNING: CLIGEN file does not contain first date requested. Date requested: ', &
                            ccd, '/', ccm, '/', ccy, ' CLIGEN file starts on: ', &
                            cli_today%day, '/', cli_today%month, '/', cli_today%year, &
                            ' Now reading file by matching day of year, not exact date.'
                else
                    ! exit, missing day, all days required
                    write (0,"(6(a,i0),a)") 'ERROR: CLIGEN file does not contain first date requested. Date requested: ', &
                            ccd, '/', ccm, '/', ccy, ' CLIGEN file starts on: ', &
                            cli_today%day, '/', cli_today%month, '/', cli_today%year, &
                            ' Exact days are required to run simulation.'
                    call exit(1)
                end if
            end if

            ! reading second line of file
            cli_next = cli_read_next()

            ! set cli_prev to the same values as cli_today
            cli_prev = cli_today

        end if

        if( cwrnflg .eq. 0 ) then
            ! running in exact date mode

            ! astronomical julian day requested
            jday = julday( ccd, ccm, ccy)

            if( jday .eq. cli_today%jday ) then
                return
            else
                do while( jday .ne. cli_today%jday )
                    ! file day is earlier than day requested, scan forward in file
                    cli_prev = cli_today
                    cli_today = cli_next
                    cli_next = cli_read_next()

                    !write(*,*) 'JDAY: ', jday, cli_today%jday, ccd,cli_today%day, ccm,cli_today%month, ccy,cli_today%year

                    if( jday .eq. cli_today%jday ) then
                        return
                    elseif( jday .lt. cli_today%jday ) then
                        if( cli_warn_only ) then
                            cwrnflg = ibset(cwrnflg,0)
                            write (0,"(3(a,i0),a)") 'WARNING: Date requested: ', ccd, '/', ccm, '/', ccy, &
                                ' CLIGEN file is missing this date. Now reading file by matching day of year, not exact date.'
                            exit
                        else
                            write (0,"(3(a,i0),a)") 'ERROR: Date requested: ', ccd, '/', ccm, '/', ccy, &
                                ' CLIGEN file is missing this date. Simulation terminated with error.'
                            call exit(1)
                        end if
                    elseif( cli_today%jday .lt. cli_prev%jday ) then
                        if( cli_warn_only ) then
                            cwrnflg = ibset(cwrnflg,0)
                            write (0,"(6(a,i0),a)") 'WARNING: Date requested: ', ccd, '/', ccm, '/', ccy, &
                                ' CLIGEN file ended: ', cli_prev%day,'/',cli_prev%month,'/',cli_prev%year, &
                                ' Now reading file by matching day of year, not exact date.'
                            exit
                        else
                            write (0,"(6(a,i0),a)") 'ERROR: Date requested: ', ccd, '/', ccm, '/', ccy, &
                                ' CLIGEN file ended: ', cli_prev%day,'/',cli_prev%month,'/',cli_prev%year, &
                                ' Simulation terminated with error.'
                            call exit(1)
                        end if
                    end if
                end do
            end if
        end if
          
        if( cwrnflg .gt. 0 ) then
            ! running in day of year mode

            ! julian day of year
            doy = dayear( ccd, ccm, ccy)

            if( doy .eq. cli_today%doy ) then
                return
            elseif( (doy .eq. 366) .and. (cli_next%doy .lt. cli_today%doy) ) then
                ! leap year request, file not leap year
                ! return previous value
                return
            else
                ! read file until doy found in file
                ! Note: skips day 366, reads doy 1 into cli_today
                do while( doy .ne. cli_today%doy )
                    ! file day is not the day requested, scan forward in file
                    cli_prev = cli_today
                    cli_today = cli_next
                    cli_next = cli_read_next()
                    if( doy .eq. cli_today%doy ) then
                        return
                    end if
                end do
            end if

            ! generate one time warning of switch to day of year mode
            if( cwrnflg .lt. 8 ) then
                ! Set when file is shorter than simulation
                if( (cli_prev%year .gt. cli_today%year) ) then
                    ! set cwrnflg to not generate additional warnings
                    cwrnflg = ibset(cwrnflg,3)
                    ! print warning
                    write (0,"(3(a,i0),a)") 'WARNING: CLIGEN file is shorter than simulation. File ends on: ', &
                            cli_prev%day, '/', cli_prev%month, '/', cli_prev%year, &
                            ' Rereading file using day of year as many times as needed to complete the simulation'
                end if
            end if

        end if

    end subroutine getcli

    subroutine windinit( ijuld, ljuld )
        use erosion_data_struct_defs, only: ntstep

        integer, intent(in) :: ijuld   ! This variable contains the initial julian day of the simulation run.
        integer, intent(in) :: ljuld   ! This variable contains the last julian day of the simulation run.

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

        ! triggers first day setup
        wind_today%day = 0
        ! error flag set to no errors
        wwrnflg = 0

        call create_wind_day( ijuld, ljuld )

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
    subroutine wind_read_next( wind_oneday )

        use file_io_mod, only: luiwin     ! file unit number
        use erosion_data_struct_defs, only: ntstep
        use datetime_mod, only: julday, dayear, ckdate

        type(windgen_state) :: wind_oneday  ! structure for windgen data line values

        ! + + + LOCAL VARIABLES + + +
        character :: line*1024            ! data line read from file
        integer :: ioc                    ! error return from read statement
        integer :: errflg                 ! returns error result of read
                                          ! 0 - no error
                                          ! 1 - failure to read from file
        integer :: idx                    ! index for reading array values
        integer, dimension(1) :: tmp_hrmax   ! tmp array for hour of max wind speed
        real tmp_array(ntstep)               ! tmp array for hrly wind speed


        errflg = 0
        do while(.true.)
            ! look for a valid line in the file

            ! read single line from file
            read (luiwin,'(a)',iostat=ioc) line
            if (ioc .ne. 0) then
                ! failure reading data line from file
                errflg = 1
                ! try resetting to beginning of file
                call wind_start()
                read (luiwin,'(a)',iostat=ioc) line
                if (ioc .ne. 0) then
                    write(0,*) 'Unable to read windgen file after rewind'
                    call exit(1)
                end if
            end if

            ! successfull read from file, parse the line
            if (wind_gen_fmt_flag == 1) then   ! original wind_gen file format
                read(line, *, iostat=ioc) wind_oneday%day, wind_oneday%month, wind_oneday%year, wind_oneday%wwadir, &
                                          wind_oneday%wwudmx, wind_oneday%wwudmn, wind_oneday%wwhrmx
                ! populate derived values in windgen data record
                wind_oneday%jday = julday(wind_oneday%day,wind_oneday%month,wind_oneday%year)
                wind_oneday%doy = dayear(wind_oneday%day,wind_oneday%month,wind_oneday%year)
                exit
            else                               ! wind_gen2 file format
                read(line, *, iostat=ioc) wind_oneday%day, wind_oneday%month, wind_oneday%year, wind_oneday%wwadir, &
                                          (wind_oneday%wawu(idx), idx=1,ntstep)
            end if
            
            if( ioc .eq. 0 ) then
                ! check that day, month, year values are a valid date
                if( ckdate( wind_oneday%day,wind_oneday%month,wind_oneday%year ) ) then
                    if (wind_gen_fmt_flag .ne. 1) then    ! wind_gen2 file format
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
                        ! populate derived values in windgen data record
                        wind_oneday%jday = julday(wind_oneday%day,wind_oneday%month,wind_oneday%year)
                        wind_oneday%doy = dayear(wind_oneday%day,wind_oneday%month,wind_oneday%year)
                        exit
                    end if
                else
                    write(0,"(6(a,i0),a)") 'WARNING: Date in file: ', wind_oneday%day, '/', wind_oneday%month, '/', wind_oneday%year, &
                                ' Is not a valid date. This line is skipped.'
                end if
            else
                errflg = errflg + 1
                if( errflg .gt. 2 ) then
                    write(0,*) 'Unable to parse windgen file line after rewind'
                    call exit(1)
                end if
            end if

        end do
        
    end subroutine wind_read_next

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

        use datetime_mod, only: isleap
        use file_io_mod, only: luiwin
        use datetime_mod, only: julday, dayear

        integer :: cwd    ! requested day of month
        integer :: cwm    ! requested month of year
        integer :: cwy    ! requested year

        ! + + + LOCAL VARIABLES + + +
        integer :: jday    ! astronomical julian day for requested day
        integer :: doy     ! day of year for requested day
        integer :: idx     ! index for reading array values

        if (wind_today%day .eq. 0) then
            ! starting at beginning of the file
            call wind_start()
            ! reading first data line of file
            call wind_read_next( wind_today )

            if( julday(cwd, cwm, cwy) .lt. wind_today%jday ) then
                if( win_warn_only ) then
                    ! set error flag to indicate earlier date request, will now read by day of year.
                    wwrnflg = ibset(wwrnflg,0)
                    write (0,"(6(a,i0),a)") 'WARNING: WINDGEN file does not contain first date requested. Date requested: ', &
                            cwd, '/', cwm, '/', cwy, ' WINDGEN file starts on: ', &
                            wind_today%day, '/', wind_today%month, '/', wind_today%year, &
                            ' Now reading file by matching day of year, not exact date.'
                else
                    ! exit, missing day, all days required
                    write (0,"(6(a,i0),a)") 'ERROR: WINDGEN file does not contain first date requested. Date requested: ', &
                            cwd, '/', cwm, '/', cwy, ' CLIGEN file starts on: ', &
                            wind_today%day, '/', wind_today%month, '/', wind_today%year, &
                            ' Exact days are required to run simulation.'
                    call exit(1)

                end if
            end if

            ! reading second line of file
            call wind_read_next( wind_next )

            ! set wind_prev to the same values as wind_today
            wind_prev = wind_today

        end if

        if( wwrnflg .eq. 0 ) then
            ! running in exact date mode

            ! astronomical julian day requested
            jday = julday( cwd, cwm, cwy)
        
            if( jday .eq. wind_today%jday ) then
                return
            else
                do while( jday .ne. wind_today%jday )
                    ! file day is earlier than day requested, scan forward in file
                    wind_prev = wind_today
                    wind_today = wind_next
                    call wind_read_next( wind_next )

                    if( jday .eq. wind_today%jday ) then
                        return
                    elseif( jday .lt. wind_today%jday ) then
                        if( win_warn_only ) then
                            ! set error flag to indicate earlier date request, will now read by day of year.
                            wwrnflg = ibset(wwrnflg,0)
                            write (0,"(3(a,i0),a)") 'WARNING: Date requested: ', cwd, '/', cwm, '/', cwy, &
                                  ' WINDGEN file is missing this date. Now reading file by matching day of year, not exact date.'
                            exit
                        else
                            ! exit, missing day, all days required
                            write (0,"(3(a,i0),a)") 'ERROR: Date requested: ', cwd, '/', cwm, '/', cwy, &
                                  ' WINDGEN file is missing this date. Simulation terninated with error.'
                            call exit(1)
                        end if
                    elseif( wind_today%jday .lt. wind_prev%jday ) then
                        if( win_warn_only ) then
                            wwrnflg = ibset(wwrnflg,0)
                            write (0,"(6(a,i0),a)") 'WARNING: Date requested: ', cwd, '/', cwm, '/', cwy, &
                                ' WINDGEN file ended: ', wind_prev%day,'/',wind_prev%month,'/',wind_prev%year, &
                                ' Now reading file by matching day of year, not exact date.'
                            exit
                        else
                            write (0,"(6(a,i0),a)") 'ERROR: Date requested: ', cwd, '/', cwm, '/', cwy, &
                                ' CLIGEN file ended: ', wind_prev%day,'/',wind_prev%month,'/',wind_prev%year, &
                                ' Simulation terminated with error.'
                            call exit(1)
                        end if
                    end if
                end do
            end if
        end if

        if( wwrnflg .gt. 0 ) then
            ! running in day of year mode

            ! julian day of year
            doy = dayear( cwd, cwm, cwy)

            if( doy .eq. wind_today%doy ) then
                return
            elseif( (doy .eq. 366) .and. (wind_next%doy .lt. wind_today%doy) ) then
                ! leap year request, file not leap year
                ! return previous value
                return
            else
                ! read file until doy found in file
                ! Note: skips day 366, reads doy 1 into cli_today
                do while( doy .ne. wind_today%doy )
                    ! file day is not the day requested, scan forward in file
                    wind_prev = wind_today
                    wind_today = wind_next
                    call wind_read_next( wind_next )
                    if( doy .eq. wind_today%doy ) then
                        return
                    end if
                end do
            end if

            ! generate one time warning of switch to day of year mode
            if( wwrnflg .lt. 8 ) then
                ! Set when file is shorter than simulation
                if( (wind_prev%year .gt. wind_today%year) ) then
                    ! set wwrnflg to not generate additional warnings
                    wwrnflg = ibset(wwrnflg,3)
                    ! print warning
                    write (0,"(3(a,i0),a)") 'WARNING: WINDGEN file is shorter than simulation. File ends on: ', &
                            wind_prev%day, '/', wind_prev%month, '/', wind_prev%year, &
                            ' Rereading file using day of year as many times as needed to complete the simulation'
                end if
            end if

        end if

    end subroutine getwin
    
end module climate_input_mod
