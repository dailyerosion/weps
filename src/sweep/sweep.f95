!$Author$
!$Date$
!$Revision$
!$HeadURL$

! Make sure that any input filename specified with the -i option has an
! extension of some kind.
!
!**********************************************************************
!     MAIN for SWEEP
!**********************************************************************
      program sweep

      use sweep_interface_defs
      use weps_interface_defs
      use file_io_mod, only: fopenk
      use erosion_data_struct_defs
      use grid_geo_def, only: imax, jmax, ix, jy, xgdpt, ygdpt
      use saeinp_mod, only: mksaeinp
      use p1unconv_mod, only: SEC_PER_DAY

!     +++  PURPOSE +++

!     To start a standalone version of the EROSION submodel

!     It calls ERODEIN to read an input file (stdin),
!     calls ERODINIT to initialize grid,
!     runs the EROSION submodel code, and
!     calls ERODOUT to print the generated output (stdout).

!     + + + GLOBAL COMMON BLOCKS + + +
      include 'p1werm.inc'  ! mnsub, mnbpt, mnbr, mnarpt, mnar, mnspt, mngdpt
      include 'm1sim.inc'   ! am0jd, erod_interval, ntstep
      include 'm1geo.inc'   ! amxsim
      include 'm1subr.inc'  ! nsubr
      include 'm1flag.inc'  ! am0eif, am0efl
      include 'w1clig.inc'  ! awzypt, Requires yrly average precip.....

!      include 'util/misc/f2kcli.inc' !declarations for f2k commandline functions

!     +++ SUBROUTINES CALLED+++
!     erodin
!     erodinit
!     erosion
!     erodout

!     ++++ LOCAL VARIABLES +++
      type(subregionsurfacestate), dimension(:), allocatable :: subrsurf
      type(threshold), dimension(:), allocatable :: noerod                 ! report values to show which factors prevented erosion
      type(cellsurfacestate), dimension(:,:), allocatable :: cellstate     ! grid cell state values (allocate in erodinit)

      integer :: alloc_stat, sum_stat

      character*1024 exe_filepath
      character*1024 input_filepath
      integer i_unit
      integer o_unit

      character*1024 argv      !For Fortran 2k commandline parsing
      integer        i
      integer        numarg
      integer        ll, ss
      logical        opnd
      integer        already_read_inputs
 
      logical        have_ifile
      integer        indx, rndx

      logical        hagen_plot_flag
      logical        force_plot_flag  !ensure that commandline overrides input file settings
      integer        force_emit_val   !ensure that commandline overrides input file settings
      integer        force_debug_flag !ensure that commandline overrides input file setting

      character*1024 file_bname
      character*1024 fpath_bname
      character*1024 input_filename

      integer        o_einp_unit   !Unit number for generated input file
      integer        o_egrd_unit   !Unit number for grid summary erosion
      integer        o_sgrd_unit   !Unit number for grid subdaily erosion
      integer        o_erod_unit   !Unit number for total erosion
      integer        o_emit_unit   !Unit number for detail grid erosion
      !integer        o_eplt_unit   !Unit number for Hagen plot file

      character*80   o_einp_ext    !generated input file extension
      character*80   o_egrd_ext    !grid summary erosion file extension
      character*80   o_sgrd_ext    !grid subdaily erosion file extension
      character*80   o_erod_ext    !total erosion summary file extension
      character*80   o_emit_ext    !detail grid erosion file extension
      character*80   o_eplt_ext    !hagen plot file extension

      character*1024 o_einp_file   !generated input file name
      character*1024 o_egrd_file   !grid summary erosion file name
      character*1024 o_sgrd_file   !grid subdaily erosion file name
      character*1024 o_erod_file   !total erosion summary file name
      character*1024 o_emit_file   !detail grid erosion file name
      character*1024 o_eplt_file   !hagen plot file name

      character*1024 o_einp_fpath  !generated input file/path name
      character*1024 o_egrd_fpath  !grid summary erosion file/path name
      character*1024 o_sgrd_fpath  !grid subdaily erosion file/path name
      character*1024 o_erod_fpath  !total erosion summary file/path name
      character*1024 o_emit_fpath  !detail grid erosion file/path name
      character*1024 o_eplt_fpath  !hagen plot file/path name


      real min_erosion_awu       !Minimum erosiove wind speed (m/s)
                                 !to evaluate for erosion loss

      integer :: SURF_UPD_FLG              ! erosion surface updating (0 - disabled, 1 - enabled)

      integer :: dt(8)
      character(len=3) :: mstring
      common / datetime / dt, mstring

      save :: /datetime/

!     +++ END SPECIFICATIONS +++

      ! Determine date of Run
      call date_and_time(values=dt)

      ! Determine month of year
      select case (dt(2))
        case (1); mstring = "Jan"
        case (2); mstring = "Feb"
        case (3); mstring = "Mar"
        case (4); mstring = "Apr"
        case (5); mstring = "May"
        case (6); mstring = "Jun"
        case (7); mstring = "Jul"
        case (8); mstring = "Aug"
        case (9); mstring = "Sep"
        case (10); mstring = "Oct"
        case (11); mstring = "Nov"
        case (12); mstring = "Dec"
        case default; mstring = "???"
      end select

      ! Print date of Run
12    format(1x,'Date of tsterode run: ',a3,' ',i2.2,', ',i4,' ',       &
     &          i2.2,':',i2.2,':',i2.2)
      write(6,12) mstring, dt(3), dt(1), dt(5), dt(6), dt(7) 
      write(6,*)

      ! initialize anemometer defaults
      call anemometer_init

      awzypt = 300.0      !Requires yrly average precip. (Hagen's best estimate for us to use)

      mksaeinp%simday = 0 ! 0 means saeinp will not be used to create file.

      min_erosion_awu = 5.0  !default minimum erosive wind speed

      xgdpt = 0         !default grid spacing values if not specified
      ygdpt = 0         !on the commandline

      erod_interval = 0 !do not overide default surface updating interval

      SURF_UPD_FLG = 1  !enable erosion submodel surface updating by default

      have_ifile = .false.
! *** set plot_flag value false in WEPS
      hagen_plot_flag = .false.
      force_plot_flag = .false.
      force_emit_val = 0
      force_debug_flag = -1

      already_read_inputs = 0  !flag to keep from reading inputs more than once

!     Set unit numbers for input and output file devices.
!     (stdin = 5, stdout = 6)
      i_unit = 5         !If -i option is specified, use unit number 50
      o_unit = 6         !stdout

      o_einp_ext = ".einp"   !filename extension for echo'd input data file
      o_egrd_ext = ".egrd"   !filename extension for grid erosion summary output
      o_sgrd_ext = ".sgrd"   !filename extension for grid erosion subdaily output
      o_erod_ext = ".erod"   !filename extension for erosion summary output
      o_emit_ext = ".emit"   !filename extension for grid erosion detail output
      o_eplt_ext = ".eplt"   !filename extension for paramter data plot file output

! Uses the Fortran 2k commandline parsing support.
! There cannot be any space between the option and any arguments,
! e.g. '-i#' is ok but '-i #' is not.
! Any option arguments that have any spaces in them must be quoted,
! e.g. '-i"C:\Program Files"' is ok but '-iC:\Program Files' is not.

      numarg = COMMAND_ARGUMENT_COUNT()  ! Determine number of commandline args

      call GET_COMMAND_ARGUMENT(0,argv,ll,ss) !get name of executing program
      !write(0,*) 'argv ',i,' is: ', trim(argv) ! debug print of arg list
      exe_filepath = trim(argv)

      if (numarg .gt. 0) then
        do 09 i = 1, numarg
           call GET_COMMAND_ARGUMENT(i,argv,ll,ss)  !Fortran 2k compatible call
           !write(0,*) 'argv ',i,' is: ', trim(argv) ! debug print of arg list

           if (argv(1:1) .ne. '-') then   !make sure all options start with '-'
              write(0,*) 'Option ignored, no option flag: ', trim(argv)
              goto 09     !Go get next arg    
           endif

           !command line help prompt
           if ((argv(2:2).eq.'?').or.(argv(2:2).eq.'h')) then
              write(0,*) 'Valid command line options:'
              write(0,*) '-? or -h Display this help screen'
              write(0,*) '-x#      number grid points in x direction'
              write(0,*) '-y#      number grid points in y direction'
              write(0,*) '-t#      surface updating interval (seconds)'
              write(0,*) '-T#      minimum erosive wind speed (m/s)'
              write(0,*) '-u       disable erosion surface updating'
              write(0,*) '-d#      set read input file debug flag'
              write(0,*) '-i"input_filename.in"  specify input filename'

              write(0,*) '-Einp    Echo input to "input_filename.einp"'
              write(0,*) '         datafile'
              write(0,*) '-Erod    Output erosion summary results'
              write(0,*) '         to "input_filename.erod" filename'
              write(0,*) '-Egrd    Output grid summary erosion results'
              write(0,*) '         to "input_filename.egrd" filename'
              write(0,*) '-Esgrd   Output subdaily grid erosion results'
              write(0,*) '         to "input_filename.sgrd" filename'
              write(0,*) '-Emit    Output subdaily erosion results'
              write(0,*) '         to "input_filename.emit" filename'
              write(0,*) '-Eplt    Append to datafile "tsterode.plt"'
              write(0,*) '         in current dir for plotting vars'
              call exit(1)

           else if (argv(2:2) == 't') then !Specify updating interval (min)
             if (len_trim(argv(3:)) == 0) then
                write(0,*) 'missing surface updating interval value'
                call exit(21)
             else
                read(argv(3:),*) erod_interval
                if (erod_interval < 1) then
                  write(0,*) 'Update interval too small (val < 1 sec)'
                  call exit(22)
                else if (erod_interval > 3600*24) then
                  write(0,*) 'Update interval too big (val>3600*24 sec)'
                  call exit(23)
                endif
             endif

           else if (argv(2:2) == 'T') then !Specify minimum erosive wind speed (m/s)
             if (len_trim(argv(3:)) == 0) then
                write(0,*) 'missing minimum erosive wind speed value'
                call exit(25)
             else
                read(argv(3:),*) min_erosion_awu
                if (min_erosion_awu <= 0.0) then
                   write(0,*) 'min erosive wind speed value too small', &
     &                        ' (min_erosion_awu <= 0.0)'
                   call exit(26)
                endif
             endif

           else if (argv(2:2) == 'x') then !Specify # of grid points in x-dir
             if (len_trim(argv(3:)) == 0) then
                write(0,*) 'missing x-dir grid dimension'
                call exit(31)
             else
                read(argv(3:),*) xgdpt
                if (xgdpt < 1) then
                   write(0,*) 'x-dir grid value too small (xgdpt < 1)'
                   call exit(32)
                endif
                if (xgdpt >= mngdpt) then
                   write(0,*) 'xgdpt = ', xgdpt, 'mngdpt-1 = ', mngdpt-1
                   write(0,*) 'x-dir grid value too large'
                   call exit(33)
                endif
             endif
             
           else if (argv(2:2) == 'y') then !Specify # of grid points in y-dir
             if (len_trim(argv(3:)) == 0) then
                write(0,*) 'missing y-dir grid dimension'
                call exit(41)
             else
                read(argv(3:),*) ygdpt
                if (ygdpt < 1) then
                   write(0,*) 'y-dir grid value too small (ygdpt < 1)'
                   call exit(42)
                endif
                if (ygdpt >= mngdpt) then
                   write(0,*) 'ygdpt = ', ygdpt, 'mngdpt-1 = ', mngdpt-1
                   write(0,*) 'y-dir grid value too large'
                   call exit(43)
                endif
             endif

           else if (argv(2:2) == 'u') then !Turn off surface updating
             SURF_UPD_FLG = 0

           else if (argv(2:2) == 'd') then !Specify input file debug flag value
             if (len_trim(argv(3:)) == 0) then
                write(0,*) 'missing debug flag value'
                call exit(51)
             else
                read(argv(3:),*) force_debug_flag
                if (force_debug_flag < 0) then
                   write(0,*) 'debug flag value too small (val < 0)'
                   call exit(52)
                else if (force_debug_flag > 3) then
                   write(0,*) 'debug flag value too great (val > 3)'
                   call exit(53)
                endif
             endif

           else if (argv(2:2) == 'i') then
		     !check if arg option is missing
             if (len_trim(argv(3:)) == 0) then
                   write(0,*) 'missing -i filename option'
                   call exit(61)
             else
                input_filepath = trim(argv(3:))
                !write(0,*) 'input_filepath', trim(input_filepath)

                ! checks and exits if file does not exist
                call fopenk (i_unit, input_filepath, 'old')
                have_ifile = .true.

               !extract input file basename from it's path
               indx = index(trim(input_filepath),'/',back=.true.)
               !cut extension from input filename (if it exists)
               rndx = index(trim(input_filepath),'.',back=.true.) - 1
               if (rndx == 0) then   !No input filename extension found
                  rndx = len_trim(input_filepath)
               endif
               !input file and filepath basenames
               file_bname = trim(input_filepath(indx+1:rndx))
               fpath_bname = trim(input_filepath(:rndx))
               input_filename = trim(input_filepath(indx+1:))
             endif
			 
           else if (argv(2:5) == 'Einp') then
             !write(0,*) '"-Einp" option specified'
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Einp option'
                call exit(71)
             endif
             !create new grid erosion summary output filename from input filename
             o_einp_file = trim(file_bname) //  trim(o_einp_ext)
             o_einp_fpath = trim(fpath_bname)//trim(o_einp_ext)

             call fopenk(o_einp_unit, o_einp_fpath, 'unknown')

           else if (argv(2:5) == 'Eplt') then  !If specified print Hagen's output file
             !write(0,*) '"-Eplt" option specified'
             force_plot_flag = .true.
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Eplt option'
                call exit(81)
             endif
             !create new grid erosion summary output filename from input filename
             o_eplt_file = trim(file_bname) //  trim(o_eplt_ext)
             o_eplt_fpath = trim(fpath_bname)//trim(o_eplt_ext)

 !           call fopenk(o_eplt_unit, o_eplt_fpath, 'unknown')

           else if (argv(2:5) == 'Egrd') then
             !write(0,*) '"-Egrd" option specified'
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Egrd option'
                call exit(91)
             endif
             !create new grid erosion summary output filename from input filename
             o_egrd_file = trim(file_bname) //  trim(o_egrd_ext)
             o_egrd_fpath = trim(fpath_bname)//trim(o_egrd_ext)

             call fopenk(o_egrd_unit, o_egrd_fpath, 'unknown')

           else if (argv(2:6) == 'Esgrd') then
             !write(0,*) '"-Esgrd" option specified'
             if (.not. have_ifile) then
                write(0,*)'Must specify input file before -Esgrd option'
                call exit(101)
             endif
             !create new grid erosion summary output filename from input filename
             o_sgrd_file = trim(file_bname) //  trim(o_sgrd_ext)
             o_sgrd_fpath = trim(fpath_bname)//trim(o_sgrd_ext)

             call fopenk(o_sgrd_unit, o_sgrd_fpath, 'unknown')
		 
           else if (argv(2:5) == 'Emit') then
             !write(0,*) '"-Emit" option specified'
             force_emit_val = 4

             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Emit option'
                call exit(111)
             endif
             !create new grid erosion summary output filename from input filename
             o_emit_file = trim(file_bname) //  trim(o_emit_ext)
             o_emit_fpath = trim(fpath_bname)//trim(o_emit_ext)

             call fopenk(o_emit_unit, o_emit_fpath, 'unknown')
			 
           else if (argv(2:5) == 'Erod') then
             !write(0,*) '"-Erod" option specified'
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Erod option'
                call exit(121)
             endif

             !create new erosion summary output filename from input filename
             o_erod_file = trim(file_bname) //  trim(o_erod_ext)
             o_erod_fpath = trim(fpath_bname) // trim(o_erod_ext)

             call fopenk(o_erod_unit, o_erod_fpath, 'unknown')

           else     !Unknown option ....
             write (0,*) 'Ignoring uknown option: ', trim(argv)
           endif
 09     continue
      else
        input_filename = 'from_stdin'
      endif

      if (((xgdpt > 0) .and. (ygdpt == 0)) .or.                         &
     &    ((xgdpt == 0) .and. (ygdpt > 0))) then
        write(0,*) 'xgdpt = ', xgdpt, 'ygdpt = ', ygdpt
        write(0,*)                                                      &
     &         'Error: Only one grid dimension specified on commandline'
        call exit(131)
      endif

      inquire(unit=o_egrd_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         !write(0,*) 'calling erodin with output unit no: ', o_egrd_unit
         call erodin(i_unit, o_egrd_unit, force_debug_flag, already_read_inputs, subrsurf) !Put copy of input in .egrd file
         already_read_inputs = already_read_inputs + 1
      endif

      inquire(unit=o_einp_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         !write(0,*) 'calling erodin with output unit no: ', o_einp_unit
         call erodin(i_unit, o_einp_unit, force_debug_flag, already_read_inputs, subrsurf) !Echo input to a file
         already_read_inputs = already_read_inputs + 1
      else
         !write(0,*) 'calling erodin with output unit no: ', o_unit
         call erodin(i_unit, o_unit, force_debug_flag, already_read_inputs, subrsurf)  !Doesn't echo input to file
         already_read_inputs = already_read_inputs + 1
      endif

! Check for invalid commandline input values which are dependent
! upon erodin input values.

      if (erod_interval /= 0) then                                  
          if (modulo(SEC_PER_DAY,ntstep*erod_interval) /= 0) then
             write(0,*)                                                      &
     &       'Error: Day not evenly divisible by (ntstep*erod_interval)'
        call exit(141)
        endif 
      endif

! Checking am0efl flag value specified in input file and comparing
! to tsterode commandline options specified.  If the necessary commandline
! option(s) are not specified to match the input file am0efl flag setting(s)
! then the input file flag setting is nullified for that specific option(s),
! e.g. commandline options take precedent over the input file flag setting

      inquire(unit=o_erod_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,0)             !Print summary results
      else
         if (btest(am0efl,0)) then
            write(0,*) 'Input file Specified Summary Results'
            write(0,*) 'No file open to receive them. Option zeroed'
            am0efl = ibclr(am0efl,0)
         endif
      endif
      inquire(unit=o_egrd_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,1)
      else
         if (btest(am0efl,1)) then
            write(0,*) 'Input file Specified Grid Output'
            write(0,*) 'No file open to receive them. Option zeroed'
            am0efl = ibclr(am0efl,1)
         endif
      endif
      inquire(unit=o_emit_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,2)
      else
         if (btest(am0efl,2)) then
            write(0,*) 'Input file Specified Emit Output'
            write(0,*) 'No file open to receive them. Option zeroed'
            am0efl = ibclr(am0efl,2)
         endif
      endif
      inquire(unit=o_sgrd_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,3)
      else
         if (btest(am0efl,3)) then
            write(0,*) 'Input file Specified sgrid Output'
            write(0,*) 'No file open to receive them. Option zeroed'
            am0efl = ibclr(am0efl,3)
         endif
      endif
      !write(0,*) 'am0efl is = ', am0efl
!     inquire(unit=o_eplt_unit, opened=opnd)
!     if (opnd .eqv. .true.) then
!        hagen_plot_flag = .true.
!        am0efl = 1             !Print summary results
!     endif
      if (force_plot_flag .eqv. .true.) then
         hagen_plot_flag = .true.
      else
         hagen_plot_flag = .false.
      endif


!     Set the day, month and year for the "single day event"
!     and get the "Julian Date" for it.

      am0jd = julday(1,1,0001)

!     Initialize erosion code, create grid, etc:
!     (must come after sim field size, & no. subr specified)

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         ! check to see if grid dimensions specified via cmdline args
         if ((xgdpt > 0) .and. (ygdpt > 0)) then
           imax = xgdpt + 1
           jmax = ygdpt + 1
           ix = (amxsim(1,2) - amxsim(1,1)) / xgdpt
           jy = (amxsim(2,2) - amxsim(2,1)) / ygdpt
         else          !use Hagen's grid dimensioning as the default
           call sbgrid
         endif

         ! allocate cellstate array to cover grid
         sum_stat = 0
         allocate(noerod(nsubr), stat=alloc_stat)
         sum_stat = sum_stat + alloc_stat
         allocate(cellstate(0:imax,0:jmax), stat=alloc_stat)
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate enough memory for weps main data arrays'
         end if

         call erodinit( noerod, cellstate )
      endif

!     write (*,*) 'call to erosion '
!     start erosion
      call erosion( min_erosion_awu, SURF_UPD_FLG, subrsurf, noerod, cellstate )

      !tsterode will generate the final grid values and the summarized erosion totals
      if (btest(am0efl,0).or.btest(am0efl,1).or.btest(am0efl,3)) then
       call erodout (o_egrd_unit, o_erod_unit, o_sgrd_unit, input_filename, hagen_plot_flag, cellstate)
      endif

      if (i_unit .ne. 5) then
        close(i_unit)
      endif
      if (o_unit .ne. 6) then
        close(o_unit)
      endif
      close(o_einp_unit)
      close(o_erod_unit)
      close(o_egrd_unit)
      close(o_sgrd_unit)
      close(o_emit_unit)
      !close(o_eplt_unit)

      stop
      end program
