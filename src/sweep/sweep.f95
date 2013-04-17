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

      use sweep_io_mod
      use weps_interface_defs
      use datetime_mod, only: update_system_time, get_systime_string
      use file_io_mod, only: fopenk, luo_sgrd, luo_emit
      use erosion_data_struct_defs, only: subregionsurfacestate, threshold, cellsurfacestate, erod_interval, ntstep, awzypt, subday
      use erosion_data_struct_defs, only: am0eif, am0efl
      use grid_geo_def, only: imax, jmax, ix, jy, xgdpt, ygdpt, amxsim
      use saeinp_mod, only: mksaeinp
      use p1unconv_mod, only: SEC_PER_DAY

!     +++  PURPOSE +++

!     To start a standalone version of the EROSION submodel

!     It calls ERODEIN to read an input file (stdin),
!     calls ERODINIT to initialize grid,
!     runs the EROSION submodel code, and
!     calls ERODOUT to print the generated output (stdout).

!     +++ SUBROUTINES CALLED+++
!     erodin
!     erodinit
!     erosion
!     erodout

!     ++++ LOCAL VARIABLES +++
      integer :: nsubr       ! number of subregions (found from size of subrsurf)
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

      logical        hagen_plot_flag  ! creates sweep.eplt file that is appended to in subsequent runs
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

      character*80   o_einp_ext    !generated input file extension
      character*80   o_egrd_ext    !grid summary erosion file extension
      character*80   o_sgrd_ext    !grid subdaily erosion file extension
      character*80   o_erod_ext    !total erosion summary file extension
      character*80   o_emit_ext    !detail grid erosion file extension

      character*1024 o_einp_file   !generated input file name
      character*1024 o_egrd_file   !grid summary erosion file name
      character*1024 o_sgrd_file   !grid subdaily erosion file name
      character*1024 o_erod_file   !total erosion summary file name
      character*1024 o_emit_file   !detail grid erosion file name

      character*1024 o_einp_fpath  !generated input file/path name
      character*1024 o_egrd_fpath  !grid summary erosion file/path name
      character*1024 o_sgrd_fpath  !grid subdaily erosion file/path name
      character*1024 o_erod_fpath  !total erosion summary file/path name
      character*1024 o_emit_fpath  !detail grid erosion file/path name


      real min_erosion_awu       !Minimum erosiove wind speed (m/s)
                                 !to evaluate for erosion loss

      integer :: SURF_UPD_FLG              ! erosion surface updating (0 - disabled, 1 - enabled)

!     +++ END SPECIFICATIONS +++

      ! Determine date of Run
      call update_system_time

      ! Print date of Run
      write(6,"(1x,'Date of SWEEP run: ',a21)") get_systime_string()
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
      am0efl = 0   ! set flag for creating output file to none

! *** set plot_flag value false in WEPS
      hagen_plot_flag = .false.
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
              write(0,*) '-Emit    Output subdaily erosion results'
              write(0,*) '         to "input_filename.emit" filename'
              write(0,*) '-Esgrd   Output subdaily grid erosion results'
              write(0,*) '         to "input_filename.sgrd" filename'
              write(0,*) '-Eplt    Append to datafile "sweep.eplt"'
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
             am0efl = ibset(am0efl,0)  ! Print summary results

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
             am0efl = ibset(am0efl,1)

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
             am0efl = ibset(am0efl,2)
             ! set module level unit value
             luo_emit = o_emit_unit

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
             am0efl = ibset(am0efl,3)
             ! set module level unit value
             luo_sgrd = o_sgrd_unit

           else if (argv(2:5) == 'Eplt') then  !If specified print Hagen's output file
             !write(0,*) '"-Eplt" option specified'
             hagen_plot_flag = .true.

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

      ! Set based on allocated size of subrsurf
      nsubr = size(subrsurf)

! Check for invalid commandline input values which are dependent
! upon erodin input values.

      if (erod_interval /= 0) then                                  
          if (modulo(SEC_PER_DAY,ntstep*erod_interval) /= 0) then
             write(0,*)                                                      &
     &       'Error: Day not evenly divisible by (ntstep*erod_interval)'
        call exit(141)
        endif 
      endif

      ! set file output flag
      ! this in no longer set in input file.
      am0efl = 0
      inquire(unit=o_erod_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,0)             !Print summary results
      endif
      inquire(unit=o_egrd_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,1)
      endif
      inquire(unit=o_emit_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,2)
      endif
      inquire(unit=o_sgrd_unit, opened=opnd)
      if (opnd .eqv. .true.) then
         am0efl = ibset(am0efl,3)
      endif
      !write(0,*) 'am0efl is = ', am0efl

!     Initialize erosion code, create grid, etc:
!     (must come after sim field size, & no. subr specified)

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         ! check to see if grid dimensions specified via cmdline args
         if ((xgdpt > 0) .and. (ygdpt > 0)) then
           imax = xgdpt + 1
           jmax = ygdpt + 1
           ix = (amxsim(2)%x - amxsim(1)%x) / xgdpt
           jy = (amxsim(2)%y - amxsim(1)%y) / ygdpt
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

      !sweep will generate the final grid values and the summarized erosion totals
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

      stop
      end program
