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

      use sweep_io_mod, only: erodin, erodout
      use sweep_io_xml_mod, only: nsubr
      use datetime_mod, only: update_system_time, get_systime_string, julday
      use file_io_mod, only: fopenk, luo_erod, luo_egrd, luo_emit, luo_sgrd
      use f2kcli, only: COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT
      use grid_mod, only: sbgrid, xgdpt, ygdpt, gridfile
      use erosion_mod, only: erosion, erodinit
      use erosion_data_struct_defs, only: in_sweep, subregionsurfacestate, threshold, cellsurfacestate, erod_interval, &
                                          ntstep, am0eif, am0efl, subrsurf, cellstate
      use barriers_mod, only: minht_barriers
      use wind_mod, only: anemometer_init
      use sae_in_out_mod, only: mksaeinp, mksaeout, in_weps, saeinp, infilebase, sweepfile
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
      character(len=21) :: rundatetime
      type(threshold), dimension(:), allocatable :: noerod                 ! report values to show which factors prevented erosion

      integer :: alloc_stat
      character*1024 exe_pathfile    ! comandline full path and exe file name
      character*1024 input_pathfile  ! commandline argument with full path and file name
      integer i_unit
      integer o_unit

      character*1024 argv      !For Fortran 2k commandline parsing
      integer        i
      integer        numarg
      integer        ll, ss
      logical        input_to_new_format
 
      logical        have_ifile
      integer        indx, rndx

      logical        hagen_plot_flag  ! creates sweep.eplt file that is appended to in subsequent runs
      integer        force_emit_val   !ensure that commandline overrides input file settings
      integer        force_debug_flag !ensure that commandline overrides input file setting

      character*1024 file_bname
      character*1024 fpath_bname
      character*1024 input_filename

      integer        o_egrd_unit   !Unit number for grid summary erosion
      integer        o_sgrd_unit   !Unit number for grid subdaily erosion
      integer        o_erod_unit   !Unit number for total erosion
      integer        o_emit_unit   !Unit number for detail grid erosion

      character*80   o_egrd_ext    !grid summary erosion file extension
      character*80   o_sgrd_ext    !grid subdaily erosion file extension
      character*80   o_erod_ext    !total erosion summary file extension
      character*80   o_emit_ext    !detail grid erosion file extension

      character*1024 o_egrd_fpath  !grid summary erosion file/path name
      character*1024 o_sgrd_fpath  !grid subdaily erosion file/path name
      character*1024 o_erod_fpath  !total erosion summary file/path name
      character*1024 o_emit_fpath  !detail grid erosion file/path name


      real min_erosion_awu       !Minimum erosiove wind speed (m/s)
                                 !to evaluate for erosion loss

      integer :: SURF_UPD_FLG    ! erosion surface updating (0 - disabled, 1 - enabled)

!     +++ END SPECIFICATIONS +++
      ! indicates running stand alone erosion
      in_sweep = .true.

      ! do not write updated input files (default)
      input_to_new_format = .false.

      ! Determine date of Run
      call update_system_time

      ! Print date of Run
      rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
      write(6,"(1x,'Date of SWEEP run: ',a21)")  rundatetime
      write(6,*)

      ! initialize anemometer defaults
      call anemometer_init

      mksaeinp%simday = 0 ! 0 means saeinp will use path to sweep input file to convert from old to new format
      mksaeinp%jday = julday( 1, 1, 1 )

      ! set a calendar day. Erosion output report this day so set to match previous verions of SWEEP
      mksaeout%jday = julday( 1, 1, 1 )

      min_erosion_awu = 5.0  !default minimum erosive wind speed

      erod_interval = 0 !do not overide default surface updating interval

      SURF_UPD_FLG = 1  !enable erosion submodel surface updating by default

      have_ifile = .false.
      am0efl = 0   ! set flag for creating output file to none

      hagen_plot_flag = .false.
      force_emit_val = 0
      force_debug_flag = -1

!     Set unit numbers for input and output file devices.
!     (stdin = 5, stdout = 6)
      i_unit = 5         !If -i option is specified, use unit number 50
      o_unit = 6         !stdout

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
      exe_pathfile = trim(argv)

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
             ! check if arg option is missing
             if (len_trim(argv(3:)) == 0) then
                   write(0,*) 'missing -i filename option'
                   call exit(61)
             else
                input_pathfile = trim(argv(3:))
                !write(0,*) 'input_pathfile', trim(input_pathfile)

                ! checks and exits if file does not exist
                call fopenk (i_unit, input_pathfile, 'old')
                have_ifile = .true.

               !extract input file basename from it's path
               indx = index(trim(input_pathfile),'/',back=.true.)
               !cut extension from input filename (if it exists)
               rndx = index(trim(input_pathfile),'.',back=.true.)
               if (rndx == 0) then   !No input filename extension found
                  rndx = len_trim(input_pathfile) + 1
               endif
               !input file and filepath basenames
               file_bname = trim(input_pathfile(indx+1:rndx-1))
               mksaeinp%fullpath = trim(input_pathfile(:indx))
               fpath_bname = trim(input_pathfile(:rndx-1))
               input_filename = trim(input_pathfile(indx+1:))
             endif
			 
           else if (argv(2:5) == 'Einp') then
             !write(0,*) '"-Einp" option specified'
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Einp option'
                call exit(71)
             endif

             ! Using old input file, write out new format input files
             input_to_new_format = .true.

           else if (argv(2:5) == 'Erod') then
             !write(0,*) '"-Erod" option specified'
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Erod option'
                call exit(121)
             endif

             !create new erosion summary output filename from input filename
             o_erod_fpath = trim(fpath_bname) // trim(o_erod_ext)

             call fopenk(o_erod_unit, o_erod_fpath, 'unknown')
             am0efl = ibset(am0efl,0)  ! Print summary results
             ! set module level unit value
             luo_erod = o_erod_unit

           else if (argv(2:5) == 'Egrd') then
             !write(0,*) '"-Egrd" option specified'
             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Egrd option'
                call exit(91)
             endif
             !create new grid erosion summary output filename from input filename
             o_egrd_fpath = trim(fpath_bname)//trim(o_egrd_ext)

             call fopenk(o_egrd_unit, o_egrd_fpath, 'unknown')
             am0efl = ibset(am0efl,1)
             ! set module level unit value
             luo_egrd = o_egrd_unit

           else if (argv(2:5) == 'Emit') then
             !write(0,*) '"-Emit" option specified'
             force_emit_val = 4

             if (.not. have_ifile) then
                write(0,*) 'Must specify input file before -Emit option'
                call exit(111)
             endif
             !create new grid erosion summary output filename from input filename
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
             o_sgrd_fpath = trim(fpath_bname)//trim(o_sgrd_ext)

             call fopenk(o_sgrd_unit, o_sgrd_fpath, 'unknown')
             am0efl = ibset(am0efl,3)
             ! set module level unit value
             luo_sgrd = o_sgrd_unit

           else if (argv(2:5) == 'Eplt') then  !If specified print Hagen's output file
             !write(0,*) '"-Eplt" option specified'
             ! if configuration file exists, sets this flag to .true.
             inquire (FILE='sweepplot.cfg',EXIST=hagen_plot_flag)
             if( .not. hagen_plot_flag ) then
                write(0,*) '-Eplt option ignored, sweepplot.cfg file not found.'
             end if

           else     !Unknown option ....
             write (0,*) 'Ignoring uknown option: ', trim(argv)
           endif
 09     continue
      else
        input_filename = 'from_stdin'
      endif

      ! transfer name into erosion via mksaeout
      mksaeout%fullpath = trim(input_filename)
      ! set output flag
      in_weps = .false.

      if (((xgdpt > 0) .and. (ygdpt == 0)) .or.                         &
     &    ((xgdpt == 0) .and. (ygdpt > 0))) then
        write(0,*) 'xgdpt = ', xgdpt, 'ygdpt = ', ygdpt
        write(0,*)                                                      &
     &         'Error: Only one grid dimension specified on commandline'
        call exit(131)
      endif

      ! reading input file(s)
      if( erodin(input_pathfile, i_unit, force_debug_flag, hagen_plot_flag) ) then
        ! xml input file, cancel old to xml conversion
        input_to_new_format = .false.
      end if
 
      ! Check for invalid commandline input values which are dependent
      ! upon erodin input values.

      if (erod_interval /= 0) then
          i = SEC_PER_DAY
          if (modulo(i,ntstep*erod_interval) /= 0) then
             write(0,*)                                                      &
     &       'Error: Day not evenly divisible by (ntstep*erod_interval)'
        call exit(141)
        endif 
      endif

      ! Initialize erosion code, create grid, etc:
      ! (must come after sim field size, & no. subr specified)

      ! Grid is initialized at least once.
      if (am0eif .eqv. .true.) then
         if( .not. allocated(cellstate) ) then
           ! did not read grid file, so create cellstate array
           call sbgrid( minht_barriers() )
         end if

         ! allocate noerod array for subregions
         allocate(noerod(nsubr), stat=alloc_stat)
         if( alloc_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate enough memory for noerod data array'
         end if

         call erodinit( noerod )

         ! read in old single subregion file. Write updated input files
         if( input_to_new_format ) then
           infilebase = trim(file_bname)
           sweepfile = 'erod.sweep' 
           gridfile = 'erod.grdx'
           subrsurf(1,1)%tinfil = trim(file_bname)
           subrsurf(1,1)%sinfil = trim(file_bname)
           call saeinp( 1, nsubr )
         end if

      endif

!     write (*,*) 'call to erosion '
!     start erosion
      call erosion( min_erosion_awu, SURF_UPD_FLG, 1, noerod, cellstate )

      ! configured summary info
      call erodout (hagen_plot_flag)

      ! if (i_unit .ne. 5) then
      !   close(i_unit)
      ! endif
      if (o_unit .ne. 6) then
        close(o_unit)
      endif
      close(o_erod_unit)
      !close(o_egrd_unit)
      !close(o_sgrd_unit)
      !close(o_emit_unit)

      stop
      end program
