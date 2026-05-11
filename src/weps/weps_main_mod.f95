!$Author$
!$Date$
!$Revision$
!$HeadURL$

module weps_main_mod

    logical :: old_run_file
    character*512 :: clifil  ! climate file name
    character*512 :: runfil  ! run file name
    character*512 :: subfil  ! subdaily wind file name
    character*512 :: winfil  ! wind file name
    character*256 :: usrnam  ! user name
    character*256 :: farmid  ! Farm identifier
    character*256 :: tractid ! Tract identifier
    character*256 :: fieldid ! Field identifier
    character*256 :: siteid  ! Site identifier
    character*256 :: runtype ! cycle vs date
    character*256 :: cliflag ! interface cligen flag
    character*256 :: climethod ! method used in generation
    character*256 :: clilatitude ! cligen latitude
    character*256 :: clilongitude ! cligen longitude
    character*256 :: clistateid ! cligen state id
    character*256 :: clistationnum ! cligen station number
    character*256 :: clistationname ! cligen station name
    character*256 :: clielevation ! cligen elevation
    character*256 :: winflag ! interface windgen flag
    character*256 :: winmethod ! method used in generation
    character*256 :: winlatitude ! windgen latitude
    character*256 :: winlongitude ! windgen longitude
    character*256 :: winstationnum ! windgen station number
    character*256 :: wincountry ! windgen country
    character*256 :: winstate ! windgen state
    character*256 :: winstationname ! windgen station name

    integer :: run_rot_cycles ! number of rotation cycles

    integer :: id     ! initial simulation day of month
    integer :: im     ! initial simulation month of year
    integer :: iy     ! initial simulation year
    integer :: ld     ! final (last) simulation day of month
    integer :: lm     ! final (last) simulation month of year
    integer :: ly     ! final (last) simulation year

    character*512 :: rootp  ! the root path from which the weps command was started.

    integer :: ijday   ! This variable contains the initial julian day of the simulation run.
    integer :: ljday   ! This variable contains the last julian day of the simulation run.
    integer :: maxper  ! The maximum number of years in a rotation cycle of all subregions.
                       ! All subregion rotation cycle period lengths (in years) must be a factor
                       ! in this value.  For example, 3 subregions with individual rotation
                       ! periods of 2, 3, and 4 years each would have a "maxper" value of 12
                       ! years.  Note that each of the individual subregion rotation periods can
                       ! divide evenly into the "maxper" value.
    integer :: longest_mgt_rotation ! longest mgt rotation file in all subregions

    ! all values below are subregion specific (dimensioned accordingly)
    logical, dimension(:), allocatable :: am0ifl  ! flag to run initialization of submodels
                                                  ! .true. means initialization will be run
    logical, dimension(:), allocatable :: init_loop    ! .true. indicates the simulation is in the initialization loop
    logical, dimension(:), allocatable :: calib_loop   ! .true. indicates the simulation is in the calibration loop
    logical, dimension(:), allocatable :: report_loop  ! .true. indicates the simulation is in the report loop

    integer, dimension(:), allocatable :: calib_cycle  ! identify the calibration "cycle" we are in
                                                       ! currently set and updated in "main/weps.f95"
    integer, dimension(:), allocatable :: prev_calib_cycle
    logical, dimension(:), allocatable :: calib_done   ! flag to identify when we are "done" with calibration
                                                       ! .true. then we are done with calibration

  contains

    subroutine wepsinit( nsubr )

      ! Initializes variables in common blocks

      use erosion_data_struct_defs, only: in_sweep, erod_interval, am0eif
      use datetime_mod, only: psim_date

      ! + + + ARGUMENT DECLARATIONS + + +
      integer :: nsubr  ! number of subregions in simulation

      ! + + + LOCAL VARIABLES + + +
      integer :: sum_stat   ! error return value from allocation
      integer :: alloc_stat ! summation of error return values from allocations
      integer :: isr        ! loop index

      in_sweep = .false. ! indicates not running stand alone erosion
      erod_interval = 0  ! default value for updating eroding soil surface
                         ! (currently only used in standalone erosion submodel)
      maxper = 1
      longest_mgt_rotation = 1

      ! set initialization flags
      am0eif = .true.

      ! allocate subregion variables
      sum_stat = 0
      allocate(psim_date(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(am0ifl(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(init_loop(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(calib_loop(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(report_loop(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(calib_cycle(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(prev_calib_cycle(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(calib_done(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! initialize subregion variables
      do isr = 1, nsubr
          am0ifl(isr) = .true.
          init_loop(isr) = .false.
          calib_loop(isr) = .false.
          report_loop(isr) = .false.
          calib_cycle(isr) = 0
          prev_calib_cycle(isr) = -1
          calib_done(isr) = .false.
      end do

      return
    end subroutine wepsinit

    subroutine cmdline

!     + + + PURPOSE + + +
!     This subroutine perforns cmdline arg/opt processing

!     author: Larry Wagner

!     EDIT History
!     06-Nov-03  LEW  Extracted cmdline processing code from "input.for"

!     + + + KEY WORDS + + +
!     WEPS, cligen, windgen

!     + + + GLOBAL COMMON BLOCKS + + +
      use weps_cmdline_parms  !We use all the cmdline parms here

      use datetime_mod, only: julday, ckdate
      use f2kcli, only: COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT
      use climate_input_mod, only: wind_max_value, wind_max_flag
      use grid_mod, only: xgdpt, ygdpt

!     + + + LOCAL VARIABLES + + +
      integer       ln

      integer cmd_iarg      ! Temporary var for retrieving integer cmdline args
      real    cmd_rarg      ! Temporary var for retrieving real cmdline args

      character*512 argv    ! For Fortran 2k commandline parsing
      integer       i
      integer       numarg
      integer       ll,ss
      integer       day, mon, year

      integer       arg_len ! number of characters in argv

!     + + + LOCAL DEFINITIONS + + +

!   i         - Generic loop counter.
!   id,im,iy  - The day, month, and year
!   lchar     - This variable holds the character position in a string
!               so to ignore leading blanks in that string.
!   arglen    - This variable holds size of the specified cmdline argument
!   envlen    - This variable holds size of the specified env variable string

!     argv    - a specified arg from the list of command line arguments.
!   numarg    - number of arguments passed on the command line.

!     + + + FUNCTIONS CALLED + + +
!     julday  -  This function determines the julian day given day,
!                month, and year.
!
!     + + + DATA INITIALIZATIONS + + +

!     + + + INPUT FORMATS + + +

!     + + + OUTPUT FORMATS + + +

!     + + + HOW TO EDIT + + +
!     - define module variable
!     Section of code, edit
!     1 - set default value
!       - choose a command line switch symbol (no duplicates allowed)
!     2 - add command line help prompt
!     3 - add default options setting output
!     4 - add code to read, error check and assign value 

!     + + + END SPECIFICATIONS + + +

! ----Default argument options if not specified

      rootp = './'        !default WEPS root directory
                          !Note that all paths MUST end with a "/" for now.
      wc_type = 4         !default soil ifc file type (use Rawls texture for full properties)
      ifc_format = 0      !default soil ifc file format type (uses IFC specified slope value)
      hb_freq = 1         !default is to update each year of simulation (including initialization)
      report_info = 1     !default report information printing (0=off, 1=on)
      report_debug = 0    !default report debug printing (0=off, 1=on)
      saeinp_daysim = 0   !0 value skips creation of a stand alone erosion input file.
      saeinp_jday = 0     !0 value skips creation of a stand alone erosion input file.
      saeinp_all = 0      !0 value skips creation of stand alone erosion input files unless specific day set.
      init_cycle = 1      !default is to do one initialization cycle before reporting
      run_erosion = 1     !default is to run erosion submodel (0=no_erosion_submodel,1=WEPS,2=WEPP,3=WEPS+WEPP)
      calibrate_crops = 0 !default is to NOT run in crop calibration mode
      calibrate_rotcycles = 0 ! default is to run full number of rotation cycles during calibration
      cook_yield = 1      !default is to use partitioned Yield/Residue ratio
!      cook_yield = 0      !default is to grow using full partitioning
      growth_stress = 3   !default is to turn on all stress values
      water_stress_max = 0.0 ! default is to allow full stress
      layer_scale = 2     !default is set to 2 mm
      layer_infla = 25    !default is set to 25 percent increase per layer
      layer_weighting = 0 !default is set to use the no weighting ".5" method
      puddle_warm = 1     !default is set to use warm puddling
      winter_ann_root = 1 !default is set to use no delay fall root growth

      wind_max_value = 0.0 !default is set to not use max wind speed capping
      wind_max_flag = 0   !default is set to not use max wind speed capping
      wepp_hydro = 0      !default is set to use Darcy method for hydrology
      soil_cond = 1       !default is set to output soil conditioning index file
      upgm_growth = 0     !default is set to not use upgm growth for WEPS crops
      resurf_roots = 1    !default is set to resurface buried roots (process 26)
      calc_confidence = 0 !default is set to not calculate confidence intervals.
      frac_frst_mass_lost = 0.0 !default is set to 0.0 fraction loss of young leaf freeze damaged mass
      transpiration_depth = 0 !default is set to not set transpiration depth to more than root depth.

      make_runxml = 0     ! default is set to not create weps.runx/erod.grdx from weps.run file

      xgdpt = 0           ! default xgdpt = 0 uses Hagen's grid spacings
      ygdpt = 0           ! default ygdpt = 0 uses Hagen's grid spacings
                          ! both must be set to use alternate spacing

! ----Determine Number of Command Line Arguments.

! Will now use the Fortran 2k commandline parsing support - LEW
! There cannot be any space between the option and any arguments,
! e.g. '-i#' is ok but '-i #' is not.
! Any option arguments that have any spaces in them must be quoted,
! e.g. '-i"C:\Program Files"' is ok but '-iC:\Program Files' is not.

      ! Always print out the full  command line
      call GET_COMMAND(argv, ll, ss)
      write(6,'(a)') 'WEPS cmdline: ',trim(argv)

      ! 'report_info' not set yet, so we can't control it here (default value is 1)
!      i = 0
!      call GET_COMMAND_ARGUMENT(i,argv,ll,ss)
!      if (report_info >= 1) then
!        write(6,*) 'argv0 ', i, ' is: ', trim(argv)
!      end if
      numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call
!      if (report_info >= 1) then
!        write(6,*) 'numarg: ', numarg
!      end if

      if (numarg .gt. 0) then
         do 09 i = 1, numarg
          call GET_COMMAND_ARGUMENT(i,argv,ll,ss)  !Fortran 2k compatible call
!          if (report_info >= 1) then
!            write(6,*) 'argv ',i,' is: ', trim(argv)
!            !write(6,*) 'll ', ll, 'ss ', ss
!          end if

          if(argv(1:1) .ne. '-') then   !make sure all options start with '-'

            ! Provide backwards compatibility to older WEPS versions for now
            if (numarg .eq. 1) then 
               ! Check to see if trailing '/' is there - LEW
               ln = len_trim(argv(1:))
               if (ln .ne. 0) then
                  rootp = trim(argv(1:))
                  if (rootp(ln:ln) .ne. '/') then
                     rootp = trim(argv(1:)) // '/'   !add trailing '/' character
                  endif
               else
                  write(*,*)                                            &
     &                'Ignoring invalid WEPS root dir: ', trim(argv)
               endif
            endif

            write(*,*) 'Command line option ignored, no option flag (-): ', argv
            goto 9  !Go get next arg
          endif

          ! check length to allow meaningful error message
          arg_len = len_trim(argv)
          if( arg_len .lt. 2 ) then
            write(*,*) 'Option flag with no argument skipped'
          end if

          !command line help prompt
          if( (argv(2:2).eq.'?').or.(argv(2:2).eq.'h')) then
              write(*,*) 'Valid command line options:'
              write(*,*) '-?  Display this help screen'
              write(*,*) '-h  Display this help screen'
              write(*,*)

              write(*,*)                                                &
     &'-c  Soil Conditioning Index output'
              write(*,*) '    0 = no output'
              write(*,*)                                                &
     &'    1 = create soil-conditioning.out file (default)'

              write(*,*)                                                &
     &'-C  WEPS crop calibration mode'
              write(*,*) '    0 = Do not run crop calibration (default)'
              write(*,*) '    # = Run crop calibration # interation max'

              write(*,*)                                                &
     &'-E  Simulate \"erosion\" in WEPS run'
              write(*,*) '    0 = Do not run any erosion submodel'
              write(*,*) '    1 = Run WEPS erosion submodel (default)'
              write(*,*) '    2 = Run only WEPP erosion submodel'
              write(*,*) '    3 = Run WEPS and WEPP erosion submodels'

              write(*,*)                                                &
     &'-e  Create stand alone erosion output files, all erosion events'
              write(*,*) '    0 = Create -O, -o spec. output (default)'
              write(*,*) '    1 = Create SWEEP input files'

              write(*,*)                                                &
     &'-f  Specify leaf freeze damaged mass loss fraction'
              write(*,*)                                                &
     &'    Specify -f0.85 to make 85% of freeze damaged mass disappear'

              write(*,*)                                                &
     &'-g  Application of growth stress functions'
              write(*,*) '    0 = no growth stress function applied'
              write(*,*) '    1 = water stress function applied'
              write(*,*) '    2 = temperature stress function applied'
              write(*,*) '    3 = both stress functions applied'

              write(*,*) &
      '-G  Maximum level of water stress allowed'
              write(*,*) '-G0.00 allows maximum water stress to occur'
              write(*,*) '-G1.00 does not allow any water stress'

       ! Added to control informational messages sent to the screen
       ! Provided to assist in debugging the code without all the screen clutter
       ! hb_freq = 1 is default as this provides updates from model on a yearly basis
       ! GUI uses info sent to stdout to relay simulation progress to user ('H'eartbeat info)
       ! Thu Jul 18 09:23:31 MDT 2019 - LEW
              write(*,*)                                                &
     &'-H  WEPS "heartbeat" frequency (reporting interval used by GUI)'
              write(*,*)                                                &
     &'    0 = no yearly update messages sent to stdout'
              write(*,*)                                                &
     &'    1 = yearly interval update sent to stdout (default)'
              write(*,*)                                                &
     &'    2 = every other yearly interval update sent to stdout'
              write(*,*)                                                &
     &'    50 = every 50 year interval update sent to stdout'
              write(*,*)                                                &
     &'    etc'
       ! Added to control informational messages sent to the screen
       ! Thu Jul 18 09:23:31 MDT 2019 - LEW
              write(*,*)                                                &
     &'-i  WEPS informational messages dumped to screen'
              write(*,*)                                                &
     &'    0 = no informational messages sent to screen'
              write(*,*)                                                &
     &'    1 = 1st level informational messages sent to screen (default)'
              write(*,*)                                                &
     &'    2 = 1st and 2nd level informational messages sent to screen'
              write(*,*)                                                &
     &'    3 = 1st - 3rd level informational messages sent to screen'

       ! Initialization for multiple subregions should be a multiple
       ! of the longest mgt rotation - need to ensure this is the case - LEW
              write(*,*)                                                &
     &'-I  Specify if initialization is done and if so, the # loops'
              write(*,*) '    0 = No initialization'
              write(*,*) '    1 = Runs one management cycle (default)'
              write(*,*) '    2 = Runs x management cycles'

              write(*,*)                                                &
     &'-L  Specify soil layer thickness to scale layer splitting (mm)'
              write(*,*)                                                &
     &'    Specify -L2 for layer splitting to use 2 mm (no decimals)'

              write(*,*)                                                &
     &'-l  Specify rate of soil layer thickness increase with depth for'
              write(*,*)                                                &
     &'    layer splitting in percent increase of layer thickness'
              write(*,*)                                                &
     &'    Specify -l50 to inc. 50 percent for each layer (no decimals)'

              write(*,*)                                                &
     &'-n  Write new format weps.runx and erod.grdx files'
              write(*,*) '    0 = Do not create weps.runx/erod.grdx from weps.run'
              write(*,*) '    1 = Create weps.runx/erod.grdx from weps.run'

              write(*,*)                                                &
     &'-O  Generate stand alone erosion input file on simulation day'
              write(*,*)                                                &
     &'    Specify -O2932 to output file on simulation day 2932'

              write(*,*)                                                &
     &'-o  Generate stand alone erosion input file on DD/MM/YY'
              write(*,*)                                                &
     &'    Specify -o020901 to output file on day 2 month 9 year 1'
              write(*,*)                                                &
     &'    Day and month must be 2 digits, Year can be 1 to 4 digits'

              write(*,*)                                                &
     &'-p  Select soil puddling with saturation all above freezing'
              write(*,*) '    0 = disable'
              write(*,*) '    1 = enable'

              write(*,*)                                                &
     &'-P  Specify path to WEPS project run directory'
              write(*,*)                                                &
     &'    Must be specified if other command line switches are used'
              write(*,*)                                                &
     &'    Specifying only the path without the \"-P\" option'
              write(*,*)                                                &
     &'    only works if no other command line switches are specified'
              write(*,*)                                                &
     &'    e.g: \"weps path_to_weps.run_file\"'

              write(*,*)                                                &
     &'-r  Select winter annual root depth growth option'
              write(*,*) '    0 = depth grows at same rate as height'
              write(*,*) '    1 = depth grows with fall heat units'

              write(*,*)                                                &
     &'-R  WEPS debug messages dumped to screen'
              write(*,*)                                                &
     &'    0 = no debug messages sent to screen'
              write(*,*)                                                &
     &'    1 = 1st level debug messages sent to screen'
              write(*,*)                                                &
     &'    2 = 1st and 2nd level debug messages sent to screen'
              write(*,*)                                                &
     &'    3 = 1st, 2nd, and 3rd level debug messages sent to screen'

              write(*,*) '-s  Specify soil ifc file input format type'
              write(*,*) '    0 = new format (default)'
              write(*,*) '    1 = old format (slope set in weps.run)'

              write(*,*)                                                &
     &'-S  Vary type of value input for 1/3 bar, 15 bar water'
              write(*,*) '    0 = 1/3bar(vol) 15bar(vol)'
              write(*,*) '    1 = 1/3bar(vol) 15bar(grav)'
              write(*,*) '    2 = 1/3bar(grav) 15bar(grav)'
              write(*,*) '    3 = use texture based calc'
              write(*,*)                                                &
     &'    4 = use Rawls texture for full properties (default)'
              write(*,*)                                                &
     &'        Override 1/3bar, 15bar, bulk density w/ texture estimate'

              write(*,*)                                                &
     &'-t  Confidence Interval on Rotation Mean Annual Erosion'
              write(*,*) '    0 = no confidence interval calc (default)'
              write(*,*) '    1 = confidence interval reported'
              write(*,*) '    2 = used to limit run length (not implem)'

              write(*,*)                                                &
     &'-T  Deep Furrow Effect on Transpiration Depth'
              write(*,*) '    0 = no deep furrow effect (default)'
              write(*,*) '    1 = deep furrow affects transpiration'

              write(*,*)                                                &
     &'-u  Resurfacing buried roots'
              write(*,*) '    0 = no resurfacing of buried roots'
              write(*,*) '    1 = resurface buried roots (default)'

              write(*,*)                                                &
     &'-U  UPGM growth for all WEPS crops'
              write(*,*) '    0 = no UPGM growth for WEPS crops (default)'
              write(*,*) '    1 = grow WEPS crops using UPGM'

              write(*,*)                                                &
     &'-w  Specify method of weighting for layer conductivity and flow '
              write(*,*) '    0 = arithmetic mean 0.5 method (default)'
              write(*,*) '    1 = layer thickness porportional weighted'
              write(*,*) '    2 = internodal method, darcian mean'

              write(*,*)                                                &
     &'-W  Specify hydrology calculation method used '
              write(*,*) '    0 = darcian flow (default)'
              write(*,*) '    1 = Green-Ampt infil., simple drainage'
              write(*,*) '    2 = Green-Ampt infil., WEPP runoff'

              write(*,*)                                                &
     &'-x  Specify number grid cells in x direction'
              write(*,*)                                                &
     &'    Specify -x75 for 75 grid cells in the x direction'

              write(*,*)                                                &
     &'-X  Specify maximum wind speed cap (m/s)'
              write(*,*)                                                &
     &'    Specify -X25.0 to limit input wind speeds to a max of 25 m/s'

              write(*,*)                                                &
     &'-y  Specify number grid cells in y direction'
              write(*,*)                                                &
     &'    Specify -y75 for 75 grid cells in the y direction'

              write(*,*)                                                &
     &'-Y  Optional functional Yield/residue ratio'
              write(*,*) '    0 = Use full staged biomass partitioning'
              write(*,*) '    1 = Use partitioned Yield/residue ratio', &
     &                   ' (default)'

              write(*,*)                                                &
     &'-Z  Specify maximum number of cycles to run while calibrating'
              write(*,*) '    0 = Use all cycles (default)'

              write(*,*) ''
              write(*,*) 'Default options are set to:'
 2600         format(' -c',i1,                                          &
     &               ' -C',i1,' -E',i1,' -e',i1,                        &
     &               ' -f',f4.2,                                        &
     &               ' -g',i1,' -G',f4.2,' -H',i1,' -i',i1,' -I',i1,    &
     &               ' -L',i1,' -l',i2,' -n',i1,' -O(no file)',' -o(no file)', &
     &               ' -p',i1,' -P',a,                                  &
     &               ' -r',i1,' -R',i1,' -S',i1,                        &
     &               ' -s',i1,' -T', i1,' -t', i1,                      &
     &               ' -u', i1,' -U',i1,' -w',i1,' -W',i1,' -x',i1,     &
    &               ' -X',f4.1,' -y',i1,' -Y',i1,' -Z',i1)

              write(0,2600) soil_cond,                                  &
     &         calibrate_crops, run_erosion, saeinp_all,                &
     &         frac_frst_mass_lost, growth_stress,                      &
     &          water_stress_max, hb_freq, report_info, init_cycle,     &
     &         layer_scale, layer_infla, make_runxml,                   &
     &         puddle_warm, trim(rootp),                                &
     &         winter_ann_root, report_debug, wc_type,                  &
     &         ifc_format, transpiration_depth, calc_confidence,        &
     &         resurf_roots, upgm_growth, layer_weighting, wepp_hydro, xgdpt, &
     &         wind_max_value, ygdpt, cook_yield, calibrate_rotcycles

              call exit(1)

          !specify soil-conditioning flag
          else if(argv(2:2) .eq. 'c') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &         'Warning: Ignored invalid SCI-energy option: ', trim(argv)
            else
              soil_cond = cmd_iarg
            endif

          !specify calibrate_crops flag
          else if(argv(2:2) .eq. 'C') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 3) .and. (cmd_iarg .ne. 0) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid calibrate_crops option: ', trim(argv)
            else
              calibrate_crops = cmd_iarg
            endif

          !specify run_erosion flag
          else if(argv(2:2) .eq. 'E') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 3) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid run_erosion option: ', trim(argv)
            else
              run_erosion = cmd_iarg
            endif

          !specify stand alone input files for every erosion event flag
          else if(argv(2:2) .eq. 'e') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid saeinp_all option: ', trim(argv)
            else
              saeinp_all = cmd_iarg
            endif

          !specify young leaf freeze damaged mass loss fraction
          else if(argv(2:2) .eq. 'f') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_rarg
            if( (cmd_rarg .lt. 0.0) .or. (cmd_rarg .gt. 1.0) ) then
              write(*,*)                                                &
     &       'Warning: Ignored invalid freeze mass loss value: ', trim(argv)
            else
              frac_frst_mass_lost = cmd_rarg
            endif

          !specify growth_stress flag
          else if(argv(2:2) .eq. 'g') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 3) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid growth_stress option: ', trim(argv)
            else
              growth_stress = cmd_iarg
            endif

          !specify maximum water stress value (assumes water stress is on)
          else if(argv(2:2) .eq. 'G') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_rarg
            if( (cmd_rarg .lt. 0.0) .or. (cmd_rarg .gt. 1.0) ) then
              write(*,*)                                                &
     &       'Warning: Ignored invalid water stress maximum value: ', trim(argv)
            else
              water_stress_max = cmd_rarg
            endif

          ! specify informational (heartbeat frequency) report intervals (to stdout)
          else if(argv(2:2) .eq. 'H') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid info report option: ', trim(argv)
            else
              hb_freq = cmd_iarg
            endif

          ! specify informational reporting options (to stdout)
          else if(argv(2:2) .eq. 'i') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 3) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid info report option: ', trim(argv)
            else
              report_info = cmd_iarg
            endif

          !specify initialization cycle setting
          else if(argv(2:2) .eq. 'I') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid init_cycle option: ', trim(argv)
            else
              init_cycle = cmd_iarg
            endif

          !specify layer scale setting
          else if(argv(2:2) .eq. 'L') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .le. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid layer_scale option: ', trim(argv)
            else
              layer_scale = cmd_iarg
            endif

          !specify layer inflation setting
          else if(argv(2:2) .eq. 'l') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid layer_infla option: ', trim(argv)
            else
              layer_infla = cmd_iarg
            endif

          ! specify report debug options
          else if(argv(2:2) .eq. 'n') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid make_runxml option: ', trim(argv)
            else
              make_runxml = cmd_iarg
            endif

          !generate stand alone erosion input file on simulation day
          else if(argv(2:2) .eq. 'O') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 1 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid stand alone erosion simulation day option: ', trim(argv)
            else
              saeinp_daysim = cmd_iarg
            endif

          !generate stand alone erosion input file on DD/MM/YY
          else if(argv(2:2) .eq. 'o') then
            if( .not. check_arg( 7, arg_len, argv ) ) goto 9
            read(argv(3:4),*) day
            read(argv(5:6),*) mon
            read(argv(7:),*) year
            if( ckdate(day, mon, year) ) then
              saeinp_jday = julday( day, mon, year )
            else
              write(*,*) &
                 'Warning: Ignored invalid stand alone erosion DD/MM/YY option: ', trim(argv)
            end if

          !specify use of soil puddling when all temperatures are above freezing
          else if(argv(2:2) .eq. 'p') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid soil puddling option: ', trim(argv)
            else
              puddle_warm = cmd_iarg
            endif

          !specify root WEPS directory 
          else if(argv(2:2) .eq. 'P') then
            ! Check to see if trailing '/' is there - LEW
            if( arg_len .ge. 3 ) then
               rootp = trim(argv(3:))
               ln = len_trim(rootp)
               if (rootp(ln:ln) .ne. '/') then
                  rootp = trim(argv(3:)) // '/'     !add trailing '/' character
               endif
            else
              write(*,*)                                                &
     &           'Warning: Ignored invalid WEPS root dir option: ', trim(argv)
            endif

          !specify winter annual root growth setting
          else if(argv(2:2) .eq. 'r') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid root growth option: ', trim(argv)
            else
              winter_ann_root = cmd_iarg
            endif

          ! specify report debug options
          else if(argv(2:2) .eq. 'R') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 3 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid report_debug option: ', trim(argv)
            else
              report_debug = cmd_iarg
            endif

          !specify soil ifc file water content type
          else if(argv(2:2) .eq. 'S') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 4 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid wc_type option: ', trim(argv)
            else
              wc_type = cmd_iarg
            endif

          !specify soil ifc file input format type
          else if(argv(2:2) .eq. 's') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid ifc_format option: ', trim(argv)
            else
              ifc_format = cmd_iarg
            endif

          !specify usage of transpiration depth
          else if(argv(2:2) .eq. 'T') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &       'Warning: Ignored invalid transipration depth option: ', trim(argv)
            else
              transpiration_depth = cmd_iarg
            endif

          !specify calculation of confidence intervals
          else if(argv(2:2) .eq. 't') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 2) ) then
              write(*,*)                                                &
     &       'Warning: Ignored invalid confidence interval option: ', trim(argv)
            else
              calc_confidence = cmd_iarg
            endif

          !specify whether buried roots are resurfaced via process 26
          else if(argv(2:2) .eq. 'u') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid resurf_roots option: ', trim(argv)
            else
              resurf_roots = cmd_iarg
            endif

          !specify whether to WEPS crop or UPGM to grow WEPS crop record
          else if(argv(2:2) .eq. 'U') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid upgm_growth option: ', trim(argv)
            else
              upgm_growth = cmd_iarg
            endif

          !specify internodal conductivity layer weighting setting
          else if(argv(2:2) .eq. 'w') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 5 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid layer_weighting option: ', trim(argv)
            else
              layer_weighting = cmd_iarg
            endif

          !specify hydrology method flag
          else if(argv(2:2) .eq. 'W') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 2 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid wepp_hydro option: ', trim(argv)
            else
              wepp_hydro = cmd_iarg
            endif

          !specify number of grid cells in the y direction
          else if(argv(2:2) .eq. 'x') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid x_grid option: ', trim(argv)
            else
              xgdpt = cmd_iarg
            endif

          !specify max wind speed value and set flag to use it
          else if(argv(2:2) .eq. 'X') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_rarg
            if( cmd_rarg .le. 0.0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid max wind speed value: ', trim(argv)
            else
              wind_max_value = cmd_rarg
              wind_max_flag = 1         !set max wind value cap flag
            endif

          !specify number of grid cells in the y direction
          else if(argv(2:2) .eq. 'y') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid y_grid option: ', trim(argv)
            else
              ygdpt = cmd_iarg
            endif

          !specify functional Yield/Residue ratio flag
          else if(argv(2:2) .eq. 'Y') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid cook_yield option: ', trim(argv)
            else
              cook_yield = cmd_iarg
            endif

          !specify calibrate_rotcycles flag
          else if(argv(2:2) .eq. 'Z') then
            if( .not. check_arg( 3, arg_len, argv ) ) goto 9
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) ) then
              write(*,*)                                                &
     &           'Warning: Ignored invalid calibrate_rotcycles option: ',       &
     &            trim(argv)
            else
              calibrate_rotcycles = cmd_iarg
            endif

          !Unknown option....
          else
              write(*,*) 'Warning: Ignored unknown option: ', trim(argv)
          endif
 09     continue
      endif

      if (calibrate_crops .ge. 1 .and. init_cycle .le. 0) then
!          write (6,*) "Warning: Yield calibration run mode requires at l&
!     &east one initialization cycle (use '-I1' WEPS cmdline option)"
          write (0,*) "Error: Yield calibration run mode requires at lea&
     &st one initialization cycle (use '-I1' WEPS cmdline option)"
          call exit(1)
      endif

      if (((xgdpt > 0) .and. (ygdpt == 0)) .or.                         &
     &    ((xgdpt == 0) .and. (ygdpt > 0))) then
        write(0,*) 'xgdpt = ', xgdpt, 'ygdpt = ', ygdpt
        write(0,*)                                                      &
     &         'Error: Only one grid dimension specified on commandline'
        call exit(131)
      endif

      if (report_info >= 1) then
        write(*,'(a)') 'rootp is: ', trim(rootp)
        write(*,'(a,1x,i0,2x,a,1x,i0)') 'wc_type:', wc_type, 'ifc_format:', ifc_format
      end if

      return
    end subroutine cmdline

    function check_arg( req_len, arg_len, argv2 ) result(valid_arg)
      ! checks if a valid numeric character is present after the option character
      ! in the argument string
      integer, intent(in) :: req_len
      integer, intent(in) :: arg_len
      character(len=*), intent(in) :: argv2

      logical :: valid_arg

      if( arg_len .lt. req_len ) then
        write(*,*) 'Warning: Command line option: ', trim(argv2), ' has no argument. Skipped.'
        valid_arg = .false.
      else
        if( verify( trim(argv2(3:)), '-.0123456789' ) .eq. 0 ) then
          valid_arg = .true.
        else
          valid_arg = .false.
          write(*,*) 'Warning: Command line option: ', trim(argv2), ' expects a numerical value. Skipped.'
        end if
      end if
    end function

end module weps_main_mod

