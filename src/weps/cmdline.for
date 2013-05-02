!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine   cmdline

!     + + + PURPOSE + + +
!     This subroutine perforns cmdline arg/opt processing

!     author: Larry Wagner

!     EDIT History
!     06-Nov-03  LEW  Extracted cmdline processing code from "input.for"

!     + + + KEY WORDS + + +
!     WEPS, cligen, windgen

!     + + + GLOBAL COMMON BLOCKS + + +
      use weps_interface_defs
      use datetime_mod, only: julday
      use file_io_mod, only: fopenk, luolog

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'm1flag.inc'

!      include 'util/misc/f2kcli.inc'  !declarations for f2k commandline functions
      INTEGER  COMMAND_ARGUMENT_COUNT ! required by the Lahey f95 compiler
      EXTERNAL COMMAND_ARGUMENT_COUNT
      include 'command.inc'          !declarations for commandline args

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'

!     + + + LOCAL VARIABLES + + +
      integer       ln

      integer cmd_iarg      ! Temporary var for retrieving integer cmdline args
      real    cmd_rarg      ! Temporary var for retrieving real cmdline args

      character*512 argv    ! For Fortran 2k commandline parsing
      integer       i
      integer       numarg
      integer       ll,ss


!     + + + LOCAL DEFINITIONS + + +

!   i         - Generic loop counter.
!   id,im,iy  - The initial day, month, and year of simulation.
!   ijday     - This variable contains the initial julian day of
!               the simulation run.
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
!     - define global variable in inc/command.inc
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
      wc_type = 4         !default soil ifc file type (estimate Brooks and Corey from FC and WP)
      ifc_format = 0      !default soil ifc file format type (uses IFC specified slope value)
      report_debug = 0    !default report debug printing (0=off, 1=on)
      saeinp_daysim = 0   !0 value skips creation of a stand alone erosion input file.
      saeinp_jday = 0     !0 value skips creation of a stand alone erosion input file.
      saeinp_all = 0      !0 value skips creation of stand alone erosion input files unless specific day set.
      init_cycle = 1      !default is to do one initialization cycle before reporting
      run_erosion = 1     !default is to run erosion submodel (0=no_erosion_submodel,1=WEPS,2=WEPP,3=WEPS+WEPP)
      calibrate_crops = 0 !default is to NOT run in crop calibration mode
      calibrate_rotcycles = 0 ! default is to run specified number of rotation cycles during calibration
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
      resurf_roots = 1    !default is set to resurface buried roots (process 26)
      calc_confidence = 0 !default is set to not calculate confidence intervals.
      frac_frst_mass_lost = 0.85 !default is set to 85% loss of young leaf freeze damaged mass
      transpiration_depth = 0 !default is set to not set transpiration depth to more than root depth.

! ----Determine Number of Command Line Arguments.

! Will now use the Fortran 2k commandline parsing support - LEW
! There cannot be any space between the option and any arguments,
! e.g. '-i#' is ok but '-i #' is not.
! Any option arguments that have any spaces in them must be quoted,
! e.g. '-i"C:\Program Files"' is ok but '-iC:\Program Files' is not.

      numarg = COMMAND_ARGUMENT_COUNT()  !Fortran 2k compatible call

      if (numarg .gt. 0) then
        do 09 i = 1, numarg
          call GET_COMMAND_ARGUMENT(i,argv,ll,ss)  !Fortran 2k compatible call
!         write(6,*) 'argv ',i,' is: ', trim(argv)

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

            write(*,*) 'Option ignored, no option flag: ', argv
            goto 9  !Go get next arg
          endif

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
     &'-f  Specify leaf freeze damaged mass loss fraction'
              write(*,*)                                                &
     &'    Specify -f0.85 to make 85% of freeze damaged mass disappear'

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
     &'-g  Application of growth stress functions'
              write(*,*) '    0 = no growth stress function applied'
              write(*,*) '    1 = water stress function applied'
              write(*,*) '    2 = temperature stress function applied'
              write(*,*) '    3 = both stress functions applied'

              write(*,*) '-G  Maximum level of water stress allowed'
              write(*,*) '-G0.00 allows maximum water stress to occur'
              write(*,*) '-G1.00 does not allow any water stress'

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
     &'-X  Specify maximum wind speed cap (m/s)'
              write(*,*)                                                &
     &'    Specify -X25.0 to limit input wind speeds to a max of 25 m/s'

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
     &               ' -g',i1,' -G',f4.2,' -I',i1,                      &
     &               ' -L',i1,' -l',i2,' -O(no file)',' -o(no file)',   &
     &               ' -p',i1,' -P',a,                                  &
     &               ' -r',i1,' -R',i1,' -S',i1,                        &
     &               ' -s',i1,' -T', i1,' -t', i1,                      &
     &               ' -u', i1, ' -w',i1,' -W',i1,                      &
     &               ' -X',f4.1,' -Y',i1' -Z',i1)

              write(0,2600) soil_cond,                                  &
     &          calibrate_crops, run_erosion, saeinp_all,               &
     &          frac_frst_mass_lost,                                    &
     &          growth_stress, water_stress_max, init_cycle,            &
     &          layer_scale, layer_infla,                               &
     &          puddle_warm, trim(rootp),                               &
     &          winter_ann_root, report_debug, wc_type,                 &
     &          ifc_format, transpiration_depth, calc_confidence,       &
     &          resurf_roots, layer_weighting, wepp_hydro,              &
     &          wind_max_value, cook_yield, calibrate_rotcycles

              call exit(1)

          !specify soil-conditioning flag
          else if(argv(2:2) .eq. 'c') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &         'Ignoring invalid SCI-energy option: ', trim(argv)
            else
              soil_cond = cmd_iarg
            endif

          !specify calibrate_crops flag
          else if(argv(2:2) .eq. 'C') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) ) then
              write(*,*)                                                &
     &           'Ignoring invalid calibrate_crops option: ', trim(argv)
            else
              calibrate_crops = cmd_iarg
            endif

          !specify run_erosion flag
          else if(argv(2:2) .eq. 'E') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 3) ) then
              write(*,*)                                                &
     &           'Ignoring invalid run_erosion option: ', trim(argv)
            else
              run_erosion = cmd_iarg
            endif

          !specify stand alone input files for every erosion event flag
          else if(argv(2:2) .eq. 'e') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &           'Ignoring invalid saeinp_all option: ', trim(argv)
            else
              saeinp_all = cmd_iarg
            endif

          !specify young leaf freeze damaged mass loss fraction
          else if(argv(2:2) .eq. 'f') then
            read(argv(3:),*) cmd_rarg
            if( (cmd_rarg .lt. 0.0) .or. (cmd_rarg .gt. 1.0) ) then
              write(*,*)                                                &
     &       'Ignoring invalid freeze mass loss value: ', trim(argv)
            else
              frac_frst_mass_lost = cmd_rarg
            endif

          !specify growth_stress flag
          else if(argv(2:2) .eq. 'g') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 3 ) then
              write(*,*)                                                &
     &           'Ignoring invalid growth_stress option: ', trim(argv)
            else
              growth_stress = cmd_iarg
            endif

          !specify maximum water stress value (assumes water stress is on)
          else if(argv(2:2) .eq. 'G') then
            read(argv(3:),*) cmd_rarg
            if( (cmd_rarg .lt. 0.0) .or. (cmd_rarg .gt. 3.0) ) then
              write(*,*)                                                &
     &       'Ignoring invalid water stress maximum value: ', trim(argv)
            else
              water_stress_max = cmd_rarg
            endif

          !specify initialization cycle setting
          else if(argv(2:2) .eq. 'I') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Ignoring invalid init_cycle option: ', trim(argv)
            else
              init_cycle = cmd_iarg
            endif

          !specify layer scale setting
          else if(argv(2:2) .eq. 'L') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .le. 0 ) then
              write(*,*)                                                &
     &           'Ignoring invalid layer_scale option: ', trim(argv)
            else
              layer_scale = cmd_iarg
            endif

          !specify layer inflation setting
          else if(argv(2:2) .eq. 'l') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Ignoring invalid layer_infla option: ', trim(argv)
            else
              layer_infla = cmd_iarg
            endif

          !generate stand alone erosion input file on simulation day
          else if(argv(2:2) .eq. 'O') then
            read(argv(3:),*) saeinp_daysim

          !generate stand alone erosion input file on DD/MM/YY
          else if(argv(2:2) .eq. 'o') then
            ! id, im, iy are used here before they are read from inprun
            read(argv(3:4),*) id
            read(argv(5:6),*) im
            read(argv(7:),*) iy
            saeinp_jday = julday( id, im, iy )

          !specify use of soil puddling when all temperatures are above freezing
          else if(argv(2:2) .eq. 'p') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &           'Ignoring invalid soil puddling option: ', trim(argv)
            else
              puddle_warm = cmd_iarg
            endif

          !specify root WEPS directory 
          else if(argv(2:2) .eq. 'P') then
            ! Check to see if trailing '/' is there - LEW
            ln = len_trim(argv(3:))
            if (ln .ne. 0) then
               rootp = trim(argv(3:))
               if (rootp(ln:ln) .ne. '/') then
                  rootp = trim(argv(3:)) // '/'     !add trailing '/' character
               endif
            else
              write(*,*)                                                &
     &           'Ignoring invalid WEPS root dir option: ', trim(argv)
            endif

          !specify winter annual root growth setting
          else if(argv(2:2) .eq. 'r') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Ignoring invalid root growth option: ', trim(argv)
            else
              winter_ann_root = cmd_iarg
            endif

          ! specify report debug options
          else if(argv(2:2) .eq. 'R') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 3 ) then
              write(*,*)                                                &
     &           'Ignoring invalid report_debug option: ', trim(argv)
            else
              report_debug = cmd_iarg
            endif

          !specify soil ifc file type
          else if(argv(2:2) .eq. 'S') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 4 ) then
              write(*,*)                                                &
     &           'Ignoring invalid wc_type option: ', trim(argv)
            else
              wc_type = cmd_iarg
            endif

          !specify soil ifc file input format type
          else if(argv(2:2) .eq. 's') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Ignoring invalid ifc_format option: ', trim(argv)
            else
              ifc_format = cmd_iarg
            endif

          !specify usage of transpiration depth
          else if(argv(2:2) .eq. 'T') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 1) ) then
              write(*,*)                                                &
     &       'Ignoring invalid transipration depth option: ', trim(argv)
            else
              transpiration_depth = cmd_iarg
            endif

          !specify calculation of confidence intervals
          else if(argv(2:2) .eq. 't') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) .or. (cmd_iarg .gt. 2) ) then
              write(*,*)                                                &
     &       'Ignoring invalid confidence interval option: ', trim(argv)
            else
              calc_confidence = cmd_iarg
            endif

          !specify whether buried roots are resurfaced via process 26
          else if(argv(2:2) .eq. 'u') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 1 ) then
              write(*,*)                                                &
     &           'Ignoring invalid resurf_roots option: ', trim(argv)
            else
              resurf_roots = cmd_iarg
            endif

          !specify internodal conductivity layer weighting setting
          else if(argv(2:2) .eq. 'w') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 2 ) then
              write(*,*)                                                &
     &           'Ignoring invalid layer_weighting option: ', trim(argv)
            else
              layer_weighting = cmd_iarg
            endif

          !specify hydrology method flag
          else if(argv(2:2) .eq. 'W') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .gt. 2 ) then
              write(*,*)                                                &
     &           'Ignoring invalid wepp_hydro option: ', trim(argv)
            else
              wepp_hydro = cmd_iarg
            endif

          !specify max wind speed value and set flag to use it
          else if(argv(2:2) .eq. 'X') then
            read(argv(3:),*) cmd_rarg
            if( cmd_rarg .le. 0.0 ) then
              write(*,*)                                                &
     &           'Ignoring invalid max wind speed value: ', trim(argv)
            else
              wind_max_value = cmd_rarg
              wind_max_flag = 1         !set max wind value cap flag
            endif

          !specify functional Yield/Residue ratio flag
          else if(argv(2:2) .eq. 'Y') then
            read(argv(3:),*) cmd_iarg
            if( cmd_iarg .lt. 0 ) then
              write(*,*)                                                &
     &           'Ignoring invalid cook_yield option: ', trim(argv)
            else
              cook_yield = cmd_iarg
            endif

          !specify calibrate_rotcycles flag
          else if(argv(2:2) .eq. 'Z') then
            read(argv(3:),*) cmd_iarg
            if( (cmd_iarg .lt. 0) ) then
              write(*,*)                                                &
     &           'Ignoring invalid calibrate_rotcycles option: ',       &
     &            trim(argv)
            else
              calibrate_rotcycles = cmd_iarg
            endif

          !Unknown option....
          else
              write(*,*) 'Ignoring unknown option: ', trim(argv)
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

      write(*,*) 'rootp is: ', trim(rootp)
      write(*,*) 'wc_type: ',wc_type, ' ifc_format: ', ifc_format

      call fopenk (luolog, rootp(1:len_trim(rootp)) // 'logfil.txt',    &
     &        'unknown')

      write(luolog, *) 'Using ', rootp(1:len_trim(rootp)),              &
     &           ' as the simulation input directory'

      return
      end

