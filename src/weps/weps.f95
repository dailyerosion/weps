!$Author$
!$Date$
!$Revision$
!$HeadURL$

   program weps

      ! + + + PURPOSE + + +
      ! This is the MAIN program of the Wind Erosion Prediction System.
      ! Its purpose is to control the sequence of events during a
      ! simulation run.

      ! author: John Tatarko
      ! version: 92.10

      ! + + + KEY WORDS + + +
      ! wind, erosion, hydrology, tillage, soil,
      ! crop, decomposition, management

      ! + + + GLOBAL COMMON BLOCKS + + +
      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT
      use weps_cmdline_parms, only: calibrate_crops, calibrate_rotcycles, &
                                   init_cycle, calc_confidence, hb_freq, &
                                   report_debug, run_erosion, &
                                   saeinp_all, saeinp_daysim, saeinp_jday, wepp_hydro, &
                                   make_runxml
      use weps_main_mod, only: old_run_file, run_rot_cycles, id, im, iy, ld, lm, ly, rootp, &
                               ijday, ljday, maxper, longest_mgt_rotation, &
                               init_loop, calib_loop, report_loop, &
                               calib_cycle, calib_done, &
                               am0ifl, wepsinit, cmdline
      use weps_submodel_mod, only: submodels, erodsubr_update
      use weps_output_mod, only: openfils, plotdata, closefils, bpools
      use timer_mod
      use datetime_mod, only: update_system_time, get_systime_string, julday, lstday, isleap, &
                              set_psimdate, get_psim_ndiy, get_psim_doy, get_psim_year, get_psim_mon, get_psim_day, &
                              get_psim_daysim, &
                              update_simulation_date, get_simdate, get_simdate_doy, get_simdate_daysim, &
                              get_simdate_ndiy
      USE pd_dates_vars, only: reporting_dates
      USE pd_update_vars, only: reporting_update
      USE pd_report_vars, only: reporting_report
      use report_update_vars_mod, only: alloc_rep_daily, update_daily_vars, sim_area_average, &
                                        update_period_update_vars, update_period_report_vars, &
                                        update_hmonth_update_vars, update_hmonth_report_vars, &
                                        update_monthly_update_vars, update_monthly_report_vars, &
                                        update_yrly_update_vars, update_yrly_report_vars
      use report_init_mod, only: init_report_vars
      use report_print_mod, only: print_report_vars, print_yr_report_vars, print_mandate_output
      use print_ui1_output_mod, only: print_ui1_output
      use barriers_mod, only: barrier, barseas, minht_barriers, destroy_barrier, set_barrier_season
      use file_io_mod, only: luo_egrd, luo_emit, luo_sgrd, luogui1, luomandate, makedir, fopenk, in_weps
      use input_soil_mod, only: input_ifc, soil_in
      use soil_data_struct_defs, only: soil_def, allocate_soil, deallocate_soil, print_soil
      use crop_mod, only: cprevseasonrotation
      use report_harvest_mod, only: cprevrotation, cprevcalibrotation
      use biomaterial, only: plants_struct, biototal, decomp_factors, create_biototal, create_decomp_factors, &
                             destroy_biototal, destroy_biototal, destroy_biototal, destroy_decomp_factors, plantdestroyall
      use update_mod, only: plantupdate, am0cropupfl
      use debug_mod, only: create_decomp_debug, destroy_decomp_debug
      use mandate_mod, only: mandate_array, get_nperiods, allmandates, sync_harvcropnames
      use manage_data_struct_defs, only: lastoper, manFile
      use manage_mod, only: mfinit, cropres, opstate, am0til
      use manage_xml_mod, only: setup_man_xml
      use erosion_mod, only: erosion, erodinit
      use erosion_data_struct_defs, only: create_subregion_alloc, destroy_subregion_alloc, &
                                          threshold, cellsurfacestate, cellstate, &
                                          awudmx, am0eif, am0efl, subrsurf
      use wind_mod, only: anemometer_init
      use precision_mod, only: precision_init
      use grid_mod, only: sbgrid, write_grid, gridfile
      use sae_in_out_mod, only: mksaeinp, mksaeout
      use stir_soil_texture_mod, only: create_stir_soil_multiplier, destroy_stir_soil_multiplier
      use sci_soil_texture_mod, only: create_sci_soil_multiplier, destroy_sci_soil_multiplier
      use stir_report_mod, only: alloc_stir_accumulators, destroy_stir_accumulators, stir_report
      use sci_report_mod, only: scisum, sci_cum, sci_report
      use hydro_data_struct_defs, only: hydro_derived_et, hydro_state, hhrs
      use hydro_mod, only: hydrinit
      use hydro_darcy_mod, only: allocate_lsoda_sls1, allocate_lsoda_slsa, allocate_lsoda_sloc
      use hydro_darcy_mod, only: allocate_lsoda_stoc, allocate_dvolw_param
      use hydro_darcy_mod, only: deallocate_lsoda_sls1, deallocate_lsoda_slsa, deallocate_lsoda_sloc
      use hydro_darcy_mod, only: deallocate_lsoda_stoc, deallocate_dvolw_param
      use report_hydrobal_mod, only: h1bal
      use wepp_param_mod, only: wepp_param, create_wepp_param, destroy_wepp_param
      use climate_input_mod, only: cliginit, windinit, set_cli_today, set_wind_today, amzele
      use input_run_mod, only: input
      use input_run_xml_mod, only: write_run_xml
      use lcm_mod, only: lcm_n
      use asd_mod, only: asdini, msieve
      use decomp_out_mod, only: decopen
      use water_erosion_mod, only: water_erosion, weppsum
      use confidence_interval_mod, only: confidence_interval
      use binomial_mod, only: init_buffer
      use calib_plant_mod, only: init_calib_arrays

      external init_wepp

!      use omp_lib
      
      ! build and release info, fpp created by cook
      include 'build.inc'

      ! + + + LOCAL VARIABLES + + +
      character(len=21) :: rundatetime

      integer, dimension(:), allocatable :: nperiods       ! number of reporting periods being accumulated
      integer, dimension(:), allocatable :: pd             ! index counter into reporting periods
      integer, dimension(:), allocatable :: n_rot_cycles   ! actual number of rotation cycles simulated
      integer, dimension(:), allocatable :: t_mperod       ! temporary array for management years

      integer, dimension(:), allocatable :: keep  ! in calibration, reset mnryr to precalibration loop value (why?)

      integer, dimension(:), allocatable :: cday            ! The current day of simulation month.
      integer, dimension(:), allocatable :: cmon            ! The current month of simulation year.
      integer, dimension(:), allocatable :: cyear           ! The current year of simulation run.
      
      integer :: beg_init_jday ! The first julian day of initialization
      integer :: beg_init_d    ! the first day of initialization
      integer :: beg_init_m    ! the first month of initialization
      integer :: beg_init_y    ! the first year of initialization
      integer :: isr           ! This variable holds the subregion index.
      integer :: simyrs        ! The number of years being simulated (for console output)
      integer :: lcaljday      ! last julian day of calibration cycle
      integer :: ci_year       ! indicates how many years of data have been printed into ci.out
      integer :: nsubr         ! total number of subregions (read in inprun, derived from allocated soil_in)

      type(soil_def), dimension(:), allocatable :: soil             ! structure with soil state and parameters as updated during simulation
      type(plants_struct), dimension(:), allocatable :: plants      ! array of pointers to structure for all biomaterial
                                                                    ! structure also references older biomaterial
      type(biototal), dimension(:), allocatable :: croptot          ! structure with totalized values of crop state
      type(biototal), dimension(:), allocatable :: restot           ! structure with totalized values of residue state
      type(biototal), dimension(:), allocatable :: biotot           ! structure with totalized values of all biomass state
      type(decomp_factors), dimension(:), allocatable :: decompfac  ! structure with decomposition factors
      type(mandate_array), dimension(:), allocatable :: mandatbs    ! structure with management dates, operation names and crops

      type(threshold), dimension(:), allocatable :: noerod                 ! report values to show which factors prevented erosion

      type(reporting_report), dimension(:), target, allocatable :: rep_report
      type(reporting_update), dimension(:), target, allocatable :: rep_update
      type(reporting_dates), dimension(:), target, allocatable :: rep_dates
      type(hydro_derived_et), dimension(:), allocatable :: h1et   ! structure with reporting values for Evaporation/Transpiration
      type(hydro_state), dimension(:), allocatable :: hstate
      type(wepp_param), dimension(:), allocatable :: wp           ! structure for wepp parameters by subregion

      integer :: alloc_stat, sum_stat
      integer :: am0jd   ! Current julian day of simulation
      integer :: ncycles ! a count of the number of maxper cycles completed (used in confidence interval loop)

!      integer :: nthreads

      ! + + + END SPECIFICATIONS + + +

      ! Before anything else is done, output the build date and release/version information.
      write(*,"(a)")
      write(*,"(a6,a)") 'WEPS ', trim(build_version)
      write(*,"(a10,a)") 'Release: ', trim(build_release)
      write(*,"(a11,a)") 'Built on: ', trim(build_date)
      write(*,"(a16,a)") 'Compiled with: ', trim(build_compiler)
      write(*,"(a17,a)") 'Compiled flags: ', trim(build_compiler_options)
      write(*,"(a16,a)") 'Built by user: ', trim(build_user)
      write(*,"(a17,a)") 'Repository URL: ', trim(build_svn_repo_url)
      write(*,"(a26,a)") 'SVN repository Revision: ', trim(build_svn_repo_revision)
      write(*,"(a22,a)") 'SVN update Revision: ', trim(build_svn_updt_revision)
      write(*,"(a30,a)") 'Local and SVN Modfied Files: ', trim(build_cnt_mods)
      write(*,"(a)")

      ! Determine date of Run
      call update_system_time

      ! Print date of Run
      rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
      write(*,"(a19,a21)") 'Date of WEPS run: ', rundatetime
      write(*,"(a)")

      call timer(0,TIMSTART)
      call timer(TIMWEPS,TIMSTART)
      call timer(TIMINIT,TIMSTART)

      ! initialize anemometer defaults
      call anemometer_init

      ! initialize math precision global variables
      call precision_init

      ! Read command line arguments and options
      call cmdline()

      ! open input files and read run files
      ! The argument soil_in is only accessed when reading the leagacy run file
      ! When reading the xml input, soil_in is accessed through the input_soil_mod definition.
      call input(soil_in)

      ! set total number of subregions from size of allocated soil_in array
      ! Note: soil array has 0 index, soil_in does not
      nsubr = size(soil_in)

      ! initialization and allocation for all of weps
      call wepsinit(nsubr)

      ! keep
      sum_stat = 0
      allocate(keep(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      
      ! date keeping values
      allocate(cday(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(cmon(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(cyear(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
     
      ! create sci and stir soil multiplier arrays (before input_ifc which needs them)
      call create_sci_soil_multiplier(nsubr)
      call create_stir_soil_multiplier(nsubr)
      call alloc_stir_accumulators(nsubr)

      ! erosion subregion surface values array
      allocate(noerod(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(soil(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(hstate(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! done before allocations which use layers
      do isr = 1, nsubr 
         ! read soil file and setup layers
         call input_ifc(rootp, isr, soil_in(isr), hstate(isr))
      end do

      ! allocate subregion crop and residue pool arrays
      allocate(plants(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(croptot(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(decompfac(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! crop end of season reporting variables
      allocate(cprevseasonrotation(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(cprevrotation(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(cprevcalibrotation(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(am0cropupfl(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! summary / total types are allocated with 0 for total simulation area
      allocate(restot(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(biotot(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! allocate management data arrays for reports
      allocate(mandatbs(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(lastoper(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(cropres(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(opstate(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(am0til(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! report accummulation arrays
      allocate(rep_report(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(rep_update(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(rep_dates(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(nperiods(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(pd(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(n_rot_cycles(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(scisum(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(h1et(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(h1bal(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(wp(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(t_mperod(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate enough memory for weps main data arrays'
      end if

      if( calibrate_crops > 0 ) then
        call init_calib_arrays(nsubr)
      end if

      if( wepp_hydro .eq. 0 ) then
        ! allocate arrays used in darcy
        call allocate_lsoda_sls1(nsubr)
        call allocate_lsoda_slsa(nsubr)
        call allocate_lsoda_sloc(nsubr)
        call allocate_lsoda_stoc(nsubr)
        call allocate_dvolw_param(nsubr, soil_in)
      end if

      do isr = 1, nsubr
         ! no plants yet
         nullify(plants(isr)%plant)
         plants(isr)%plantIndex = 0
         ! complete allocation of layers
         croptot(isr) = create_biototal(soil_in(isr)%nslay)
         ! allocate layer arrays in totaling structures
         restot(isr) = create_biototal(soil_in(isr)%nslay)
         biotot(isr) = create_biototal(soil_in(isr)%nslay)
         decompfac(isr) = create_decomp_factors(soil_in(isr)%nslay)
         wp(isr) = create_wepp_param(soil_in(isr)%nslay)
      end do

      ! allocate debug local arrays
      call create_decomp_debug(nsubr)

      call openfils()

      call setup_man_xml()
      do isr = 1, nsubr
          ! Likely that we will put all management data into memory
          ! and only read and initialize everything here, looping through
          ! each management file (one for each subregion).

          ! Read in management file and initialize rotation counters
          call mfinit(rootp, isr, manFile(isr))
          t_mperod(isr) = manFile(isr)%mperod
          ! initializing this to 1 or greater eliminates (random) blank line in season.out
          cprevseasonrotation(isr) = 1
      end do

      ! find maxper, which is the least common multiple of the number of years in each rotation
      maxper = lcm_n( t_mperod )
      ! find the longest mgt rotation for any subregion
      longest_mgt_rotation = maxval(t_mperod)
      !print *, 'longest_mgt_rotation:', longest_mgt_rotation, 'maxper:', maxper
      !print *, 't_mperod:', t_mperod

!     check for consistency maxper, n_rot_cycles, number of years to run
      if( maxper*run_rot_cycles .ne. ly-iy+1 ) then
          write(*,'(4(a,i0,a,1x))') 'Warning: Number of rotations (',run_rot_cycles,')', &
                     'times Years in rotation (',maxper,')', &
                     'does not match Number of simulation years (', &
                     ly-iy+1,')'
          run_rot_cycles = (ly-iy+1) / maxper
          if( mod( (ly-iy+1), maxper ) .gt. 0 ) then
              write(*,*) 'Warning: Not simulating complete rotations'
              run_rot_cycles = run_rot_cycles + 1
          end if
      end if
      if (calibrate_rotcycles .eq. 0) then
         calibrate_rotcycles = run_rot_cycles
      else
         ! limit number of rotation cycles to speed up calibration
         calibrate_rotcycles = min(calibrate_rotcycles, run_rot_cycles)
      endif

      ! Everything required for stir_report is available
      ! This is all the initialization for the new output reporting code
      ! Note: this also resets manFile%oper to manFile%operFirst
      do isr=1,nsubr
        mandatbs(isr)%mperod = manFile(isr)%mperod
        call stir_report( mandatbs(isr)%mandate, manFile(isr) )
      end do

      ! initialize full mandate series for all subregions for full maxper length.
      mandatbs(0)%mperod = maxper
      call allmandates( mandatbs )  ! examine dates for all subregions and put composite list into 0 array

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         if( old_run_file ) then
           call sbgrid( minht_barriers(), amzele )
           if( make_runxml .gt. 0 ) then
             ! create new weps.runx
             gridfile = 'erod.grdx'
             call write_run_xml()
             call write_grid( trim(rootp) )
           end if
         end if
         call erodinit( noerod )
      endif

      do isr = 1, nsubr
         ! this prints header to decomp.out file
         call bpools( isr, plants(isr)%plant, restot(isr), biotot(isr), decompfac(isr) )
      end do

      call asdini()     ! calculates sieve cut parameters, does not set values
      call init_buffer(msieve)  ! intialize binomial coefficients (mproc/crush uses)

!     calculate first and last Julian dates for simulation
      ijday = julday(id, im, iy)
      ljday = julday(ld, lm, ly)

      call cliginit( ijday, ljday ) ! read "yearly average info" from cligen header
      call windinit( ijday, ljday ) ! allocate memory for reading subdaily wind velocities from windgen format input file

      call alloc_rep_daily( nsubr, ijday, ljday )

      do isr = 0, nsubr
          n_rot_cycles(isr) = run_rot_cycles * maxper / mandatbs(isr)%mperod

          nperiods(isr) = get_nperiods(mandatbs(isr)%mperod, mandatbs(isr)%mandate)   !Get # of periods for reports ( 0 is for global simulation area)
          if( report_debug >= 1 ) then
              write(*,*) '# rot years', maxper, "nperiods", nperiods(isr), '# cycles', n_rot_cycles(isr)
          end if
          call init_report_vars(nperiods(isr), mandatbs(isr)%mperod, n_rot_cycles(isr), mandatbs(isr)%mandate, &
                                rep_report(isr), rep_update(isr), rep_dates(isr))
          pd(isr) = 1
      end do

!     calculate last julian date for initialization cycle
      beg_init_d = 1
      beg_init_m = 1
      ! The following line is incorrect for calculating the initialization cycles - LEW
!      beg_init_y = iy + (maxper*init_cycle) - 1
!      beg_init_y = iy + (longest_mgt_rotation*init_cycle) - 1

      ! Wrong!! Using longest_mgt_rotation results in some management cycles being terminated
      ! part way through the rotation. This means that a winter annual may be growing
      ! only to be terminated by a spring planted crop or the erosion simulation would start with
      ! the harvest of a winter annual crop that has not been planted, which is one reason we do
      ! initialization in the first place. An alternative would be to start
      ! part way through managment cycles so they all end at the end of initialization, so
      ! the erosion simulation begins with all managment cycles at the beginning. - FAF

      beg_init_y = ly - (longest_mgt_rotation*init_cycle) + 1
      if( beg_init_y .eq. 0 ) beg_init_y = -1
      beg_init_jday = julday(beg_init_d, beg_init_m, beg_init_y)

      ! fix up for -I rotation count .gt. the specified run dates
      if( beg_init_jday .lt. ijday ) then
         write(*,*) 'Warning: -I Initialization longer than simulation. Reduced to allowable value.'
         beg_init_d = id
         beg_init_m = im
         beg_init_y = iy
         beg_init_jday = ijday
      end if

      ! allocate subregion surface condition array (all juldays, all subregions)
      allocate(subrsurf(ijday:ljday,nsubr), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to allocate enough memory for weps main erosiondata arrays'
      end if

      call timer(TIMINIT,TIMSTOP)
      call timer(TIMSUBR,TIMSTART)

!      nthreads = omp_get_num_procs()
!      call omp_set_num_threads(min(nsubr+1,nthreads))

!$omp parallel do schedule(static)

      do  isr = 1, nsubr

       do ! initialization (Calibration) loop

        ! Assign input soil values to changeable soil arrays.
        soil(isr) = soil_in(isr)
        ! Initializations unique to particular submodels
        call decopen(isr) ! prints headers in above.out and below.out
        ! Initialize the water holding capacity variable
        call hydrinit(isr, soil(isr), hstate(isr), h1et(isr), h1bal(isr), wp(isr))
        ! initialize all dependent variables
        call plantupdate( isr, soil(isr), &
                          plants(isr)%plant, croptot(isr), restot(isr), biotot(isr) )

        ! write(*,*) 'biotot, croptot, restot', biotot(isr), croptot(isr), restot(isr)

        ! begin initialization simulation phase
        do am0jd = beg_init_jday, ljday   !will not enter if end before beginning

          ! store day for use in simulation date routines
          call set_psimdate(isr, ijday, am0jd)
          
          ! print current day of simulation to screen periodically
          if( get_psim_doy(isr) .eq. 1 ) then
            simyrs = (ly - beg_init_y + 1)
            if( hb_freq .eq. 0 ) then
               write(*,'(a,3(1x,i0,2x,a))') 'Subregion', isr, 'Year', get_psim_year(isr), 'of', simyrs, '(initialization)'
            else if( mod(get_psim_year(isr), hb_freq) .eq. 0 ) then
               write(*,'(a,3(1x,i0,2x,a))') 'Subregion', isr, 'Year', get_psim_year(isr), 'of', simyrs, '(initialization)'
            end if
            call flush(OUTPUT_UNIT)
          end if
          init_loop(isr) = .true. ! Signifies that we are in the "initialization" loop
          ! do multiple subregion
          call submodels(isr, soil(isr), plants(isr)%plant, plants(isr)%plantIndex, restot(isr), croptot(isr),  &
               biotot(isr), decompfac(isr), hstate(isr), h1et(isr), h1bal(isr), wp(isr), manFile(isr))
          ! set initialization flag to .false. after first day
          if (am0ifl(isr)) am0ifl(isr) = .false.

          ! write decomposition biomass pool amounts to files
          call bpools(isr, plants(isr)%plant, restot(isr), biotot(isr), decompfac(isr))

          ! if last day of year, check for end of rotation
          if (get_psim_doy(isr) .eq. get_psim_ndiy(isr)) then
            ! check if at end of subregion's rotation cycle
            if (mod(manfile(isr)%mnryr,manFile(isr)%mperod) == 0) then
               manfile(isr)%mnryr = 1
               lastoper(isr)%yr = manfile(isr)%mnryr
            else
               manfile(isr)%mnryr = manfile(isr)%mnryr + 1
               lastoper(isr)%yr = manfile(isr)%mnryr
            end if
          end if
          init_loop(isr) = .false.
        end do    ! end loop of multiple years
  
        ! set initialization flag to .false. if initialization was skipped
        if (am0ifl(isr)) am0ifl(isr) = .false.
        write(*,'(a,1x,i0,2x,a)') 'Subregion', isr, 'Finished initialization stage'
        ! End of "initialization" section

        ! Start of "calibrate" section
         keep(isr) = manfile(isr)%mnryr

        if( (calibrate_crops > 0) .and. (.not. calib_done(isr)) .and. (calib_cycle(isr) < calibrate_crops) ) then
           calib_cycle(isr) = calib_cycle(isr) + 1
           write(*,'(a,1x,i0,2x,a)') 'Subregion', isr, 'Starting calibrate phase'
           lcaljday = ljday
           if (calibrate_rotcycles .lt. run_rot_cycles) then
               !calculate last julian date for single calibration cycle
               lcaljday = julday(31, 12, iy - 1  + (maxper*calibrate_rotcycles) )
               lcaljday = min(lcaljday, ljday)
           endif

           ! set for beginning of calibration cycle
           manFile(isr)%mcount = 1

           do am0jd = ijday,lcaljday

             ! store day for use in simulation date routines
             call set_psimdate(isr, ijday, am0jd)

             ! print current day of simulation to screen periodically
             if( get_psim_doy(isr) .eq. 1 ) then
               simyrs = (ly - beg_init_y + 1)
               if( hb_freq .eq. 0 ) then
                  write(*,'(a,5(1x,i0,2x,a))') 'Subregion ', isr, 'Year', get_psim_year(isr), 'of', maxper*calibrate_rotcycles, &
                             '(calibrating',calib_cycle(isr),'/', calibrate_crops,')'
               else if( mod(get_psim_year(isr), hb_freq) .eq. 0 ) then
                  write(*,'(a,5(1x,i0,2x,a))') 'Subregion ', isr, 'Year', get_psim_year(isr), 'of', maxper*calibrate_rotcycles, &
                             '(calibrating',calib_cycle(isr),'/', calibrate_crops,')'
               end if
               call flush(OUTPUT_UNIT)
             end if

             calib_loop(isr) = .true. ! Signifies that we are in the calibration loop
             call submodels(isr, soil(isr), plants(isr)%plant, plants(isr)%plantIndex, restot(isr), croptot(isr), &
                 biotot(isr), decompfac(isr), hstate(isr), h1et(isr), h1bal(isr), wp(isr), manFile(isr))

             ! write decomposition biomass pool amounts to files
             call bpools(isr, plants(isr)%plant, restot(isr), biotot(isr), decompfac(isr))

             ! set initialization flag to .false. after first day
             if (am0ifl(isr)) am0ifl(isr) = .false.

             ! if last day of year, check for end of rotation
             if (get_psim_doy(isr) .eq. get_psim_ndiy(isr)) then
               ! check if at end of subregion's rotation cycle
               if (mod(manfile(isr)%mnryr,manFile(isr)%mperod) == 0) then
                  manfile(isr)%mnryr = 1
                  lastoper(isr)%yr = manfile(isr)%mnryr
               else
                  manfile(isr)%mnryr = manfile(isr)%mnryr + 1
                  lastoper(isr)%yr = manfile(isr)%mnryr
               end if
             end if
           end do   ! "calibration" phase
           manfile(isr)%mnryr = keep(isr)
           ! at end of managment file, reset mcount
           manFile(isr)%mcount = 0
           calib_loop(isr) = .false.

           ! Go back to "initialization" and restart after resetting the appropriate variables here
           am0eif = .true.
           am0ifl(isr) = .false.
           manfile(isr)%mnryr = 1
           ! destroy all plant biomass pools for each calibration iteration
           call plantDestroyAll(plants(isr)%plant)

          ! End of "calibrate" section
        else
           exit ! calibration not being done or completed or max cycles
        end if

       end do  ! initialization (Calibration) loop

        ! Start of "report" section

         ! run wepp erosion
         if( (run_erosion .eq. 2) .or. (run_erosion .eq. 3) ) then
            call init_wepp(isr, 1, soil(isr))        ! specific wepp initializations
         end if
     
         ! begin report simulation phase
         write(*,*) "Starting report phase"
         do am0jd = ijday,ljday

            ! store day for use in simulation date routines
            call set_psimdate(isr, ijday, am0jd)
            cday(isr) = get_psim_day(isr)
            cmon(isr) = get_psim_mon(isr)
            cyear(isr) = get_psim_year(isr)

            ! print current day of simulation to screen periodically
             if( get_psim_doy(isr) .eq. 1 ) then
               simyrs = (ly - iy + 1)
               if( hb_freq .eq. 0 ) then
                  write(*,'(a,2(1x,i0,2x,a),1x,i0)') 'Subregion', isr, 'Year', cyear(isr), 'of', simyrs
               else if( mod(cyear(isr), hb_freq) .eq. 0 ) then
                  write(*,'(a,2(1x,i0,2x,a),1x,i0)') 'Subregion', isr, 'Year', cyear(isr), 'of', simyrs
               end if
               call flush(OUTPUT_UNIT)
            end if

            report_loop(isr) = .true.  ! Signifies that we are in the "report" loop

            call submodels(isr, soil(isr), plants(isr)%plant, plants(isr)%plantIndex, restot(isr), croptot(isr), &
                           biotot(isr), decompfac(isr), hstate(isr), h1et(isr), h1bal(isr), wp(isr), &
                           manFile(isr))

            if (run_erosion > 0) then   ! Are we simulating erosion in this RUN
               ! zero brcdinput, allocate layer and per/day in subregion surface state passed to erosion
               call create_subregion_alloc(soil_in(isr)%nslay, hhrs, subrsurf(am0jd, isr))
               ! transfer data values from submodel structures into erosion input structure
               ! these values are shown in plot.out, so do every day
               call erodsubr_update( manFile(isr), soil(isr), plants(isr)%plant, biotot(isr), hstate(isr), h1et(isr), &
                                        subrsurf(am0jd, isr) )
            end if

            if ((run_erosion .eq. 2) .or. (run_erosion .eq. 3)) then
               call water_erosion( isr, cday(isr), cmon(isr), cyear(isr), soil(isr), restot(isr), croptot(isr), wp(isr) )
            end if

            call sci_cum( isr, restot(isr) )   ! Keep running total for soil conditioning index (SCI)
            ! write decomposition biomass pool amounts to files
            call bpools(isr, plants(isr)%plant, restot(isr), biotot(isr), decompfac(isr))

            ! if last day of year, check for end of rotation
            if( get_psim_doy(isr) .eq. get_psim_ndiy(isr) ) then
               ! check if at end of subregion's rotation cycle
               if (mod(manfile(isr)%mnryr,manFile(isr)%mperod) == 0) then
                  ! end of management rotation cycle
                  manfile(isr)%mnryr = 1
                  lastoper(isr)%yr = manfile(isr)%mnryr
               else
                  ! continue through rotation cycle
                  manfile(isr)%mnryr = manfile(isr)%mnryr + 1
                  lastoper(isr)%yr = manfile(isr)%mnryr
               end if
            end if

            ! set initialization flag to .false. after first day
            if (am0ifl(isr)) am0ifl(isr) = .false.

            ! insert report values into daily arrays
            call update_daily_vars( isr, am0jd, soil(isr), restot(isr), croptot(isr), &
                                    biotot(isr), h1et(isr), h1bal(isr) )

         end do   ! end of "reporting" loop

         report_loop(isr) = .false.
        ! End of "report" section

      end do  ! end of subregion loop

!$omp end parallel do

      call timer(TIMSUBR,TIMSTOP)
      call timer(TIMEROD,TIMSTART)

      ! keep no longer needed
      deallocate(keep)

      if (run_erosion > 0) then   ! Are we simulating erosion in this RUN

         do isr = 1, nsubr
            am0ifl(isr) = .true.  ! prints plot.out headers
            call plotdata( isr, noerod(isr), manFile(isr), subrsurf(ijday,isr), cellstate )
            am0ifl(isr) = .false. ! initialization normally .false. during erosion
            if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
               call init_wepp(isr, 0, soil(isr))        ! specific wepp initializations
            end if
         end do

         ! settings for creation of erosion submodel detailed outputs
         mksaeinp%maxday = ljday - ijday + 1  ! set maximum daysim possible for saeinp file name extension
         mksaeout%maxday = ljday - ijday + 1  ! set maximum daysim possible for saeout file name extension
         if( saeinp_all .gt. 0 ) then
            ! creating many SWEEP input files
            mksaeinp%fullpath = trim(rootp)//'sae_in_out_files/'
            call makedir(mksaeinp%fullpath)
         else
            ! creating one SWEEP input file
            mksaeinp%fullpath = trim(rootp)
         end if

         ! erosion submodel grid output
         if( am0efl .gt. 0 ) then
            mksaeout%fullpath = trim(rootp)//'sae_in_out_files/'
            call makedir(mksaeout%fullpath)
         end if

         ! set output flag
         in_weps = .true.

         ! begin erosion report simulation phase
         write(*,"(a)") "Starting erosion report phase"
         do am0jd = ijday,ljday

            ! store day for use in simulation date routines
            call update_simulation_date(ijday, am0jd)
            call get_simdate( cday(0), cmon(0), cyear(0) )

            ! print current day of simulation to screen periodically
             if( get_simdate_doy() .eq. 1 ) then
               simyrs = (ly - iy + 1)
               if( hb_freq .eq. 0 ) then
                  write(*,"(a,1x,i0,1x,a,1x,i0)") 'Erosion Year', cyear(0), 'of', simyrs
               else if( mod(cyear(0), hb_freq) .eq. 0 ) then
                  write(*,"(a,1x,i0,1x,a,1x,i0)") 'Erosion Year', cyear(0), 'of', simyrs
               end if
               call flush(OUTPUT_UNIT)
            end if

            ! set cli_today for time dependent barriers, air density
            call set_cli_today(am0jd)
            ! set wind_today for erosion
            call set_wind_today(am0jd)

            ! set the barrier interpolation in time
            call set_barrier_season(get_simdate_doy())

            ! set plot.out indicator flags (initialization complete so cellstate unaltered)
            call erodinit( noerod )

            if (awudmx .gt. 8.0) then ! if wind is great enough, call erosion

               ! check for creation of stand alone erosion input files on this day
               if( (saeinp_daysim .eq. get_simdate_daysim()) .or. (saeinp_jday .eq. am0jd) .or. (saeinp_all .gt. 0) ) then
                  mksaeinp%jday = am0jd
                  mksaeinp%simday = get_simdate_daysim()
               else 
                  mksaeinp%simday = 0
               end if

               ! create setting for multiple output files
               mksaeout%jday = am0jd
               mksaeout%simday = get_simdate_daysim()
               if (btest(am0efl,0) .or. btest(am0efl,1)) then
                  luo_egrd = 0   ! setting this here signals daily erodout to create a separate file for each erosion day
               endif
               if (btest(am0efl,2)) then
                  luo_emit = 0   ! setting this here signals erosion to create a separate file for each erosion day
               end if
               if (btest(am0efl,3)) then
                  luo_sgrd = 0   ! setting this here signals erosion to create a separate file for each erosion day
               end if

               ! write(*,*) "Start erosion"

               ! min_erosion_awu = 5.0 Minimum erosive wind speed (m/s) to evaluate for erosion loss
               ! SURF_UPD_FLG = 1      erosion surface updating (0 - disabled, 1 - enabled)
               call erosion( 5.0, 1, am0jd, noerod, cellstate )
            end if

            do isr = 1, nsubr
               call sci_cum( isr, cellstate )   ! Keep running total for soil conditioning index (SCI)
               call plotdata( isr, noerod(isr), manFile(isr), subrsurf(am0jd,isr), cellstate)  ! print to plot data file

               call update_daily_vars( isr, am0jd, cellstate )

            end do

         end do   ! end of "reporting" loop
      end if  ! end of run erosion

      ! Done with simulation here ..................

      ! area average values over simregion for 0 index reporting
      call sim_area_average( nsubr, ijday, ljday )

      ncycles = 1   ! set here for use in confidence interval calculation (no other use?)
      ci_year = 0  ! nothing has yet been printed into ci.out

      call timer(TIMEROD,TIMSTOP)
      call timer(TIMREPO,TIMSTART)

!$omp parallel do schedule(static)

      do isr = 0, nsubr

         ! populate
         do am0jd = ijday, ljday
            ! store day for use in simulation date routines
            call set_psimdate(isr, ijday, am0jd)
            cday(isr) = get_psim_day(isr)
            cmon(isr) = get_psim_mon(isr)
            cyear(isr) = get_psim_year(isr)

            ! Compute yrly values
            call update_yrly_update_vars( isr, am0jd, rep_update(isr)%yrly_update, &
                                          rep_update(isr)%yrot_update, rep_update(isr)%yr_update )
            if ( (cmon(isr) == 12) .and. (cday(isr) == 31) ) then          ! end of current year
               call update_yrly_report_vars(iy, cyear(isr), mandatbs(isr)%mperod, rep_update(isr)%yrly_update, &
                                            rep_update(isr)%yrot_update, rep_update(isr)%yr_update, &
                                            rep_report(isr)%yrly_report, rep_report(isr)%yr_report, &
                                            rep_dates(isr)%yrly)
            end if

            ! Compute monthly values
            call update_monthly_update_vars(isr, am0jd, cmon(isr), rep_update(isr)%monthly_update, rep_update(isr)%mrot_update)
            if (cday(isr) == lstday(cmon(isr),cyear(isr))) then                    ! end of current month
               call update_monthly_report_vars(cmon(isr), cyear(isr), mandatbs(isr)%mperod, &
                                               rep_update(isr)%monthly_update, rep_update(isr)%mrot_update, &
                                               rep_report(isr)%monthly_report, rep_dates(isr)%monthly)
            end if

            ! Compute half month values
            call update_hmonth_update_vars(isr, am0jd, cday(isr), cmon(isr), rep_update(isr)%hmonth_update, &
                                                                         rep_update(isr)%hmrot_update)
            if ((cday(isr) == 14) .or. (cday(isr) == lstday(cmon(isr),cyear(isr)))) then  ! end of half month
               call update_hmonth_report_vars(cday(isr), cmon(isr), cyear(isr), mandatbs(isr)%mperod, &
                    rep_update(isr)%hmonth_update, rep_update(isr)%hmrot_update, rep_report(isr)%hmonth_report)
            end if

            ! Compute period values
            call update_period_update_vars(isr, am0jd, rep_update(isr)%period_update)
            ! check for end of period and increment period counter
            if ( (cday(isr) == 14) .or. (cday(isr) == lstday(cmon(isr),cyear(isr))) &
               .or. ( (cday(isr) == rep_dates(isr)%period(pd(isr))%ed) .and. (cmon(isr) == rep_dates(isr)%period(pd(isr))%em) &
                      .and. ((mod((cyear(isr)-1),mandatbs(isr)%mperod)+1) == rep_dates(isr)%period(pd(isr))%ey) ) ) then
               ! end of period
               call update_period_report_vars( pd(isr), nperiods(isr), cyear(isr), mandatbs(isr)%mperod, &
                                            rep_update(isr)%period_update, rep_report(isr)%period_report, &
                                            rep_dates(isr)%period )
               ! Update the current period index
               if (pd(isr) == nperiods(isr)) then   ! Keep track of number of periods
                  pd(isr) = 1
               else
                  pd(isr) = pd(isr) + 1
               endif
            end if

            ! how many times have we passed maxper
            if( isr .eq. 0 ) then
              if( get_psim_doy(isr) .eq. get_psim_ndiy(isr) ) then
                if (mod(cyear(isr),maxper) == 0) then
                  ! calculate confidence interval
                  ! early exit not implemented
                  if( calc_confidence .gt. 0 ) then
                    ! uses    0.90    ! default confidence interval value
                    call confidence_interval(0.90, maxper, ncycles, ci_year, rep_report(isr)%yrly_report, rep_report(isr)%yr_report)
                  end if
                  ncycles = ncycles + 1
                end if
              endif
            end if

         end do

      end do

!$omp end parallel do

      ! place crop names associated with harvests into whole region mangement file
      call sync_harvcropnames( mandatbs )

      ! assemble and write output reports
      do isr = 0, nsubr   ! 0 is whole region, and then all subregion     
          if (report_debug >= 1) then
              call print_report_vars(nperiods(0), mandatbs(isr)%mperod, rep_report(isr), mandatbs(isr)%mandate)
          end if
          if (report_debug >= 2) then
              call print_yr_report_vars(mandatbs(0)%mperod, n_rot_cycles(0), rep_report(isr)%yr_report)
          end if
          if(  (.not. old_run_file .or. (nsubr .gt. 1)) .or. (isr .gt. 0) ) then

             call sci_report( isr, cellstate, soil )
             call print_ui1_output(luogui1(isr), nperiods(isr), mandatbs(isr)%mperod, n_rot_cycles(isr), rep_report(isr), &
                                   rep_dates(isr), mandatbs(isr)%mandate) !Use for new WEPS gui
             call print_mandate_output(luomandate(isr), mandatbs(isr)%mperod, mandatbs(isr)%mandate)
          end if
      end do

      if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
          call weppsum(isr,simyrs, wp(isr))
      endif

      call timer(TIMREPO,TIMSTOP)

      ! close all open files
      call closefils()

      ! deallocate barrier storage arrays
      if( allocated(barrier) ) then
          do isr = 1, size(barrier)
              ! clear internal storage for each barrier
              call destroy_barrier(barrier(isr))
          end do
          deallocate(barrier)
      end if

      ! deallocate seasonal barrier storage arrays
      if( allocated(barseas) ) then
          do isr = 1, size(barseas)
              ! clear internal storage for each seasonal barrier array
              call destroy_barrier(barseas(isr))
          end do
          deallocate(barseas)
      end if

      if( wepp_hydro .eq. 0 ) then
        ! allocate arrays used in darcy
        call deallocate_lsoda_sls1()
        call deallocate_lsoda_slsa()
        call deallocate_lsoda_sloc()
        call deallocate_lsoda_stoc()
        call deallocate_dvolw_param(nsubr)
      end if

      ! deallocate soil arrays
      sum_stat = 0
      do isr = 1, nsubr
        call deallocate_soil(soil_in(isr))
        call deallocate_soil(soil(isr))
      end do
      deallocate(soil_in, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(soil, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! deallocate subregion crop and residue pool arrays
      ! destroy layers
      do isr = 1, nsubr
         call plantDestroyAll(plants(isr)%plant)
         call destroy_biototal(croptot(isr))
         call destroy_biototal(restot(isr))
         call destroy_biototal(biotot(isr))
         call destroy_decomp_factors(decompfac(isr))
         call destroy_wepp_param(wp(isr))
         do am0jd = ijday,ljday
            call destroy_subregion_alloc(subrsurf(am0jd,isr))
         end do
      end do
      !deallocate subrsurf array
      deallocate(subrsurf, stat=alloc_stat)
      !deallocate management data arrays
      deallocate(mandatbs, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(lastoper, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      !remove main arrays
      deallocate(plants, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(croptot, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(restot, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(biotot, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(decompfac, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! crop end of season reporting variables
      deallocate(cprevseasonrotation, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(cprevrotation, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(cprevcalibrotation, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      deallocate(h1et, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(h1bal, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(wp, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(t_mperod, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: unable to deallocate crop and residue'
      end if

      ! remove debug local arrays
      call destroy_decomp_debug

      ! remove sci and stir arrays
      call destroy_sci_soil_multiplier
      call destroy_stir_soil_multiplier
      call destroy_stir_accumulators(nsubr)

      write (*,*) 'The WEPS simulation run is finished'

      call timer(TIMWEPS,TIMSTOP)
      call timer(0,TIMSTOP)
      call timer(0,TIMPRINT)

      stop
   end program weps

