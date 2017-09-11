!$Author$
!$Date$
!$Revision$
!$HeadURL$

      program weps

!     + + + PURPOSE + + +
!     This is the MAIN program of the Wind Erosion Prediction System.
!     Its purpose is to control the sequence of events during a
!     simulation run.

!     author: John Tatarko
!     version: 92.10

!     + + + KEY WORDS + + +
!     wind, erosion, hydrology, tillage, soil,
!     crop, decomposition, management
!
!       EDIT HISTORY
!       05-Feb-99       wjr     changed all open's to calls to fopenk
!       19-Feb-99       wjr     changed call to mfinit removing unit #
!       19-Feb-99       wjr     changed call to manage removing unit #
!       03-Mar-99       wjr     changed calls to hydro & soil to wrapper calls
!       04-Mar-99       wjr     changed call to crop to wrapper call
!       04-Mar-99       wjr     put plot and inithydr into separate files &
!                                removed all hydro includes
!       10-Mar-99       wjr     did lots of stuff -- put init code into separate
!                                files, put total loops into sep file, open into ...
!                                created cliginit, ...

!     external continue_hdl
!     external common_handler
!     + + + GLOBAL COMMON BLOCKS + + +

      use weps_main_mod, only: wepsinit, cmdline, &
                               daysim, ijday, ljday, maxper, ncycles, &
                               init_loop, calib_loop, report_loop, &
                               calibrate_crops, calibrate_rotcycles, max_calib_cycles, calib_cycle, calib_done, &
                               am0ifl, init_cycle, calc_confidence, report_debug, run_erosion, &
                               saeinp_all, saeinp_daysim, saeinp_jday
      use weps_interface_defs
      use wepp_interface_defs
      use timer_mod, only: timer, TIMWEPS, TIMSTART, TIMSTOP, TIMPRINT
      use datetime_mod, only: update_system_time, get_systime_string, julday, lstday, isleap, &
                              update_simulation_date, get_simdate, get_simdate_doy
      USE pd_dates_vars
      USE pd_update_vars
      USE pd_report_vars
      USE pd_var_tables
      use Polygons_Mod, only: destroy_polygon
      use subregions_mod, only: subr_poly, acct_poly
      use barriers_mod, only: barrier, barseas, minht_barriers, destroy_barrier, set_barrier_season
      use file_io_mod, only: luo_egrd, luo_emit, luo_sgrd, luogui1, luomandate, makedir
      use input_soil_mod, only: input_ifc, soil_in
      use soil_data_struct_defs, only: soil_def, allocate_soil, deallocate_soil
      use crop_mod, only: cropinit, cprevseasonrotation
      use report_harvest_mod, only: cprevrotation, cprevcalibrotation
      use biomaterial
      use debug_mod
      use mandate_mod
      use manage_data_struct_defs, only: lastoper, manFile
      use manage_mod, only: mfinit
      use erosion_mod, only: erosion, erodinit
      use erosion_data_struct_defs, only: create_subregionsoillayers, create_subregionsurfacewet, &
                                          subregionsurfacestate, threshold, cellsurfacestate, &
                                          erod_interval, awudmx, am0eif, am0efl, subrsurf
      use wind_mod, only: anemometer_init
      use precision_mod, only: precision_init
      use grid_mod, only: sbgrid, sbigrd, imax, jmax, ix, jy, xgdpt, ygdpt, amxsim
      use sae_in_out_mod, only: mksaeinp, mksaeout, in_weps
      use stir_soil_texture_mod, only: create_stir_soil_multiplier, destroy_stir_soil_multiplier
      use sci_soil_texture_mod, only: create_sci_soil_multiplier, destroy_sci_soil_multiplier
      use stir_report_mod, only: alloc_stir_accumulators, destroy_stir_accumulators, stir_report
      use sci_report_mod
      use hydro_data_struct_defs, only: hydro_derived_et
      use report_hydrobal_mod, only: h1bal
      use sim_area_average_mod, only: sim_area_average
      use wepp_param_mod
      use climate_input_mod, only: cliginit, getcli, windinit, getwin
      use input_run_mod, only: old_run_file, input, run_rot_cycles, id, im, iy, ld, lm, ly, rootp
      use lcm_mod, only: lcm_n
      use asd_mod, only: asdini

! build and release info, fpp created by cook
      include 'build.inc'
      include 'p1werm.inc'
      include 'h1hydro.inc'

!     + + + LOCAL VARIABLES + + +
      character(len=21) :: rundatetime

      integer, dimension(:), allocatable :: nperiods       ! number of reporting periods being accumulated
      integer, dimension(:), allocatable :: pd             ! index counter into reporting periods
      integer, dimension(:), allocatable :: n_rot_cycles   ! actual number of rotation cycles simulated
      integer, dimension(:), allocatable :: t_mperod       ! temporary array for management years

      integer, dimension(:), allocatable :: keep  ! in calibration, reset mnryr to precalibration loop value (why?)

      integer cd, cm, cy,                                               &
     &        end_init_jday, end_init_d, end_init_m, end_init_y,        &
     &        ndiy,                                                     &
     &        isr, ipl,                                                 &
     &        simyrs,                                                   &
     &        yrsim
      integer lcaljday
      integer ci_flag, ci_year
      real    ci

      integer :: SURF_UPD_FLG              ! erosion surface updating (0 - disabled, 1 - enabled)
      integer :: nsubr                     ! total number of subregions (read in inprun, derived from allocated subr_poly)

      type(soil_def), dimension(:), allocatable :: soil             ! structure with soil state and parameters as updated during simulation
      type(plants_struct), dimension(:), allocatable :: plants      ! array of pointers to structure for all biomaterial
                                                                    ! structure also references older biomaterial
      type(biomatter), dimension(:), allocatable :: crop            ! structure with crop state and parameters
      type(biototal), dimension(:), allocatable :: croptot          ! structure with totalized values of crop state
      type(bio_prevday), dimension(:), allocatable :: cropprev      ! structure with crop values from the previous day
      type(biomatter), dimension(:,:), allocatable :: residue       ! structure with residue state and parameters
      type(biototal), dimension(:), allocatable :: restot           ! structure with totalized values of residue state
      type(biototal), dimension(:), allocatable :: biotot           ! structure with totalized values of all biomass state
      type(decomp_factors), dimension(:), allocatable :: decompfac  ! structure with decompisition factors
      type(mandate_array), dimension(:), allocatable :: mandatbs    ! structure with management dates, operation names and crops

      type(threshold), dimension(:), allocatable :: noerod                 ! report values to show which factors prevented erosion
      type(cellsurfacestate), dimension(:,:), allocatable :: cellstate     ! grid cell state values (allocate in erodinit)

      type(reporting_report), dimension(:), target, allocatable :: rep_report
      type(reporting_update), dimension(:), target, allocatable :: rep_update
      type(reporting_dates), dimension(:), target, allocatable :: rep_dates
      type(hydro_derived_et), dimension(:), allocatable :: h1et   ! structure with reporting values for Evaporation/Transpiration
      type(wepp_param), dimension(:), allocatable :: wp           ! structure for wepp parameters by subregion

      integer :: alloc_stat, sum_stat
      integer :: am0jd   ! Current julian day of simulation

!     + + + LOCAL DEFINITIONS + + +

!   am0*fl    - These are switches for production of submodel
!               output, where the asterisk represents the first
!               letter of the submodel name.
!   am0ifl    - This variable is an initialization flag which is
!               set to .false. after the first simulation day.
!   cd        - The current day of simulation month.
!   cm        - The current month of simulation year.
!   cy        - The current year of simulation run.
!   end_init_jday - The last julian day of initialization
!   end_init_d - the last day of initialization
!   end_init_m - the last month of initialization
!   end_init_y - the last year of initialization
!   daysim    - This variable holds the total current days of simulation.
!   ijday     - The initial julian day of the simulation run.
!   isr       - This variable holds the subregion index.
!   ljday     - The last julian day of the simulation run.
!   maxper    - The maximum number of years in a rotation of all
!               subregions.
!   ndiy      - The number of days in the year.
!   ngdpt     - This variable holds the total number of grig points in an
!               accounting region.
!   simyrs    - The number of years in a simulation run excluding the
!               years for surface initialization.
!   usrid     - This character variable is an identification string
!               to aid the user in identifying the simulation run.
!   usrloc    - This character variable holds a location
!               description of the simulation site.
!   usrnam    - This character variable holds the user name.
!   ci_flag   - determines when confidence interval is calculated in report loop
!               0 - no calculation
!               1 - calcuation called
!   ci_year   - indicates how many years of data have been printed into ci.out
!   ci        - confidence interval value (decimal)

!     + + + SUBROUTINES CALLED + + +
!     caldat   -  Converts julian day to day, month, and year (cd,cm,cy)
!     cdbug    -  Prints global variables before and after call to CROP
!     crop     -  Crop submodel
!     ddbug    -  Prints global variables before and after call to DECOMP
!     decomp   -  Decomposition submodel
!     erodinit -  Erosion initialization routines
!     erosion  -  Erosion submodel
!     growhandles - Expands the number of handles allowed.
!     hdbug    -  Prints global variables before and after call to HYDRO
!     hydro    -  Hydrology submodel main program
!     input    -  Open files and perform input
!     manage   -  Management (tillage) submodel main program
!     mfinit   -  Management initialization subroutines
!     sdbug    -  Prints global variables before and after call to SOIL
!     soil     -  Soil submodel
!     asdini - aggregate size distribution initialization
!     bpools   -  prints many biomass pool components (for debugging)

!     + + + DATA INITIALIZATIONS + + +

      data yrsim /0/
      data lcaljday /0/

!     + + + END SPECIFICATIONS + + +

!     Before anything else is done, have WEPS output to stderr(?)
!     the build date and release/version information.

      write(6,*)
      write(6,*) 'WEPS ', trim(build_version)
      write(6,*) 'Release: ', trim(build_release)
      write(6,*) 'Built on: ', trim(build_date)
      write(6,*) 'Compiled with: ', trim(build_compiler)
      write(6,*) 'Compiled flags: ', trim(build_compiler_options)
      write(6,*) 'Built by user: ', trim(build_user)
      write(6,*) 'Reposity URL: ', trim(build_svn_repo_url)
      write(6,*) 'SVN repository Revision: ', trim(build_svn_repo_revision)
      write(6,*) 'SVN update Revision: ', trim(build_svn_updt_revision)
      write(6,*) 'Local and SVN Modfied Files: ', trim(build_cnt_mods)
      write(6,*)

      ! Determine date of Run
      call update_system_time

      ! Print date of Run
      rundatetime = get_systime_string() ! with Lahey f95, had to assign to variable first
      write(6,"(1x,'Date of WEPS run: ',a21)") rundatetime
      write(6,*)

      call timer(0,TIMSTART)
      call timer(TIMWEPS,TIMSTART)

      ! initialziation for all of weps
      call wepsinit

      ! initialize anemometer defaults
      call anemometer_init

      ! initialize math precision global variables
      call precision_init

      SURF_UPD_FLG = 1  ! enable surface updating by erosion submodel

      xgdpt = 0         !use default grid spacing values if
      ygdpt = 0         !these are not specified on the commandline

      erod_interval = 0 !default value for updating eroding soil surface
                        !(currently only used in standalone erosion submodel)
      calib_cycle = 0
      max_calib_cycles = 3  ! Default value unless increased via cmdline option
      calib_done = .false.

      ci = 0.90    ! default confidence interval value

!     Read command line arguments and options
      call cmdline()

      if (calibrate_crops > 3) max_calib_cycles = calibrate_crops

      ! open input files and read run files
      ! The argument soil_in is only accessed when reading the leagacy run file
      ! When rading the xml input, soil_in is accessed through the soil_def module.
      call input(soil_in)

      ! set total number of subregions from size of allocated subr_poly array
      nsubr = size(subr_poly)

      ! keep
      sum_stat = 0
      allocate(keep(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
     
      ! create sci and stir soil multiplier arrays (before input_ifc which needs them)
      call create_sci_soil_multiplier(nsubr)
      call create_stir_soil_multiplier(nsubr)
      call alloc_stir_accumulators(nsubr)

      ! erosion subregion surface values array
      allocate(subrsurf(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(noerod(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
!      allocate(soil_in(nsubr), stat=alloc_stat)
!      sum_stat = sum_stat + alloc_stat
      allocate(soil(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! done before allocations which use layers
      do isr = 1, nsubr 
         ! read soil file and setup layers
         call input_ifc(isr, soil_in(isr))
      end do

      ! allocate subregion crop and residue pool arrays
      allocate(plants(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(crop(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(cropprev(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(croptot(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(residue(mnbpls, nsubr), stat=alloc_stat)
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

      ! summary / total types are allocated with 0 for total simulation area
      allocate(restot(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(biotot(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! allocate management data arrays for reports
      allocate(mandatbs(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(lastoper(0:nsubr), stat=alloc_stat)
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

      do isr = 1, nsubr
         ! no plants yet
         nullify(plants(isr)%plant)
         ! complete allocation of layers
         crop(isr) = create_biomatter(soil_in(isr)%nslay)
         cropprev(isr) = create_bio_prevday(soil_in(isr)%nslay)
         croptot(isr) = create_biototal(soil_in(isr)%nslay)
         do ipl = 1, mnbpls
            residue(ipl,isr) = create_biomatter(soil_in(isr)%nslay)
         end do
         ! allocate layer arrays in totaling structures
         restot(isr) = create_biototal(soil_in(isr)%nslay)
         biotot(isr) = create_biototal(soil_in(isr)%nslay)
         decompfac(isr) = create_decomp_factors(soil_in(isr)%nslay)
         ! allocate layer and per/day in subregion surface state passed to erosion
         call create_subregionsoillayers(soil_in(isr)%nslay, subrsurf(isr))
         call create_subregionsurfacewet(24, subrsurf(isr))
         wp(isr) = create_wepp_param(soil_in(isr)%nslay)
      end do

      ! allocate debug local arrays
      call create_decomp_debug(nsubr)

      call openfils(residue)

      ! allocate soil layer arrays for soil(0)
      soil(0)%nslay = 1 ! only need one for report usage
      ! allocate layer arrays
      call allocate_soil(soil(0))

      do isr = 1, nsubr
          ! Likely that we will put all management data into memory
          ! and only read and initialize everything here, looping through
          ! each management file (one for each subregion).

          ! Initialize the management file and rotation counters
          call mfinit(manFile(isr))
          t_mperod(isr) = manFile(isr)%mperod
      end do

      ! find maxper, which is the least common multiple of the number of years in each rotation
      maxper = lcm_n( t_mperod )

!     check for consistency maxper, n_rot_cycles, number of years to run
      if( maxper*run_rot_cycles .ne. ly-iy+1 ) then
          write(*,*) 'Warning: Number of rotations (',run_rot_cycles,') ',&
     &               'times Years in rotation (',maxper,') ',           &
     &               ' does not match Number of simulation years (',    &
     &               ly-iy+1,') '
          run_rot_cycles = (ly-iy+1) / maxper
          if( mod( (ly-iy+1), maxper ) .gt. 0 ) then
              write(*,*) 'Warning: Not simulating complete rotations'
              run_rot_cycles = run_rot_cycles + 1
          end if
      end if
      if (calibrate_rotcycles .eq. 0) then
         calibrate_rotcycles = run_rot_cycles
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

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         ! check to see if grid dimensions specified via cmdline args
         if ((xgdpt > 0) .and. (ygdpt > 0)) then
           imax = xgdpt + 1
           jmax = ygdpt + 1
           ix = (amxsim(2)%x - amxsim(1)%x) / xgdpt
           jy = (amxsim(2)%y - amxsim(1)%y) / ygdpt
         else          !use Hagen's grid dimensioning as the default
           call sbgrid( minht_barriers() )
         endif

         ! allocate cellstate array to cover grid
         sum_stat = 0
         allocate(cellstate(0:imax,0:jmax), stat=alloc_stat)
         sum_stat = sum_stat + alloc_stat
         if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate enough memory for weps main data arrays'
         end if

         call erodinit( noerod, cellstate )
      endif

      do isr = 1, nsubr
         ! intialize for use in plotdata, -1 indicates it has not been set yet in management
         lastoper(isr)%yr = -1
         lastoper(isr)%mon = -1
         lastoper(isr)%day = -1
         ! this prints header to plot.out file
         call plotdata( isr, soil(isr), plants(isr)%plant, restot(isr), croptot(isr), biotot(isr), noerod(isr), &
                             manFile(isr), cellstate )
         ! this prints header to decomp.out file
         call bpools( isr, residue(1:size(residue,1),isr), restot(isr), biotot(isr), decompfac(isr) )
      end do

      call cliginit     ! read "yearly average info" from cligen header
      call windinit     ! allocate memory for reading subdaily wind velocities from windgen format input file

      call asdini()     ! calculates sieve cut parameters, does not set values

9898  continue    !Start of initialization section (calibration)

      ! Assign input soil values to changeable soil arrays.
      do  isr = 1, nsubr   
         soil(isr) = soil_in(isr)
      end do

!     Initializations unique to particular submodels
      do isr=1,nsubr
         do ipl = 1, mnbpls
            call decoinit(residue(ipl, isr), decompfac(isr))
         end do
         call decopen(isr) ! prints headers in above.out and below.out
         ! eliminate here as soon as all crop usages are removed
         call cropinit(isr, crop(isr)) ! no longer needed since plant created and initialized from P51
         ! initialize all dependent variables
         call updres(soil(isr), residue(1:size(residue,1), isr), restot(isr))
         ! Initialize the water holding capacity variable
         call hydrinit(isr, soil(isr), h1et(isr), h1bal(isr), wp(isr))
         ! initialize croptot variables
         call cropupdate( soil(isr)%aszrgh, soil(isr)%aszlyd, soil(isr)%nslay, &
                          ahztranspdepth(isr), ahzfurcut(isr), ahztransprtmin(isr), ahztransprtmax(isr), &
                          crop(isr), croptot(isr) )
         call sumbio(soil(isr), crop(isr), residue(1:size(residue,1), isr), restot(isr), croptot(isr), biotot(isr))

      !write(*,*) 'biotot, croptot, restot', biotot(isr), croptot(isr), restot(isr)

         if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
            call init_wepp(isr, 0, soil(isr))        ! specific wepp initializations
         end if
      end do

!     calculate first and last Julian dates for simulation
      ijday = julday(id, im, iy)
      ljday = julday(ld, lm, ly)

!     calculate last julian date for initialization cycle
      end_init_d = 31
      end_init_m = 12
      end_init_y = iy + (maxper*init_cycle) - 1
      if( end_init_y .eq. 0 ) end_init_y = -1
      end_init_jday = julday(end_init_d, end_init_m, end_init_y)

      if (init_cycle > 0) then   ! to avoid printing it when not being done
          write(6,*) "Starting initialization phase"
      else
        ! reset year value for next time through
        do isr=1,nsubr   ! do multiple subregion      
          lastoper(isr)%yr = 1
        end do
      endif

      ! begin initialization simulation phase
      init_loop = .true. ! Signifies that we are in the "initialization" loop
      do am0jd = ijday, end_init_jday   !will not enter if end before beginning

        ! store day for use in simulation date routines
        call update_simulation_date( am0jd )
        call get_simdate( cd, cm, cy )

        ! determine number of days in the year
        ndiy = 365; if (isleap(cy) .eqv. .true.) ndiy = 366
        call getcli(cd, cm, cy); call getwin(cd, cm, cy)
        ! print current day of simulation to screen periodically
        daysim = daysim + 1
        if ((cm .eq. 1) .and. (cd .eq. 1)) then
            yrsim = yrsim + 1
            simyrs = (end_init_y - iy + 1)
            write(6,*) 'Year', yrsim, ' of', simyrs, '(initialization)'
            call flush(6)
        end if
        do isr=1,nsubr
          ! do multiple subregion      
          call submodels(isr, soil(isr), crop(isr), cropprev(isr), residue(1:size(residue,1),isr), restot(isr), croptot(isr),  &
               biotot(isr), decompfac(isr), mandatbs(isr)%mandate, h1et(isr), h1bal(isr), wp(isr), manFile(isr))
          ! set initialization flag to .false. after first day
          if (am0ifl) am0ifl = .false.
          ! print to plot data file
          call plotdata(isr, soil(isr), plants(isr)%plant, restot(isr), croptot(isr), biotot(isr), noerod(isr), &
                             manFile(isr), cellstate)

          ! write decomposition biomass pool amounts to files
          call bpools(isr, residue(1:size(residue,1),isr), restot(isr), biotot(isr), decompfac(isr))

          ! if last day of year, check for end of rotation
          if (get_simdate_doy() .eq. ndiy) then
            ! check if at end of subregion's rotation cycle
            if (mod(manfile(isr)%mnryr,manFile(isr)%mperod) == 0) then
               manfile(isr)%mnryr = 1
               lastoper(isr)%yr = manfile(isr)%mnryr
            else
               manfile(isr)%mnryr = manfile(isr)%mnryr + 1
               lastoper(isr)%yr = manfile(isr)%mnryr
            end if
          end if
       end do  ! end for subregion loop
      end do    ! end loop of multiple years
      init_loop = .false.
      ! set initialization flag to .false. if initialization was skipped
      if (am0ifl) am0ifl = .false.
      write(6,*) "Finished initialization stage"
      ! End of "initialization" section

      ! Do all resetting of variables necessary for "calibrate" and "report" portions of the simulation
      am0jd = ijday           ! Reset loop counter to first day of simulation
      call update_simulation_date( am0jd ) ! store for use by simulation day routines
      daysim = 0              ! Reset to zero (blkdat.for)
      yrsim = 0               ! Reset to zero (weps.for)
      call getcli(0, cm, cy)  ! Reset cligen file (day == 0)
      call getwin(0, cm, cy)  ! Reset windgen file (day == 0)

      ! Start of "calibrate" section
      do isr=1,nsubr   ! do multiple subregion     
          keep(isr) = manfile(isr)%mnryr
      end do
      if ((calibrate_crops > 0) .and. (.not. calib_done) .and. (calib_cycle < max_calib_cycles)) then
         calib_cycle = calib_cycle + 1
         write(6,*) "Starting calibrate phase"
         calib_loop = .true. ! Signifies that we are in the calibration loop
         lcaljday = ljday
         if (calibrate_rotcycles .ne. max_calib_cycles) then
             !calculate last julian date for single calibration cycle
             lcaljday = julday(31, 12,(maxper*calibrate_rotcycles) )

             write(6,*) "CAL1 ",ljday, lcaljday, calibrate_rotcycles,   &
     &                         maxper, maxper*calibrate_rotcycles
         endif
             write(6,*) "CAL2 ",ljday, lcaljday, calibrate_rotcycles,   &
     &                         maxper, maxper*calibrate_rotcycles

         do am0jd = ijday,lcaljday

           ! store day for use in simulation date routines
           call update_simulation_date( am0jd )
           call get_simdate (cd, cm, cy)

           ! determine number of days in the year
           ndiy = 365; if (isleap(cy) .eqv. .true.) ndiy = 366
           call getcli(cd, cm, cy); call getwin(cd, cm, cy)
           ! print current day of simulation to screen periodically
           daysim = daysim + 1
           if ((cm .eq. 1) .and. (cd .eq. 1)) then
              yrsim = yrsim + 1
              simyrs = (ly - iy + 1)
              write(6,*) 'Year', yrsim, ' of', maxper*calibrate_rotcycles, &
                        '(calibrating',calib_cycle,'/', max_calib_cycles,')'
              call flush(6)
           end if

           do isr=1,nsubr   ! do multiple subregion     
             call submodels(isr, soil(isr), crop(isr), cropprev(isr), residue(1:size(residue,1),isr), restot(isr), croptot(isr), &
                 biotot(isr), decompfac(isr), mandatbs(isr)%mandate, h1et(isr), h1bal(isr), wp(isr), manFile(isr))
             ! print to plot data file
             call plotdata(isr, soil(isr), plants(isr)%plant, restot(isr), croptot(isr), biotot(isr), noerod(isr), &
                                manFile(isr), cellstate)

             ! write decomposition biomass pool amounts to files
             call bpools(isr, residue(1:size(residue,1),isr), restot(isr), biotot(isr), decompfac(isr))

             ! set initialization flag to .false. after first day
             if (am0ifl) am0ifl = .false.

             ! if last day of year, check for end of rotation
             if (get_simdate_doy() .eq. ndiy) then
               ! check if at end of subregion's rotation cycle
               if (mod(manfile(isr)%mnryr,manFile(isr)%mperod) == 0) then
                  manfile(isr)%mnryr = 1
                  lastoper(isr)%yr = manfile(isr)%mnryr
               else
                  manfile(isr)%mnryr = manfile(isr)%mnryr + 1
                  lastoper(isr)%yr = manfile(isr)%mnryr
               end if
             end if
           end do  ! end subregion
         end do   ! "calibration" phase
         do isr=1,nsubr   ! do multiple subregion     
             manfile(isr)%mnryr = keep(isr)
             ! at end of managment file, reset mcount
             manFile(isr)%mcount = 0
         end do
         calib_loop = .false.

!        if (calib_cycle == 1) then
!           if (too_high) then
!           else
!             !too low
!           end if
!           goto bracket
!        else if (still bracketing) then
!
!        else !calibrate
!
!        end if
! Go back to "initialization" and restart after resetting the appropriate variables here
         daysim = 0
         do isr = 1, nsubr
            manfile(isr)%mnryr = 1
         end do
         am0eif = .true.
         am0ifl = .false.
         am0jd = ijday           ! Reset loop counter to first day of simulation
         ! store day for use in simulation date routines
         call update_simulation_date( am0jd )
         daysim = 0              ! Reset to zero (blkdat.for)
         yrsim = 0               ! Reset to zero (weps.for)
         call getcli(0, cm, cy)  ! Reset cligen file (day == 0)
         call getwin(0, cm, cy)  ! Reset windgen file (day == 0)

         goto 9898

! End of "calibrate" section
! Start of "report" section

      else

         ! keep no longer needed
         deallocate(keep)
     
         if ((run_erosion.eq.2).or.(run_erosion.eq.3)) then
            do isr = 1, nsubr
               call init_wepp(isr, 1, soil(isr))        ! specific wepp initializations
            end do
         end if

         ncycles = 1   ! set here for use in confidence interval calculation (no other use?)
         ci_year = 0  ! nothing has yet been printed into ci.out

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
         if( btest(am0efl,1) ) then
            mksaeout%fullpath = trim(rootp)//'sae_in_out_files/'
            call makedir(mksaeout%fullpath)
         end if

         ! set output flag
         in_weps = .true.

         ! begin report simulation phase
         write(6,*) "Starting report phase"
         report_loop = .true.  ! Signifies that we are in the "report" loop
         do am0jd = ijday,ljday

            ! store day for use in simulation date routines
            call update_simulation_date( am0jd )
            call get_simdate (cd, cm, cy)

            ! determine number of days in the year
            ndiy = 365; if (isleap(cy) .eqv. .true.) ndiy = 366

            ! set confidence interval calculation flag
            ci_flag = 0

            call getcli(cd, cm, cy); call getwin(cd, cm, cy)

            ! print current day of simulation to screen periodically
            daysim = daysim + 1
            if ((cm .eq. 1) .and. (cd .eq. 1)) then
               yrsim = yrsim + 1
               simyrs = (ly - iy + 1)
               write(6,*) 'Year', yrsim, ' of', simyrs
               call flush(6)
            end if

            do isr=1,nsubr   ! do multiple subregion     
               !if (am0jd.eq.ijday+1) then
               !   call dbgdmp(daysim, isr, soil(isr), crop(isr), residue(1:size(residue,1),isr),croptot(isr),biotot(isr),h1et(isr))
               !end if
               !if (am0jd.eq.ljday) then
               !   call dbgdmp(daysim, isr, soil(isr), crop(isr), residue(1:size(residue,1),isr),croptot(isr),biotot(isr),h1et(isr))
               !end if

               call submodels(isr, soil(isr), crop(isr), cropprev(isr), residue(1:size(residue,1),isr), restot(isr), croptot(isr), &
                              biotot(isr), decompfac(isr), mandatbs(isr)%mandate, h1et(isr), h1bal(isr), wp(isr), manFile(isr))
            end do

            ! set the barrier interpolation in time
            call set_barrier_season(get_simdate_doy())

            if (run_erosion > 0) then   ! Are we simulating erosion in this RUN
               if (awudmx .gt. 8.0) then ! if wind is great enough, call erosion
                  ! transfer data values from submodel structures into erosion input structure
                  do isr=1,nsubr   ! do multiple subregion
                     call erodsubr_update( isr, soil(isr), crop(isr), restot(isr), croptot(isr), &
                                           biotot(isr), h1et(isr), subrsurf(isr) )
                  end do

                  ! check for creation of stand alone erosion input files on this day
                  if( (saeinp_daysim .eq. daysim) .or. (saeinp_jday .eq. am0jd) .or. (saeinp_all .gt. 0) ) then
                     mksaeinp%jday = am0jd
                     mksaeinp%simday = daysim
                  else 
                        mksaeinp%simday = 0
                  end if

                  ! create setting for multiple output files
                  mksaeout%jday = am0jd
                  mksaeout%simday = daysim
                  if (btest(am0efl,0) .or. btest(am0efl,1)) then
                     luo_egrd = -1   ! setting this here signals daily erodout to create a separate file for each erosion day
                  endif
                  if (btest(am0efl,2)) then
                     luo_emit = -1   ! setting this here signals erosion to create a separate file for each erosion day
                  end if
                  if (btest(am0efl,3)) then
                     luo_sgrd = -1   ! setting this here signals erosion to create a separate file for each erosion day
                  end if

                  ! write(*,*) "Start erosion"
                  call erosion( 5.0, SURF_UPD_FLG, subrsurf, noerod, cellstate )
               else
                  ! set plot.out indicator flags (initialization complete so cellstate unaltered)
                  call erodinit( noerod, cellstate )
               end if
            end if

            do isr=1,nsubr   ! do multiple subregion     
               if ((run_erosion .eq. 2) .or. (run_erosion .eq. 3)) then
                  call water_erosion( isr, cd, cm, cy, soil(isr), restot(isr), croptot(isr) )
               end if

               call sci_cum( isr, restot(isr), cellstate )   ! Keep running total for soil conditioning index (SCI)
               call plotdata( isr, soil(isr), plants(isr)%plant, restot(isr), croptot(isr), biotot(isr), noerod(isr), &
                                   manFIle(isr),cellstate)  ! print to plot data file
               ! write decomposition biomass pool amounts to files
               call bpools(isr, residue(1:size(residue,1),isr), restot(isr), biotot(isr), decompfac(isr))

!           write(*,*) 'weps:yrsim cd,cm,cy am0jd,daysim',              &
!    &              yrsim," ",cd,cm,cy," ",am0jd,daysim

               ! if last day of year, check for end of rotation
               if (get_simdate_doy() .eq. ndiy) then
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
            end do

            ! set initialization flag to .false. after first day
            if (am0ifl) am0ifl = .false.

            ! how many times have we passed maxper
            if (get_simdate_doy() .eq. ndiy) then
               if (mod(cy,maxper) == 0) then
                  ncycles = ncycles + 1
                  ! trigger confidence interval calculation
                  ci_flag = 1
               end if
            end if

            ! area average values over simregion for 0 index reporting
            call sim_area_average( subr_poly, h1et, h1bal, subrsurf, soil, croptot, restot, biotot )

            do isr = 0, nsubr   ! 0 is whole region, and then all subregion     
               ! Compute yrly values
               call update_yrly_update_vars( isr, rep_update(isr)%yrly_update, rep_update(isr)%yrot_update, &
                                             rep_update(isr)%yr_update, cellstate, h1et(isr) )
               if ( (cm == 12) .and. (cd == 31) ) then          ! end of current year
                  call update_yrly_report_vars(yrsim, mandatbs(isr)%mperod, rep_update(isr)%yrly_update, &
                                               rep_update(isr)%yrot_update, rep_update(isr)%yr_update, &
                                               rep_report(isr)%yrly_report, rep_report(isr)%yr_report, &
                                               rep_dates(isr)%yrly, rep_dates(isr)%yr)
               end if

               ! Compute monthly values
               call update_monthly_update_vars(isr, cm, rep_update(isr)%monthly_update, &
                                               rep_update(isr)%mrot_update, cellstate, h1et(isr))
               if (cd == lstday(cm,cy)) then                    ! end of current month
                  call update_monthly_report_vars(cm, yrsim, mandatbs(isr)%mperod, &
                       rep_update(isr)%monthly_update, rep_update(isr)%mrot_update, &
                       rep_report(isr)%monthly_report, rep_dates(isr)%monthly)
               end if

               ! Compute half month values
               call update_hmonth_update_vars(isr, cd, cm, rep_update(isr)%hmonth_update, rep_update(isr)%hmrot_update, h1et(isr))
               if ((cd == 14) .or. (cd == lstday(cm,cy))) then  ! end of half month
                  call update_hmonth_report_vars(cd, cm, yrsim, mandatbs(isr)%mperod, &
                       rep_update(isr)%hmonth_update, rep_update(isr)%hmrot_update, rep_report(isr)%hmonth_report)
               end if

               ! Compute period values
               call update_period_update_vars(isr, rep_update(isr)%period_update, soil(isr), &
                    restot(isr), croptot(isr), biotot(isr), cellstate, h1et(isr), h1bal(isr))
                                             
               ! print *, pd, "  ",cy,cm,cd,"  ", rep_dates(isr)%period(pd(isr))

            end do

            do isr = 0, nsubr   ! 0 is whole region, and then all subregion     
               ! check for end of period and increment period counter
               if ( (cd == 14) .or. (cd == lstday(cm,cy)) &
                  .or. ( (cd == rep_dates(isr)%period(pd(isr))%ed) .and. (cm == rep_dates(isr)%period(pd(isr))%em) &
                         .and. ((mod((cy-1),mandatbs(isr)%mperod)+1) == rep_dates(isr)%period(pd(isr))%ey) ) ) then
                  ! end of period
                  call update_period_report_vars( pd(isr), nperiods(isr), yrsim, mandatbs(isr)%mperod, &
                                               rep_update(isr)%period_update, rep_report(isr)%period_report, &
                                               rep_dates(isr)%period )
                  ! Update the current period index
                  if (pd(isr) == nperiods(isr)) then   ! Keep track of number of periods
                     pd(isr) = 1
                  else
                     pd(isr) = pd(isr) + 1
                  endif
               end if
            end do

            if( ci_flag .eq. 1) then
               ! calculate confidence interval
               ! early exit not implemented
               if( calc_confidence .gt. 0 ) then
                 call confidence_interval(ci, maxper, ncycles, ci_year, rep_report(0)%yrly_report, rep_report(0)%yr_report)
               end if
            endif

            ! set erosion accumulators on grid to zero in preparation for next day
            call sbigrd( cellstate )

         end do   ! end of "reporting" loop
         report_loop = .false.
      end if
! End of "report" section
! Done with simulation here ..................

      ! place crop names associated with harvests into whole region mangement file
      call sync_harvcropnames( mandatbs )

      ! write output reports
      do isr = 0, nsubr   ! 0 is whole region, and then all subregion     
          if (report_debug >= 1) then
              call print_report_vars(nperiods(0), mandatbs(isr)%mperod, rep_report(isr), mandatbs(isr)%mandate)
          end if
          if (report_debug >= 2) then
              call print_yr_report_vars(nperiods(0), mandatbs(0)%mperod, n_rot_cycles(0), rep_report(isr)%yr_report)
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

      ! close all open files
      call closefils(residue)

      ! deallocate accounting region polygon storage, no longer needed
      do isr = 1, size(acct_poly)
          ! free memory in polygon point arrays
          call destroy_polygon(acct_poly(isr))
      end do
      ! free memory for array of polygons
      deallocate(acct_poly)

      ! deallocate subregion polygon storage, no longer needed
      do isr = 1, nsubr
          ! free memory in polygon point arrays
          call destroy_polygon(subr_poly(isr))
      end do
      ! free memory for array of polygons
      deallocate(subr_poly)

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

      ! deallocate soil arrays
      do isr = 1, nsubr
        call deallocate_soil(soil_in(isr))
        call deallocate_soil(soil(isr))
      end do
      call deallocate_soil(soil(0))
      deallocate(soil_in, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(soil, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      ! deallocate subregion crop and residue pool arrays
      ! destroy layers
      do isr = 1, nsubr
         call destroy_biomatter(crop(isr))
         call destroy_bio_prevday(cropprev(isr))
         call destroy_biototal(croptot(isr))
         do ipl = 1, mnbpls
            call destroy_biomatter(residue(ipl,isr))
         end do
         call destroy_biototal(restot(isr))
         call destroy_biototal(biotot(isr))
         call destroy_decomp_factors(decompfac(isr))
         call destroy_wepp_param(wp(isr))
      end do
      sum_stat = 0
      !deallocate management data arrays
      deallocate(mandatbs, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(lastoper, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat

      !remove main arrays
      deallocate(crop, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(cropprev, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(croptot, stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      deallocate(residue, stat=alloc_stat)
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

