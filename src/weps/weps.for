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
!                                created soilinit, cliginit, ...
!

!     external continue_hdl
!     external common_handler
!     + + + GLOBAL COMMON BLOCKS + + +

      USE pd_dates_vars
      USE pd_update_vars
      USE pd_report_vars
      USE pd_var_tables

! build and release info, fpp created by cook
      include 'build.inc'

      include 'p1werm.inc'
      include 'wpath.inc'
      include 'p1unconv.inc'
      include 'm1subr.inc'
      include 'm1sim.inc'
      include 'm1geo.inc'
      include 'm1flag.inc'
      include 'm1dbug.inc'
      include 's1layr.inc'
      include 's1sgeo.inc'
      include 's1phys.inc'
      include 'c1info.inc'
      include 'c1gen.inc'
      include 'w1clig.inc'
      include 'w1wind.inc'
      include 'w1pavg.inc'
      include 'file.inc'

      include 'b1glob.inc'
      include 'd1glob.inc'
      include 'c1glob.inc'
      include 'c1db1.inc'
      include 'h1hydro.inc'
      include 'timer.inc'
      include 'command.inc'   !declarations for commandline args
      include 'precision.inc' !declaration for portable math range checking

!     + + + LOCAL COMMON BLOCKS + + +
      include 'main/main.inc'
      include 'manage/man.inc'
      include 'manage/oper.inc'
      include 'decomp/decomp.inc'
      include 'erosion/p1erode.inc' !Needs the SURF_UPD_FLG variable
      include 'erosion/m2geo.inc'   !Need tsterode cmdline arg vars(xgdpt,ygdpt)


!     + + + LOCAL VARIABLES + + +
      logical first
   
      integer :: dt(8)
      character(len=3) :: mstring
      common / datetime / dt, mstring
      save :: /datetime/

      integer get_nperiods
      integer pd, nperiods

      integer cd, cm, cy,                                               &
     &        end_init_jday, end_init_d, end_init_m, end_init_y,        &
     &        ndiy,                                                     &
     &        isr,                                                      &
     &        o_unit,                                                   &
     &        simyrs,                                                   &
     &        yrsim
      integer lcaljday, keep
      integer ci_flag, ci_year
      real    ci

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
!   clifil    - This variable holds the CLIGEN input file name.
!   daysim    - This variable holds the total current days of simulation.
!   id,im,iy  - The initial day, month, and year of simulation.
!   ijday     - The initial julian day of the simulation run.
!   isr       - This variable holds the subregion index.
!   iy        - Starting year of simulation run (used in management).
!   ld,lm,ly  - The last day, month, and year of simulation.
!   ljday     - The last julian day of the simulation run.
!   maxper    - The maximum number of years in a rotation of all
!               subregions.
!   ndiy      - The number of days in the year.
!   nsubr     - This variable holds the total number of subregions.
!   ngdpt     - This variable holds the total number of grig points in an
!               accounting region.
!   period    - The number of years in a management rotation.  This
!               variable is defined in (/inc/manage/man.inc)
!   simyrs    - The number of years in a simulation run excluding the
!               years for surface initialization.
!   subflg    - This logical variable is used to read header information
!               in the subdaily wind file (if .true., read header).
!   usrid     - This character variable is an identification string
!               to aid the user in identifying the simulation run.
!   usrloc    - This character variable holds a location
!               description of the simulation site.
!   usrnam    - This character variable holds the user name.
!   winfil    - This variable holds the WINDGEN input file name.
!   ci_flag   - determines when confidence interval is calculated in report loop
!               0 - no calculation
!               1 - calcuation called
!   ci_year   - indicates how many years of data have been printed into ci.out
!   ci        - confidence interval value (decimal)

!     + + + SUBROUTINES CALLED + + +
!     calcwu   -  Subdaily wind speed generation
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

!     + + + FUNCTIONS CALLED + + +
      integer dayear, julday, lstday
      logical isleap

!     + + + UNIT NUMBERS FOR INPUT/OUTPUT DEVICES + + +
!     * = screen and keyboard
!     1 = simulation run file
!     2 = general report output file
!     5 = Reserved
!     6 = Reserved - screen
!     7 = Reserved
!     8 = sub-daily wind speed data file
!     9 = SOIL/HYDROLOGY run file
!    10 = management (tillage) run file
!    11 = decomp run file (not now used)
!    12 = 'water.out'   - hourly hydrology output file
!    13 = 'temp.out'    - daily soil temperature output file
!    14 = 'hydro.out'   - daily hydrology output file
!    15 =    ?          - management output file
!    16 = 'soil.out'    - soil output file
!    17 = 'crop.out'    - crop output file
!    18 = 'dabove.out'  - decomp above ground output file
!    19 = 'dbelow.out'  - decomp below ground output file
!    20 = 'erod.out'    - erosion output file
       o_unit = 20
!    21 = 'eegt'        - period loss or deposition from grid point file
!    22 = 'eegtss       - period suspension loss from grid point file
!    23 = 'eegt10'      - period PM-10 loss from grid point file
!    24 = hourly wind distribution output file (subday.out)
!    25 = debug hydro
!    26 = debug soil
!    27 = debug crop
!    28 = debug decomp
!    29 = debug management
!    32 = 'plot.out'    - plotting output file
!    40 = temporary file to hold accounting region erosion values

!     + + + DATA INITIALIZATIONS + + +

      data yrsim /0/
      data lcaljday /0/
      data first /.true./

!     + + + END SPECIFICATIONS + + +

!     Before anything else is done, have WEPS output to stderr(?)
!     the build date and release/version information.

      write(6,*)
      write(6,*) 'WEPS ', trim(build_version)
      write(6,*) 'Release: ', trim(build_release)
      write(6,*) 'Built on: ', trim(build_date)
      write(6,*) 'Compiled with: ', trim(build_compiler)
      write(6,*) 'Compiled flags: ', trim(build_compiler_options)
      write(6,*)

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


      ! Print date of WEPS Run
12    format(1x,'Date of WEPS Run: ',a3,' ',i2.2,', ',i4,' ',           &
     &          i2.2,':',i2.2,':',i2.2)
      write(6,12) mstring, dt(3), dt(1), dt(5), dt(6), dt(7)
      write(6,*)

      call timer(0,TIMSTART)
      call timer(TIMWEPS,TIMSTART)

      ! initialziation for all of weps
      call wepsinit

      ! initialize anemometer defaults
      call anemometer_init

!     initialize math precision global variables
!     the factor here is due to the implementation of the EXP function
!     apparently, the limit is not the real number limit, but something else
!     this works in Lahey, but I cannot attest to it's portability
      max_real = huge(1.0) * 0.999150
      max_arg_exp = log(max_real)

      SURF_UPD_FLG = 1  ! enable surface updating by erosion submodel

      xgdpt = 0         !use default grid spacing values if
      ygdpt = 0         !these are not specified on the commandline

      erod_interval = 0 !default value for updating eroding soil surface
                        !(currently only used in standalone erosion submodel)
      calib_cycle = 0
      max_calib_cycles = 3  ! Default value unless increased via cmdline option
      calib_done = .false.

      SoilRockFragments(1) = -1  ! Setting default value to -1 (single subregion only for now!!!)

      ci = 0.90    ! default confidence interval value

!     Read command line arguments and options
      call cmdline()
      
      if (calibrate_crops > 3) max_calib_cycles = calibrate_crops

!     open input files and read run files
      call input

      write(*,*) "Made it here after input"

! save variabled for each subregion by JG
      do isr =1, nsubr 
      call save_soil(isr)  
      end do

      write(*,*) "Made it here after save_soil"

9898  continue    !Start of initialization section (calibration)

      ! moved from input.for so that IFC file can be re-read and re-initialized
      ! for calibration run purposes.
      ! call input_ifc  !Changed bck for now
      do  isr =1, nsubr   
      call restore_soil(isr)  !Assuming only one subregion for now
      end do

      write(*,*) "Made it here after restore_soil"
! add do loop for subregion JG
!     temporarily initialize old random roughness
      do isr =1, nsubr 
      aslrrc(isr) = 10.
      as0rrk(isr) = 0.9
      
 
      call openfils

      ! this prints header to plot.out file (isr not yet set)
      call plotdata(isr)  ! print to plot data file
      ! this prints header to decomp.out file (isr not yet set)
      call bpools(1,1,1,isr)

!     Initialize the management file and rotation counters
      call mfinit(isr, tinfil(isr), maxper)

!     check for consistency maxper, n_rot_cycles, number of years to run
      if( maxper*n_rot_cycles .ne. ly-iy+1 ) then
          write(*,*) 'Warning: Number of rotations (',n_rot_cycles,') ',&
     &               'times Years in rotation (',maxper,') ',           &
     &               ' does not match Number of simulation years (',    &
     &               ly-iy+1,') '
          n_rot_cycles = (ly-iy+1) / maxper
          if( mod( (ly-iy+1), maxper ) .gt. 0 ) then
              write(*,*) 'Warning: Not simulating complete rotations'
              n_rot_cycles = n_rot_cycles + 1
          end if
      end if
      if (calibrate_rotcycles .eq. 0) then
         calibrate_rotcycles = n_rot_cycles
      endif

!     This is all the initialization for the new output reporting code
      call mandates(isr)  !Get man dates, op names, and crop names
      nperiods = get_nperiods(maxper)   !Get # of periods for reports
      if( report_debug >= 1 ) then
          write(*,*) '# rot years', maxper, "nperiods", nperiods,       &
     &    '# cycles', n_rot_cycles
      end if
      call init_report_vars(nperiods, maxper, n_rot_cycles)
      pd = 1

      call asdini()
      call erodinit
      am0gdf = .true.
      end do    
! end of the subregion do loop

!     Likely that we will put all management data into memory
!     and only read and initialize everything here, looping through
!     each management file (one for each subregion).

!     Initializations unique to particular submodels
      do 10 isr=1,nsubr
         call decoinit(isr)
         call cropinit(isr)
         ! initialize all dependent variables
         call updres(isr)
         call sumbio(isr)
         call sci_init(isr)

!       Initialize the water holding capacity variable
        call hydrinit(isr)

!       initialize soil depth to bottom of layers (mm) from layer thickness (mm)
!       and initialize applied NO3
        call soilinit(isr)

   10 continue
! Subregion running loop
      do isr=1,nsubr   ! do multiple subregion      
      call cliginit     !read "yearly average info" from cligen header

!     calculate first and last Julian dates for simulation
      ijday = julday(id, im, iy)
      ljday = julday(ld, lm, ly)

!     calculate last julian date for initialization cycle
      end_init_d = 31
      end_init_m = 12
      end_init_y = iy + (maxper*init_cycle) - 1
      if( end_init_y .eq. 0 ) end_init_y = -1
      end_init_jday = julday(end_init_d, end_init_m, end_init_y)

      am0csr = 1  ! set global current subregion variable?

      if (init_cycle > 0) then   ! to avoid printing it when not being done
          write(6,*) "Starting initialization phase"
      else
          lopyr = 1
      endif

      ! begin initialization simulation phase
      init_loop = .true. ! Signifies that we are in the "initialization" loop
      do am0jd = ijday, end_init_jday   !will not enter if end before beginning

         call caldatw (cd, cm, cy)
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
!         isr = 1 !Note: we are no longer dealing with multiple subregions here
         call submodels(isr, cd, cm, cy)
! set initialization flag to .false. after first day
         if (am0ifl) am0ifl = .false.

         call plotdata(isr)  ! print to plot data file
       ! write decomposition biomass pool amounts to files
         call bpools(cd,cm,cy,isr)
!        write(*,*) 'weps:yrsim cd,cm,cy am0jd,daysim',                 &
!     &              yrsim," ",cd,cm,cy," ",am0jd,daysim

         ! if last day of year, check for end of rotation
         if (dayear(cd,cm,cy) .eq. ndiy) then
            ! check if at end of subregion's rotation cycle
            if (mod(amnryr(isr),maxper) == 0) then
               amnryr(isr) = 1
               lopyr = amnryr(isr)
               amnrotcycle(isr) = amnrotcycle(isr) + 1
            else
               amnryr(isr) = amnryr(isr) + 1
               lopyr = amnryr(isr)
            end if
         end if
      end do    ! end loop of multiple years
      init_loop = .false.
      write(6,*) "Finished initialization stage"
!     End of "initialization" section
! Do all resetting of variables necessary for "calibrate" and "report"
! portions of the simulation
      am0jd = ijday           ! Reset loop counter to first day of simulation
      daysim = 0              ! Reset to zero (blkdat.for)
      yrsim = 0               ! Reset to zero (weps.for)
      call getcli(0, cm, cy)  ! Reset cligen file (day == 0)
      call getwin(0, cm, cy)  ! Reset windgen file (day == 0)

!     Start of "calibrate" section
      keep = amnryr(isr)
      if ((calibrate_crops > 0) .and. (.not. calib_done) .and.          &
     &    (calib_cycle < max_calib_cycles))                         then
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
            call caldatw (cd, cm, cy)
            ! determine number of days in the year
            ndiy = 365; if (isleap(cy) .eqv. .true.) ndiy = 366
            call getcli(cd, cm, cy); call getwin(cd, cm, cy)
!           print current day of simulation to screen periodically
            daysim = daysim + 1
            if ((cm .eq. 1) .and. (cd .eq. 1)) then
               yrsim = yrsim + 1
               simyrs = (ly - iy + 1)
!               write(6,*) 'Year', yrsim, ' of', simyrs,                 &
!     &                    '(calibrating',calib_cycle,'/',               &
!     &                                              max_calib_cycles,')'
               write(6,*) 'Year', yrsim, ' of',                         &
     &                      maxper*calibrate_rotcycles,                 &
     &                    '(calibrating',calib_cycle,'/',               &
     &                                              max_calib_cycles,')'
                call flush(6)
            end if

!            isr = 1 !Note: we are no longer dealing with multiple subregions here
            call submodels(isr, cd, cm, cy)

            call plotdata(isr)  ! print to plot data file

            ! write decomposition biomass pool amounts to files
            call bpools(cd,cm,cy,1)

            ! set initialization flag to .false. after first day
            if (am0ifl) am0ifl = .false.

            ! if last day of year, check for end of rotation
            if (dayear(cd,cm,cy) .eq. ndiy) then
               ! check if at end of subregion's rotation cycle
               if (mod(amnryr(isr),maxper) == 0) then
                  amnryr(isr) = 1
                  lopyr = amnryr(isr)
                  amnrotcycle(isr) = amnrotcycle(isr) + 1
               else
                  amnryr(isr) = amnryr(isr) + 1
                  lopyr = amnryr(isr)
               end if
            end if
         end do   ! "calibration" phase
         amnryr(isr) = keep
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
         outcnt = 0
         amnryr = 1
         amnrotcycle = 1
         !mnsize = 0.09
         !mxsize = 601.2
         !ahzpta = 0
         am0dif = .true.
         am0eif = .true.
         am0sif = .true.
         am0cgf = .false.
         am0ifl = .false.
         am0jd = ijday           ! Reset loop counter to first day of simulation
         daysim = 0              ! Reset to zero (blkdat.for)
         yrsim = 0               ! Reset to zero (weps.for)
         call getcli(0, cm, cy)  ! Reset cligen file (day == 0)
         call getwin(0, cm, cy)  ! Reset windgen file (day == 0)

         goto 9898

! End of "calibrate" section
! Start of "report" section

      else
         amnrotcycle(isr) = 1   ! set here for use in confidence interval calculation (no other use?)
         am0sif = .false.  ! Done with all initialization and calibration phases
         ci_year = 0  ! nothing has yet been printed into ci.out

         ! begin report simulation phase
         write(6,*) "Starting report phase"
         report_loop = .true.  ! Signifies that we are in the "report" loop
         do am0jd = ijday,ljday

            call caldatw (cd, cm, cy)

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

!           if (am0jd.eq.ijday+1) call dbgdmp(daysim, isr)
!           if (am0jd.eq.ljday) call dbgdmp(daysim, isr)

 !           isr = 1 !Note: we are no longer dealing with multiple subregions here
            call submodels(isr, cd, cm, cy)
            if (run_erosion > 0) then   ! Are we simulating erosion in this RUN
               if (awudmx .gt. 8.0) then ! if wind is great enough, call erosion
                  ! write(*,*) "Start calcwu"
                  call calcwu
                  ! write(*,*) "Start erosion"
                  call erosion (5.0,isr)
                  if (btest(am0efl,0) .or. btest(am0efl,1)) then
                     call daily_erodout (luo_egrd, luo_erod,isr)
                  endif
               end if
            end if

            call sci_cum(isr)   ! Keep running total for soil conditioning index (SCI)
            call plotdata(isr)  ! print to plot data file
    ! write decomposition biomass pool amounts to files
            call bpools(cd,cm,cy,1)

!           write(*,*) 'weps:yrsim cd,cm,cy am0jd,daysim',              &
!    &              yrsim," ",cd,cm,cy," ",am0jd,daysim

            ! set initialization flag to .false. after first day
            if (am0ifl) am0ifl = .false.
       ! if last day of year, check for end of rotation
            if (dayear(cd,cm,cy) .eq. ndiy) then
               ! check if at end of subregion's rotation cycle
               if (mod(amnryr(isr),maxper) == 0) then
                  ! end of management rotation cycle
                  amnryr(isr) = 1
                  lopyr = amnryr(isr)
                  amnrotcycle(isr) = amnrotcycle(isr) + 1
                  ! trigger confidence interval calculation
                  ci_flag = 1
               else
                  ! continue through rotation cycle
                  amnryr(isr) = amnryr(isr) + 1
                  lopyr = amnryr(isr)
               end if
            end if

            ! Compute yrly values
            call update_yrly_update_vars()
            if ( (cm == 12) .and. (cd == 31) ) then          ! end of current year
               call update_yrly_report_vars(yrsim, maxper)
            end if

            ! Compute monthly values
            call update_monthly_update_vars(cm)
            if (cd == lstday(cm,cy)) then                    ! end of current month
               call update_monthly_report_vars(cm, yrsim, maxper)
            end if

            ! Compute half month values
            call update_hmonth_update_vars(cd, cm)
            if ((cd == 14) .or. (cd == lstday(cm,cy))) then  ! end of half month
               call update_hmonth_report_vars(cd, cm, yrsim, maxper)
            end if

            ! Compute period values
            call update_period_update_vars()
            ! print *, pd, "  ",cy,cm,cd,"  ", period_dates(pd)
            if ( (cd == 14) .or. (cd == lstday(cm,cy)) .or.             &
     &           ((cd == period_dates(pd)%ed) .and.                     &
     &            (cm == period_dates(pd)%em) .and.                     &
     &           ((mod((cy-1),maxper)+1) == period_dates(pd)%ey)) )     &
     &                                                              then
               ! end of period
               call update_period_report_vars                           &
     &                                  (pd,nperiods,cd,cm,yrsim,maxper)
               ! print *, "eop",pd,"  ",cy,cm,cd,"  ", period_dates(pd)
               ! Update the current period index
               if (pd == nperiods) then   ! Keep track of number of periods
                  pd = 1
               else
                  pd = pd + 1
               endif
            end if

            call clear_erosion()

            if( ci_flag .eq. 1) then
               ! calculate confidence interval
               ! early exit not implemented
               if( calc_confidence .gt. 0 ) then
                  call confidence_interval(ci, maxper, amnrotcycle(isr),&
     &                                     ci_year)
               end if
            endif
         end do   ! end of "reporting" loop
         report_loop = .false.
      end if
! End of "report" section
      end do 
! end for the do loop of subregions
! Done with simulation here ..................

      if (report_debug >= 1) then
          call print_report_vars(nperiods, maxper)
      end if
      if (report_debug >= 2) then
          call print_yr_report_vars(nperiods, maxper, n_rot_cycles)
      end if
      call sci_report
      call print_ui1_output(nperiods, maxper, n_rot_cycles) !Use for new WEPS gui
      call print_mandate_output(luomandate)
! call print_nui_output(nperiods, maxper) !Obsolete
! We need to create a "close files ()" call and remove/move all of this stuff - LEW
      close (luiwin)
      close (luicli)
      close (luiwsd)
      close (luogui1)
      close (luomandate)
      close (unit = 40)
      close (luoplt)
      if ((calc_confidence .gt. 0)) close (luoci)

!     output the weather summary report
!     call wsum

      write (*,*) 'The WEPS simulation run is finished'

      call timer(TIMWEPS,TIMSTOP)
      call timer(0,TIMSTOP)
      call timer(0,TIMPRINT)

      stop 'The WEPS simulation run is finished '
      end program weps

