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

    integer :: run_rot_cycles ! number of rotation cycles

    integer :: id     ! initial simulation day of month
    integer :: im     ! initial simulation month of year
    integer :: iy     ! initial simulation year
    integer :: ld     ! final (last) simulation day of month
    integer :: lm     ! final (last) simulation month of year
    integer :: ly     ! final (last) simulation year

    character*512 :: rootp*512  ! the root path from which the weps command was started.

    integer :: daysim  ! current day number of the simulation run
    integer :: ijday   ! This variable contains the initial julian day of the simulation run.
    integer :: ljday   ! This variable contains the last julian day of the simulation run.
    integer :: maxper  ! The maximum number of years in a rotation cycle of all subregions.
                       ! All subregion rotation cycle period lengths (in years) must be a factor
                       ! in this value.  For example, 3 subregions with individual rotation
                       ! periods of 2, 3, and 4 years each would have a "maxper" value of 12
                       ! years.  Note that each of the individual subregion rotation periods can
                       ! divide evenly into the "maxper" value.
    integer :: ncycles ! a count of the number of maxper cycles that have been completed in the simulation run.

    logical :: init_loop    ! .true. indicates the simulation is in the initialization loop
    logical :: calib_loop   ! .true. indicates the simulation is in the calibration loop
    logical :: report_loop  ! .true. indicates the simulation is in the report loop

    integer :: max_calib_cycles ! Maximum number of calibration cycles to run either specified
                                ! as a commandline parameter value or a default value set in "weps.for".
    integer :: calib_cycle  ! identify the calibration "cycle" we are in
                            ! currently set and updated in "main/weps.for"
    logical :: calib_done   ! flag to identify when we are "done" with calibration
                            ! .true. then we are done with calibration
    logical :: am0ifl  ! flag to run initialization of submodels
                       ! .true. means initialization will be run

  contains

    subroutine wepsinit

      ! Initializes variables in common blocks

      use erosion_data_struct_defs, only: am0eif

      include 'p1werm.inc'
      include 'm1subr.inc'

      integer idx

      ! main/weps.for
      do idx = 1, mnsub
          amnryr(idx) = 1        ! m1subr.inc
      end do
      daysim = 0
      maxper = 1

      ! set initialization flags
      am0eif = .true.
      am0ifl = .true.

      ! set initialization, calibration, and report loop flags
      init_loop = .false.
      calib_loop = .false.
      report_loop = .false.

      return
    end subroutine wepsinit


end module weps_main_mod

