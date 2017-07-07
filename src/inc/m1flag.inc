!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!      file 'm1flag.inc'

!      this common block contains logical variables for the control of
!      initializations, printing of output, etc.

!     + + + VARIABLE DECLARATIONS + + +

      logical          am0ifl
!      logical          am0cif
!      logical          am0eif
!      logical          am0cgf
!      logical          am0dif
!      logical          am0sif
!      logical          am0oif
!      integer          am0hfl
!      integer          am0sfl
!      integer          am0tfl
!      integer          am0cfl
!      integer          am0dfl
!      integer          am0efl
!      logical          am0gdf
!      logical          am0til

!      logical          init_loop
!      logical          calib_loop
!      logical          report_loop

!      integer          am0defoliatefl
!      integer          am0kilfl
!      integer          am0cropupfl
!      integer          wind_gen_fmt_flag
!      integer          cli_gen_fmt_flag

      integer          wind_max_flag

      integer          max_calib_cycles
      integer          calib_cycle

      logical          calib_done


      common / m1flag /                                                 &
!     &                  am0ifl, am0eif, am0cgf, am0cif, am0dif, am0hfl, &
     &                  am0ifl,                                         &
!     &                  am0gdf, am0til, am0defoliatefl, am0kilfl,       &
!     &                  am0cropupfl, am0sfl, am0tfl, am0cfl, am0dfl,    &
!     &                  am0cropupfl,                                    &
!     &                  am0efl, am0oif, am0sif,                         &
!     &                  init_loop, calib_loop, report_loop, calib_done, &
     &                  max_calib_cycles, calib_cycle,                  &
!     &                  wind_gen_fmt_flag, cli_gen_fmt_flag,            &
     &                  wind_max_flag

      save :: /m1flag/

!     + + + VARIABLE DEFINITIONS + + +

!     am0ifl - flag to run initialization of submodels
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              initialization subroutines.
!
!     am0eif - flag to run initialization of EROSION
!              submodel
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              initialization subroutines.
!
!     am0cif - flag to run initialization of CROP
!              submodel at each crop planting
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              initialization subroutines.
!
!-----------------------------------------------------------------------
!!! NOTE: don't think am0sif and am0oif are currently used this way - LEW
!!!
!!! am0sif is currently being used to signal if we are in the "init_loop"
!!! I'm not exactly sure what am0oif is being used for at this time.
!!!
!!! I've guessed at what they "should" be used for, based upon the other
!!! flags using this naming structure - LEW
!!!
!     am0sif - flag to run initialization of SOIL
!              submodel
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              initialization subroutines.
!
!     am0oif - flag to run initialization of output
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              initialization subroutines.
!-----------------------------------------------------------------------
!
!
!     am0cgf - flag to run CROP growth if between plant
!              and harvest
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              growth subroutines.
!
!     am0dif - flag to run initialization of DECOMP
!              submodel at the start of simulation and harvest
!
!               values   -    .true.     .false.
!
!            - If variable is set to .true. then run
!              initialization subroutines.
!
!     am0hfl - flag to print HYDROlogy output
!
!                values  -    0, 1, 2, 3, 4,,5 ,6 ,7
!
!            - If variable is set tothe following then print
!              a detailed output from HYDROlogy submodel
!
!                  0 = no output
!                  1 = daily
!                  2 = hourly
!                  3 = daily and hourly
!                  4 = soil temperature
!                  5 = daily and soil temperature
!                  6 = hourly and soil temperature
!                  7 = daily, hourly, and soil temperature

!     am0sfl - flag to print SOIL output
!
!                values  -    0, 1
!
!            - If am0sfl = 1 then print detailed output,
!              if am0sfl = 0 then print no output

!     am0tfl - flag to print MANAGEMENT (TILLAGE) output

!                values  -   0, 1

!            - If am0tfl = 1 then print detailed output,
!              if am0tfl = 0 then print no output

!     am0cfl - flag to print CROP output

!                values  -    0, 1

!            - If am0cfl = 1 then print detailed output,
!              if am0cfl = 0 then print no output

!     am0dfl - flag to print DECOMP output

!                values  -    0, 1, 2, 3

!            - If am0dfl = 1 then print detailed output for above
!                            ground residue,
!            - If am0dfl = 2 then print detailed output for below
!                            ground residue,
!            - If am0dfl = 3 then print detailed output for both
!                            above and below ground residue,
!            - If am0dfl = 0 then print no output

!     am0efl - flag to print EROSION output

!                values  -    0, 1

!            - if am0efl = 0 then print no output
!              If am0efl = 1 then print detailed output,
!              if am0efl = 3  print heading and reset to val of 98  (WEPS)
!              if am0efl = 98 print hourly emissions for erosion day (WEPS)
!              if am0efl = 4  print heading and reset to val of 99  (tsterode)
!              if am0efl = 99 print period emissions for erosion day (tsterode)
!              if am0efl = 6 then call sb1out to print debug data to "tst.out"

!     am0gdf - flag to determine if grid has been calculated

!                values  -    .true., .false.

!            - If am0gdf = .true., determine if points are within acct region
!              if am0gdf = .false., do not

!     am0til - flag to determine if surfce has been updated by management

!                values  -    .true., .false.

!            - If am0til = .true., tillage has occurred
!              if am0til = .false., not

!-----------------------------------------------------------------------
!     init_loop - flag to identify when we are in the "initialization" loop
!
!               values   -    .true.     .false.
!
!            - If variable is .true. then we are in the initialization loop.
!
!     calib_loop - flag to identify when we are in the "calibration" loop
!
!               values   -    .true.     .false.
!
!            - If variable is .true. then we are in the calibration loop.
!     report_loop - flag to identify when we are in the "report" loop
!
!               values   -    .true.     .false.
!
!            - If variable is .true. then we are in the report loop.
!-----------------------------------------------------------------------
!     max_calib_cycles - Maximum number of calibration cycles to run
!                        Either specified as a commandline parameter
!                        value or a default value set in "weps.for".
!
!               values   -    3 or greater   
!                        
!     calib_cycle - identify the calibration "cycle" we are in
!                   currently set and updated in "main/weps.for"
!     calib_done - flag to identify when we are "done" with calibration
!
!               values   -    .true.     .false.
!
!            - If variable is .true. then we are done with calibration.
!-----------------------------------------------------------------------

!    am0cropupfl - flag to determine that the crop state has been changed
!               external to crop and that the crop update process must
!               run to synchronize dependent variable values with state
!               values

!    am0defoliatefl - flag to determine if leaf removal (defoliation) is
!                     to occur

!               values - 0 - does not defoliate
!                        1 - leaves killed and dropped to ground (defoliation)
!               type   - integer

!    am0kilfl - flag to determine if an operation is killing a perennial
!               or annual crop. Also used to indicate leaf removal
!               (defoliation) as of 8/23/00.

!               values  -  0 - does not kill anything
!                          1 - kills annual crop, but not perennial
!                          2 - kills annual and perennial crop
!                          3 - leaves killed and dropped to ground (defoliation)
!               type    -  integer

!     wind_gen_fmt_flag - flag indicating type of wind_gen daily input file
!                 wind_gen_fmt_flag = 1 --> original max/min wind_gen file format
!                 wind_gen_fmt_flag = 2 --> new hourly wind_gen file format
!
!     cli_gen_fmt_flag - flag indicating type of cli_gen daily input file
!                 cli_gen_fmt_flag = 1 --> original cli_gen file format
!                 cli_gen_fmt_flag = 2 --> new WEPP compatible cli_gen file format
!
!-----------------------------------------------------------------------
!     wind_max_flag - flag indicating whether input wind speeds should be capped
!                 wind_max_flag = 1 --> cap wind speeds to specified max value
