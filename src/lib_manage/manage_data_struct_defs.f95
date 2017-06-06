!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_data_struct_defs

   type man_file_struct
      character(len=512) :: tinfil    ! management file name
      integer :: mperod               ! length of management of rotation
   end type man_file_struct

   type(man_file_struct), dimension(:), allocatable :: manFile

   !character(len=512), dimension(:), allocatable :: tinfil

   !integer, dimension(:), allocatable :: mperod

   integer, dimension(:), allocatable :: am0tfl    ! flag to print MANAGEment (TILLAGE) output
                                                     ! 0 = no output
                                                     ! 1 = detailed output file created
                                                     ! 2 = ASD output file(s) created
   integer, dimension(:), allocatable :: am0tdb    ! flag to print MANAGEment variables before and after the call to MANAGE
                                                     ! 0 = no output
                                                     ! 1 = output
   integer, dimension(:), allocatable :: asdhflag    ! flag to control printing ASD header info
                                                     ! 0 = ASD header line not yet printed
                                                     ! 1 = ASD header (first) line now printed
   integer, dimension(:), allocatable :: wchflag     ! flag to control printing WC header info
                                                     ! 0 = WC header line not yet printed
                                                     ! 1 = WC header (first) line now printed
   type last_operation
      integer  ::    day       ! The day of the last operation.
      integer  ::    mon       ! The month, and year of the last operation.
      integer  ::    yr        ! The year of the last operation.
      integer  ::    code       ! code indicating operation type
                                ! 0 - indicates an operation that will be run only mcount times
                                !     (normally used for initialzation)
                                ! 1 - triggers a read of tillage related operation parameters
                                !     (speed and direction)
      integer  ::    skip       ! used to skip all groups and processes in an operation that
                                ! has already completed mcount invocations
                                ! 0 - do not skip
                                ! 1 - skip
      character*80 :: name       ! name of current operation read from management file
      character*80 :: fuel       ! name of fuel used for operation
      real     ::    energyarea  ! diesel fuel equivalent energy required for operation Liters per hectare
      real     ::    stir        ! Operation Stir value (assigned from RUSLE2)

      character*80 grname       ! name of group read from management file
      integer  ::    grcode       ! group code designating which parameters will follow name
                                ! 1 - soil distrubance parameters
                                ! 2 - biomass manipulation
                                ! 3 - crop growth
                                ! 4 - ammendments
      real     ::    cutht        ! read from process as fraction or distance (flag controlled).
                                ! Converted to distance from ground up in meters by cut.for
   end type last_operation

   type(last_operation), dimension(:), allocatable :: lastoper 

! contains

end module manage_data_struct_defs

