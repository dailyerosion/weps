module upgm_mod
    use plantcycle_mod
    use constants, only : int32
    use json_module
    use environment_state_mod
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    !  "class"
    type, public :: UPGM
        type(plantcycle), pointer :: plant
      contains
      ! "destructor"
!      final :: upgm_destructor
      procedure, public, pass(self) :: initialize => load_from_file, load_from_jsonrecord
      procedure, public, pass(self) :: grow => grow
      procedure, public, pass(self) :: preproc => preproc
    end type UPGM

    ! "constructor"
    interface upgm
      module procedure :: newugpm
    end interface upgm

    public :: upgm_delete
    interface upgm_delete
      module procedure upgm_destructor
    end interface upgm_delete


  contains

    function newugpm() result(self)
      implicit none
      type(upgm) :: self
      integer(int32) :: status
      allocate(self%plant, STAT = status)
      self%plant => plantcycle()
      print *, "constructed upgm", status
    end function newugpm

    subroutine load_from_file(self, fpath)
    ! Variables
    class(upgm), intent(inout) :: self
    character(len=*), intent(in) :: fpath

    type(json_file) :: jsonfile
    ! Body of load_from_record

    call jsonfile%initialize()
    call jsonfile%load_file(filename = fpath)
    if (jsonfile%failed()) then
        call jsonfile%print_error_message(error_unit)
        return
    endif
    call jsonfile%print_file()
    call jsonfile%destroy()
    end subroutine load_from_file

    subroutine load_from_jsonrecord(self, jrec)
    ! Variables
    class(upgm), intent(inout) :: self
    type(json_value),pointer, intent(in) :: jrec
    type(json_core) :: jcore
    ! Body of load_from_state

    ! someone/something constructed a json record that contains all the data we need.
    call jcore%initialize()
    !extract from passed rec to create model state.

    end subroutine load_from_jsonrecord

    subroutine preproc(self, env)
      ! Variables
      class(upgm), intent(inout) :: self
      type(environment_state) :: env
      ! Body of preproc
      call self%plant%preproc(env)
    end subroutine preproc
    
    subroutine grow(self, env)
      ! Variables
      class(upgm), intent(inout) :: self
      type(environment_state) :: env
      ! Body of grow
      call self%plant%grow(env)
    end subroutine grow


    subroutine upgm_destructor(self)
    implicit none
    type(upgm), intent(inout) :: self
    integer(int32) :: status
    if( associated(self%plant) ) then
      DEALLOCATE(self%plant, STAT=status)
      print *, "destructed upgm", status
    endif
    end subroutine upgm_destructor


end module upgm_mod
