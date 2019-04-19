module UPGM_state
    use json_module
    use constants, only : int32, dp
    implicit none
    private

    type, public :: hash_state
        type(json_core), private  :: core
        type(json_value), private, pointer :: base, jstate
        logical, private :: initialized = .false.
      contains
        final :: destructor
        procedure, public, pass(self) :: init => initHashState
        procedure, public, pass(self) :: clone => clonestate
        generic, public :: put => put_logical, put_integer, put_real, put_string, put_int_array, put_real_array, put_str_array
        generic, public :: get => get_logical, get_integer, get_real, get_string, get_int_array, get_real_array, get_str_array
        generic, public :: replace => update_logic, update_int, update_real, update_str, update_int_array, update_real_array
        procedure, pass(self), private :: put_logical, put_integer, put_real,put_string,put_int_array,put_real_array,put_str_array
        procedure, pass(self), private :: get_logical, get_integer, get_real,get_string,get_int_array,get_real_array,get_str_array
        procedure, pass(self), private :: update_logic, update_int, update_real, update_str, update_int_array, update_real_array
    end type hash_state

    interface hash_state
      module procedure :: newstate
    end interface hash_state

    contains

    subroutine clonestate(self, other_state)
    ! Variables
    class(hash_state), intent(inout) :: self
    type(hash_state), intent(inout) :: other_state
    ! Body of clonestate
    call self%core%destroy(self%jstate)
    call self%core%clone(other_state%jstate, self%jstate)
    end subroutine clonestate
    
    
    function newstate() result (state)
      implicit none
      type(hash_state) :: state
    end function newstate

    subroutine destructor(self)
      type(hash_state) :: self
      if (self%initialized) then
        call self%core%destroy(self%jstate)
        call self%core%destroy(self%base)
        self%initialized = .false.
      end if
    end subroutine destructor

    subroutine initHashState(self)
      class(hash_state), intent(inout) :: self
      call self%core%initialize()
      call self%core%create_object(self%base, '')
      call self%core%create_object(self%jstate, 'state')
      self%initialized = .true.
    end subroutine initHashState

    ! ********** logical ********************************
    subroutine put_logical(self, key, value, success)
      class(hash_state), intent(inout) :: self
      logical, target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, key, value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_logical

    subroutine get_logical(self, key, value, found)
      class(hash_state), intent(inout) :: self
      logical, target, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%get(self%jstate, key, value, found)
    end subroutine get_logical

    subroutine update_logic(self, key, value, found)
      class(hash_state), intent(inout) :: self
      logical, target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%update(self%jstate, key, value, found)
    end subroutine update_logic

    ! ********** Integers ********************************
    subroutine put_integer(self, key, value, success)
      class(hash_state), intent(inout) :: self
      integer(int32), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, key, value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_integer

    subroutine get_integer(self, key, value, found)
      class(hash_state), intent(inout) :: self
      integer(int32), target, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%get(self%jstate, key, value, found)
    end subroutine get_integer

    subroutine update_int(self, key, value, found)
      class(hash_state), intent(inout) :: self
      integer(int32), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%update(self%jstate, key, value, found)
    end subroutine update_int

    ! ********** IntegerArray ********************************
    subroutine put_int_array(self, key, value, success)
      class(hash_state), intent(inout) :: self
      integer(int32),dimension(:), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, key, value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_int_array

    subroutine get_int_array(self, key, value, found)
      class(hash_state), intent(inout) :: self
      integer(int32), dimension(:), allocatable, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%get(self%jstate, key, value, found)
    end subroutine get_int_array

    subroutine update_int_array(self, key, value, found)
      class(hash_state), intent(inout) :: self
      integer(int32),dimension(:), target, intent(in) :: value
      character(len=*), intent(in) :: key
      type(json_value), pointer :: array
      logical, intent(inout) :: found
      nullify(array)
      call self%core%get(self%jstate, key, array, found)
      if (found) then
        call self%core%remove(array, destroy=.true.)
        nullify(array)
        call self%core%add(self%jstate, key, value)
        found = (self%core%failed() .eqv. .false.)
      end if
    end subroutine update_int_array

    ! ********** Reals ********************************
    subroutine put_real(self, key, value, success)
      class(hash_state), intent(inout) :: self
      real(dp), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, trim(key), value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_real

    subroutine get_real(self, key, value, found)
      class(hash_state), intent(inout) :: self
      real(dp), target, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found
      call self%core%get(self%jstate, key, value, found)
    end subroutine get_real

    subroutine update_real(self, key, value, found)
      class(hash_state), intent(inout) :: self
      real(dp), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found
      call self%core%update(self%jstate, key, value, found)
    end subroutine update_real

    ! ********** RealArray ********************************
    subroutine put_real_array(self, key, value, success)
      class(hash_state), intent(inout) :: self
      real(dp),dimension(:), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, key, value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_real_array

    subroutine get_real_array(self, key, value, found)
      class(hash_state), intent(inout) :: self
      real(dp), dimension(:), allocatable, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%get(self%jstate, key, value, found)
    end subroutine get_real_array

    subroutine update_real_array(self, key, value, found)
      class(hash_state), intent(inout) :: self
      real(dp),dimension(:), target, intent(in) :: value
      character(len=*), intent(in) :: key
      type(json_value), pointer :: array
      logical, intent(inout) :: found
      nullify(array)
      call self%core%get(self%jstate, key, array, found)
      if (found) then
        call self%core%remove(array, destroy=.true.)
        nullify(array)
        call self%core%add(self%jstate, key, value)
        found = (self%core%failed() .eqv. .false.)
      end if
    end subroutine update_real_array

    ! ********** Strings ********************************
    subroutine put_string(self, key, value, success)
      class(hash_state), intent(inout) :: self
      character(len=*), intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, trim(key), value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_string

    subroutine get_string(self, key, value, found)
      class(hash_state), intent(inout) :: self
      character(len=:), allocatable, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%get(self%jstate, key, value, found)
    end subroutine get_string

    subroutine update_str(self, key, value, found)
      class(hash_state), intent(inout) :: self
      character(len=*), intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found
      call self%core%update(self%jstate, key, value, found)
    end subroutine update_str

    ! ********** StringArray ********************************
    subroutine put_str_array(self, key, value, success)
      class(hash_state), intent(inout) :: self
      character(len=*),dimension(:), target, intent(in) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: success

      call self%core%add(self%jstate, key, value)
      success = (self%core%failed() .eqv. .false.)
    end subroutine put_str_array

    subroutine get_str_array(self, key, value, found)
      class(hash_state), intent(inout) :: self
      character(len=*), dimension(:), allocatable, intent(inout) :: value
      character(len=*), intent(in) :: key
      logical, intent(inout) :: found

      call self%core%get(self%jstate, key, value, found)
    end subroutine get_str_array

    end module UPGM_state
