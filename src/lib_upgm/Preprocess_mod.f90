    module Preprocess_mod
    use plant_mod
    use environment_state_mod
    implicit none
    type, abstract :: Preprocess
      character(len=40) :: processName ! please trim, all lower case.
      character(len=40) :: processLabel ! please trim, all lower case.
      type(hash_state), public :: process_state ! dynamic setup for processes
      class(Preprocess), public, pointer :: processNext
    contains
    procedure(generic_loadstate), deferred, pass(self) :: load
    procedure(generic_process), deferred, pass(self) :: doProcess
    procedure(generic_register), deferred, pass(self) :: register
    end type

    abstract interface

    subroutine generic_process(self, plnt, env)
    import :: Preprocess, plant, environment_state
    class(Preprocess), intent(in) :: self
    type(plant), intent(inout) :: plnt
    type(environment_state), intent(inout) :: env
    end subroutine generic_process

    subroutine generic_register(self, req_input, prod_output)
    import :: Preprocess, hash_state
    class(Preprocess), intent(in) :: self
    type(hash_state), intent(inout) :: req_input
    type(hash_state), intent(inout) :: prod_output
    end subroutine generic_register

    subroutine generic_loadstate(self, process_state)
    import :: Preprocess, hash_state
    class(Preprocess), intent(inout) :: self
    type(hash_state), intent(inout) :: process_state
    end subroutine generic_loadstate

    end interface

    end module Preprocess_mod
