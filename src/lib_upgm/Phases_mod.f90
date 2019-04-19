  module phases_mod
    use plant_mod
    use environment_state_mod
    use UPGM_state
    implicit none

    type, abstract :: phase
      character(len=40) :: phaseName ! please trim, all lower case.
      character(len=40) :: phaseLabel ! please trim, all lower case.
      integer :: phaseType ! 0 primary, 1 sets a state and next runs immediately, 2 secondary(sub) phase
                           ! 4 regrowth, 8 allows recall of a regrowth phase
      type(hash_state), public :: phasePars ! parameters initialized for this phase
      type(hash_state), public :: phaseState ! dynamic setup for phases
      class(phase), public, pointer :: phaseParent
      class(phase), public, pointer :: phaseChild
      class(phase), public, pointer :: phaseSub
      class(phase), public, pointer :: phaseRegrow
    contains
      procedure(generic_loadstate), deferred, pass(self) :: load
      procedure(generic_phase), deferred, pass(self) :: doPhase ! may not need to pass self
      procedure(generic_register), deferred, pass(self) :: register
    end type

    abstract interface

      subroutine generic_phase(self, plnt, env)
        import :: phase, plant, environment_state
        class(phase), intent(inout) :: self
        type(plant), intent(inout) :: plnt
        type(environment_state), intent(inout) :: env
      end subroutine generic_phase

      subroutine generic_register(self, req_input, prod_output)
        import :: phase, hash_state
        class(phase), intent(in) :: self
        type(hash_state), intent(inout) :: req_input
        type(hash_state), intent(inout) :: prod_output
      end subroutine generic_register

      subroutine generic_loadstate(self, phaseState)
        import :: phase, hash_state
        class(phase), intent(inout) :: self
        type(hash_state), intent(inout) :: phaseState
      end subroutine generic_loadstate

    end interface

  contains

  end module phases_mod
