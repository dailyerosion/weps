    module environment_state_mod
    use UPGM_state
    use constants, only : int32, dp
    implicit none
    
    type, public :: environment_state
        type(hash_state) :: state
    contains
        procedure, pass(self) :: init => initEnvironment
    end type environment_state
    
    interface environment_state
        module procedure :: environment_state_init
    end interface environment_state

    contains

    function environment_state_init () result (env)
    type(environment_state) :: env
    end function environment_state_init
    
    subroutine initEnvironment(self)
    class(environment_state), intent(inout) :: self
    self%state = hash_state()
    call self%state%init()
    end subroutine initEnvironment
    
    end module environment_state_mod