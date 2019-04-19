module gddmethod1_mod
    use Preprocess_mod
    use constants, only : dp
    use plant_mod
    implicit none

    type, extends(preprocess) :: gdd1_method
      contains
      procedure, pass(self) :: load => load_state
      procedure, pass(self) :: doProcess => gdd_process
      procedure, pass(self) :: register => register_proc
    end type gdd1_method

  contains

    subroutine load_state(self, process_state)
      ! Variables
      implicit none
      class(gdd1_method), intent(inout) :: self
      type(hash_state), intent(inout) :: process_state
      ! Body of loadState
      ! load process_state into my state:
      self%process_state = hash_state()
      call self%process_state%init()
      call self%process_state%clone(process_state)
    end subroutine load_state
    
    subroutine register_proc(self, req_input, prod_output)
      ! Variables
      implicit none
      class(gdd1_method), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of register_proc
    end subroutine register_proc

    subroutine gdd_process(self, plnt, env)
      implicit none
      class(gdd1_method), intent(in) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: tmin, tmax, tbase, daygdd
      logical :: succ = .false.

      ! get tsMin
      call env%state%get("tmin", tmin, succ)
      call env%state%get("tmax", tmax, succ)
      call plnt%pars%get("tbase", tbase, succ)

      daygdd =  max(0.0_dp, ((tmax+tmin)/2) - tbase)
      call plnt%state%replace("daygdd", daygdd, succ)
    end subroutine gdd_process

end module gddmethod1_mod
