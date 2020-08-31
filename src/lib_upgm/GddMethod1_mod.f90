!$Author$
!$Date$
!$Revision$
!$HeadURL$

module gddmethod1_mod
    use Preprocess_mod
    use constants, only : dp, check_return
    use plant_mod
    implicit none

    type, extends(preprocess) :: gdd1_method
      contains
      procedure, pass(self) :: load => load_state
      procedure, pass(self) :: doProcess => gdd_process
      procedure, pass(self) :: register => register_proc
    end type gdd1_method

  contains

    subroutine load_state(self, processState)
      ! Variables
      implicit none
      class(gdd1_method), intent(inout) :: self
      type(hash_state), intent(inout) :: processState
      ! Body of loadState
      ! load processState into my state:
      self%processState = hash_state()
      call self%processState%init()
      call self%processState%clone(processState)
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
      class(gdd1_method), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: tmin, tmax, tbase, daygdd
      logical :: succ = .false.

      ! get temperatures
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( "tmin", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( "tmax", succ ) ) return
      call self%processPars%get("tbas", tbase, succ)
      if( .not. check_return( "tbas", succ ) ) return

      daygdd =  max(0.0_dp, ((tmax+tmin)/2) - tbase)

      call plnt%state%replace("daygdd", daygdd, succ)
      if( .not. check_return( "daygdd", succ ) ) return

    end subroutine gdd_process

end module gddmethod1_mod
