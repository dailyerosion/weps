!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSwarmdays_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: warmday_cum
  implicit none

  type, extends(preprocess) :: WEPSwarmdays
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => warmday_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSwarmdays

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPSwarmdays), intent(inout) :: self
      type(hash_state), intent(inout) :: processState
      ! Body of loadState
      ! load processState into my state:
      self%processState = hash_state()
      call self%processState%init()
      call self%processState%clone(processState)
    end subroutine load_state

    subroutine proc_register(self, req_input, prod_output)
      ! Variables
      implicit none
      class(WEPSwarmdays), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine warmday_proc(self, plnt, env)
      implicit none
      class(WEPSwarmdays), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: warmdays       ! accumulated warm days
      real(dp) :: tbase          ! minimum growth temperature
      real(dp) :: tmax           ! Maximum temperature for this growth day
      real(dp) :: tmin           ! Minimum temperature for this growth day
      logical :: succ = .false.

      ! get input values
      call plnt%state%get("warmdays", warmdays, succ)
      if( .not. check_return( "warmdays", succ ) ) return
      call self%processPars%get("tbas", tbase, succ)
      if( .not. check_return( "tbas", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( "tmax", succ ) ) return
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( "tmin", succ ) ) return

      call warmday_cum(warmdays, tbase, tmax, tmin)

      call plnt%state%replace("warmdays", warmdays, succ)
    end subroutine warmday_proc

end module WEPSwarmdays_mod
