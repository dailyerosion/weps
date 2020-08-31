!$Author$
!$Date$
!$Revision$
!$HeadURL$

module gddmethodWEPS_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: huc
  implicit none

  type, extends(preprocess) :: gddWEPS_method
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => gdd_process
    procedure, pass(self) :: register => proc_register
  end type gddWEPS_method

  contains

    subroutine load_state(self, processState)
      implicit none
      class(gddWEPS_method), intent(inout) :: self
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
      class(gddWEPS_method), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of register_proc
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine gdd_process(self, plnt, env)
      implicit none
      class(gddWEPS_method), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: tmin, tmax, tbase, topt, daygdd
      logical :: succ = .false.

      ! get temperatures
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( "tmin", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( "tmax", succ ) ) return
      call self%processPars%get("tbas", tbase, succ)
      if( .not. check_return( "tbas", succ ) ) return
      call self%processPars%get("topt", topt, succ)
      if( .not. check_return( "topt", succ ) ) return

      daygdd = huc( tmax, tmin, topt, tbase )

      call plnt%state%replace("daygdd", daygdd, succ)
      if( .not. check_return( "daygdd", succ ) ) return

    end subroutine gdd_process

end module gddmethodWEPS_mod
