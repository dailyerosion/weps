module WEPSTempStress_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: temp_stress
  implicit none

  type, extends(preprocess) :: WEPSTempStress
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => TempStress ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSTempStress

  contains

    subroutine load_state(self, process_state)
      implicit none
      class(WEPSTempStress), intent(inout) :: self
      type(hash_state), intent(inout) :: process_state
      ! Body of loadState
      ! load process_state into my state:
      self%process_state = hash_state()
      call self%process_state%init()
      call self%process_state%clone(process_state)
    end subroutine load_state

    subroutine proc_register(self, req_input, prod_output)
      ! Variables
      implicit none
      class(WEPSTempStress), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine TempStress(self, plnt, env)
      implicit none
      class(WEPSTempStress), intent(in) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: tstress     ! temperature stress factor for growth
      real(dp) :: tmax   ! Maximum temperature for this growth day
      real(dp) :: tmin   ! Minimum temperature for this growth day
      real(dp) :: topt   ! optimum temperature for plant growth
      real(dp) :: tbase  ! minimum temperature for plant growth
      logical :: succ = .false.

      ! get input values
      call plnt%state%get("tstress", tstress, succ)
      if( .not. check_return( "tstress", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( "tmax", succ ) ) return
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( "tmin", succ ) ) return
      call plnt%pars%get("topt", topt, succ)
      if( .not. check_return( "topt", succ ) ) return
      call plnt%pars%get("tbase", tbase, succ)
      if( .not. check_return( "tbase", succ ) ) return

      tstress = temp_stress( tmax, tmin, topt, tbase )

      call plnt%state%replace("tstress", tstress, succ)

    end subroutine TempStress

end module WEPSTempStress_mod
