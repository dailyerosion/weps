module ritchieVernalization_mod
  use Preprocess_mod
  use constants, only: dp, int32, check_return
  use WEPSCrop_util_mod, only: chillunit_cum
  implicit none

  type, extends(preprocess) :: ritchieVernalization
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => Vernalization ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type ritchieVernalization

  contains

    subroutine load_state(self, process_state)
      implicit none
      class(ritchieVernalization), intent(inout) :: self
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
      class(ritchieVernalization), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of stage_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine Vernalization(self, plnt, env)
      implicit none
      class(ritchieVernalization), intent(in) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: chill_unit_cum  ! accumulated chill units for vernalization
      real(dp) :: tmax           ! Maximum temperature for this growth day
      real(dp) :: tmin           ! Minimum temperature for this growth day
      integer(int32) :: tmp
      logical :: succ = .false.

      ! initialized to zero at process beginning
      call plnt%state%get("chill_unit_cum", chill_unit_cum, succ)
      if( .not. check_return( "chill_unit_cum", succ ) ) return
      call env%state%get("tmax", tmax, succ)
      if( .not. check_return( "tmax", succ ) ) return
      call env%state%get("tmin", tmin, succ)
      if( .not. check_return( "tmin", succ ) ) return

      call chillunit_cum(chill_unit_cum, tmax, tmin)

      !write(*,*) 'Chill Units: ', chill_unit_cum, tmax, tmin

      call plnt%state%replace("chill_unit_cum", chill_unit_cum, succ)

    end subroutine Vernalization

end module ritchieVernalization_mod
