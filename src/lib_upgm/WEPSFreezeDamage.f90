module WEPSFreezeDamage_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: freeze_damage
  implicit none

  type, extends(preprocess) :: WEPSFreezeDamage
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => FreezeDamage ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSFreezeDamage

  contains

    subroutine load_state(self, process_state)
      implicit none
      class(WEPSFreezeDamage), intent(inout) :: self
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
      class(WEPSFreezeDamage), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine FreezeDamage(self, plnt, env)
      implicit none
      class(WEPSFreezeDamage), intent(in) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: thucum ! accumulated growing degree days
      real(dp) :: thum   ! Maximum accumulated grwoing degree days (maturity)
      real(dp) :: ehu0   ! fraction of season where senescence occurs
      real(dp) :: tsmn1  ! minimum temperature of surface soil layer
      real(dp) :: a_fr   ! parameter in the frost damage s-curve
      real(dp) :: b_fr   ! parameter in the frost damage s-curve
      real(dp) :: mstandleaf ! mass of standing leaf
      real(dp) :: fliveleaf ! mass of standing leaf
      real(dp) :: frst   ! the fraction of living leaf killed by freezing
      real(dp) :: lost_mass ! the amount of mass lost due to freeze damage
      logical :: succ = .false.

      ! get input values
      call plnt%state%get("thucum", thucum, succ)
      if( .not. check_return( "thucum", succ ) ) return
      call plnt%state%get("mstandleaf", mstandleaf, succ)
      if( .not. check_return( "mstandleaf", succ ) ) return
      call plnt%state%get("fliveleaf", fliveleaf, succ)
      if( .not. check_return( "fliveleaf", succ ) ) return

      call plnt%pars%get("thum", thum, succ)
      if( .not. check_return( "thum", succ ) ) return
      call plnt%pars%get("ehu0", ehu0, succ)
      if( .not. check_return( "ehu0", succ ) ) return
      call plnt%pars%get("a_fr", a_fr, succ)
      if( .not. check_return( "a_fr", succ ) ) return
      call plnt%pars%get("b_fr", b_fr, succ)
      if( .not. check_return( "b_fr", succ ) ) return

      call env%state%get("tsmn1", tsmn1, succ)
      if( .not. check_return( "tsmn1", succ ) ) return

      call freeze_damage( thucum/thum, ehu0, tsmn1, a_fr, b_fr, mstandleaf, fliveleaf, frst, lost_mass )

      call plnt%state%replace("mstandleaf", mstandleaf, succ)
      call plnt%state%replace("fliveleaf", fliveleaf, succ)
      call plnt%state%replace("frst", frst, succ)
      call plnt%state%replace("lost_mass", lost_mass, succ)

    end subroutine FreezeDamage

end module WEPSFreezeDamage_mod
