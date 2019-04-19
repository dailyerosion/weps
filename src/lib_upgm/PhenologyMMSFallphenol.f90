module PhenologyMMSFallphenol_mod
  use phases_mod
  use constants, only: dp, int32, check_return
  use WEPSCrop_util_mod, only: chilluv, dev_floor
  use PhenologyMMS_mod, only: gdd_stressed_del, height_stressed
  implicit none

  type, extends(phase) :: PhenologyMMS_Fallphenol
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doPhase => pmms_fallphenol ! may not need to pass self
    procedure, pass(self) :: register => phase_register
  end type PhenologyMMS_Fallphenol

  contains

    subroutine load_state(self, phaseState)
      implicit none
      class(PhenologyMMS_Fallphenol), intent(inout) :: self
      type(hash_state), intent(inout) :: phaseState
      ! Body of loadState
      ! load phaseState into my state:
      self%phaseState = hash_state()
      call self%phaseState%init()
      call self%phaseState%clone(phaseState)
    end subroutine load_state

    subroutine phase_register(self, req_input, prod_output)
      ! Variables
      implicit none
      class(PhenologyMMS_Fallphenol), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of stage_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine phase_register

    subroutine pmms_fallphenol(self, plnt, env)
      implicit none
      class(PhenologyMMS_Fallphenol), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: stagegdd       ! GDD accumulated for this phase
      real(dp) :: phase_rel_gdd  ! relative GDD accumulated for this phase (1 means phase completion)
      real(dp) :: daygdd         ! GDD total for this growth day
      real(dp) :: height         ! height of plant
      real(dp) :: stress         ! level of stress for this day (0 = no stress, 1 = maximum stress)
      real(dp) :: chill_unit_cum ! accumulated chill units for vernalization
      real(dp) :: GN_trans_gdd   ! Non-stressed transition GDD
      real(dp) :: GN_stress      ! level of stress corresponding to Non-stressed transition GDD
      real(dp) :: GS_trans_gdd   ! Stressed transition GDD
      real(dp) :: GS_stress      ! level of stress corresponding to stressed transition GDD
      real(dp) :: tverndel      ! thermal delay coefficient pre-vernalization
      real(dp) :: vern_delay     ! vernalization delay
      real(dp) :: height_inc     ! potential height increase during this phase
      real(dp) :: begin_phase_rel ! phase_rel_gdd at beginning of day step
      integer(int32) :: i_setter
      logical :: succ = .false.
      ! Body of mms_fallphenol

      ! initialized to zero at phase beginning
      call self%phaseState%get("phase_rel_gdd", phase_rel_gdd, succ)
      if( .not. check_return( "phase_rel_gdd", succ ) ) return
      call self%phaseState%get("stagegdd", stagegdd, succ)
      if( .not. check_return( "stagegdd", succ ) ) return
      call plnt%state%get("height", height, succ)
      if( .not. check_return( "height", succ ) ) return
      call plnt%state%get("daygdd", daygdd, succ)
      if( .not. check_return( "daygdd", succ ) ) return
      call plnt%state%get("stress", stress, succ)
      if( .not. check_return( "stress", succ ) ) return
      call plnt%state%get("chill_unit_cum", chill_unit_cum, succ)
      if( .not. check_return( "chill_unit_cum", succ ) ) return

      call self%phasePars%get("GN_trans_gdd", GN_trans_gdd, succ)
      if( .not. check_return( "GN_trans_gdd", succ ) ) return
      call self%phasePars%get("GN_stress", GN_stress, succ)
      if( .not. check_return( "GN_stress", succ ) ) return
      call self%phasePars%get("GS_trans_gdd", GS_trans_gdd, succ)
      if( .not. check_return( "GS_trans_gdd", succ ) ) return
      call self%phasePars%get("GS_stress", GS_stress, succ)
      if( .not. check_return( "GS_stress", succ ) ) return
      call self%phasePars%get("tverndel", tverndel, succ)
      if( .not. check_return( "tverndel", succ ) ) return
      call self%phasePars%get("height_inc", height_inc, succ)
      if( .not. check_return( "height_inc", succ ) ) return

      begin_phase_rel = phase_rel_gdd

      vern_delay = max( dev_floor, min(1.0_dp, 1.0_dp - tverndel * (chilluv-chill_unit_cum) ) )

      ! if phase_rel_gdd exceeds 1.0, remainder of daygdd is returned, else daygdd is 0.0
      call gdd_stressed_del(phase_rel_gdd, stagegdd, daygdd, stress, GN_trans_gdd, GN_stress, GS_trans_gdd, GS_stress, vern_delay)

      call height_stressed(height, begin_phase_rel, phase_rel_gdd, stress, height_inc)

      if (phase_rel_gdd .ge. 1.0_dp) then
        ! update plant stage pointer to next stage.
        ! set control variables
        i_setter = 1
        call plnt%state%replace("nextstage", i_setter, succ)
        i_setter = 0
        call plnt%state%replace("specstage", i_setter, succ)
      end if

      ! return modified values
      call self%phaseState%replace("phase_rel_gdd", phase_rel_gdd, succ)
      call self%phaseState%replace("stagegdd", stagegdd, succ)
      call plnt%state%replace("height", stagegdd, succ)
      call plnt%state%replace("remgdd", daygdd, succ)
      return

    end subroutine pmms_fallphenol

end module PhenologyMMSFallphenol_mod
