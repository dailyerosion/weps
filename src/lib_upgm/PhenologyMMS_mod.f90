!$Author$
!$Date$
!$Revision$
!$HeadURL$

module PhenologyMMS_mod
  use constants, only: dp, int32
  implicit none

  contains

    subroutine gdd_stressed_del(phase_rel_gdd, stagegdd, daygdd, stress, GN_gdd, GN_stress, GS_gdd, GS_stress, vern_Delay)
      real(dp), intent(inout) :: phase_rel_gdd ! relative GDD accumulated for this phase (1 means phase completion)
      real(dp), intent(inout) :: stagegdd    ! GDD total for this growth phase
      real(dp), intent(inout) :: daygdd      ! GDD total for this growth day
      real(dp), intent(in) :: stress         ! level of stress for this day (0 = no stress, 1 = maximum stress)
      real(dp), intent(in) :: GN_gdd         ! Non-stressed period GDD
      real(dp), intent(in) :: GN_stress      ! level of stress corresponding to Non-stressed transition GDD
      real(dp), intent(in) :: GS_gdd         ! Stressed period GDD
      real(dp), intent(in) :: GS_stress      ! level of stress corresponding to stressed transition GDD
      real(dp), intent(in) :: vern_delay     ! vernalization delay

      real(dp) :: today_rel_gdd          ! relative gdd accumulated today
      real(dp) :: today_required_gdd     ! stress adjusted phase total GDD for today
      real(dp) :: today_rel_stress       ! relative stress for today

      if( stress .le. GN_stress ) then
        today_rel_stress = 0.0_dp
      else if( stress .ge. GS_stress ) then
        today_rel_stress = 1.0_dp
      else
        today_rel_stress = (stress-GN_stress) / (GS_stress-GN_stress)
      end if

      today_required_gdd =  today_rel_stress*GS_gdd + (1.0_dp-today_rel_stress)*GN_gdd

      today_rel_gdd = vern_delay * daygdd/today_required_gdd

      phase_rel_gdd = phase_rel_gdd + today_rel_gdd

      if( phase_rel_gdd .gt. 1.0_dp ) then
        ! phase complete, find daygdd remainder to pass back
        stagegdd = today_required_gdd
        daygdd = (phase_rel_gdd-1.0_dp) * today_required_gdd / vern_delay
        ! adjust phase_rel_gdd to max value
        phase_rel_gdd = 1.0_dp
      else
        stagegdd = stagegdd + vern_delay * daygdd
        daygdd = 0.0_dp
      end if

    end subroutine gdd_stressed_del

    !subroutine height_stressed(height, begin_phase_rel, phase_rel_gdd, stress, height_inc)
    !  real(dp), intent(inout) :: height
    !  real(dp), intent(in) :: begin_phase_rel
    !  real(dp), intent(in) :: phase_rel_gdd
    !  real(dp), intent(in) :: stress
    !  real(dp), intent(in) :: height_inc

    !  height = height + height_inc * stress

    !end subroutine height_stressed

end module PhenologyMMS_mod
