!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSleafoff_mod
  use Preprocess_mod
  use constants, only: dp, check_return, u_mgtokg
  use plant_mod
  use WEPSCrop_util_mod, only: shootnum, shoot_delay, shoot_flg
  use solar_mod, only: N_fall_eqx, N_winter_sol
  use solar_mod, only: S_fall_eqx, S_winter_sol
  use solar_mod, only: amalat
  implicit none

  type, extends(preprocess) :: WEPSleafoff
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => leafoff_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSleafoff

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPSleafoff), intent(inout) :: self
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
      class(WEPSleafoff), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine leafoff_proc(self, plnt, env)
      implicit none
      class(WEPSleafoff), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.

      ! environment
      real(dp) :: hrlty  ! length of day (hours) yesterday
      real(dp) :: hrlt   ! length of day (hours) today

      ! plant state
      real(dp) :: bcmstandleaflive ! crop standing leaf mass (kg/m^2)
      real(dp) :: bcmstandleafdead ! crop standing leaf mass (kg/m^2)
      real(dp) :: bcmflatleaf  ! crop flat leaf mass (kg/m^2)
      real(dp) :: bgmflatleaf
      logical :: can_regrow ! flag set to indicate that crop is able to regrow

      integer(int32) :: bcdayleafoff
      integer(int32) :: bcdayleafon
      real(dp) :: bctcolddays ! number of days that the daily average temperature has been below the minimum growth temperature with decay
      real(dp) :: dropfrac

      ! locally computed values
      integer(int32) :: jd     ! simulation day of year
      integer :: fall_eqx
      integer :: winter_sol
      logical :: do_leafoff

      real(dp) :: dead_frac       ! fraction of leaf mass which is living
      real(dp) :: dead_drop_mass  ! mass of dead leaf mass to drop
      real(dp) :: live_drop_mass  ! mass of live leaf mass to drop

      ! Body of leafoff

      ! retrieve required inputs

      ! get plant state
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( trim(self%processName) , "can_regrow", succ ) ) return

      do_leafoff = .false.
      ! check crop type for shoot growth action
      if( can_regrow ) then

        ! get plant state
        call plnt%state%get("dayleafoff", bcdayleafoff, succ)
        if( .not. check_return( trim(self%processName) , "dayleafoff", succ ) ) return

        ! get environment variables
        call env%state%get("hrlty", hrlty, succ)
        if( .not. check_return( trim(self%processName) , "hrlty", succ ) ) return
        call env%state%get("hrlt", hrlt, succ)
        if( .not. check_return( trim(self%processName) , "hrlt", succ ) ) return

        ! check for fall conditions and leaf drop
        if( (hrlty .gt. hrlt) &
          ! days shortening (ie. fall)
          .and. (bcdayleafoff .eq. 0)  ) then
          ! fall not triggered yet

          ! set winter solstice based on latitude
          if( amalat .gt. 0.0d0 ) then
            fall_eqx = N_fall_eqx
            winter_sol = N_winter_sol
          else
            fall_eqx = S_fall_eqx
            winter_sol = S_winter_sol
          end if

          call env%state%get("day_of_year", jd, succ)
          if( .not. check_return( trim(self%processName) , "day_of_year", succ ) ) return

          if( jd .ge. fall_eqx ) then
            ! at least the first day of fall

            ! get plant state
            call plnt%state%get("colddays", bctcolddays, succ)
            if( .not. check_return( trim(self%processName) , "cold_days", succ ) ) return

            if(    (bctcolddays .ge. shoot_delay) &  ! enough cold to trigger leaf drop
              .or. (jd .eq. winter_sol) &         ! always drop leaves by winter solstice
              ) then
              ! cold days meet threshold

              ! get Process Parameters
              call self%processPars%get("dropfrac", dropfrac, succ)
              if( .not. check_return( trim(self%processName) , "dropfrac", succ ) ) return

              ! get plant state
              call plnt%state%get("mstandleaflive", bcmstandleaflive, succ)
              if( .not. check_return( trim(self%processName) , "mstandleaflive", succ ) ) return
              call plnt%state%get("mstandleafdead", bcmstandleafdead, succ)
              if( .not. check_return( trim(self%processName) , "mstandleafdead", succ ) ) return
              call plnt%state%get("mflatleaf", bcmflatleaf, succ)
              if( .not. check_return( trim(self%processName) , "mflatleaf", succ ) ) return

              ! drop fraction of leaf mass (range 0.0-1.0)
              ! drop fraction should include all dead leaves and then any live leaves to complete the fraction
              dead_frac = bcmstandleafdead / (bcmstandleaflive + bcmstandleafdead)
              dead_drop_mass = (bcmstandleaflive + bcmstandleafdead) * dropfrac
              if( dead_frac .ge. dropfrac ) then
                live_drop_mass = 0.0_dp
              else
                live_drop_mass = dead_drop_mass - bcmstandleafdead
                dead_drop_mass = bcmstandleafdead
              end if
              bgmflatleaf = bcmflatleaf + live_drop_mass + dead_drop_mass
              ! reset crop values
              bcmstandleaflive = bcmstandleaflive - live_drop_mass
              bcmstandleafdead = bcmstandleafdead - dead_drop_mass

              ! drop any remaining dead evergreen leaves into flat residue pool
              bgmflatleaf = bgmflatleaf + bcmstandleafdead
              ! reset crop values
              bcmstandleafdead = 0.0_dp
              bcmflatleaf = 0.0_dp
              ! activate leafoff resets in growth
              do_leafoff = .true.
              ! set day of year on which transition took place
              bcdayleafoff = jd
              ! reset triggers
              bcdayleafon = 0

              ! update changed plant state values
              call plnt%state%replace("dayleafoff", bcdayleafoff, succ)
              if( .not. check_return( trim(self%processName) , "dayleafoff", succ ) ) return
              call plnt%state%replace("dayleafon", bcdayleafon, succ)
              if( .not. check_return( trim(self%processName) , "dayleafon", succ ) ) return
              call plnt%state%replace("mstandleaflive", bcmstandleaflive, succ)
              if( .not. check_return( trim(self%processName) , "mstandleaflive", succ ) ) return
              call plnt%state%replace("mstandleafdead", bcmstandleafdead, succ)
              if( .not. check_return( trim(self%processName) , "mstandleafdead", succ ) ) return
              call plnt%state%replace("mflatleaf", bcmflatleaf, succ)
              if( .not. check_return( trim(self%processName) , "mflatleaf", succ ) ) return
              call plnt%state%replace("res_flatleaf", bgmflatleaf, succ)
              if( .not. check_return( trim(self%processName) , "res_flatleaf", succ ) ) return

            end if
          end if
        end if
      end if

      ! update plant state values
      call plnt%state%replace("do_leafoff", do_leafoff, succ)
      if( .not. check_return( trim(self%processName) , "do_leafoff", succ ) ) return

    end subroutine leafoff_proc

end module WEPSleafoff_mod
