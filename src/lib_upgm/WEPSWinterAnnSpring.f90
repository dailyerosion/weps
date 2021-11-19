!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSwinterAnnSpring_mod
  use Preprocess_mod
  use constants, only: dp, check_return, u_mgtokg
  use plant_mod
  use WEPSCrop_util_mod, only: chilluv, shootnum, shoot_delay, shoot_flg, per_release, stage_release, verndelmax, hard_spring
  implicit none

  type, extends(preprocess) :: WEPSwinterAnnSpring
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => winter_ann_spring_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSwinterAnnSpring

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPSwinterAnnSpring), intent(inout) :: self
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
      class(WEPSwinterAnnSpring), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine winter_ann_spring_proc(self, plnt, env)
      implicit none
      class(WEPSwinterAnnSpring), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.

      ! plant database
      real(dp) :: bctverndel ! thermal delay coefficient pre-vernalization

      ! environment

      ! plant state
      real(dp) :: bczgrowpt ! depth in the soil of the growing point (m)
      real(dp) :: bcthardnx ! hardening index for winter annuals (range from 0 t0 2)
      real(dp) :: bctwarmdays ! number of consecutive days that the temperature has been above the minimum growth temperature
      real(dp) :: bctchillucum ! accumulated chilling units (deg C day)
      integer(int32) :: dayspring
      logical :: can_regrow ! flag set to indicate that crop is able to regrow (past bc0hue, partition to root store)
      logical :: do_spring  ! flag set to indicate that spring release has been triggered

      ! locally computed values
      integer(int32) :: spring_flg

      ! Body of regrowth

      ! retrieve required inputs

      ! plant state
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( trim(self%processName) , "can_regrow", succ ) ) return

      ! check crop type for shoot growth action
      spring_flg = -1
      do_spring = .false.
      if( can_regrow ) then
        ! check winter annuals for completion of vernalization,
        ! warming and spring day length 

        ! plant state
        call plnt%state%get("zgrowpt", bczgrowpt, succ)
        if( .not. check_return( trim(self%processName) , "zgrowpt", succ ) ) return

        if( bczgrowpt .le. 0.0_dp ) then
          ! remember, negative number means above ground
          spring_flg = 1

          ! plant state
          call plnt%state%get("chill_unit_cum", bctchillucum, succ)
          if( .not. check_return( trim(self%processName) , "chill_unit_cum", succ ) ) return

          if( bctchillucum .ge. chilluv ) then
            spring_flg = 2

            ! plant database
            call plnt%pars%get("tverndel", bctverndel, succ)
            if( .not. check_return( trim(self%processName) , "tverndel", succ ) ) return

            ! plant state
            call plnt%state%get("warmdays", bctwarmdays, succ)
            if( .not. check_return( trim(self%processName) , "warmdays", succ ) ) return

            if( bctwarmdays .ge. shoot_delay*bctverndel/verndelmax) then
              spring_flg = 3

              ! plant state
              call plnt%state%get("harden_index", bcthardnx, succ)
              if( .not. check_return( trim(self%processName) , "harden_index", succ ) ) return

              !if( huiy .gt. spring_trig ) then
              !if( bcthardnx .le. 0.0 ) then
              if( bcthardnx .lt. hard_spring ) then
                spring_flg = 4

                ! plant state
                call plnt%state%get("dayspring", dayspring, succ)
                if( .not. check_return( trim(self%processName) , "dayspring", succ ) ) return

                ! vernalized and ready to grow in spring
                if( dayspring .eq. 0 ) then
                  ! value is not set, so set it
                  call env%state%get("day_of_year", dayspring, succ)
                  if( .not. check_return( trim(self%processName) , "day_of_year", succ ) ) return
                  ! should only be triggered once
                  do_spring = .true.

                  ! update plant state values
                  call plnt%state%replace("dayspring", dayspring, succ)
                  if( .not. check_return( trim(self%processName) , "dayspring", succ ) ) return

                end if
              end if
            end if
          end if
        end if
      end if

      ! update plant state values
      call plnt%state%replace("spring_flg", spring_flg, succ)
      if( .not. check_return( trim(self%processName) , "spring_flg", succ ) ) return
      call plnt%state%replace("do_spring", do_spring, succ)
      if( .not. check_return( trim(self%processName) , "do_spring", succ ) ) return

    end subroutine winter_ann_spring_proc

end module WEPSwinterAnnSpring_mod
