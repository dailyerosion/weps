!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSleafon_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  use WEPSCrop_util_mod, only: shoot_delay
  implicit none

  type, extends(preprocess) :: WEPSleafon
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => leafon_proc ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type WEPSleafon

  contains

    subroutine load_state(self, processState)
      implicit none
      class(WEPSleafon), intent(inout) :: self
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
      class(WEPSleafon), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine leafon_proc(self, plnt, env)
      implicit none
      class(WEPSleafon), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.

      ! environment
      real(dp) :: hrlty  ! length of day (hours) yesterday
      real(dp) :: hrlt   ! length of day (hours) today

      ! plant state
      real(dp) :: bcmstandstore ! crop standing storage mass (kg/m^2) (head with seed, or vegetative head (cabbage, pineapple))
      real(dp) :: bcmflatstore ! crop flat storage mass (kg/m^2)
      real(dp) :: bgmflatstore
      logical :: can_regrow ! flag set to indicate that crop is able to regrow
      logical :: shoot_growing ! flag set to indicate that shoot growth is occuring
      integer(int32) :: bcdayleafon ! day leafon was triggered (0 indicates not yet triggered this season, set to 0 by leafoff event)
      real(dp) :: bctwarmdays ! number of consecutive days that the temperature has been above the minimum growth temperature
      integer(int32) :: bcdayleafoff ! day leafoff was triggered (set to 0 by leafon event)

      ! locally computed values
      logical :: do_leafon

      ! Body of leafon

      ! retrieve required inputs

      ! plant state
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( trim(self%processName) , "can_regrow", succ ) ) return

      do_leafon = .false.
      ! check crop type for shoot growth action
      if( can_regrow ) then

        ! plant state
        call plnt%state%get("shoot_growing", shoot_growing, succ)
        if( .not. check_return( trim(self%processName) , "shoot_growing", succ ) ) return

        ! check for spring and leaf appearance
        if( .not. shoot_growing ) then
          ! heat units past emergence

          ! plant state
          call plnt%state%get("dayleafon", bcdayleafon, succ)
          if( .not. check_return( trim(self%processName) , "dayleafon", succ ) ) return

          ! environment variables
          call env%state%get("hrlty", hrlty, succ)
          if( .not. check_return( trim(self%processName) , "hrlty", succ ) ) return
          call env%state%get("hrlt", hrlt, succ)
          if( .not. check_return( trim(self%processName) , "hrlt", succ ) ) return

          if(     (hrlty .lt. hrlt) &
            ! days lengthening (ie. spring)
            .and. (bcdayleafon .eq. 0) ) then
            ! spring not yet triggered

            ! plant state
            call plnt%state%get("warmdays", bctwarmdays, succ)
            if( .not. check_return( trim(self%processName) , "warmdays", succ ) ) return

            if( bctwarmdays .ge. shoot_delay) then
              ! consecutive warm days meets threshold trigger leaf on
              ! drop any remaining reproductive into flat residue pool

              ! retrieve required inputs
              ! plant state
              call plnt%state%get("mstandstore", bcmstandstore, succ)
              if( .not. check_return( trim(self%processName) , "mstandstore", succ ) ) return
              call plnt%state%get("mflatstore", bcmflatstore, succ)
              if( .not. check_return( trim(self%processName) , "mflatstore", succ ) ) return

              bgmflatstore = bcmflatstore + bcmstandstore
              ! reset crop values
              bcmstandstore = 0.0
              bcmflatstore = 0.0
              ! activate leafon in growth
              do_leafon = .true.
              ! set day of year on which transition took place
              call env%state%get("day_of_year", bcdayleafon, succ)
              if( .not. check_return( trim(self%processName) , "day_of_year", succ ) ) return
              ! reset triggers
              bcdayleafoff = 0

              ! update plant state values
              call plnt%state%replace("mstandstore", bcmstandstore, succ)
              if( .not. check_return( trim(self%processName) , "mstandstore", succ ) ) return
              call plnt%state%replace("mflatstore", bcmflatstore, succ)
              if( .not. check_return( trim(self%processName) , "mflatstore", succ ) ) return
              call plnt%state%replace("res_flatstore", bgmflatstore, succ)
              if( .not. check_return( trim(self%processName) , "res_flatstore", succ ) ) return
              call plnt%state%replace("dayleafon", bcdayleafon, succ)
              if( .not. check_return( trim(self%processName) , "dayleafon", succ ) ) return
              call plnt%state%replace("dayleafoff", bcdayleafoff, succ)
              if( .not. check_return( trim(self%processName) , "dayleafoff", succ ) ) return

            end if
          end if
        end if

      end if

      ! update plant state values
      call plnt%state%replace("do_leafon", do_leafon, succ)
      if( .not. check_return( trim(self%processName) , "do_leafon", succ ) ) return

    end subroutine leafon_proc

end module WEPSleafon_mod
