module PhenologyMMSGermination_mod
    use phases_mod
    use constants, only: dp, int32, check_return
    use UPGM_state

    implicit none

    type, extends(phase) :: PhenologyMMS_Germination
      contains
      procedure, pass(self) :: load => load_state
      procedure, pass(self) :: doPhase => germination ! may not need to pass self
      procedure, pass(self) :: register => germ_register
    end type PhenologyMMS_Germination

  contains

    subroutine load_state(self, phaseState)
      ! Variables
      implicit none
      class(PhenologyMMS_Germination), intent(inout) :: self
      type(hash_state), intent(inout) :: phaseState
      ! Body of loadState
      ! load phaseState into my state:
      self%phaseState = hash_state()
      call self%phaseState%init()
      call self%phaseState%clone(phaseState)
    end subroutine load_state

    subroutine germ_register(self, req_input, prod_output)
      ! Variables
      implicit none
      class(PhenologyMMS_Germination), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of germ_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine germ_register

    subroutine germination(self, plnt, env)
      implicit none
      class(PhenologyMMS_Germination), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp), dimension(:), allocatable :: soil_moisture, gdd_resp, gdd_curve
      real(dp) :: daygdd, stagegdd
      real(dp) :: soilmoistureplanting
      real(dp) :: requiredgdd
      integer(int32) :: p_depth, idx, tmp
      logical :: succ = .false.
      ! Body of germination
      call plnt%state%get("p_depth", p_depth, succ)
      if( .not. check_return( "p_depth", succ ) ) return
      call env%state%get("swc", soil_moisture, succ)
      if( .not. check_return( "swc", succ ) ) return
      call plnt%state%get("daygdd", daygdd, succ)
      if( .not. check_return( "daygdd", succ ) ) return
      call self%phaseState%get("stagegdd", stagegdd, succ)
      if( .not. check_return( "stagegdd", succ ) ) return
      ! phase specific parameters
      call self%phasePars%get("gdd_curve", gdd_curve, succ)
      if( .not. check_return( "gdd_curve", succ ) ) return
      call self%phasePars%get("gdd_resp", gdd_resp, succ)
      if( .not. check_return( "gdd_resp", succ ) ) return

      write(*,*) 'PhaseName: ', self%phaseName

      !NOTE: assume p_depth has been calculated as the appropriate index in the swc array.
      soilmoistureplanting = soil_moisture(p_depth)

      ! curve contains lower exclusive limit for that response range.
      ! 1) 45 (so > 45)
      ! 2) 35 (so > 35, <= 45 )
      ! 3) 25 (so > 25, <= 35)
      ! 4) -1 ( anything > 0 but <= 25)
      do idx = 1, size(gdd_curve)
        if (soilmoistureplanting >= gdd_curve(idx)) then
            requiredgdd = gdd_resp(idx)
            exit
        end if
      end do

      stagegdd = stagegdd + daygdd

      if (stagegdd >= requiredgdd) then
        ! Germination complete.
        ! stage over, reset stagegdd to 0, update plant stage pointer to next stage.
        daygdd = stagegdd - requiredgdd
        stagegdd = requiredgdd
        call self%phaseState%replace("stagegdd", stagegdd, succ)
        call plnt%state%replace("daygdd", daygdd, succ)
        ! update pointer here
        !plnt%next_phase() or something
        tmp = 1
        call plnt%state%replace("nextstage", tmp, succ)
        tmp = 0
        call plnt%state%replace("specstage", tmp, succ)
        ! done
        return
      else
        ! Germination not complete.
        call self%phaseState%replace("stagegdd", stagegdd, succ)
        return
      end if

    end subroutine germination

end module PhenologyMMSGermination_mod
