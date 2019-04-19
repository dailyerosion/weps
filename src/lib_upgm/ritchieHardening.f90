module ritchieHardening_mod
  use Preprocess_mod
  use constants, only: dp, check_return
  use plant_mod
  implicit none

  type, extends(preprocess) :: ritchieHardening
    contains
    procedure, pass(self) :: load => load_state
    procedure, pass(self) :: doProcess => Hardening ! may not need to pass self
    procedure, pass(self) :: register => proc_register
  end type ritchieHardening

  contains

    subroutine load_state(self, process_state)
      implicit none
      class(ritchieHardening), intent(inout) :: self
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
      class(ritchieHardening), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
      ! Body of proc_register
      ! add stuff here the component requires and any outputs it will generate.
    end subroutine proc_register

    subroutine Hardening(self, plnt, env)
      implicit none
      class(ritchieHardening), intent(in) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env
      real(dp) :: harden_index   ! hardening index for winter hardiness
      real(dp) :: tmax           ! Maximum temperature for this growth day
      real(dp) :: tmin           ! Minimum temperature for this growth day
      real(dp) :: leaf2stor      ! fraction of assimilate partitioned to leaf that is diverted to root store
      real(dp) :: stem2stor      ! fraction of assimilate partitioned to stem that is diverted to root store
      real(dp) :: stor2stor      ! fraction of assimilate partitioned to standing storage
                                 ! (reproductive) that is diverted to root store
      logical :: succ = .false.

      ! get input values
      call plnt%state%get("harden_index", harden_index, succ)
      if( .not. check_return( "harden_index", succ ) ) return
      call env%state%get("tsmx1", tmax, succ)
      if( .not. check_return( "tsmx1", succ ) ) return
      call env%state%get("tsmn1", tmin, succ)
      if( .not. check_return( "tsmn1", succ ) ) return
      call plnt%pars%get("leaf2stor", leaf2stor, succ)
      if( .not. check_return( "leaf2stor", succ ) ) return
      call plnt%pars%get("stem2stor", stem2stor, succ)
      if( .not. check_return( "stem2stor", succ ) ) return
      call plnt%pars%get("stor2stor", stor2stor, succ)
      if( .not. check_return( "stor2stor", succ ) ) return

      if(    (leaf2stor .gt. 0.0_dp) &
        .or. (stem2stor .gt. 0.0_dp) &
        .or. (stor2stor .gt. 0.0_dp) ) then
        call freezeharden(harden_index, tmax, tmin)

        !write(*,*) 'Hardening: ', harden_index, tmax, tmin

      end if

      call plnt%state%replace("harden_index", harden_index, succ)
    end subroutine Hardening

    ! calculates the freeze hardening index for the day. The input value
    ! is modified to reflect the effect of temperature on either increasing
    ! or decreasing the index. Stage 1 hardening occurs when the plant
    ! experiences cool temperatures from -1 to 8 degrees C. Stage 2 hardening
    ! occurs only after stage 1 is complete and temperatures fall below
    ! freezing.

    ! method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
    ! Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
    ! Agronomy Monograph 31, pages 40-42, 52

    subroutine freezeharden( bcthardnx, day_max_temp, day_min_temp )
      real(dp), intent(inout) :: bcthardnx   ! hardening index for winter annuals (range from 0 t0 2)
      real(dp), intent(in) :: day_max_temp   ! daily maximum temperature (deg.C)
      real(dp), intent(in) :: day_min_temp   ! daily minimum temperature (deg.C)

      ! note: input crown temperature rather than air temperature for best results

      ! local variables
      real(dp) :: tavg   ! daily everage temperature (deg.C)
      real(dp) :: hinc   ! daily hardening increment

      ! parameters
      real(dp), parameter :: t1min = -1.0_dp  ! minimum temperature in stage 1 index calculation(deg.C)
      real(dp), parameter :: t1opt = 3.5_dp   ! optimum temperature in stage 1 index calculation(deg.C)
      real(dp), parameter :: t1max = 8.0_dp   ! maximum temperature in stage 1 index calculation(deg.C)
      real(dp), parameter :: t2max = 0.0_dp   ! maximum temperature in stage 2 index calculation(deg.C)
      real(dp), parameter :: tbase = 0.0_dp   ! base temperature for hardening effects(deg.C) (like base growth temperature)
      real(dp), parameter :: tdeh = 10.0_dp   ! temperature above which dehardening can occur (deg.C)
      real(dp), parameter :: hs1 = 1.0_dp     ! index value at completion of stage 1 hardening
      real(dp), parameter :: hs2 = 2.0_dp     ! index value at completion of stage 2 hardening
      real(dp), parameter :: deht = 0.02_dp   ! index reduction multiplier for dehardening temperature excess
      real(dp), parameter :: hardinc1 = 0.1_dp  ! stage 1 hardening index increment
      real(dp), parameter :: hardinc2 = 0.083_dp  ! stage 2 hardening index increment

      ! find average temperature
      tavg = 0.5_dp * (day_max_temp + day_min_temp)

      if( bcthardnx .ge. hs1 ) then
          ! stage 1 complete, into stage 2
          if( tavg .le. tbase + t2max ) then
              ! add stage 2 amount to index
              bcthardnx = bcthardnx + hardinc2
          end if
          if( day_max_temp .ge. tbase + tdeh ) then
              ! stage 2 dehardening
              hinc = deht * (tbase + tdeh - day_max_temp)
              bcthardnx = bcthardnx + hinc
              if( bcthardnx .ge. hs1 ) then
                  ! still in stage 2, take off some more
                  bcthardnx = bcthardnx + hinc
              end if
          end if
          bcthardnx = max( bcthardnx, 0.0_dp)
          bcthardnx = min( bcthardnx, hs2)

      else if( tavg .ge. tbase + t1min) then
          ! stage 1 hardening
          if( tavg .le. tbase + t1max ) then
              ! add stage 1 amount to index, minus deduction for being on either side of optimum
              bcthardnx = bcthardnx + hardinc1                          &
     &                  - ((tavg - (tbase + t1opt))**2_dp) / 506.0_dp
              if( bcthardnx .ge. hs1 ) then
                  ! stage 1 complete, into stage 2
                  if( tavg .le. tbase + t2max ) then
                      ! add stage 2 amount to index
                      bcthardnx = bcthardnx + hardinc2
                  end if
              end if
          end if
          if( day_max_temp .ge. tbase + tdeh ) then
              ! stage 1 dehardening
              hinc = deht * (tbase + tdeh - day_max_temp)
              bcthardnx = bcthardnx + hinc
              if( bcthardnx .ge. hs1 ) then
                  ! really in stage 2, take off some more
                  bcthardnx = bcthardnx + hinc
              end if
          end if
          bcthardnx = max( bcthardnx, 0.0_dp)
          bcthardnx = min( bcthardnx, hs2)

      end if
      
      return
    end subroutine freezeharden

end module ritchieHardening_mod
