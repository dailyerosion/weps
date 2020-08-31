!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSShootGrow_mod
    use phases_mod
    use constants, only: dp, int32, check_return, u_mgtokg, u_max_arg_exp, u_max_real
    use WEPSCrop_util_mod, only: chilluv, scrv1, shootnum, dev_floor, shoot_delay, shoot_flg, spring_trig, verndelmax, hard_spring
   implicit none

    type, extends(phase) :: WEPS_ShootGrow
    contains
    procedure, pass(self) :: load => dummyload
    procedure, pass(self) :: doPhase => shootgrow ! may not need to pass self
    procedure, pass(self) :: register => dummyregister
    end type WEPS_ShootGrow

  contains

    subroutine dummyload(self, phaseState)
      implicit none
      class(WEPS_ShootGrow), intent(inout) :: self
      type(hash_state), intent(inout) :: phaseState
    end subroutine dummyload

    subroutine dummyregister(self, req_input, prod_output)
      implicit none
      class(WEPS_ShootGrow), intent(in) :: self
      type(hash_state), intent(inout) :: req_input
      type(hash_state), intent(inout) :: prod_output
    end subroutine dummyregister

    subroutine shootgrow(self, plnt, env)
      implicit none
      class(WEPS_ShootGrow), intent(inout) :: self
      type(plant), intent(inout) :: plnt
      type(environment_state), intent(inout) :: env

      logical :: succ = .false.
      integer(int32) :: lay ! soil layer loop index

      ! plant database
      real(dp) :: bcdpop ! Number of plants per unit area (#/m^2)
                     ! Note: bcdstm/bcdpop gives the number of stems per plant
      real(dp) :: bcfleafstem ! crop leaf to stem mass ratio for shoots
      integer(int32) :: bc0idc ! crop type:annual,perennial,etc
      real(dp) :: bctverndel ! thermal delay coefficient pre-vernalization
      real(dp) :: bcfleaf2stor ! fraction of assimilate partitioned to leaf that is diverted to root store
      real(dp) :: bcfstem2stor ! fraction of assimilate partitioned to stem that is diverted to root store
      real(dp) :: bcfstor2stor ! fraction of assimilate partitioned to standing storage
                               ! (reproductive) that is diverted to root store
      real(dp) :: bc0shoot ! mass from root storage required for each shoot (mg/shoot)
      real(dp) :: bcdmaxshoot ! maximum number of shoots possible from each plant
      real(dp) :: bc0hue ! relative heat unit for emergence (fraction)
      real(dp) :: bcthum ! potential heat units for crop maturity (deg. C)
      real(dp) :: alf  ! leaf partitioning s-curve coefficient a
      real(dp) :: blf  ! leaf partitioning s-curve coefficient b
      real(dp) :: clf  ! leaf partitioning s-curve coefficient c
      real(dp) :: dlf  ! leaf partitioning s-curve coefficient d
      real(dp) :: arp  ! reproductive partitioning s-curve coefficient a
      real(dp) :: brp  ! reproductive partitioning s-curve coefficient b
      real(dp) :: crp  ! reproductive partitioning s-curve coefficient c
      real(dp) :: drp  ! reproductive partitioning s-curve coefficient d
      real(dp) :: aht  ! height (and rooting depth) s-curve coefficient a
      real(dp) :: bht  ! height (and rooting depth) s-curve coefficient b
      real(dp) :: zmxc ! maximum plant height
      real(dp) :: zmrt ! maximum plant rooting depth
      real(dp) :: ehu0 ! heat unit fraction where senescence starts

      ! environment
      integer(int32) :: bnslay ! number of soil layers
      integer(int32) :: jd ! day of year
!      real(dp) :: hrlt   ! length of day (hours) today

      ! plant state
      real(dp) :: bcmtotshoot ! total mass released from root storage biomass (kg/m^2)
                              ! in the period from beginning to completion of emegence heat units
      real(dp), dimension(:), allocatable :: bcmrootstorez ! crop root storage mass by soil layer (kg/m^2)
                                                       ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      real(dp) :: bcdstm ! Number of crop stems per unit area (#/m^2)
      real(dp) :: bczgrowpt ! depth in the soil of the growing point (m)
      real(dp) :: bctrthucum ! accumulated root growth heat units (degree-days)
      real(dp) :: bcthu_shoot_beg ! heat unit index (fraction) for beginning of shoot grow from root storage period
      real(dp) :: bcthu_shoot_end ! heat unit index (fraction) for end of shoot grow from root storage period
      real(dp) :: bcthardnx ! hardening index for winter annuals (range from 0 t0 2)
      real(dp) :: bctwarmdays ! number of consecutive days that the temperature has been above the minimum growth temperature
      real(dp) :: bctchillucum ! accumulated chilling units (deg C day)
      integer(int32) :: dayspring
      logical :: can_regrow

      ! stage state
      real(dp) :: bcthucum ! plant accumulated heat units (degree-days)
!      real(dp) :: bprevhucum ! previous day plant accumulated heat units (degree-days)
      real(dp) :: daygdd   ! plant heat units for this day (degree-days)

      ! locally computed values
      real(dp) :: hui          ! heat unit index (ratio of acthucum to acthum)
      real(dp) :: huiy         ! heat unit index (ratio of acthucum to acthum) on day (i-1)
      real(dp) :: huirt        ! heat unit index for root expansion (ratio of actrthucum to acthum)
      real(dp) :: huirty       ! heat unit index for root expansion (ratio of actrthucum to acthum) on day (i-1)
      real(dp) :: vern_delay ! reduction in heat unit accumulation based on vernalization
      real(dp) :: photo_delay  ! reduction in heat unit accumulation based on photoperiod
      real(dp) :: hu_delay ! fraction of heat units accummulated based on incomplete vernalization and day length
      real(dp) :: ffa  ! leaf senescence factor (ratio)
      real(dp) :: ffw  ! leaf weight reduction factor (ratio)
      real(dp) :: ffr  ! root weight reduction factor (ratio)
      real(dp) :: gif  ! grain index accounting for development of chaff before grain fill
      real(dp) :: shoot_hui    ! today fraction of heat unit shoot growth index accumulation
      real(dp) :: shoot_huiy   ! previous day fraction of heat unit shoot growth index accumulation
      real(dp) :: p_rw ! fibrous root partitioning ratio
      real(dp) :: p_st ! stem partitioning ratio
      real(dp) :: p_lf ! leaf partitioning ratio
      real(dp) :: p_rp ! reproductive partitioning ratio
      real(dp) :: pdht ! increment in potential height (m)'
      real(dp) :: pdrd ! potential increment in root length (m)
      integer(int32) :: spring_flg
      real(dp) :: arg_exp    ! argument calculated for exponential function (to test for validity)
      real(dp) :: p_lf_rp    ! sum of leaf and reproductive partitioning fractions
      real(dp) :: huf        ! heat unit factor for driving root depth, plant height development
      real(dp) :: hufy       ! value of huf on day (i-1)
      real(dp) :: pchty      ! potential plant height from previous day
      real(dp) :: pcht       ! potential plant height for today
      real(dp) :: prdy       ! potential root depth from previous day
      real(dp) :: prd        ! potential root depth today
      real(dp) :: hui0f      ! relative gdd at start of scenescence
      real(dp) :: ff         ! senescence factor (ratio)
      real(dp) :: hux        ! relative gdd offset to start at scenescence

      integer(int32) :: tmp
      integer(int32), parameter :: winter_ann_root = 1
      logical :: lastday

      ! Body of shootgrow

      ! retrieve required inputs
      ! plant database
      call plnt%pars%get("plantpop", bcdpop, succ)
      if( .not. check_return( "plantpop", succ ) ) return
      call plnt%pars%get("idc", bc0idc, succ)
      if( .not. check_return( "idc", succ ) ) return
      call plnt%pars%get("tverndel", bctverndel, succ)
      if( .not. check_return( "tverndel", succ ) ) return
      call plnt%pars%get("leaf2stor", bcfleaf2stor, succ)
      if( .not. check_return( "leaf2stor", succ ) ) return
      call plnt%pars%get("stem2stor", bcfstem2stor, succ)
      if( .not. check_return( "stem2stor", succ ) ) return
      call plnt%pars%get("stor2stor", bcfstor2stor, succ)
      if( .not. check_return( "stor2stor", succ ) ) return
      call plnt%pars%get("regrmshoot", bc0shoot, succ)
      if( .not. check_return( "regrmshoot", succ ) ) return
      call plnt%pars%get("dmaxshoot", bcdmaxshoot, succ)
      if( .not. check_return( "dmaxshoot", succ ) ) return
      call plnt%pars%get("huie", bc0hue, succ)
      if( .not. check_return( "huie", succ ) ) return
      call plnt%pars%get("thum", bcthum, succ)
      if( .not. check_return( "thum", succ ) ) return
      call plnt%pars%get("alf", alf, succ)
      if( .not. check_return( "alf", succ ) ) return
      call plnt%pars%get("blf", blf, succ)
      if( .not. check_return( "blf", succ ) ) return
      call plnt%pars%get("clf", clf, succ)
      if( .not. check_return( "clf", succ ) ) return
      call plnt%pars%get("dlf", dlf, succ)
      if( .not. check_return( "dlf", succ ) ) return
      call plnt%pars%get("arp", arp, succ)
      if( .not. check_return( "arp", succ ) ) return
      call plnt%pars%get("brp", brp, succ)
      if( .not. check_return( "brp", succ ) ) return
      call plnt%pars%get("crp", crp, succ)
      if( .not. check_return( "crp", succ ) ) return
      call plnt%pars%get("drp", drp, succ)
      if( .not. check_return( "drp", succ ) ) return
      call plnt%pars%get("aht", aht, succ)
      if( .not. check_return( "aht", succ ) ) return
      call plnt%pars%get("bht", bht, succ)
      if( .not. check_return( "bht", succ ) ) return
      call plnt%pars%get("zmxc", zmxc, succ)
      if( .not. check_return( "zmxc", succ ) ) return
      call plnt%pars%get("zmrt", zmrt, succ)
      if( .not. check_return( "zmrt", succ ) ) return
      call plnt%pars%get("ehu0", ehu0, succ)
      if( .not. check_return( "ehu0", succ ) ) return

      ! environment variables
      call env%state%get("dayofyear", jd, succ)
      if( .not. check_return( "dayofyear", succ ) ) return
!      call env%state%get("hrlt", hrlt, succ)
!      if( .not. check_return( "hrlt", succ ) ) return

      ! plant state
      call plnt%state%get("mtotshoot", bcmtotshoot, succ)
      if( .not. check_return( "mtotshoot", succ ) ) return
      call plnt%state%get("mrootstorez", bcmrootstorez, succ)
      if( .not. check_return( "mrootstorez", succ ) ) return
      bnslay = size(bcmrootstorez)

      call plnt%state%get("dstm", bcdstm, succ)
      if( .not. check_return( "dstm", succ ) ) return
      call plnt%state%get("zgrowpt", bczgrowpt, succ)
      if( .not. check_return( "zgrowpt", succ ) ) return
      call plnt%state%get("trthucum", bctrthucum, succ)
      if( .not. check_return( "trthucum", succ ) ) return
      call plnt%state%get("thu_shoot_beg", bcthu_shoot_beg, succ)
      if( .not. check_return( "thu_shoot_beg", succ ) ) return
      call plnt%state%get("thu_shoot_end", bcthu_shoot_end, succ)
      if( .not. check_return( "thu_shoot_end", succ ) ) return
      call plnt%state%get("harden_index", bcthardnx, succ)
      if( .not. check_return( "harden_index", succ ) ) return
      call plnt%state%get("warmdays", bctwarmdays, succ)
      if( .not. check_return( "warmdays", succ ) ) return
      call plnt%state%get("chill_unit_cum", bctchillucum, succ)
      if( .not. check_return( "chill_unit_cum", succ ) ) return
      call plnt%state%get("dayspring", dayspring, succ)
      if( .not. check_return( "dayspring", succ ) ) return
      call plnt%state%get("can_regrow", can_regrow, succ)
      if( .not. check_return( "can_regrow", succ ) ) return

      ! stage state
      call self%phaseState%get("stagegdd", bcthucum, succ)
      if( .not. check_return( "stagegdd", succ ) ) return
      !call self%phaseState%get("prevhucum", bprevhucum, succ)
      !if( .not. check_return( "prevhucum", succ ) ) return
      call plnt%state%get("daygdd", daygdd, succ)
      if( .not. check_return( "daygdd", succ ) ) return

      ! check crop type for shoot growth action
      spring_flg = -1
      if( can_regrow ) then
        if( (bc0idc.eq.2) .or. (bc0idc.eq.5) ) then

          ! check winter annuals for completion of vernalization,
          ! warming and spring day length 
          if( bczgrowpt .le. 0.0_dp ) then
           ! remember, negative number means above ground
           spring_flg = 1
           if( bctchillucum .ge. chilluv ) then
            spring_flg = 2
            if( bctwarmdays .ge. shoot_delay*bctverndel/verndelmax) then
             spring_flg = 3
             !if( huiy .gt. spring_trig ) then
             !if( bcthardnx .le. 0.0 ) then
             if( bcthardnx .lt. hard_spring ) then
              spring_flg = 4
              ! vernalized and ready to grow in spring
              bcthu_shoot_beg = bcthucum / bcthum
              bcthu_shoot_end = bcthucum / bcthum + bc0hue
              call shootnum(shoot_flg, bnslay, bc0idc, bcdpop, bc0shoot,&
     &             bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )
              ! eliminate diversion of biomass to crown storage
              bcfleaf2stor = 0.0_dp
              bcfstem2stor = 0.0_dp
              bcfstor2stor = 0.0_dp
              ! turn off freeze hardening
              bcthardnx = 0.0_dp
              dayspring = jd
             end if
            end if
           end if
          end if
        end if
      end if

      ! accumulate growing degree days
      ! set default heat unit delay value
      hu_delay = 1.0_dp
      if( (bcthum .le. 0.0_dp) .or. (bcdstm .le. 0.0_dp) ) then
          ! always keep this invalid plant in first stage growth
          ! stem count can be set to zero by harvest, but not reset by
          ! regrowth early in spring, causing divide by zero in shoot_grow
          huiy = 0.0_dp
          hui = 0.0_dp
      else
          ! previous day heat unit index
          huiy = min(1.0_dp, bcthucum / bcthum)
          huirty = bctrthucum / bcthum
          ! check for growth completion
          if( huiy .lt. 1.0_dp ) then
              ! accumulate additional for today
              ! check for emergence status
              if( (huiy .ge. bc0hue).and. (huiy .lt. spring_trig) ) then
                  ! emergence completed, account for vernalization and
                  ! photo period by delaying development rate until chill
                  ! units completed and spring trigger reached
                  vern_delay = 1.0_dp-bctverndel*(chilluv-bctchillucum)
                  !vern_delay = 1.0        ! delay disabled
                  !photo_delay = 1.0-bctphotodel*(max_photo_per-hrlt)**2
                  photo_delay = 1.0_dp       ! delay disabled
                  hu_delay =  max(dev_floor,min(vern_delay,photo_delay))
              end if
              ! accumulate heat units using set heat unit delay
              bcthucum = bcthucum + daygdd * hu_delay
              ! root depth growth heat units
              bctrthucum = bctrthucum + daygdd
              ! do not cap this for annuals, to allow it to continue
              ! root mass partition is reduced to lower levels after the
              ! first full year. Out of range is capped in the function
              ! in growth.for
              ! bctrthucum = min(bctrthucum, bcthum)
              ! calculate heat unit index
              hui = min(1.0_dp, bcthucum / bcthum)
              huirt = bctrthucum / bcthum
              !if( hui .ge. 1.0_dp ) then
              !  ! stage complete, point to next stage
              !  tmp = 1  ! go to next stage
              !  call plnt%state%replace("nextstage", tmp, succ)
              !  tmp = 0  ! do not go to specific stage number
              !  call plnt%state%replace("specstage", tmp, succ)
              !  ! return stage status
              !  call self%phaseState%replace("stagegdd", bcthum, succ)
              !  ! remainder daygdd
              !  call plnt%state%replace("remgdd", bcthucum-bcthum, succ)
              !  if( .not. check_return( "remgdd", succ ) ) return
              !  hui = min(1.0_dp, hui)
              !else
              ! return stage status
              tmp = 0  ! clear nextstage, this phase never transitions
              call plnt%state%replace("nextstage", tmp, succ)
              call self%phaseState%replace("stagegdd", bcthucum, succ)
              if( .not. check_return( "stagegdd", succ ) ) return
              !end if
          else
              hui = huiy
          end if

      end if

      ! find partitioning between fibrous roots and all other biomass
      ! root partition done using root heat unit index, which is not reset
      ! when a harvest removes all the leaves. This index also is not delayed
      ! in prevernalization winter annuals. Made to parallel winter annual
      ! rooting depth flag as well.
      if( winter_ann_root .eq. 0 ) then
          p_rw = (0.4_dp-0.2_dp*hui)
      else
          p_rw = max(0.05_dp, (0.4_dp-0.2_dp*huirt) )
      end if

      ! find partitioning factors of the remaining biomass (not fibrous root)
      ! calculate leaf partitioning.
      arg_exp = -(hui - clf) / dlf
      if( arg_exp .ge. u_max_arg_exp ) then
          p_lf = alf + blf / u_max_real
      else
          p_lf = alf + blf / (1.0_dp + exp(arg_exp))
      end if
      p_lf = max( 0.0_dp, min( 1.0_dp, p_lf ))

      ! calculate reproductive partitioning based on partioning curve
      arg_exp = -(hui - crp) / drp
      if( arg_exp .ge. u_max_arg_exp ) then
          p_rp = arp + brp / u_max_real
      else
          p_rp = arp + brp / (1.0_dp + exp(arg_exp))
      end if
      p_rp = max( 0.0_dp, min( 1.0_dp, p_rp ))

      ! normalize leaf and reproductive fractions so sum never greater than 1.0
      p_lf_rp = p_lf + p_rp
      if( p_lf_rp .gt. 1.0_dp ) then
          p_lf = p_lf / p_lf_rp
          p_rp = p_rp / p_lf_rp
          ! set stem partitioning parameter.
          p_st = 0.0_dp
      else
          ! set stem partitioning parameter.
          p_st = 1.0_dp - p_lf_rp
      end if

      ! added method (different from EPIC) of calculating plant height
      ! pht=cummulated potential height,pdht=daily potential height
      ! aczht(am0csr) = cummulated actual height
      ! adht=daily actual height, plant%database%aht,plant%database%bht are
      ! height-scurve parameters (formerly lai parameters)
      ! previous day
      hufy = 0.01_dp + 1.0_dp / (1.0_dp + exp((huiy - aht) / bht))
      ! today
      huf = 0.01_dp + 1.0_dp / (1.0_dp + exp((hui - aht) / bht))

      pchty = min(zmxc, zmxc * hufy)
      pcht = min(zmxc, zmxc * huf)
      pdht = pcht - pchty

      ! calculate rooting depth (eq. 2.203) and check that it is not deeper
      ! than the maximum potential depth, and the depth of the root zone.
      ! This change from the EPIC method is undocumented!! It says that root depth
      ! starts at 10cm and increases from there at the rate determined by huf.
      ! the 10 cm assumption was prevously removed from elsewhere in the code
      ! and is subsequently removed here. The initial depth is now set in 
      ! crop record seeding depth, and  the function just increases it.
      ! This is now based on a no delay heat unit accumulation to allow
      ! rapid root depth development by winter annuals.
      if( winter_ann_root .eq. 0 ) then
          prdy = min(zmrt, zmrt * hufy + 0.1_dp)
          prd = min(zmrt, zmrt * huf + 0.1_dp)
      else
          prdy = zmrt *(0.01_dp + 1.0_dp / (1.0_dp + exp((huirty - aht) / bht)))
          prd = zmrt * (0.01_dp + 1.0_dp / (1.0_dp + exp((huirt - aht) / bht)))
      end if
      pdrd = max(0.0_dp, prd - prdy)

      ! senescence is done on a whole plant mass basis not incremental mass
      ! This starts senescence before the entered heat unit index for
      ! the start of senscence. For most leaf partitioning functions
      ! the coefficients draw a curve that approaches 1 around -0.5 but
      ! the value at zero, raised to fractional powers is still very small
      hui0f = ehu0 - ehu0 * 0.1_dp
      if (hui.ge.hui0f) then
          hux = hui - ehu0
          ff = 1.0_dp / (1.0_dp + exp(-(hux - clf / 2.0_dp) / dlf))
          ffa = ff**0.125_dp
          ffw = ff**0.0625_dp
          ffr = 0.98_dp
      else
          ! set a value to be written out
          ffa = 1.0_dp
          ffw = 1.0_dp
          ffr = 1.0_dp
      endif

      ! this factor prorates the grain reproductive fraction (grf) defined
      ! in the database for crop type 1, grains. Compensates for the
      ! development of chaff before grain filling, ie., grain is not
      ! uniformly a fixed fraction of reproductive mass during the entire 
      ! reproductive development stage.
      gif=1.0_dp / (1.0_dp + exp(-(hui - 0.64_dp) / 0.05_dp))

      if( (huiy .lt. 1.0) .and. (bcdstm .gt. 0.0)) then
        ! crop growth not yet complete
        ! stem count can be set to zero by harvest, but not reset by
        ! regrowth early in spring, causing divide by zero in shoot_grow

        if( hui .ge. 1.0_dp ) then
          lastday = .true.
        else
          lastday = .false.
        end if

        if( huiy .lt. bcthu_shoot_end ) then

          if( hui .gt. bcthu_shoot_beg ) then

            ! fraction of shoot growth from stored reserves (today and yesterday)
            shoot_hui = min( 1.0_dp, (hui - bcthu_shoot_beg) / (bcthu_shoot_end - bcthu_shoot_beg) )
            shoot_huiy = max( 0.0_dp, (huiy - bcthu_shoot_beg) / (bcthu_shoot_end - bcthu_shoot_beg) )

          else
            shoot_hui = 0.0_dp
            shoot_huiy = 0.0_dp
          end if

        else
            shoot_hui = 1.0_dp
            shoot_huiy = 1.0_dp
        end if
      end if

      ! update plant par values
      call plnt%pars%replace("leaf2stor", bcfleaf2stor, succ)
      if( .not. check_return( "leaf2stor", succ ) ) return
      call plnt%pars%replace("stem2stor", bcfstem2stor, succ)
      if( .not. check_return( "stem2stor", succ ) ) return
      call plnt%pars%replace("stor2stor", bcfstor2stor, succ)
      if( .not. check_return( "stor2stor", succ ) ) return

      ! update plant state values
      call plnt%state%replace("mtotshoot", bcmtotshoot, succ)
      if( .not. check_return( "mtotshoot", succ ) ) return
      call plnt%state%replace("dstm", bcdstm, succ)
      if( .not. check_return( "dstm", succ ) ) return
      call plnt%state%replace("trthucum", bctrthucum, succ)
      if( .not. check_return( "trthucum", succ ) ) return
      call plnt%state%replace("thu_shoot_beg", bcthu_shoot_beg, succ)
      if( .not. check_return( "thu_shoot_beg", succ ) ) return
      call plnt%state%replace("thu_shoot_end", bcthu_shoot_end, succ)
      if( .not. check_return( "thu_shoot_end", succ ) ) return
      call plnt%state%replace("harden_index", bcthardnx, succ)
      if( .not. check_return( "harden_index", succ ) ) return
      call plnt%state%replace("dayspring", dayspring, succ)
      if( .not. check_return( "dayspring", succ ) ) return
      call plnt%state%replace("ffa", ffa, succ)
      if( .not. check_return( "ffa", succ ) ) return
      call plnt%state%replace("ffw", ffw, succ)
      if( .not. check_return( "ffw", succ ) ) return
      call plnt%state%replace("ffr", ffr, succ)
      if( .not. check_return( "ffr", succ ) ) return
      call plnt%state%replace("gif", gif, succ)
      if( .not. check_return( "gif", succ ) ) return
      call plnt%state%replace("shoot_hui", shoot_hui, succ)
      if( .not. check_return( "shoot_hui", succ ) ) return
      call plnt%state%replace("shoot_huiy", shoot_huiy, succ)
      if( .not. check_return( "shoot_huiy", succ ) ) return
      call plnt%state%replace("p_rw", p_rw, succ)
      if( .not. check_return( "p_rw", succ ) ) return
      call plnt%state%replace("p_st", p_st, succ)
      if( .not. check_return( "p_st", succ ) ) return
      call plnt%state%replace("p_lf", p_lf, succ)
      if( .not. check_return( "p_lf", succ ) ) return
      call plnt%state%replace("p_rp", p_rp, succ)
      if( .not. check_return( "p_rp", succ ) ) return
      call plnt%state%replace("pdht", pdht, succ)
      if( .not. check_return( "pdht", succ ) ) return
      call plnt%state%replace("pdrd", pdrd, succ)
      if( .not. check_return( "pdrd", succ ) ) return
      call plnt%state%replace("hu_delay", hu_delay, succ)
      if( .not. check_return( "hu_delay", succ ) ) return
      call plnt%state%replace("spring_flg", spring_flg, succ)
      if( .not. check_return( "spring_flg", succ ) ) return
      call plnt%state%replace("lastday", lastday, succ)
      if( .not. check_return( "lastday", succ ) ) return

      ! update phase state
      call self%phaseState%replace("phase_rel_gdd", hui, succ)
      if( .not. check_return( "phase_rel_gdd", succ ) ) return

      ! update stage state
      !call self%phaseState%replace("prevhucum", bprevhucum, succ)
      !if( .not. check_return( "prevhucum", succ ) ) return

    end subroutine shootgrow

end module WEPSShootGrow_mod
