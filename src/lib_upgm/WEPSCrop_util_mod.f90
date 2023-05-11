!$Author$
!$Date$
!$Revision$
!$HeadURL$

module WEPSCrop_util_mod

    use constants, only: dp, int32, u_pi, u_mgtokg, u_hatom2, u_max_arg_exp, u_max_real, u_mmtom
    implicit none

    real(dp), parameter :: chilluv = 50.0_dp     ! total of chill units require for vernalization (deg C)
    real(dp), parameter :: shoot_delay = 7.0_dp  ! number of days minimum temperature must be above base
                                                 ! crop growth temperature for shoot growth to occur
    real(dp), parameter :: verndelmax = 0.04_dp  ! maximum value of vernalization delay parameter
    real(dp), parameter :: dev_floor = 0.01_dp   ! minimum development rate fraction allowed 
                                                 ! (1-full rate, 0-no development)
    real(dp), parameter :: max_photo_per = 20_dp ! photo period where maximum development rate occurs (hours)
    real(dp), parameter :: spring_trig = 0.29_dp ! heat units ratio to spring allowing release of winter annual crown storage
    real(dp), parameter :: hard_spring = 1.0_dp  ! hardening index threshold for spring growth breakout
    integer(int32), parameter :: shoot_flg = 0   ! used to control the behavior of the shootnum subroutine
                                                 ! 0 - returns the shoot number constrained by bcdmaxshoot
    real(dp), parameter :: per_release = 0.9_dp  ! idc == 3 or 6 perennials, fraction of available root stoage mass
                                                 ! released to grow new shoots. 
    real(dp), parameter :: stage_release = 0.5_dp ! idc == 8, staged release, fraction of available root stoage mass
                                                 ! released to grow new shoots
    real(dp), parameter :: hu_leaf_days = 7.0d0  ! number of days of optimum heat units to get full leaf emergence

  contains

    ! To calculate the temperature stress factor
    ! This algorithms was taken from the EPIC subroutine cgrow.
    function temp_stress( bwtdmx, bwtdmn, bctopt, bctmin ) result(temps)
      real(dp), intent(in) :: bwtdmx   ! daily maximum air temperature
      real(dp), intent(in) :: bwtdmn   ! daily minimum air temperature
      real(dp), intent(in) :: bctopt   ! optimum crop growth temperature
      real(dp), intent(in) :: bctmin   ! minimum crop growth temperature
      real(dp) :: temps

      ! local variables
      real(dp) :: tgx   ! difference between the soil surface temperature and the minimum temperature for plant growth
      real(dp) :: x1    ! difference between the optimum and minimum temperatures for plant growth
      real(dp) :: rto   ! interim variable
      real(dp) :: dst0  ! estimated soil temperature from daily max and min temperatures

      ! calculate temperature stress factor
      ! following one statement to be removed when soil temperature is available
      dst0 = (bwtdmx + bwtdmn) / 2.0_dp
      tgx=dst0-bctmin
      if (tgx.le.0.) tgx=0.0_dp
      x1=bctopt-bctmin
      rto=tgx/x1
      temps=sin(1.5707_dp*rto)
      if (rto.gt.2.) temps=0.0_dp

      ! this reduces temperature stress around the optimum
      temps = temps**0.25_dp

      return
    end function temp_stress

    subroutine shootnum( shoot_flg, bnslay, frac_release, bcdpop, bc0shoot, &
                 bcdmaxshoot, bcmtotshoot, bcmrootstorez, bcdstm )

      ! + + + PURPOSE + + +
      ! determine the number of shoots that root storage mass can support,
      ! and set the total mass to be released from root storage.

      ! + + + KEYWORDS + + +
      ! stem number, shoot growth

      !use p1unconv_mod, only: mgtokg

      ! + + + ARGUMENT DECLARATIONS + + +
      integer(int32), intent(in) :: shoot_flg  ! Used to control the behavior of the shootnum subroutine
                                               ! 0 - returns the shoot number constrained by bcdmaxshoot
                                               ! 1 - returns the shoot number unconstrained by bcdmaxshoot
      integer(int32), intent(in) :: bnslay     ! Number of soil layers
      real(dp), intent(in) :: frac_release     ! Fraction of root store released for use in regrowth
      real(dp), intent(in) :: bcdpop           ! Number of plants per unit area (#/m^2)
                                               ! Note: bcdstm/bcdpop gives number of stems per plant
      real(dp), intent(in) :: bc0shoot         ! Mass from root storage required for each shoot (mg/shoot)
      real(dp), intent(in) :: bcdmaxshoot      ! Maximum number of shoots possible from each plant
      real(dp), intent(inout) :: bcmtotshoot   ! Total mass released from root storage biomass (kg/m^2)
                                               ! in the period from beginning to completion of emergence heat units
      real(dp), intent(in) :: bcmrootstorez(*) ! Crop root storage mass by soil layer (kg/m^2)
                                               ! (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
      real(dp), intent(inout) :: bcdstm        ! Number of crop stems per unit area (#/m^2)

      ! + + + LOCAL VARIABLES + + +
      integer(int32) :: lay       ! Layer index for summing root storage
      real(dp) :: root_store_sum  ! Sum of root storage


      ! Find number of shoots (stems) that can be supported from
      ! root storage mass up to the maximum
      root_store_sum = 0.0_dp
      do lay = 1,bnslay
          root_store_sum = root_store_sum + bcmrootstorez(lay)
      end do

      ! determine number of regrowth shoots
      ! units are kg/m^2 / kg/shoot = shoots/m^2
      bcdstm = max( bcdpop, frac_release * root_store_sum/(bc0shoot*u_mgtokg)  )

      if( shoot_flg .eq. 0 ) then
          ! respect maximum limit
          bcdstm =  min( bcdmaxshoot*bcdpop, bcdstm )
      end if

      !  write(*,*) 'shootnum:bcdstm: ', bcdstm
      ! set the mass of root storage that is released (for use in shoot grow)
      ! units are shoots/m^2 * kg/shoot = kg/m^2
      bcmtotshoot = min( root_store_sum, bcdstm * bc0shoot * u_mgtokg )

      return
    end subroutine shootnum

    ! Calculate single day heat units for given temperatures
    function huc( bwtdmx, bwtdmn, bctmax, bctmin ) result(huc1)
      real(dp), intent(in) :: bwtdmx   ! daily maximum air temperature
      real(dp), intent(in) :: bwtdmn   ! daily minimum air temperature
      real(dp), intent(in) :: bctmax   ! max or optimum crop growth temperature
      real(dp), intent(in) :: bctmin   ! minimum crop growth temperature
      real(dp) :: huc1

      huc1 = heatunit(bwtdmx, bwtdmn, bctmin)                           &
     &     - heatunit(bwtdmx, bwtdmn, bctmax)
      if (huc1.lt.0.) huc1=0.

      return
    end function huc

    ! calculates the amount of heat units in degree-days above a
    ! threshold temperature assuming a fully sinusoidal daily 
    ! temperature cycle with maximum and minimum 12 hours apart.
    function heatunit( tmax, tmin, thres ) result(heat_unit)
      real(dp), intent(in) :: tmax   ! maximum daily air temperature
      real(dp), intent(in) :: tmin   ! minimum daily air temperature
      real(dp), intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real(dp) :: heat_unit

      ! local variables
      real(dp) :: tmean   ! arithmetic average of tmax and tmin
      real(dp) :: delta   ! difference between daily maximum and minimum temperature
      real(dp) :: theta   ! point where threshold and air temperature are equal, defines integration limits

      if (thres .ge. tmax) then
          heat_unit = 0.0
      else if ((thres .le. tmin) .or. (tmax .le. tmin)) then
          tmean = (tmax + tmin) / 2.0
          heat_unit = tmean - thres
      else
          tmean = (tmax + tmin) / 2.0
          delta = (tmax - tmin) / 2.0
          theta = asin( (thres - tmean) / delta )
          heat_unit = ( (tmean - thres) * (u_pi/2.0 - theta) + delta * cos(theta) ) / u_pi
      end if

      return
    end function heatunit

    ! calculates the vernalization effectiveness of a day. For fully
    ! effective tmeperatures, a full day is returned. for temperatures
    ! that are less than fully effective, a partial day or zero is 
    ! returned. If temperatures are above the upper temperature 
    ! threshold and insufficient chill days are accumulated, devernalization
    ! occurs.

    ! method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
    ! Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
    ! Agronomy Monograph 31, pages 34-36.

    subroutine chillunit_cum( bctchillucum, day_max_temp, day_min_temp )
      real(dp), intent(inout) :: bctchillucum  ! accumulated chilling units (days)
      real(dp), intent(in) :: day_max_temp     ! daily maximum temperature (deg.C)
      real(dp), intent(in) :: day_min_temp     ! daily minimum temperature (deg.C)

      ! local variables
      real(dp) :: tavg     ! daily everage temperature  (deg.C)
      real(dp) :: relvern  ! relative vernalization effectiveness

      real(dp), parameter :: tmin = 0.0     ! minimum temperature in vernalization function (deg.C)
      real(dp), parameter :: tmax = 18.0    ! maximum temperature in vernalization function (deg.C)
      real(dp), parameter :: topt = 7.0     ! optimum temperature in vernalization function (deg.C)
      real(dp), parameter :: tdev = 30.0    ! temperature above which devernalization can occur (deg.C)
      real(dp), parameter :: daylim = 10.0  ! vernalization days beyond which no devernalization can occur (days)
      real(dp), parameter :: daydev = -0.5  ! devernalization days subtracted for each degree C above tdev (days)

      ! find average temperature
      tavg = 0.5 * (day_max_temp + day_min_temp)

      if( (tavg .ge. tmin) .and. (tavg .le. tmax) ) then
          if( tavg .le. topt ) then
              ! full vernalization effectiveness
              relvern = 1.0
          else
              ! reduced vernalization effectiveness
              relvern = (tmax-tavg)/(tmax-topt)
          end if
      else if((day_max_temp.gt.tdev).and.(bctchillucum.lt.daylim)) then
          ! devernalization
          relvern = daydev * (day_max_temp - tdev)
      else
          relvern = 0.0
      end if
      
      bctchillucum = max( 0.0, bctchillucum + relvern )

      return
    end subroutine chillunit_cum

    ! calculates the cumulative number of days the daily average temperature
    ! is above a threshold temperature.
    subroutine warmday_cum( bctwarmdays, thres, tmax, tmin )
      real(dp), intent(inout) :: bctwarmdays  ! total number of consequtive days tempeature is above threshold
      real(dp), intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real(dp), intent(in) :: tmax   ! maximum daily air temperature
      real(dp), intent(in) :: tmin   ! minimum daily air temperature

      ! local variables
      real(dp) :: tmean   ! arithmetic average of tmax and tmin

      tmean = (tmax + tmin) / 2.0_dp
      if (tmean .gt. thres) then
          ! this is a warm day
          bctwarmdays = bctwarmdays + 1.0_dp
      else
          ! reduce warm day total, but do not zero, for proper fall regrow of perennials
          bctwarmdays = bctwarmdays / 2.0_dp
      end if

      return
    end subroutine warmday_cum

    ! calculates the cumulative number of days the daily average temperature
    ! is below a threshold temperature.
    subroutine coldday_cum( colddays, thres, tmax, tmin )
      real(dp), intent(inout) :: colddays  ! total number of consequtive days tempeature is below threshold
      real(dp), intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real(dp), intent(in) :: tmax   ! maximum daily air temperature
      real(dp), intent(in) :: tmin   ! minimum daily air temperature

      ! local variables
      real(dp) :: tmean   ! arithmetic average of tmax and tmin

      tmean = (dble(tmax) + dble(tmin)) / 2.0d0
      if (tmean .le. dble(thres)) then
          ! this is a cold day
          colddays = colddays + 1.0d0
      else
          ! reduce cold day total, but do not zero
          colddays = colddays / 2.0d0
      end if

      return
    end subroutine coldday_cum

    subroutine freeze_damage(ff_senescence, stsmn1, frsx1, frsx2, frsy1, frsy2, bcmstandleaflive, bcmstandleafdead, frst,lost_mass)
      real(dp), intent(in) :: ff_senescence
      real(dp), intent(in) :: stsmn1
      real(dp), intent(in) :: frsx1
      real(dp), intent(in) :: frsx2
      real(dp), intent(in) :: frsy1
      real(dp), intent(in) :: frsy2
      real(dp), intent(inout) :: bcmstandleaflive
      real(dp), intent(inout) :: bcmstandleafdead
      real(dp), intent(out) :: frst
      real(dp), intent(out) :: lost_mass

      real(dp), parameter :: frac_frst_mass_lost = 0.0_dp

      real(dp) :: xx1
      real(dp) :: xx2
      real(dp) :: fx1
      real(dp) :: fx2
      real(dp) :: xxw
      real(dp) :: arg_exp
      real(dp) :: delta_x
      real(dp) :: froz_mass

      ! reduce green leaf mass in freezing weather
      if( ((bcmstandleaflive + bcmstandleafdead) .gt. 0.0_dp) .and. (stsmn1 .lt. -2.0_dp) ) then
          ! use daily minimum soil temperature of first layer to account for snow cover effects
          xx1 = abs( frsx1 )
          xx2 = abs( frsx2 )
          xxw = abs( stsmn1 )

          delta_x = (xx2 - xx1)

          if( (delta_x .gt. 0.0d0) .or. (delta_x .lt. 0.0d0) ) then
            ! delta_x is non-zero
            fx1 = log(xx1/frsy1-xx1)
            fx2 = log(xx2/frsy2-xx2)
          else
            ! delta_x is zero. Make non-zero in correct direction for frost damage.
            xx1 = xx1 - spacing(xx1)
            xx2 = xx2 + spacing(xx2)
            delta_x = (xx2 - xx1)
            fx1 = log(xx1/frsy1-xx1)
            fx2 = log(xx2/frsy2-xx2)
          end if

          arg_exp = fx1 + (xx1 - xxw) * (fx1 - fx2) / delta_x

          if( abs(arg_exp) .gt. u_max_arg_exp ) then
            ! cap the value to avoid floating point error on exponential function
            ! preserve sign
            arg_exp = sign( (u_max_arg_exp - spacing(u_max_arg_exp)), arg_exp)
          end if

          ! b_fr = (fx1 - fx2) / delta_x
          ! a_fr = fx1 + b_fr * xx1
          ! frst = xw / (xw + exp(a_fr-b_fr*xw))

          frst = xxw / (xxw + exp(arg_exp))

          ! is it before or after scenescence?
          if (ff_senescence .gt. 0.9999_dp) then
              ! before scenescence, frost killed mass is fragile and a fraction disappears
              lost_mass  = (bcmstandleaflive + bcmstandleafdead) * frst * frac_frst_mass_lost

              ! reduce green leaf area due to frost damage (10/1/99)
              froz_mass = bcmstandleaflive * frst
              bcmstandleaflive = bcmstandleaflive - froz_mass
              bcmstandleafdead = bcmstandleafdead + froz_mass*(1.0_dp-frac_frst_mass_lost)
          else
              ! after scenescence, frost killed mass is tougher and is not lost immediately
              ! reduce green leaf area due to frost damage (9/22/2003)
              froz_mass = bcmstandleaflive * frst
              bcmstandleaflive = bcmstandleaflive - froz_mass
              bcmstandleafdead = bcmstandleafdead + froz_mass
              lost_mass = 0.0_dp
          end if
      else
          frst = 0.0_dp
          lost_mass = 0.0_dp
      endif

    end subroutine freeze_damage

    function total_leaf( bnslay, bcmrootstorez ) result(bcmtotleaf)
      ! determine the mass of leaf emergence that root storage mass can support,
      ! and set the total mass to be released from root storage.

      integer(int32) :: bnslay    ! number of soil layers
      real(dp), intent(in) :: bcmrootstorez(*) ! crop root storage mass by soil layer (kg/m^2)
      real(dp) :: bcmtotleaf      ! total mass released from root storage biomass (kg/m^2)
                                  ! in the period from beginning to completion of leaf emergence heat units

      ! + + + LOCAL VARIABLES + + +
      integer(int32) :: lay       ! layer index for summing root storage
      real(dp) :: root_store_sum  ! sum of root storage (kg/m^2)

      ! + + + PARAMETERS + + +
      real(dp), parameter :: leaf_release = 0.5d0 ! fraction of available root storage mass released to
                                                  ! grow new leaves. Default is set to 50% of available

      ! find mass of leaf that can be supported from
      ! root storage mass up to the maximum
      root_store_sum = 0.0d0
      do lay = 1,bnslay
          root_store_sum = root_store_sum + dble(bcmrootstorez(lay))
      end do

      ! set the mass of root storage that is released (for use in leaf emergence)
      bcmtotleaf = root_store_sum * leaf_release

      return
    end function total_leaf

end module WEPSCrop_util_mod
