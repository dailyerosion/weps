!$Author$
!$Date$
!$Revision$
!$HeadURL$

module crop_climate_mod

  use climate_input_mod, only: cli_today

  interface huc1
      module procedure huc1_today
      module procedure huc1_temps
  end interface huc1

  interface heatunit
      module procedure heatunit_today
      module procedure heatunit_temps
  end interface heatunit

  interface warmday_cum
      module procedure warmday_cum_today
      module procedure warmday_cum_temps
  end interface warmday_cum

  interface coldunit
      module procedure coldunit_today
      module procedure coldunit_temps
  end interface coldunit

  interface coldday_cum
      module procedure coldday_cum_today
      module procedure coldday_cum_temps
  end interface coldday_cum

  interface temp_stress
      module procedure temp_stress_today
      module procedure temp_stress_soil
      module procedure temp_stress_temps
  end interface temp_stress

  interface freezeharden
      module procedure freezeharden_today
      module procedure freezeharden_temps
  end interface freezeharden

  interface chillunit_cum
      module procedure chillunit_cum_today
      module procedure chillunit_cum_temps
  end interface chillunit_cum

  contains

    ! Calculate single day heat units for given temperatures
    function huc1_today( bctmax, bctmin ) result(huc1)
      real, intent(in) :: bctmax   ! max or optimum crop growth temperature
      real, intent(in) :: bctmin   ! minimum crop growth temperature
      real :: huc1

      huc1 = heatunit(bctmin) - heatunit(bctmax)
      if (huc1.lt.0.) huc1=0.

      return
    end function huc1_today

    ! Calculate single day heat units for given temperatures
    function huc1_temps( bwtdmx, bwtdmn, bctmax, bctmin ) result(huc1)
      real, intent(in) :: bwtdmx   ! daily maximum air temperature
      real, intent(in) :: bwtdmn   ! daily minimum air temperature
      real, intent(in) :: bctmax   ! max or optimum crop growth temperature
      real, intent(in) :: bctmin   ! minimum crop growth temperature
      real :: huc1

      huc1 = heatunit(bwtdmx, bwtdmn, bctmin)                           &
     &     - heatunit(bwtdmx, bwtdmn, bctmax)
      if (huc1.lt.0.) huc1=0.

      return
    end function huc1_temps

    ! calculates the amount of heat units in degree-days above a
    ! threshold temperature assuming a fully sinusoidal daily 
    ! temperature cycle with maximum and minimum 12 hours apart.
    function heatunit_today( thres ) result(heat_unit)
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real :: heat_unit

      heat_unit = heatunit( cli_today%tdmx, cli_today%tdmn, thres )

      return
    end function heatunit_today

    ! calculates the amount of heat units in degree-days above a
    ! threshold temperature assuming a fully sinusoidal daily 
    ! temperature cycle with maximum and minimum 12 hours apart.
    function heatunit_temps( tmax, tmin, thres ) result(heat_unit)
      use p1unconv_mod, only: pi
      real, intent(in) :: tmax   ! maximum daily air temperature
      real, intent(in) :: tmin   ! minimum daily air temperature
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real :: heat_unit

      ! local variables
      real :: tmean   ! arithmetic average of tmax and tmin
      real :: delta   ! difference between daily maximum and minimum temperature
      real :: theta   ! point where threshold and air temperature are equal, defines integration limits

      if (thres .ge. tmax) then
          heat_unit = 0.0
      else if ((thres .le. tmin) .or. (tmax .le. tmin)) then
          tmean = (tmax + tmin) / 2.0
          heat_unit = tmean - thres
      else
          tmean = (tmax + tmin) / 2.0
          delta = (tmax - tmin) / 2.0
          theta = asin( (thres - tmean) / delta )
          heat_unit = ( (tmean - thres) * (pi/2.0 - theta) + delta * cos(theta) ) / pi
      end if

      return
    end function heatunit_temps

    ! calculates the cumulative number of days the daily average temperature
    ! is above a threshold temperature.
    subroutine warmday_cum_today( bctwarmdays, thres )
      real, intent(inout) :: bctwarmdays  ! total number of consequtive days temperature is above threshold
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)

      call warmday_cum( bctwarmdays, thres, cli_today%tdmx, cli_today%tdmn )

      return
    end subroutine warmday_cum_today

    ! calculates the cumulative number of days the daily average temperature
    ! is above a threshold temperature.
    subroutine warmday_cum_temps( bctwarmdays, thres, tmax, tmin )
      real, intent(inout) :: bctwarmdays  ! total number of consequtive days tempeature is above threshold
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real, intent(in) :: tmax   ! maximum daily air temperature
      real, intent(in) :: tmin   ! minimum daily air temperature

      ! local variables
      real :: tmean   ! arithmetic average of tmax and tmin

      tmean = (tmax + tmin) / 2.0
      if (tmean .ge. thres) then
          ! this is a warm day
          bctwarmdays = bctwarmdays + 1
      else
          ! reduce warm day total, but do not zero, for proper fall regrow of perennials
          bctwarmdays = bctwarmdays / 2
      end if

      return
    end subroutine warmday_cum_temps

    ! calculates the amount of cold units in degree-days below a
    ! threshold temperature assuming a fully sinusoidal daily 
    ! temperature cycle with maximum and minimum 12 hours apart.
    function coldunit_today( thres ) result(cold_unit)
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real :: cold_unit

      cold_unit = coldunit( cli_today%tdmx, cli_today%tdmn, thres )

      return
    end function coldunit_today

    ! calculates the amount of cold units in degree-days below a
    ! threshold temperature assuming a fully sinusoidal daily 
    ! temperature cycle with maximum and minimum 12 hours apart.
    function coldunit_temps( tmax, tmin, thres ) result(cold_unit)
      use p1unconv_mod, only: pi
      real, intent(in) :: tmax   ! maximum daily air temperature
      real, intent(in) :: tmin   ! minimum daily air temperature
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real :: cold_unit

      ! local variables
      real :: tmean   ! arithmetic average of tmax and tmin
      real :: delta   ! difference between daily maximum and minimum temperature
      real :: theta   ! point where threshold and air temperature are equal, defines integration limits

      if (thres .le. tmin) then
          cold_unit = 0.0
      else if ((thres .ge. tmax) .or. (tmax .le. tmin)) then
          tmean = (tmax + tmin) / 2.0
          cold_unit = thres - tmean
      else
          tmean = (tmax + tmin) / 2.0
          delta = (tmax - tmin) / 2.0
          theta = asin( (tmean - thres) / delta )
          cold_unit = ( (thres - tmean) * (pi/2.0 - theta) + delta * cos(theta) ) / pi
      end if

      return
    end function coldunit_temps

    ! calculates the cumulative number of days the daily average temperature
    ! is below a threshold temperature.
    subroutine coldday_cum_today( colddays, thres )
      real, intent(inout) :: colddays  ! total number of consequtive days tempeature is below threshold
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)

      call coldday_cum( colddays, thres, cli_today%tdmx, cli_today%tdmn )

      return
    end subroutine coldday_cum_today

    ! calculates the cumulative number of days the daily average temperature
    ! is below a threshold temperature.
    subroutine coldday_cum_temps( colddays, thres, tmax, tmin )
      real, intent(inout) :: colddays  ! total number of consequtive days tempeature is below threshold
      real, intent(in) :: thres  ! threshold temperature (such as minimum temperature for growth)
      real, intent(in) :: tmax   ! maximum daily air temperature
      real, intent(in) :: tmin   ! minimum daily air temperature

      ! local variables
      real :: tmean   ! arithmetic average of tmax and tmin

      tmean = (tmax + tmin) / 2.0
      if (tmean .le. thres) then
          ! this is a cold day
          colddays = colddays + 1
      else
          ! reduce cold day total, but do not zero
          colddays = colddays / 2
      end if

      return
    end subroutine coldday_cum_temps

    ! To calculate the temperature stress factor
    ! This algorithms was taken from the EPIC subroutine cgrow.
    function temp_stress_today( bctopt, bctmin ) result(temps)
      real, intent(in) :: bctopt   ! optimum crop growth temperature
      real, intent(in) :: bctmin   ! minimum crop growth temperature
      real :: temps

      ! local variables
      real :: tgx   ! difference between the soil surface temperature and the minimum temperature for plant growth
      real :: x1    ! difference between the optimum and minimum temperatures for plant growth
      real :: rto   ! interim variable
      real :: dst0  ! estimated soil temperature from daily max and min temperatures

      ! calculate temperature stress factor
      ! following one statement to be removed when soil temperature is available
      dst0 = (cli_today%tdmx + cli_today%tdmn) / 2.0
      tgx=dst0-bctmin
      if (tgx.le.0.) tgx=0.
      x1=bctopt-bctmin
      rto=tgx/x1
      temps=sin(1.5707*rto)
      if (rto.gt.2.) temps=0.

      ! this reduces temperature stress around the optimum
      temps = temps**0.25

      return
    end function temp_stress_today

    ! To calculate the temperature stress factor
    ! This algorithms was taken from the EPIC subroutine cgrow.
    function temp_stress_soil( dst0, bctopt, bctmin ) result(temps)
      real, intent(in) :: dst0     ! soil temperature
      real, intent(in) :: bctopt   ! optimum crop growth temperature
      real, intent(in) :: bctmin   ! minimum crop growth temperature
      real :: temps

      ! local variables
      real :: tgx   ! difference between the soil surface temperature and the minimum temperature for plant growth
      real :: x1    ! difference between the optimum and minimum temperatures for plant growth
      real :: rto   ! interim variable

      ! calculate temperature stress factor
      tgx=dst0-bctmin
      if (tgx.le.0.) tgx=0.
      x1=bctopt-bctmin
      rto=tgx/x1
      temps=sin(1.5707*rto)
      if (rto.gt.2.) temps=0.

      ! this reduces temperature stress around the optimum
      temps = temps**0.25

      return
    end function temp_stress_soil

    ! To calculate the temperature stress factor
    ! This algorithms was taken from the EPIC subroutine cgrow.
    function temp_stress_temps( bwtdmx, bwtdmn, bctopt, bctmin ) result(temps)
      real, intent(in) :: bwtdmx   ! daily maximum air temperature
      real, intent(in) :: bwtdmn   ! daily minimum air temperature
      real, intent(in) :: bctopt   ! optimum crop growth temperature
      real, intent(in) :: bctmin   ! minimum crop growth temperature
      real :: temps

      ! local variables
      real :: tgx   ! difference between the soil surface temperature and the minimum temperature for plant growth
      real :: x1    ! difference between the optimum and minimum temperatures for plant growth
      real :: rto   ! interim variable
      real :: dst0  ! estimated soil temperature from daily max and min temperatures

      ! calculate temperature stress factor
      ! following one statement to be removed when soil temperature is available
      dst0 = (bwtdmx + bwtdmn) / 2.0
      tgx=dst0-bctmin
      if (tgx.le.0.) tgx=0.
      x1=bctopt-bctmin
      rto=tgx/x1
      temps=sin(1.5707*rto)
      if (rto.gt.2.) temps=0.

      ! this reduces temperature stress around the optimum
      temps = temps**0.25

      return
    end function temp_stress_temps

    ! calculates the freeze hardening index for the day. The input value
    ! is modified to reflect the effect of temperature on either increasing
    ! or decreasing the index. Stage 1 hardening occurs when the plant
    ! experiences cool temperatures from -1 to 8 degrees C. Stage 2 hardening
    ! occurs only after stage 1 is complete and temperatures fall below
    ! freezing.

    ! method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
    ! Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
    ! Agronomy Monograph 31, pages 40-42, 52

    subroutine freezeharden_today( bcthardnx )
      real, intent(inout) :: bcthardnx   ! hardening index for winter annuals (range from 0 t0 2)

      ! note: input crown temperature rather than air temperature for best results

      ! local variables
      real :: tavg   ! daily everage temperature (deg.C)
      real :: hinc   ! daily hardening increment

      ! parameters
      real, parameter :: t1min = -1.0  ! minimum temperature in stage 1 index calculation(deg.C)
      real, parameter :: t1opt = 3.5  ! optimum temperature in stage 1 index calculation(deg.C)
      real, parameter :: t1max = 8.0  ! maximum temperature in stage 1 index calculation(deg.C)
      real, parameter :: t2max = 0.0  ! maximum temperature in stage 2 index calculation(deg.C)
      real, parameter :: tbase = 0.0  ! base temperature for hardening effects(deg.C) (like base growth temperature)
      real, parameter :: tdeh = 10.0  ! temperature above which dehardening can occur (deg.C)
      real, parameter :: hs1 = 1.0  ! index value at completion of stage 1 hardening
      real, parameter :: hs2 = 2.0  ! index value at completion of stage 2 hardening
      real, parameter :: deht = 0.02  ! index reduction multiplier for dehardening temperature excess
      real, parameter :: hardinc1 = 0.1  ! stage 1 hardening index increment
      real, parameter :: hardinc2 = 0.083  ! stage 2 hardening index increment

      ! find average temperature
      tavg = 0.5 * (cli_today%tdmx + cli_today%tdmn)

      if( bcthardnx .ge. hs1 ) then
          ! stage 1 complete, into stage 2
          if( tavg .le. tbase + t2max ) then
              ! add stage 2 amount to index
              bcthardnx = bcthardnx + hardinc2
          end if
          if( cli_today%tdmx .ge. tbase + tdeh ) then
              ! stage 2 dehardening
              hinc = deht * (tbase + tdeh - cli_today%tdmx)
              bcthardnx = bcthardnx + hinc
              if( bcthardnx .ge. hs1 ) then
                  ! still in stage 2, take off some more
                  bcthardnx = bcthardnx + hinc
              end if
          end if
          bcthardnx = max( bcthardnx, 0.0)
          bcthardnx = min( bcthardnx, hs2)

      else if( tavg .ge. tbase + t1min) then
          ! stage 1 hardening
          if( tavg .le. tbase + t1max ) then
              ! add stage 1 amount to index, minus deduction for being on either side of optimum
              bcthardnx = bcthardnx + hardinc1                          &
     &                  - ((tavg - (tbase + t1opt))**2)/506.
              if( bcthardnx .ge. hs1 ) then
                  ! stage 1 complete, into stage 2
                  if( tavg .le. tbase + t2max ) then
                      ! add stage 2 amount to index
                      bcthardnx = bcthardnx + hardinc2
                  end if
              end if
          end if
          if( cli_today%tdmx .ge. tbase + tdeh ) then
              ! stage 1 dehardening
              hinc = deht * (tbase + tdeh - cli_today%tdmx)
              bcthardnx = bcthardnx + hinc
              if( bcthardnx .ge. hs1 ) then
                  ! really in stage 2, take off some more
                  bcthardnx = bcthardnx + hinc
              end if
          end if
          bcthardnx = max( bcthardnx, 0.0)
          bcthardnx = min( bcthardnx, hs2)

      end if
      
      return
    end subroutine freezeharden_today

    ! calculates the freeze hardening index for the day. The input value
    ! is modified to reflect the effect of temperature on either increasing
    ! or decreasing the index. Stage 1 hardening occurs when the plant
    ! experiences cool temperatures from -1 to 8 degrees C. Stage 2 hardening
    ! occurs only after stage 1 is complete and temperatures fall below
    ! freezing.

    ! method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
    ! Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
    ! Agronomy Monograph 31, pages 40-42, 52

    subroutine freezeharden_temps( bcthardnx, day_max_temp, day_min_temp )
      real, intent(inout) :: bcthardnx   ! hardening index for winter annuals (range from 0 t0 2)
      real, intent(in) :: day_max_temp   ! daily maximum temperature (deg.C)
      real, intent(in) :: day_min_temp   ! daily minimum temperature (deg.C)

      ! note: input crown temperature rather than air temperature for best results

      ! local variables
      real :: tavg   ! daily everage temperature (deg.C)
      real :: hinc   ! daily hardening increment

      ! parameters
      real, parameter :: t1min = -1.0  ! minimum temperature in stage 1 index calculation(deg.C)
      real, parameter :: t1opt = 3.5  ! optimum temperature in stage 1 index calculation(deg.C)
      real, parameter :: t1max = 8.0  ! maximum temperature in stage 1 index calculation(deg.C)
      real, parameter :: t2max = 0.0  ! maximum temperature in stage 2 index calculation(deg.C)
      real, parameter :: tbase = 0.0  ! base temperature for hardening effects(deg.C) (like base growth temperature)
      real, parameter :: tdeh = 10.0  ! temperature above which dehardening can occur (deg.C)
      real, parameter :: hs1 = 1.0  ! index value at completion of stage 1 hardening
      real, parameter :: hs2 = 2.0  ! index value at completion of stage 2 hardening
      real, parameter :: deht = 0.02  ! index reduction multiplier for dehardening temperature excess
      real, parameter :: hardinc1 = 0.1  ! stage 1 hardening index increment
      real, parameter :: hardinc2 = 0.083  ! stage 2 hardening index increment

      ! find average temperature
      tavg = 0.5 * (day_max_temp + day_min_temp)

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
          bcthardnx = max( bcthardnx, 0.0)
          bcthardnx = min( bcthardnx, hs2)

      else if( tavg .ge. tbase + t1min) then
          ! stage 1 hardening
          if( tavg .le. tbase + t1max ) then
              ! add stage 1 amount to index, minus deduction for being on either side of optimum
              bcthardnx = bcthardnx + hardinc1                          &
     &                  - ((tavg - (tbase + t1opt))**2)/506.
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
          bcthardnx = max( bcthardnx, 0.0)
          bcthardnx = min( bcthardnx, hs2)

      end if
      
      return
    end subroutine freezeharden_temps

!     calculates the vernalization effectiveness of a day. For fully
!     effective tmeperatures, a full day is returned. for temperatures
!     that are less than fully effective, a partial day or zero is 
!     returned. If temperatures are above the upper temperature 
!     threshold and insufficient chill days are accumulated, devernalization
!     occurs.

!     method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
!     Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
!     Agronomy Monograph 31, pages 34-36.

    subroutine chillunit_cum_today( bctchillucum )
      real, intent(inout) :: bctchillucum  ! accumulated chilling units (days)

      ! local variables
      real :: tavg     ! daily everage temperature  (deg.C)
      real :: relvern  ! relative vernalization effectiveness

      real, parameter :: tmin = 0.0     ! minimum temperature in vernalization function (deg.C)
      real, parameter :: tmax = 18.0    ! maximum temperature in vernalization function (deg.C)
      real, parameter :: topt = 7.0     ! optimum temperature in vernalization function (deg.C)
      real, parameter :: tdev = 30.0    ! temperature above which devernalization can occur (deg.C)
      real, parameter :: daylim = 10.0  ! vernalization days beyond which no devernalization can occur (days)
      real, parameter :: daydev = -0.5  ! devernalization days subtracted for each degree C above tdev (days)

      ! find average temperature
      tavg = 0.5 * (cli_today%tdmx + cli_today%tdmn)

      if( (tavg .ge. tmin) .and. (tavg .le. tmax) ) then
          if( tavg .le. topt ) then
              ! full vernalization effectiveness
              relvern = 1.0
          else
              ! reduced vernalization effectiveness
              relvern = (tmax-tavg)/(tmax-topt)
          end if
      else if((cli_today%tdmx.gt.tdev).and.(bctchillucum.lt.daylim)) then
          ! devernalization
          relvern = daydev * (cli_today%tdmx - tdev)
      else
          relvern = 0.0
      end if
      
      bctchillucum = max( 0.0, bctchillucum + relvern )

      return
    end subroutine chillunit_cum_today

!     calculates the vernalization effectiveness of a day. For fully
!     effective tmeperatures, a full day is returned. for temperatures
!     that are less than fully effective, a partial day or zero is 
!     returned. If temperatures are above the upper temperature 
!     threshold and insufficient chill days are accumulated, devernalization
!     occurs.

!     method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
!     Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
!     Agronomy Monograph 31, pages 34-36.

    subroutine chillunit_cum_temps( bctchillucum, day_max_temp, day_min_temp )
      real, intent(inout) :: bctchillucum  ! accumulated chilling units (days)
      real, intent(in) :: day_max_temp     ! daily maximum temperature (deg.C)
      real, intent(in) :: day_min_temp     ! daily minimum temperature (deg.C)

      ! local variables
      real :: tavg     ! daily everage temperature  (deg.C)
      real :: relvern  ! relative vernalization effectiveness

      real, parameter :: tmin = 0.0     ! minimum temperature in vernalization function (deg.C)
      real, parameter :: tmax = 18.0    ! maximum temperature in vernalization function (deg.C)
      real, parameter :: topt = 7.0     ! optimum temperature in vernalization function (deg.C)
      real, parameter :: tdev = 30.0    ! temperature above which devernalization can occur (deg.C)
      real, parameter :: daylim = 10.0  ! vernalization days beyond which no devernalization can occur (days)
      real, parameter :: daydev = -0.5  ! devernalization days subtracted for each degree C above tdev (days)

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
    end subroutine chillunit_cum_temps

end module crop_climate_mod

