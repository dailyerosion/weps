!$Author$
!$Date$
!$Revision$
!$HeadURL$

module air_water_mod

  use climate_input_mod, only: cli_today

  interface precip_cum
    module procedure precip_cum_today
    module procedure precip_cum_p
  end interface precip_cum

  interface no_precip_cum
    module procedure no_precip_cum_today
    module procedure no_precip_cum_p
  end interface no_precip_cum

  interface high_humid_cum
    module procedure high_humid_cum_today
    module procedure high_humid_cum_h
  end interface high_humid_cum

  interface low_humid_cum
    module procedure low_humid_cum_today
    module procedure low_humid_cum_h
  end interface low_humid_cum

  interface rel_humid
    module procedure rel_humid_today
    module procedure rel_humid_tair_tdew
    module procedure rel_humid_2tair_tdew
    module procedure rel_humid_2tair_2tdew
  end interface rel_humid

  interface rel_humid_vp
    module procedure rel_humid_vp_today
    module procedure rel_humid_sat_act
    module procedure rel_humid_2sat_act
    module procedure rel_humid_2sat_2act
  end interface rel_humid_vp

  interface et
    module procedure et_today
    module procedure et_temps
  end interface et

  contains

    ! accumulates daily precipitation amounts above a base value
    function precip_cum_today( p_begin, p_base ) result(p_end)
      real, intent(in) :: p_begin   ! previous precipitation accumlation
      real, intent(in) :: p_base    ! base below which no precipitation accumulates
      real :: p_end

      p_end = p_begin + max(0.0, (cli_today%zdpt-p_base) )

      return
    end function precip_cum_today

    ! accumulates daily precipitation amounts above a base value
    function precip_cum_p( p_begin, p_base, p_day ) result(p_end)
      real, intent(in) :: p_begin   ! previous precipitation accumlation
      real, intent(in) :: p_base    ! base below which no precipitation accumulates
      real, intent(in) :: p_day     ! daily precipitation
      real :: p_end

      p_end = p_begin + max(0.0, (p_day-p_base) )

      return
    end function precip_cum_p

    ! accumulates day with lack of daily precipitation amounts above a base value
    function no_precip_cum_today( np_begin, np_base ) result(np_end)
      real, intent(in) :: np_begin   ! previous accumlation of days without precipitation
      real, intent(in) :: np_base    ! base below which no precipitation accumulates
      real :: np_end

      np_end = no_precip_cum( np_begin, np_base, cli_today%zdpt )

      return
    end function no_precip_cum_today

    ! accumulates day with lack of daily precipitation amounts above a base value
    function no_precip_cum_p( np_begin, np_base, p_day ) result(np_end)
      real, intent(in) :: np_begin   ! previous accumlation of days without precipitation
      real, intent(in) :: np_base    ! base below which no precipitation accumulates
      real, intent(in) :: p_day      ! daily precipitation
      real :: np_end

      if( p_day .lt. np_base ) then
        ! no significant rain
        np_end = np_begin + 1
      else
        ! significant rain
        np_end = np_begin / 2.0
      end if 

      return
    end function no_precip_cum_p

    ! accumulates consecutive days daily humidity is above a base value
    function high_humid_cum_today( h_begin, h_base ) result(h_end)
      real, intent(in) :: h_begin   ! previous humidity days accumlation
      real, intent(in) :: h_base    ! base below which no high humidity days accumulate
      real :: h_end

      if( rel_humid() .ge. h_base ) then
        ! humidity is equal or above base value
        h_end = h_begin + 1
      else
        ! humidity is below base value
        h_end = h_begin / 2.0
      end if

      return
    end function high_humid_cum_today

    ! accumulates consecutive days daily humidity is above a base value
    function high_humid_cum_h( h_begin, h_base, h_day ) result(h_end)
      real, intent(in) :: h_begin   ! previous humidity days accumlation
      real, intent(in) :: h_base    ! base below which no high humidity days accumulate
      real, intent(in) :: h_day     ! daily humidity
      real :: h_end

      if( h_day .ge. h_base ) then
        ! humidity is equal or above base value
        h_end = h_begin + 1
      else
        ! humidity is below base value
        h_end = h_begin / 2.0
      end if

      return
    end function high_humid_cum_h

    ! accumulates consecutive days daily humidity is below a base value
    function low_humid_cum_today( h_begin, h_base ) result(h_end)
      real, intent(in) :: h_begin   ! previous humidity days accumlation
      real, intent(in) :: h_base    ! base above which no low humidity days accumulate
      real :: h_end

      if( rel_humid() .le. h_base ) then
        ! humidity is equal or below base value
        h_end = h_begin + 1
      else
        ! humidity is above base value
        h_end = h_begin / 2.0
      end if

      return
    end function low_humid_cum_today

    ! accumulates consecutive days daily humidity is below a base value
    function low_humid_cum_h( h_begin, h_base, h_day ) result(h_end)
      real, intent(in) :: h_begin   ! previous humidity days accumlation
      real, intent(in) :: h_base    ! base above which no low humidity days accumulate
      real, intent(in) :: h_day     ! daily humidity
      real :: h_end

      if( h_day .le. h_base ) then
        ! humidity is equal or below base value
        h_end = h_begin + 1
      else
        ! humidity is above base value
        h_end = h_begin / 2.0
      end if

      return
    end function low_humid_cum_h

    ! finds the relative humidity of the air using todays climate data
    function rel_humid_today( ) result(rel_hum)
      real :: rel_hum

      rel_hum = rel_humid( cli_today%tdmx, cli_today%tdmn, cli_today%tdpt )

      return
    end function rel_humid_today

    ! finds the relative humidity of the air using 1 air temperature and 1 dew point temperature
    function rel_humid_tair_tdew( tair, tdew ) result(rel_hum)
      real, intent(in) :: tair  ! air temperature
      real, intent(in) :: tdew  ! dew point temperature
      real :: rel_hum

      rel_hum = satvappres(tdew)/satvappres(tair)

      return
    end function rel_humid_tair_tdew

    ! finds the relative humidity of the air using 2 air temperature and 1 dew point temperature
    ! uses the recommendation of FAO pub 56, Crop Evapotranspiration - Guidelines for computing crop water requirements.
    function rel_humid_2tair_tdew( tmax, tmin, tdew ) result(rel_hum)
        real, intent(in) :: tmax  ! maximum temperature in period (day or less)
        real, intent(in) :: tmin  ! minimum temperature in period (day or less)
        real, intent(in) :: tdew  ! average dew point temperature in period (day or less)
        real :: rel_hum

        rel_hum = satvappres(tdew) &
                / (0.5 * (satvappres(tmax) + satvappres(tmin)))

      return
    end function rel_humid_2tair_tdew

    ! finds the relative humidity of the air using 2 air temperature and 2 dew point temperature
    ! this is a logical extension of the FAO pub 56 recommendation
    function rel_humid_2tair_2tdew( tmax, tmin, tdamax, tdamin ) result(rel_hum)
        real, intent(in) :: tmax  ! maximum temperature in period (day or less)
        real, intent(in) :: tmin  ! minimum temperature in period (day or less)
        real, intent(in) :: tdamax  ! dew point temperature at maximum air temperature in period (day or less)
        real, intent(in) :: tdamin  ! dew point temperature at minimum air temperature in period (day or less)
        real :: rel_hum

        rel_hum = (0.5 * (satvappres(tdamax) + satvappres(tdamin))) &
                / (0.5 * (satvappres(tmax) + satvappres(tmin)))

      return
    end function rel_humid_2tair_2tdew

    ! finds the relative humidity of the air using 2 saturated vapor pressures and 2 actual vapor pressures
    ! measure at the temperature extremes for the period.
    ! This is a logical extension of the FAO pub 56 recommendation
    function rel_humid_vp_today() result(rel_hum)
        real :: rel_hum

        ! cligen record currently does not store vapor pressure values. If extended, replace function call with values.
        rel_hum = rel_humid_2sat_act( satvappres(cli_today%tdmx), satvappres(cli_today%tdmn), satvappres(cli_today%tdpt) )

      return
    end function rel_humid_vp_today

    ! finds the relative humidity of the air using 2 saturated vapor pressures and 2 actual vapor pressures
    ! measure at the temperature extremes for the period.
    ! This is a logical extension of the FAO pub 56 recommendation
    function rel_humid_sat_act( satvp, actvp ) result(rel_hum)
        real, intent(in) :: satvp  ! saturated vapor pressure
        real, intent(in) :: actvp  ! actual vapor pressure
        real :: rel_hum

        rel_hum = actvp / satvp

      return
    end function rel_humid_sat_act

    ! finds the relative humidity of the air using 2 saturated vapor pressures and 2 actual vapor pressures
    ! measure at the temperature extremes for the period.
    ! This is a logical extension of the FAO pub 56 recommendation
    function rel_humid_2sat_act( satvptmax, satvptmin, actvptdew ) result(rel_hum)
        real, intent(in) :: satvptmax  ! saturated vapor pressure at maximum temperature in period (day or less)
        real, intent(in) :: satvptmin  ! saturated vapor pressure at minimum temperature in period (day or less)
        real, intent(in) :: actvptdew  ! actual vapor pressure from average dew point temperature for period (day or less)
        real :: rel_hum

        rel_hum =  actvptdew / (0.5 * (satvptmax + satvptmin))

      return
    end function rel_humid_2sat_act

    ! finds the relative humidity of the air using 2 saturated vapor pressures and 2 actual vapor pressures
    ! measure at the temperature extremes for the period.
    ! This is a logical extension of the FAO pub 56 recommendation
    function rel_humid_2sat_2act( satvptmax, satvptmin, actvpmax, actvpmin ) result(rel_hum)
        real, intent(in) :: satvptmax  ! saturated vapor pressure at maximum temperature in period (day or less)
        real, intent(in) :: satvptmin  ! saturated vapor pressure at minimum temperature in period (day or less)
        real, intent(in) :: actvpmax  ! actual vapor pressure at time of maximum air temperature in period (day or less)
        real, intent(in) :: actvpmin  ! actual vapor pressure at time of minimum air temperature in period (day or less)
        real :: rel_hum

        rel_hum = (0.5 * (actvpmax + actvpmin)) &
                / (0.5 * (satvptmax + satvptmin))

      return
    end function rel_humid_2sat_2act

    ! returns the saturated vapor pressure for water (kPa)
    ! Approximation from Jensen ASCE manual 70 evapotranspiration
    ! referenced to Tetens (1930), and transformed by Murray (1966)
    ! Converted here to use temperature in (C)
    ! valid in normal climatic condition range
    function satvappres( airtemp ) result(sat_vp)
      real, intent(in) :: airtemp ! the temperature of the air (C)
      real :: sat_vp

      ! c1, c2, c3 -  coefficients for saturated equation
      real, parameter :: c1 = 0.611  
      real, parameter :: c2 = 17.27
      real, parameter :: c3 = 237.3

      sat_vp = c1 * exp( c2 * airtemp/( airtemp + c3 ) )

      return
    end function satvappres

    ! returns the air wet bulb temperature (C)
    ! Approximation from R.L. Snyder, http://biomet/ucdaves.edu
    function wetbulb( airtemp, dewtemp, elevation ) result(wet_bulb)
      real, intent(in) :: airtemp    ! the temperature of the air (C)
      real, intent(in) :: dewtemp    ! the dewpoint temperature of the air (C)
      real, intent(in) :: elevation  ! elevation of location (m)
      real :: wet_bulb

      ! local variables
      real :: vp_sat_air    ! saturated vapor pressure at air temperature (kpa)
      real :: vp_sat_dew    ! saturated vapor pressure at dew point temperature (kpa)
      real :: svp_slope_air ! slope of saturated vapor pressure at air temperature
      real :: svp_slope_dew ! slope of saturated vapor pressure at dew temperature
      real :: svp_slope     ! average slope of saturated vapor pressure curve
      real :: bp            ! barometric pressure (kpa)

      vp_sat_air = satvappres( airtemp )
      vp_sat_dew = satvappres( dewtemp )

      svp_slope_air = 4099.0 * vp_sat_air / (airtemp + 237.3)**2.
      svp_slope_dew = 4099.0 * vp_sat_dew / (dewtemp + 237.3)**2.

      svp_slope = 0.5 * ( svp_slope_air + svp_slope_dew )

      bp = preslaps( elevation )

      wet_bulb = airtemp - (vp_sat_air-vp_sat_dew)/(svp_slope + 0.066*bp)

      return
    end function wetbulb

    ! returns the standard atmospheric pressure (kpa) as a function of
    ! elevation (m) based on curve fit by Abdu Durar to standard U.S.
    ! Atmosphere tables
    function preslaps( elevation ) result(pres_lapse)
      real, intent(in) :: elevation  ! elevation of location (m)
      real :: pres_lapse

      ! a1,a2,a3 - constants used to compute barametric pressure
      real, parameter :: a1 = 824.4996
      real, parameter :: a2 = 35702.8022
      real, parameter :: a3 = -607945000.

      pres_lapse = a1 * exp(((elevation + a2)**2.) / a3)

      return
    end function preslaps

    ! This subroutine calculates daily potential evapotranspiration
    ! using Van Bavel's (1966) revised combination method.
    function  et_today(rn, g_soil, vel_wind, bmzele, loc_za, loc_zo, loc_zd) result(bhzetp)
      real, intent(in) :: rn        ! Net radiation (Mj/m^2/day)
      real, intent(in) :: g_soil    ! ground heat flux (Mj/m^2/day)
      real, intent(in) :: vel_wind  ! Wind speed (m/s) at meteorological height (loc_za)
      real, intent(in) :: bmzele    ! Elevation of the site (m)
      ! the following must be in consistent units (length)
      real, intent(in) :: loc_za    ! height of meteorological measurement 
      real, intent(in) :: loc_zo    ! aerodynamic roughness length
      real, intent(in) :: loc_zd    ! zero plane displacement
      real :: bhzetp   ! potential evaporation depth (mm)

      bhzetp = et(rn, g_soil, vel_wind, bmzele, cli_today%tdmx, cli_today%tdmn, cli_today%tdav, &
                  cli_today%tdpt, loc_za, loc_zo, loc_zd)

    end function et_today

    ! This subroutine calculates daily potential evapotranspiration
    ! using Van Bavel's (1966) revised combination method.
    function  et_temps(rn, g_soil, vel_wind, bmzele, bwtdmx, bwtdmn, &
                   bwtdav, bwtdpt, loc_za, loc_zo, loc_zd) result(bhzetp)
      real, intent(in) :: rn        ! Net radiation (Mj/m^2/day)
      real, intent(in) :: g_soil    ! ground heat flux (Mj/m^2/day)
      real, intent(in) :: vel_wind  ! Wind speed (m/s) at meteorological height (loc_za)
      real, intent(in) :: bmzele    ! Elevation of the site (m)
      real, intent(in) :: bwtdmn    ! daily maximum temperature (C)
      real, intent(in) :: bwtdmx    ! daily minimum temperature (C)
      real, intent(in) :: bwtdav    ! daily average temperature (C)
      real, intent(in) :: bwtdpt    ! daily average dew point temperature (C)
      ! the following must be in consistent units (length)
      real, intent(in) :: loc_za    ! height of meteorological measurement 
      real, intent(in) :: loc_zo    ! aerodynamic roughness length
      real, intent(in) :: loc_zd    ! zero plane displacement
      real :: bhzetp   ! potential evaporation depth (mm)  

      ! b1,b2,b3 - constants used in eqn for (svpg0)
      real, parameter :: b1 = 67.5242
      real, parameter :: b2 = 149.531
      real, parameter :: b3 = -4859.0665

      ! d1,d2 - constants used to compute latent heat of vaporization
      real, parameter :: d1 = 2.5002773719
      real, parameter :: d2 = 0.0023644939

      real, parameter :: e = 0.622     ! water to air molecular weights ratio
      real, parameter :: vk = 0.41     ! Von Karman's constant
      real, parameter :: dt_cli = 2.0  ! minimum dew point depression for no adjustment
      real, parameter :: k_arid = 0.5  ! emperical aridity proportioning coefficient

      ! local variables
      real :: term1   ! temporary
      real :: term2   ! temporary
      real :: term3   ! temporary
      real :: bp      ! Barometric pressure (kpa)
      real :: svpg    ! Ratio of saturation vapor pressure curve slope to the pychrometric constant,
                      ! adjusted to ambient barometric pressure (unitless)
      real :: svpg0   ! Unadjusted ratio of the saturation vapor pressure curve slope to the pychrometric constant (unitless)
      real :: vlh     ! Latent heat of vaporization (Mj/kg)
      real :: vpa     ! Actual vapor pressure (kpa)
      real :: vpd     ! Saturation vapor pressure deficit (kpa)
      real :: vps     ! Saturated vapor pressure (kpa)
      real :: vpsmn   ! Saturated vapor pressure at min air temp (kpa)
      real :: vpsmx   ! Saturated vapor pressure at max air temp (kpa)
      real :: arho    ! Air density (kg/m^3)
      real :: ttc     ! Turbulent transfer coefficient (kg/m^2/kpa/day)
      real :: zo_v    ! estimate of aerodynamic roughness length for vapor transfer

      real :: deltat  ! dew point depression below minimum temperature with climate adjustment
      real :: tmaxadj ! maximum temperature adjusted to field site for potential ET conditions
      real :: tminadj ! minimum temperature adjusted to field site for potential ET conditions
      real :: tdavadj ! daily average temperature adjusted to field site for potential ET conditions
      real :: tdewadj ! dew point temperature adjusted to field site for potential ET conditions

      !real :: etpr    ! Potential evapotransp due to radiation (mm/day)
      !real :: etpw    ! Potential evapotranspiration due to wind (mm/day)

      ! check dew point and minimum temperature for variance from a freely
      ! transpiring surface. In calculating potential ET, Tmax and Tmin will 
      ! tend to be lower than over a non-transpiring surface as indicated by
      ! Tmin being significantly greater than Tdew. This adjustment process
      ! is described in Allen, R.G. 1996. Assessing Integrity of Weather
      ! Data for Reference Evapotranspiration Estimation. Journal of Irrigation 
      ! and Drainage Engineering, vol 122(2)

      deltat = bwtdmn - bwtdpt - dt_cli
      if( deltat .gt. 0.0 ) then
          tmaxadj = bwtdmx - k_arid * deltat
          tminadj = bwtdmn - k_arid * deltat
          tdavadj = 0.5 * (tmaxadj + tminadj)
          tdewadj = bwtdpt + (1.0 - k_arid) * deltat
      else
          tmaxadj = bwtdmx
          tminadj = bwtdmn
          tdavadj = bwtdav
          tdewadj = bwtdpt
      end if

      bp = preslaps( bmzele )
      svpg0 = b1 * exp(((tdavadj-b2)**2.) / b3)
      svpg = svpg0*(101.325/bp)
      vlh = d1 - (d2*tdavadj)
      term1 = svpg * ((rn-g_soil)/vlh)

      vpa = satvappres( tdewadj )
      vpsmn = satvappres( tminadj )
      vpsmx = satvappres( tmaxadj )
      vps = 0.5 * (vpsmn + vpsmx)
      vpd = vps - vpa
      arho = 1000.*((bp/101.325)*((0.001293)/(1+(0.00367*tdavadj))))

      ! Jensen, M.E., R.D. Burman, R.G.Allen. 1990. Evapotranspiration and
      ! Irrigation Water Requirements. ASCE Manuals and Reports on Engineering
      ! Practice No. 70, Page 91 states that the Bussinger-Van Bavel method does
      ! not work using aerodynamic roughness for momentum. Their estimate is that
      ! using 1/10th of zo gives more reasonable answers. This is consistent
      ! with discussion on page 94 that the aerodynamic roughness length for
      ! vapor transfer is at least 2/10th and maybe even less than 1/10th
      ! that of the aerodynamic roughness for momentum.
      zo_v = 0.1 * loc_zo

      ! van Bavel gives za as the height of the measurement instruments above
      ! the surface. In his example, he clarifies this as a height above the crop 
      ! surface, not the ground surface. This is probably best approximated here
      ! as height above the ground minus zero plane displacement.
      ! (meteorological height adjusted to account for crop (residue) height in
      ! hydro before call)
      ttc = (arho * e * (vk**2.) * vel_wind * 86400.0) &
     &    / (bp * (log( (loc_za - loc_zd) / zo_v) )**2.)

      term2 = vpd*ttc
      term3 = svpg + 1.0
      bhzetp = (term1+term2)/term3
      if ( bhzetp  .le. 0.0 )  bhzetp = 0.0

      !etpr = term1 / term3
      !etpw = term2 / term3

      return
    end function  et_temps

    ! returns the water vapor density in air (kg/m^3)
    ! calculated directly from PV = nRT
    function vaporden( airtemp, relhum ) result(vap_den)
      real, intent(in) :: airtemp  ! the temperature of the air (C)
      real, intent(in) :: relhum   ! relative humidity of the air (fraction)
      real :: vap_den

      ! local variables
      real actvappres  ! actual vapor pressure (kPa)

      actvappres =  relhum * satvappres(airtemp)

      !if(actvappres.lt.1.0e-34) write(*,*) 'vaporden:',relhum,airtemp
      vap_den = 2.166 * actvappres / (airtemp + 273.15)

      return
    end function vaporden

end module air_water_mod

