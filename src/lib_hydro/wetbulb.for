!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function wetbulb( airtemp, dewtemp, elevation )

!     + + + PURPOSE + + +
      ! returns the air wet bulb temperature (C)
      ! Approximation from R.L. Snyder, http://biomet/ucdaves.edu

!     + + + KEY WORDS + + +
!     wet bulb temperature

!     + + + COMMON BLOCKS + + +

!     + + + LOCAL COMMON BLOCKS + + +

!     + + + ARGUMENT DECLARATIONS + + +
      real airtemp, dewtemp, elevation

!     + + + ARGUMENT DEFINITIONS + + +
!     airtemp    - the temperature of the air (C)
!     dewtemp    - the dewpoint temperature of the air (C)
!     elevation  - elevation of location (m)

!     + + + PARAMETERS + + +

!     + + + LOCAL VARIABLES + + +
      real vp_sat_air, vp_sat_dew
      real svp_slope_air, svp_slope_dew, svp_slope
      real bp

!     + + + LOCAL DEFINITIONS + + +
!     vp_sat_air - saturated vapor pressure at air temperature (kpa)
!     vp_sat_dew - saturated vapor pressure at dew point temperature (kpa)
!     svp_slope_air - slope of saturated vapor pressure at air temperature
!     svp_slope_dew - slope of saturated vapor pressure at dew temperature
!     svp_slope - average slope of saturated vapor pressure curve
!     bp - barometric pressure (kpa)

!     + + +   FUNCTION CALLS +++
      real satvappres, preslaps
!     satvappres - function to calculate saturated vapor pressure from temperature
!     preslaps - function to calculate barometric pressure from elevation

!     + + + END SPECIFICATIONS + + +

      vp_sat_air = satvappres( airtemp )
      vp_sat_dew = satvappres( dewtemp )

      svp_slope_air = 4099.0 * vp_sat_air / (airtemp + 237.3)**2.
      svp_slope_dew = 4099.0 * vp_sat_dew / (dewtemp + 237.3)**2.

      svp_slope = 0.5 * ( svp_slope_air + svp_slope_dew )

      bp = preslaps( elevation )

      wetbulb = airtemp - (vp_sat_air-vp_sat_dew)/(svp_slope + 0.066*bp)

      return
      end
