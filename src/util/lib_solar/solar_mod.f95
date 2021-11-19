!$Author$
!$Date$
!$Revision$
!$HeadURL$

module solar_mod

    real, parameter :: beamrise = 90.833  ! solar altitude angle at which the upper edge of the sun is visible
    real, parameter :: civilrise = 96.0   ! solar altitude angle defined as civil twilight
    real, parameter :: nautrise = 102.0   ! solar altitude angle defined as nautical twilight
    real, parameter :: astrorise = 108.0  ! solar altitude angle defined as astronomical twilight
    integer, parameter :: N_spring_eqx = 79 ! day of year for spring equinox in Northern Hemishpere (non leap year)
    integer, parameter :: N_summer_sol = 172 ! day of year for summer solstice in Northern Hemishpere (non leap year)
    integer, parameter :: N_fall_eqx = 264 ! day of year for fall equinox in Northern Hemishpere (non leap year)
    integer, parameter :: N_winter_sol = 355 ! day of year for winter solstice in Northern Hemishpere (non leap year)
    integer, parameter :: S_spring_eqx = 264 ! day of year for spring equinox in Southern Hemishpere (non leap year)
    integer, parameter :: S_summer_sol = 355 ! day of year for summer solstice in Southern Hemishpere (non leap year)
    integer, parameter :: S_fall_eqx = 79 ! day of year for fall equinox in Southern Hemishpere (non leap year)
    integer, parameter :: S_winter_sol = 172 ! day of year for winter solstice in Southern Hemishpere (non leap year)

    real :: amalat  ! site latitude (degrees)
    real :: amalon  ! site longitude (degrees)

  contains

    pure function declination(idoy) result(declin)
      ! This function calculates the declination of the earth with respect
      ! the sun based on the day of the year

      use p1unconv_mod, only: degtorad

      integer, intent(in) :: idoy  ! Day of year

      real :: declin

      real :: b  ! sub calculation (time of year, radians)

!     Calculate declination angle (dec)
      b = (360.0/365.0)*(idoy-81.25) * degtorad       !h-55
      declin = 23.45*sin(b)                           !h-58

    end function declination

    pure function hourangle(dlat, dec, riseangle) result(hangle)
      ! This function calculates the hour angle (degrees)
      ! of sunrise (-), sunset (+) based on the declination of the earth

      use p1unconv_mod, only: degtorad, radtodeg

      real, intent(in) :: dlat      ! Latitude of the site, degrees (north > 0, south < 0)
      real, intent(in) :: dec       ! declination of earth with respect to the sun (degrees)
      real, intent(in) :: riseangle ! angle of earths rotation where sunrise occurs
                                    ! this varies depending on whether you are calculating
                                    ! direct beam, civil twilight, nautical twilight or
                                    ! astronomical twilight hourangle

      real :: hangle

      real :: coshr        ! Cosine of hour angle at sunrise
      real :: dlat_rad     ! latitude of site, converted to radians
      real :: dec_rad      ! declination of earth wrt the sun (radians)

      real, parameter :: dlat_rad_lim  = 1.57079 !  pi/2 minus a small bit

!     convert to radians
      dlat_rad = dlat * degtorad
      dec_rad = dec * degtorad

!     Calculate the cosine of hour angle (h) at sunset
!     To get the sunrise hour angle, take the negative.
!     Using the equation from "Solar Thermal Energy Systems,
!     Howell, Bannerot, Vliet, 1982, page 51 equation 3-4)
!     modified to account for atmospheric refraction as in
!     NOAA document (it just indicates that the sun is seen
!     before it physically is above the horizon)
!     ie. not at 90 degrees, but 90.833 degrees
!     This expression is undefined at 90 and -90 degrees. If
!     roundoff error pushes it beyond the answer flips. Limit
!     set here to get correct answer at 90 and -90 degrees.
      dlat_rad = max( -dlat_rad_lim, min(dlat_rad_lim, dlat_rad))
      coshr = cos(riseangle*degtorad)/(cos(dlat_rad)*cos(dec_rad))      &
     &      - tan(dlat_rad)*tan(dec_rad)

!     check for artic circle conditions
      if( coshr.ge.1.0) then
          hangle = 0.0          !sunrise occurs at solar noon
      else if( coshr.le.-1.0) then
          hangle = 180.0        !the sun is always above the horizon
      else
          hangle = acos(coshr) * radtodeg
      end if

    end function hourangle

    pure function radext(idoy, bmalat) result(extrad)
      ! this subroutine estimates the incoming extraterrestial radiation
      ! for a given location (Mj/m^2/day)

      use p1unconv_mod, only: pi, degtorad

      integer, intent(in) :: idoy  ! julian day of year, 1-366
      real, intent(in) :: bmalat   ! latitude of the site, degrees

      real :: extrad

      real :: rlat  ! latitude (radians)
      real :: dec   ! declination of the earth with respect to the sun (degrees)
      real :: rdec  ! declination (radians)
      real :: dr    ! direct radiation variation with distance from sun of earth orbit
      real :: ws    ! sunset hour angle (radians)
      real :: ra1   ! intermediate calculations
      real :: ra2   ! intermediate calculations

!     + + + parameters + + +
      real, parameter :: gsc = 0.08202 ! solar_constant in Mj/m^2-min (0.08202 Mj/m^2-min = 1367 W/m^2)

!     + + + end specifications + + +

!     convert to radians for trig functions
      rlat = bmalat * degtorad
      dec = declination(idoy)
      rdec  = dec * degtorad

!     compute factor for variable distance from sun along orbital path
      dr = 1 + 0.033*cos(2*pi*idoy/365)                                !h-21

      ws = hourangle(bmalat, dec, beamrise ) * degtorad
      ra1 = ((24.0*60.0)/pi)*gsc*dr                                    !h-20(a)
      ra2 = (ws*sin(rlat)*sin(rdec))+(cos(rlat)*cos(rdec)*sin(ws))     !h-20(b)
      extrad = ra1*ra2                                                 !h-20

    end function radext

    pure function equa_time(idoy) result(equa)
      ! This function calculates the declination of the earth with respect
      ! the sun based on the day of the year

      use p1unconv_mod, only: degtorad

      integer, intent(in) :: idoy  ! Day of year

      real :: equa

      real :: b  ! sub calculation (time of year, radians)

      ! Calculate time of year (b)
      b = (360.0/365.0)*(idoy-81.25) * degtorad            !h-55
      equa = 9.87*sin(2*b)-7.53*cos(b)-1.5*sin(b)          !h-54

    end function equa_time

    pure function daylen( dlat, idoy, riseangle ) result(lenday)
      ! This function calculates the daylength (hours) for any simulation
      ! site based on the global position of the site, and day of the
      ! year.  The inputs for the function are day of the year, and latitude
      ! of the site.

      integer, intent(in) :: idoy   ! Day of year
      real, intent(in) :: dlat      ! Latitude of the site, degrees (north > 0, south < 0)
      real, intent(in) :: riseangle ! angle of earths rotation where sunrise occurs
                                    ! this varies depending on whether you are calculating
                                    ! direct beam, civil twilight, nautical twilight or
                                    ! astronomical twilight daylength

      real :: lenday

      real :: dec  ! declination of earth with respect to the sun (degrees)
      real :: h    ! Hour angle (degrees)

!     declination angle (dec)
      dec = declination(idoy)

!     sunrise or sunset hour angle
      h = hourangle(dlat, dec, riseangle)

!     Calculate the length of the day
      lenday = 2.0*h/15.0

    end function daylen

    pure function dawn( dlat, dlong, idoy, riseangle ) result(dawn_res)
      ! calculates the time of sunrise (hours) for any simulation
      ! site based on the global position of the site, and day of the
      ! year.  The inputs for the function are day of the year, latitude
      ! of the site, and longitude of the site.

      integer, intent(in) :: idoy   ! Day of year
      real, intent(in) :: dlat      ! Latitude of the site, degrees (north > 0, south < 0)
      real, intent(in) :: dlong     ! Longitude of the site, degrees (east > 0, west < 0)
      real, intent(in) :: riseangle ! angle of earths rotation where sunrise occurs
                                    ! this varies depending on whether you are calculating
                                    ! direct beam, civil twilight, nautical twilight or
                                    ! astronomical twilight hourangle

      real :: dawn_res

      real dec  ! declination of earth with respect to the sun (degrees)
      real e    ! Equation of time (minutes)
      real h    ! Hour angle (degrees)
      real sn   ! Solar noon (hour of the day, midnight = 0.0)

!     + + + END SPECIFICATIONS + + +

!     declination angle (dec)
      dec = declination(idoy)

!     sunset hour angle (noon is zero degrees, sunset (+), sunrise (-))
      h = hourangle(dlat, dec, riseangle)

!     equation of time (e)
      e = equa_time(idoy)

!     Calculate solar noon (sn)
      sn = 12.0-e/60.0-4.0*(15*nint(-dlong/15.0)+dlong)/60.0 !h-53

!     Calculate the time of sunrise (rise)
      dawn_res = sn - h/15.0                                   !h-52

!     to prevent errors of bleed over into previous day where
!     where daylength is 24 hours, limit time of sunrise
      dawn_res = max(0.0, dawn_res)

    end function dawn

end module solar_mod

