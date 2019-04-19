!$Author: fredfox $
!$Date: 2017-07-11 19:01:32 -0600 (Tue, 11 Jul 2017) $
!$Revision: 15514 $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion.plants/src/util/lib_solar/solar_mod.f95 $

module WEPS_solar_mod

    use constants, only: dp, int32, u_pi

    real(dp), parameter :: beamrise = 90.833_dp  ! solar altitude angle at which the upper edge of the sun is visible
    real(dp), parameter :: civilrise = 96.0_dp   ! solar altitude angle defined as civil twilight
    real(dp), parameter :: nautrise = 102.0_dp   ! solar altitude angle defined as nautical twilight
    real(dp), parameter :: astrorise = 108.0_dp ! solar altitude angle defined as astronomical twilight
    real(dp), parameter :: degtorad = u_pi/180.0_dp !pi/180
    real(dp), parameter :: radtodeg = 180.0_dp/u_pi !180/pi

  contains

    real(dp) function declination(idoy)

!     + + + PURPOSE + + +
!     This function calculates the declination of the earth with respect
!     the sun based on the day of the year

!     + + + KEYWORDS + + +
!     solar declination

!     + + + ARGUMENT DECLARATIONS + + +
      integer(int32) :: idoy  ! Day of year

!     + + + LOCAL VARAIBLES + + +
      real(dp) :: b  ! sub calculation (time of year, radians)

!     + + + END SPECIFICATIONS + + +

!     Calculate declination angle (dec)
      b = (360.0_dp/365.0_dp)*(idoy-81.25_dp) * degtorad            !h-55
      declination = 23.45_dp * sin(b)                           !h-58

      return
    end function declination

    real(dp) function hourangle(dlat, dec, riseangle)

!     + + + PURPOSE + + +
!     This function calculates the hour angle (degrees)
!     of sunrise (-), sunset (+) based on the declination of the earth

!     + + + KEYWORDS + + +
!     sunrise sunset hourangle

!     + + + ARGUMENT DECLARATIONS + + +
      real(dp) dlat      ! Latitude of the site, degrees (north > 0, south < 0)
      real(dp) dec       ! declination of earth with respect to the sun (degrees)
      real(dp) riseangle ! angle of earths rotation where sunrise occurs
                     ! this varies depending on whether you are calculating
                     ! direct beam, civil twilight, nautical twilight or
                     ! astronomical twilight hourangle

!     + + + LOCAL VARIABLES + + +
      real(dp) coshr        ! Cosine of hour angle at sunrise
      real(dp) dlat_rad     ! latitude of site, converted to radians
      real(dp) dec_rad      ! declination of earth wrt the sun (radians)

      real(dp), parameter :: dlat_rad_lim  = 1.57079_dp !  pi/2 minus a small bit

!     + + + END SPECIFICATIONS + + +

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
          hourangle = 0.0_dp          !sunrise occurs at solar noon
      else if( coshr.le.-1.0_dp) then
          hourangle = 180.0_dp        !the sun is always above the horizon
      else
          hourangle = acos(coshr) * radtodeg
      end if

      return
    end function hourangle

    real(dp) function radext(idoy, bmalat)

!     + + + purpose + + +
!     this subroutine estimates the incoming extraterrestial radiation
!     for a given location (Mj/m^2/day)

!     + + + key words + + +
!     radiation, solar, extraterrestrial

!     + + + argument declarations + + +
      integer(int32) :: idoy  ! julian day of year, 1-366
      real(dp) :: bmalat   ! latitude of the site, degrees

!     + + + local variables + + +
      real(dp) :: rlat  ! latitude (radians)
      real(dp) :: dec   ! declination of the earth with respect to the sun (degrees)
      real(dp) :: rdec  ! declination (radians)
      real(dp) :: dr    ! direct radiation variation with distance from sun of earth orbit
      real(dp) :: ws    ! sunset hour angle (radians)
      real(dp) :: ra1   ! intermediate calculations
      real(dp) :: ra2   ! intermediate calculations

!     + + + parameters + + +
      real(dp), parameter :: gsc = 0.08202_dp ! solar_constant in Mj/m^2-min (0.08202 Mj/m^2-min = 1367 W/m^2)

!     + + + end specifications + + +

!     convert to radians for trig functions
      rlat = bmalat * degtorad
      dec = declination(idoy)
      rdec  = dec * degtorad

!     compute factor for variable distance from sun along orbital path
      dr = 1 + 0.033_dp*cos(2_dp*u_pi*idoy/365_dp)                                !h-21

      ws = hourangle(bmalat, dec, beamrise ) * degtorad
      ra1 = ((24.0_dp*60.0_dp)/u_pi)*gsc*dr                                    !h-20(a)
      ra2 = (ws*sin(rlat)*sin(rdec))+(cos(rlat)*cos(rdec)*sin(ws))     !h-20(b)
      radext = ra1*ra2                                                 !h-20

      return
    end function radext

    real(dp) function equa_time(idoy)

!     + + + PURPOSE + + +
!     This function calculates the declination of the earth with respect
!     the sun based on the day of the year

!     + + + KEYWORDS + + +
!     solar equation of time

!     + + + ARGUMENT DECLARATIONS + + +
      integer(int32) idoy  ! Day of year

!     + + + LOCAL VARIABLES + + +
      real(dp) b  ! sub calculation (time of year, radians)

!     + + + END SPECIFICATIONS + + +

!     Calculate time of year (b)
      b = (360.0_dp/365.0_dp)*(idoy-81.25_dp) * degtorad                 !h-55
      equa_time = 9.87_dp*sin(2_dp*b)-7.53_dp*cos(b)-1.5_dp*sin(b)          !h-54

      return
    end function equa_time

    real(dp) function daylen(dlat,idoy,riseangle)

!     + + + PURPOSE + + +
!     This function calculates the daylength (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  The inputs for the function are day of the year, and latitude
!     of the site.

!     + + + KEYWORDS + + +
!     day length

!     + + + ARGUMENT DECLARATIONS + + +
      integer(int32) idoy   ! Day of year
      real(dp) dlat      ! Latitude of the site, degrees (north > 0, south < 0)
      real(dp) riseangle ! angle of earths rotation where sunrise occurs
                     ! this varies depending on whether you are calculating
                     ! direct beam, civil twilight, nautical twilight or
                     ! astronomical twilight daylength

!     + + + LOCAL VARIABLES + + +
      real(dp) dec  ! declination of earth with respect to the sun (degrees)
      real(dp) h    ! Hour angle (degrees)

!     + + + END SPECIFICATIONS + + +

!     declination angle (dec)
      dec = declination(idoy)

!     sunrise or sunset hour angle
      h = hourangle(dlat, dec, riseangle)

!     Calculate the length of the day
      daylen= 2.0_dp*h/15.0_dp

      return
    end function daylen

    real(dp) function dawn(dlat,dlong,idoy,riseangle)

!     + + + PURPOSE + + +
!     This function calculates the time of sunrise (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  The inputs for the function are day of the year, latitude
!     of the site, and longitude of the site.

!     + + + KEYWORDS + + +
!     sunrise

!     + + + ARGUMENT DECLARATIONS + + +
      integer(int32) idoy   ! Day of year
      real(dp) dlat      ! Latitude of the site, degrees (north > 0, south < 0)
      real(dp) dlong     ! Longitude of the site, degrees (east > 0, west < 0)
      real(dp) riseangle ! angle of earths rotation where sunrise occurs
                     ! this varies depending on whether you are calculating
                     ! direct beam, civil twilight, nautical twilight or
                     ! astronomical twilight hourangle

!     + + + LOCAL VARIABLES + + +
      real(dp) dec  ! declination of earth with respect to the sun (degrees)
      real(dp) e    ! Equation of time (minutes)
      real(dp) h    ! Hour angle (degrees)
      real(dp) sn   ! Solar noon (hour of the day, midnight = 0.0)

!     + + + END SPECIFICATIONS + + +

!     declination angle (dec)
      dec = declination(idoy)

!     sunset hour angle (noon is zero degrees, sunset (+), sunrise (-))
      h = hourangle(dlat, dec, riseangle)

!     equation of time (e)
      e = equa_time(idoy)

!     Calculate solar noon (sn)
      sn = 12.0_dp-e/60.0_dp-4.0_dp*(15_dp*nint(-dlong/15.0_dp)+dlong)/60.0_dp !h-53

!     Calculate the time of sunrise (rise)
      dawn = sn - h/15.0_dp                                   !h-52

!     to prevent errors of bleed over into previous day where
!     where daylength is 24 hours, limit time of sunrise
      dawn = max(0.0_dp, dawn)

      return
    end function dawn

end module WEPS_solar_mod

