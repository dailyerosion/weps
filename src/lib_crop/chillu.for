!$Author$
!$Date$
!$Revision$
!$HeadURL$
       subroutine chillu(bctchillucum, day_max_temp, day_min_temp)

!     + + + PURPOSE + + +
!     calculates the vernalization effectiveness of a day. For fully
!     effective tmeperatures, a full day is returned. for temperatures
!     that are less than fully effective, a partial day or zero is 
!     returned. If temperatures are above the upper temperature 
!     threshold and insufficient chill days are accumulated, devernalization
!     occurs.

!     method taken from: Ritchie, J.T. 1991. Wheat Phasic development in: 
!     Hanks, J. and Ritchie, J.T. eds. Modeling plant and soil systems.
!     Agronomy Monograph 31, pages 34-36.

!     + + + KEYWORDS + + +
!     vernalization chill units

!     + + + ARGUMENT DECLARATIONS + + +
      real bctchillucum, day_max_temp, day_min_temp

!     + + + ARGUMENT DEFINITIONS + + +
!     bctchillucum - accumulated chilling units (days)
!     day_max_temp - daily maximum temperature (deg.C)
!     day_min_temp - daily minimum temperature (deg.C)

!     + + + LOCAL VARIABLES + + +
      real tavg, relvern
      real tmin, tmax, topt, tdev
      real daylim, daydev

      parameter(tmin = 0.0)
      parameter(tmax = 18.0)
      parameter(topt = 7.0)
      parameter(tdev = 30.0)
      parameter(daylim = 10.0)
      parameter(daydev = -0.5)

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     tavg     - daily everage temperature  (deg.C)
!     relvern  - relative vernalization effectiveness
!     tmin     - minimum temperature in vernalization function (deg.C)
!     tmax     - maximum temperature in vernalization function (deg.C)
!     topt     - optimum temperature in vernalization function (deg.C)
!     tdev     - temperature above which devernalization can occur (deg.C)
!     daylim   - vernalization days beyond which no devernalization can occur (days)
!     daydev   - devernalization days subtracted for each degree C above tdev (days)

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
      end