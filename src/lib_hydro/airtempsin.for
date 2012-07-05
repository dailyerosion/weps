!$Author$
!$Date$
!$Revision$
!$HeadURL$

      real function airtempsin(tsec, tmax, tmin)

!     + + + PURPOSE + + +
!     Returns the value of air temperature as a function of time of day
!     using a sinusoidal approximation of temperature through the daily
!     maximum and daily minimum, which are assumed to occur at 6pm and
!     6am respectively.

!     + + + ARGUMENT DECLARATIONS + + +
      real  tsec, tmax, tmin

!     + + + ARGUMENT DEFINITIONS + + +
!     tsec  - time of day with 0 at midnight (seconds)
!     tmax  - daily maximum temperature (C)
!     tmin  - daily minimum temperature (C)

!*** LOCAL DECLARATIONS ***
      real pi, halfperiod
      parameter( pi = 3.1415927 )
      parameter( halfperiod = 43200 )

      airtempsin = 0.5*(tmax + tmin                                     &
     &           + (tmax-tmin)*sin(pi*(tsec/halfperiod +1.0)))

      return
      end
