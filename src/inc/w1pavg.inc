!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'w1pavg.inc'
!
!     This common block contains the state variables relating to
!     the monthly average WERM weather parameters.
!
!     Parameter include files:
!
!     + + + VARIABLE DECLARATIONS + + +
!
      real             awtmmx
      real             awtmmn
      real             awdair
      real             awztpt
      real             awtpav
      real             awupav
      real             awnuet
      real             aweuet
!
      common / w1pavg /                                                 &
     &                  awtmmx, awtmmn, awdair, awztpt, awtpav,         &
     &                  awupav, awnuet, aweuet

      save :: /w1pavg/
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     awtmmx - Maximum monthly air temperature (deg C) average
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the average for the month
!              of the maximum daily air temperature.
!
!     awtmmn - Minimum monthly air temperature (deg C) average
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the average of the month
!              of the minimum daily air temperature.
!
!     awdair - Average air density (Kg/m^3) for the month
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the average of the monthly
!              air density.
!
!     awzppt - Total period precipitation (mm)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the value of the total period
!              precipitation.
!
!     awtpav - Mean period air temperature (deg C)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the value of the mean period
!              air temperature.
!
!     awupav - Average daily daily windspeed (m/s)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the value of the average daily
!              windspeed the simulation period.
!
!     awnuet - Number of days wind speed exceeds threshold
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the number of days the wind
!              speed exceeds the threshold.
!
!     aweuet - Total wind energy exceeding threshold (MJ)
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------
!
!            - This variable contains the value of the total wind
!              energy that exceeds the threasold.
!
!     ------------------------------------------------------------------


