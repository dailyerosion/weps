!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'main\w1win.inc'

!     This common block contains the buffers that wingen data is read into

!     Edit History
!     08-Mar-99   wjr   created
!
!     Parameter include files: p1werm.inc

!     + + + VARIABLE DECLARATIONS + + +

      integer  wwd(mndayr)
      integer  wwm(mndayr)
      integer  wwy(mndayr)
      integer  daywdx
      integer  newyrwdx
      logical  wwrnflg
      
      real     wwadir(mndayr) 
      real     wwudmx(mndayr)
      real     wwudmn(mndayr)
      real     wwhrmx(mndayr)

! New variables for reading in wind_gen2 data
      real     wawu(mntime,mndayr)
      real     wawudav(mndayr)
      real     wewudav(mndayr)
      real     twe(mndayr)
      
      common   /w1win/                                                  &
     &  wwadir, wwudmx, wwudmn, wwhrmx,                                 &
     &  wawu, wewudav, wawudav, twe,                                    &
     &  wwd, wwm, wwy, daywdx, newyrwdx, wwrnflg

      save :: /w1win/

! the following definitions don't match the variables declared.  
! this should be fixed.      
! NOTE: these came directly from w1wind.inc, that is why they are not correct.
! LEW - Wed Nov  8 08:54:56 CST 2000

!     + + + VARIABLE DEFINITIONS + + +

!     daywdx  - index for current day on year in windgen record buffer
!     newyrwdx - index for first day of next year read into buffer
!     wwrnflg - flag to show or turn off date mismatch warnings

!     wwadir - Predominant daily wind direction (degrees)

!              range values     min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the value of the predominant
!              daily wind direction.

!     wwhrmx - Hour maximum daily wind speed occurs (hr)

!              range values     min        max
!              ------------   ---------  ---------
!              fixed             1          24
!              typical          tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the hour at which the maximum
!              daily wind speed occurs.

!     wwrmxn - Daily maximum/minimum ratio

!              range values     min        max
!              ------------   ---------  ---------
!              fixed             0         fmax
!              typical          tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the ratio of the maximum to
!              minimum wind speed for the day.

!     wwudmx - Maximum daily wind speed (m/s)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the value of the maximum daily
!              wind speed.

!     wwudmn - Minimum daily wind speed (m/s)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the value of the minimum daily
!              wind speed.

!     wawudav - Average daily wind speed (m/s)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the value of the average daily
!              wind speed.

!     wewudav - Average daily wind speed (m/s) (to obtain the daily wind energy)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          fmin       fmax
!              typical        tmin       tmax
!              ------------   ---------  ---------

!            - This variable contains the value of the average daily
!              wind speed that would generate the total wind energy for the day.

!     wawu   - Subdaily (hourly) wind speeds (m/s)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          0          fmax
!              typical        0          tmax
!              ------------   ---------  ---------

!            - This variable contains the value of the (hourly) subdaily
!              wind speeds for the day.

!      twe   - Daily total wind energy (KJ)

!              range values       min        max
!              ------------   ---------  ---------
!              fixed          0          fmax
!              typical        0          tmax
!              ------------   ---------  ---------

!            - This variable contains the total daily wind energy value.

!     ------------------------------------------------------------------
