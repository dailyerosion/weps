!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'p1unconv.inc'

!     These parameter variables are used for unit conversion values
!     within WERM to make the coding more readable and maintainable.
!     Generally, only common unit conversions likely to be used
!     throughout the WERM coding should reside here.  Conversion units
!     used locally in the WERM coding should be defined in separate
!     parameter include files.

!     These parameter values may be consulted from within any sections
!     of the WERM code if this file has been included.

      real             mtomm
      real             mmtom
      integer          hrday
      real             hrtosec
      real             daytosec
      real             degtorad
      real             radtodeg
      real             hatom2
      real             mgtokg
      real             fractopercent
      real             percenttofrac
      real             hrtomin
      real             sectohr

!     + + + VARIABLE DECLARATIONS + + +

      parameter (mtomm  = 1000.0)
      parameter (mmtom  = 0.001)
      parameter (hrday  = 24)
      parameter (hrtosec = 3600.0)
      parameter (daytosec = 86400.0)
      parameter (degtorad = 0.017453293) !pi/180
      parameter (radtodeg = 57.2957795) !180/pi
      parameter (hatom2 = 10000.0) ! hectare to square meters
      parameter (mgtokg = 0.000001) ! milligram to kilogram
      parameter (fractopercent = 100.0)
      parameter (percenttofrac = 0.01)
      parameter (hrtomin = 60.0)
      parameter (sectohr =  1.0/3600.0)

!     + + + VARIABLE DEFINITIONS + + +
!     mtomm  - Unit conversion constant (mm/m)
!     mmtom  - Unit conversion constant (m/mm)
!     hrday  - Number of hours in a day (hrs/day)
!     hrtosec - Unit conversion constant (seconds/hour)
!     daytosec - Unit conversion constant (seconds/day)
!     degtorad - To convert Degrees to Radians, multiply by pi/180 (rad/deg)
!     radtodeg - To convert Radians to Degrees, multiply by 180/pi (deg/rad)
!     hatom2 - To convert hectares to square meters, multiply by 10,000
!     mgtokg - To convert milligrams to kilograms, multiply by 0.000001
!     fractopercent - convert fraction to percent
!     percenttofrac - convert percent to fraction
!     hrtomin - Unit conversion constant (min/hr)
!     sectohr - Unit conversion constant (hr/sec)
!     ------------------------------------------------------------------
