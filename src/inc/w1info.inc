!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'w1info.inc'
!
!     This common block contains the character variables containing
!     information about the weather database files and the generator
!     inputs used (if generated data).
!
!     Parameter include files:  p1strlen.inc
!
!     + + + VARIABLE DECLARATIONS + + +
!
      character*(mfname) aw0dif
      character*(mwsta)  aw0cst
      character*(mdate)  aw0ed
      character*(mfname) aw0hif
      character*(mdate)  aw0sd
      character*(mwsta)  aw0wst
      real               aw0cln
      real               aw0clt
      real               aw0wln
      real               aw0wlt
!
      common / w1info /                                                 &
     &                  aw0dif, aw0cst, aw0ed, aw0hif, aw0sd, aw0wst,   &
     &                  aw0cln, aw0clt, aw0wln, aw0wlt

      save :: /w1info/
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     aw0dif - Daily weather database input filename.
!
!            - The daily weather database input file contains all the
!              daily climatic data values necessary for WERM between
!              the starting date and the ending date.
!
!     aw0cst - The weather station used for cligen.
!
!            - The name of the weather station whose historical
!              statistical parameters were used to develop the daily
!              "cligen" weather data used in WERM.
!
!     aw0ed  - The ending date of the daily climatic input file.
!
!            - The date of the last entry in the daily climatic input
!              data file.
!
!     aw0hif - Hourly weather database input filename.
!
!            - The hourly weather database input file contains all the
!              hourly climatic data values necessary for WERM between
!              the starting date and the ending date.
!
!     aw0sd  - The starting date of the daily climatic input file.
!
!            - The date of the first entry in the daily climatic input
!              data file.
!
!     aw0wst - The weather station used for the wind weather generator.
!
!            - The name of the weather station whose historical
!              statistical parameters were used to develop the daily
!              and hourly wind data used in WERM.
!
!     aw0cln - The location of the cligen weather station (longitude).
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.00      360.00
!              typical         70.00      170.00
!              ------------   ---------  ---------
!
!            - The location of the weather station (longitude) used
!              to develop the daily cligen climatic data for WERM.
!
!     aw0clt - The location of the cligen weather station (latitude).
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.00      90.00
!              typical         15.00      75.00
!              ------------   ---------  ---------
!
!            - The location of the weather station (latitude) used
!              to develop the daily cligen climatic data for WERM.
!
!     aw0cln - The location of the wind weather station (longitude).
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.00      360.00
!              typical         70.00      170.00
!              ------------   ---------  ---------
!
!            - The location of the weather station (longitude) used
!              to develop the daily and hourly wind data for WERM.
!
!     aw0wlt - The location of the wind weather station (latitude).
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed            0.00      90.00
!              typical         15.00      75.00
!              ------------   ---------  ---------
!
!            - The location of the weather station (latitude) used
!              to develop the daily and hourly wind data for WERM.
!
!     ------------------------------------------------------------------
