!$Author$
!$Date$
!$Revision$
!$HeadURL$
!     ------------------------------------------------------------------
!     file 'p1strlen.inc'
!
!     These parameter variables are used to size certain global strings
!     at compile time for WERM.  They may also be consulted from within
!     sections of the WERM code if this file has been included.
!
      integer          mfname
      integer          mwsta
      integer          mloc
      integer          mdate
!
!     + + + VARIABLE DECLARATIONS + + +
!
      parameter (mfname = 30)
      parameter (mwsta = 30)
      parameter (mloc = 10)
      parameter (mdate = 10)
!
!     + + + VARIABLE DEFINITIONS + + +
!
!     mfname - Maximum length of filenames
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          OS dependent
!              typical        15         15
!              ------------   ---------  ---------
!
!            - The length of character strings containing filenames
!              to be handled by any compiled version of WERM is
!              specified by this parameter variable.
!
!     mwsta  - Maximum length of weather station names
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          OS dependent
!              typical        30         30
!              ------------   ---------  ---------
!
!            - The length of character strings containing weather
!              station names to be handled by any compiled version
!              of WERM is specified by this parameter variable.
!
!     mloc   - Maximum length of weather station location strings
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          1          OS dependent
!              typical        10         10
!              ------------   ---------  ---------
!
!            - The length of character strings containing the
!              location of weather station names to be handled
!              by any compiled version of WERM is specified
!              by this parameter variable.
!
!     mdate  - Maximum length of date strings
!
!              range values       min        max
!              ------------   ---------  ---------
!              fixed          8          OS dependent
!              typical        10         10
!              ------------   ---------  ---------
!
!            - The length of character strings containing dates
!              to be handled by any compiled version of WERM is
!              specified by this parameter variable.
!
!     ------------------------------------------------------------------

