!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine mgdreset (sr)

!     + + + PURPOSE + + +
!     mgdreset is called before any management operations for the day are 
!     executed. It resets global variables that are set in management
!     that should only apply for a single day. Resetting them here makes
!     sure that any submodel that needs to use them will have access to
!     them for exactly one day.

!     + + + PARAMETERS AND COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'h1hydro.inc'
      include 'm1flag.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      integer sr
!
!     + + + ARGUMENT DEFINITIONS + + +
!     sr - current subregion
!
!     + + + LOCAL VARIABLES + + +
!
!     + + + SUBROUTINES CALLED + + +
!
!     + + + FUNCTION DECLARATONS + + +

!     + + + DATA INITIALIZATIONS + + +

!     + + + END SPECIFICATIONS + + +

      am0til = .false.
      ahzirr(sr) = 0.0	! zero out irrig amount from previous day

      return
      end