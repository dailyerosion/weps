!$Author$
!$Date$
!$Revision$
!$HeadURL$

      subroutine mgdreset (bhzirr)

!     + + + PURPOSE + + +
!     mgdreset is called before any management operations for the day are 
!     executed. It resets global variables that are set in management
!     that should only apply for a single day. Resetting them here makes
!     sure that any submodel that needs to use them will have access to
!     them for exactly one day.

      use manage_mod, only: am0til

!     + + + PARAMETERS AND COMMON BLOCKS + + +

      include 'p1werm.inc'
      include 'm1flag.inc'

!     + + + ARGUMENT DECLARATIONS + + +
      real :: bhzirr   ! daily irrigation amount

!     + + + END SPECIFICATIONS + + +

      am0til = .false.
      bhzirr = 0.0   ! zero out irrig amount from previous day

      return
      end
