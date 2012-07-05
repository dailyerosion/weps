!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/lstday.for,v 1.2 2002-09-04 20:22:18 wagner Exp $

      integer function  lstday (mm, yyyy)

!     + + + PURPOSE + + +
!     Given a date in mm/yyyy format, lstday will return the last day
!     of that month.
!
!     + + + KEYWORDS + + +
!     date, utility
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer mm, yyyy
!
!     + + + ARGUMENT DEFINITIONS + + +
!     mm     - month
!     yyyy   - year
!
!     + + + LOCAL VARIABLES + + +
      integer lm, ld, ly
      integer julian
!
!     + + + LOCAL DEFINITIONS + + +
!     julian - julian day value
!
!     + + + SUBROUTINES CALLED + + +
!     caldat
!
!     + + + FUNCTION DECLARATIONS + + +
      integer julday
!
!     + + + END SPECIFICATIONS + + +
!
!     Go to the first day of the next month
!     (This is exactly one day after the day we want to find)
      lm=mm+1
      ld=1
      ly=yyyy
      if (lm.eq.13) then
         lm=1
         ly=yyyy+1
      end if
!     We simply find the Julian Day and subtract 1 day to get the last
!     day of the previous month
      julian=julday (ld, lm, ly)-1
!     Now convert back to gregorian calendar to get the actual day
      call caldat (julian, ld, lm, ly)
      lstday=ld
!      return
      end
!
!$Log: not supported by cvs2svn $
!Revision 1.1.1.1  1999/03/12 17:05:31  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.2  1998/09/03  18:35:39  jt
!  check in a full copy of WEPS - I hope ???
!
! Revision 1.1.1.1  1995/01/18  04:20:07  wagner
! Initial checkin
!
! Revision 2.1  1992/03/27  17:22:53  wagner
! Version 2 code.
!
