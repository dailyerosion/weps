!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/mvdate.for,v 1.2 2002-09-04 20:22:18 wagner Exp $

      subroutine   mvdate (delta, dd, mm, yyyy, nday, nmonth, nyear)

!     + + + PURPOSE + + +
!     Compute a date which is delta number of days before or after the
!     date that is passed.
!
!     + + + KEYWORDS + + +
!     date, utility
!
!     + + + ARGUMENT DECLARATIONS + + + 
      integer delta, dd, mm, yyyy, nday, nmonth, nyear
!
!     + + + ARGUMENT DEFINITIONS + + +
!     delta  - positive or negative integer indicating number of days
!     dd     - day   -\
!     mm     - month   >-- passed in parameters. WILL NOT CHANGE
!     yyyy   - year  -/
!
!     nday   - day   -\
!     nmonth - month   >-- results are shipped out in here
!     nyear  - year  -/
!
!     + + + SUBROUTINES CALLED + + +
!     caldat
!
!     + + + FUNCTION DECLARATIONS + + +
      integer julday
!
!     + + + END SPECIFICATIONS + + +
!
      call caldat ((julday (dd, mm, yyyy)+delta), nday, nmonth, nyear)
      end
!
!$Log: not supported by cvs2svn $
!Revision 1.1.1.1  1999/03/12 17:05:31  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.1.1.1  1995/01/18  04:20:07  wagner
! Initial checkin
!
! Revision 2.1  1992/03/27  17:22:53  wagner
! Removed extraneous assignments to nday,
! nmonth, and nyear which were made before
! caldat was called.
!
! Version 2 code.
!
