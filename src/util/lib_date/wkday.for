!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/wkday.for,v 1.2 2002-09-04 20:22:18 wagner Exp $

      integer   function   wkday (dd, mm, yyyy)

!     + + + PURPOSE + + +
!     Given a date in dd/mm/yyyy format
!     wkday will give the day of the week.
!     0 = Monday
!     1 = Tuesday
!     2 = Wednesday
!     3 = Thursday
!     4 = Friday
!     5 = Saturday
!     6 = Sunday
!
!     + + + KEYWORDS + + +
!     date, utility
!
!     + + + ARGUMENT DECLARATIONS + + + 
      integer dd, mm, yyyy
!
!     + + + ARGUMENT DEFINITIONS + + +
!     dd     - day
!     mm     - month
!     yyyy   - year
!
!     + + + LOCAL DEFINITIONS + + +
!     julian - julian day value
!
!     + + + FUNCTION DECLARATIONS + + +
      integer julday
!
!     + + + END SPECIFICATIONS + + +
!
!     We simply find the Julian Day and do a modulo of the value
      wkday=mod((julday (dd,mm,yyyy)), 7)
      return
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
! Version 2 code.
!
