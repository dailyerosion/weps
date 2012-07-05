!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/wkjday.for,v 1.2 2002-09-04 20:22:18 wagner Exp $

      integer   function   wkjday (jday)

!     + + + PURPOSE + + +
!     Given a date in Julian Day format
!     wkjday will give the day of the week.
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
      integer jday
!
!     + + + ARGUMENT DEFINITIONS + + +
!     jday   - Julian Day
!
!     + + + END SPECIFICATIONS + + +
!
!     We simply take the Julian Day and do a modulo of the value
      wkjday=mod(jday, 7)
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
