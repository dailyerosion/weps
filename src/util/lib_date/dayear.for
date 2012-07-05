!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/dayear.for,v 1.2 2002-09-04 20:22:18 wagner Exp $
!
      integer   function   dayear (dd, mm, yyyy)

!     + + + PURPOSE + + +
!     Given a date in dd/mm/yyyy format,
!     dayear will return the number of days
!     from the first of that year.
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
!     + + + FUNCTION DECLARATIONS + + +
      integer difdat
!
!     + + + END SPECIFICATIONS + + +
!
!     Get the difference in days + 1
      dayear = difdat(1,1,yyyy, dd,mm,yyyy)+1
      return
      end
!
!$Log: not supported by cvs2svn $
!Revision 1.1.1.1  1999/03/12 17:05:31  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.1.1.1  1995/01/18  04:20:06  wagner
! Initial checkin
!
! Revision 2.1  1992/03/27  17:22:53  wagner
! Version 2 code.
!
