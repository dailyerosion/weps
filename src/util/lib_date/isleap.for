!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/isleap.for,v 1.2 2002-09-04 20:22:18 wagner Exp $
!
      logical   function   isleap (yyyy)

!     + + + PURPOSE + + +
!     Given a year in yyyy format,
!     isleap will return a .true. if it is a leap year
!     or a .false. if it is not a leap year.
!
!     + + + KEYWORDS + + +
!     date, utility, leap year
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer yyyy
!
!     + + + ARGUMENT DEFINITIONS + + +
!     yyyy   - year
!
!     + + + LOCAL VARIABLES + + +
      integer ld
!
!     + + + FUNCTION DECLARATIONS + + +
      integer lstday
!
!     + + + END SPECIFICATIONS + + +
!
!     Go to the last day of February and see if the 29th exists
      ld= lstday(2, yyyy)
      if (ld.eq.29) then
         isleap = .TRUE.
      else
         isleap = .FALSE.
      endif
      return
      end
!
!$Log: not supported by cvs2svn $
!Revision 1.1.1.1  1999/03/12 17:05:31  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.2  1995/07/23  05:13:25  jt
!  changed comments at top to read ".true." and ".false"
!
! Revision 1.1.1.1  1995/01/18  04:20:07  wagner
! Initial checkin
!
! Revision 2.1  1992/03/27  17:22:53  wagner
! Version 2 code.
!
