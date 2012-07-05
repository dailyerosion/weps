!$Author$
!$Date$
!$Revision$
!$HeadURL$

!$Header: /weru/cvs/weps/weps.src/util/date/caldatw.for,v 1.2 2002-09-04 20:22:17 wagner Exp $

      subroutine   caldatw (dd, mm, yyyy)

!     + + + PURPOSE + + +
!     a wrapper for the caldat routine, so that the julian day does not
!     need to be passed in. 

!     + + + KEYWORDS + + +
!     date, utility

!     + + + ARGUMENT DECLARATIONS + + +
      integer   dd, mm, yyyy

!     + + + ARGUMENT DEFINITIONS + + +
!     mm     - integer value of mm in the range 1-12
!     dd     -                  dd in the range 1-31
!     yyyy   -                  yyyy (negative A.D., positive B.C.)

      call caldat (-1, dd, mm, yyyy)

      return ! astronomical Gregorian date
      end
