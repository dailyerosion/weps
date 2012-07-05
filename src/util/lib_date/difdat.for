!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/difdat.for,v 1.2 2002-09-04 20:22:18 wagner Exp $
!
      integer   function   difdat (d1, m1, yyy1, d2, m2, yyy2)

!     + + + PURPOSE + + +
!     Two dates are passed to this function and the number of days between
!     them is returned. The important thing to remember here is that the
!     first date is subtracted _from_ the second. 
!     Example:
!        d1 m1 yyy1    d2 m2 yyy2   returns   meaning
!        01 01 1992    02 01 1992   1         1 day from 01/01/1992 it will
!                                             be 02/01/1992
!        02 01 1992    01 01 1992   -1        -1 day from 02/01/1992 (or 1
!                                             day ago) it was 01/01/1992
!
!     + + + KEYWORDS + + +
!     date, utility
!
!     + + + ARGUMENT DECLARATIONS + + + 
      integer d1, m1, yyy1, d2, m2, yyy2
!
!     + + + ARGUMENT DEFINITIONS + + +
!     d1     - day 
!     m1     - month
!     yyy1   - year
!     d2     - day
!     m2     - month
!     yyy2   - year
!
!     + + + FUNCTION DECLARATIONS + + +
      integer julday
!
!     + + + END SPECIFICATIONS + + +
!
      difdat=julday (d2, m2, yyy2)-julday (d1, m1, yyy1)
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
