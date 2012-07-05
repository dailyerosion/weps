!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/date/julday.for,v 1.4 2005-07-18 16:49:56 fredfox Exp $
!
!     JULDAY is taken from _Numerical_Recipes:_The_Art_of_Scientific_Computing_

!     problems were found with the method above for long runs such as:
!     - the ten missing days in 1582 (we really just need 365.25 day in each year)
!     - after 1700, leap years return feb 31, not 29 and the wrong year
!     - it may only be the fortran implementation and floating point problems

!     Based on info from http://en.wikipedia.org/wiki/Julian_day, which references
!     http://www.astro.uu.nl/~strous/AA/en/reken/juliaansedag.html, the code
!     was revised to use the Astronomical Gregorian calendar, which takes the
!     present pattern of leap years back into the past. This is ideal for
!     our purposes with no year getting short changed. Integer math method
!     is taken from Wikipedia article.

      integer   function   julday (dd, mm, yyyy)

!     + + + PURPOSE + + +
!     In this routine JULDAY returns the Julian Day Number which begins at
!     noon of the gregorian calendar date specified by day "dd", month "mm", & year "yyyy"
!     All are integer variables. Positive year signifies A.D.; zero and negative, B.C.
!     Calendar dates before 1582 will not match dates on the Julian calendar used
!     at the time.

!     + + + KEYWORDS + + +
!     date, utility

!     + + + ARGUMENT DECLARATIONS + + +
      integer   dd, mm, yyyy

!     + + + ARGUMENT DEFINITIONS + + +
!     dd     - integer value of day in the range 1-31
!     mm     -                  month in the range 1-12
!     yyyy   -                  year (negative B.C., positive A.D.)

!     + + + END SPECIFICATIONS + + +

      julday = (1461 * (yyyy + 4800 + (mm - 14)/12))/4                  &
     &       + (367 * (mm - 2 - 12 * ((mm - 14)/12)))/12                &
     &       - (3 * ((yyyy + 4900 + (mm - 14)/12)/100))/4 + dd - 32075

      return
      end

!$Log: not supported by cvs2svn $
!Revision 1.3  2005/06/21 15:17:06  fredfox
!corrected date function to not modify passed year when given BC dates, only print warnings instead of pause and properly pass date to function in weps when doing initialization and year is zero, which is not allowed
!
!Revision 1.2  2002/09/04 20:22:18  wagner
!allow free format src compilation
!
!Revision 1.1.1.1  1999/03/12 17:05:31  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.2  1998/09/03  18:35:36  jt
!  check in a full copy of WEPS - I hope ???
!
! Revision 1.1.1.1  1995/01/18  04:20:07  wagner
! Initial checkin
!
! Revision 2.2  1992/04/03  23:04:07  wagner
! changed from multiplying by 0.01 to dividing by 100
! to alleviate floating point roundoff problem that
! exists with Microsoft FORTRAN v5.1 compiler when
! the dates of 28/2/1900 - 28/2/1901 are input.
! Hopefully, this will fix that problem without
! others surfacing from this change.
! The line used to be:
! ja=int(dble(0.01)*dble(jy))
! It is now:
! ja=jy/100
!
! Revision 2.1  1992/03/27  17:22:53  wagner
! Fixed roundoff problems by adding some
! double precision casts (DBLE) to some
! constants and integer variables in a
! couple of intermediate computations.
!
! Also added some preliminary error checking
! for a zero year and the "missing" days
! between 4/10/1582 and 15/10/1582.
!
! Version 2 code.
!
