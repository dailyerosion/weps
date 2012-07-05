!$Author$
!$Date$
!$Revision$
!$HeadURL$

!     CALDAT is taken from _Numerical_Recipes:_The_Art_of_Scientific_Computing_

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

      subroutine   caldat (ijulian, dd, mm, yyyy)

      include 'm1sim.inc'
!
!     + + + PURPOSE + + +
!     Inverse of the function JULDAY. Here 'julian' is input as a Julian Day
!     Number, and the routine outputs the dd, mm, and yyyy on which the
!     specified Julian Day started at noon.

!     + + + KEYWORDS + + +
!     date, utility

!     + + + ARGUMENT DECLARATIONS + + +
      integer   ijulian, dd, mm, yyyy

!     + + + ARGUMENT DEFINITIONS + + +
!     mm     - integer value of mm in the range 1-12
!     dd     -                  dd in the range 1-31
!     yyyy   -                  yyyy (negative A.D., positive B.C.)
!     ijulian - integer value equal to Julian Day Number

!     + + + PARAMETERS + + +
!     Gregorian Calendar was adopted on Oct. 15, 1582.
!      parameter   (igreg=2299161)

!     + + + LOCAL VARIABLES + + +
      integer julian
      integer s1, s2, s3, s4, n_n, i_i, q_q

!     julian - integer value equal to Julian Day Number

!     + + + END SPECIFICATIONS + + +

!     use simulation date
      if (ijulian.eq.-1) then
          julian = am0jd
      else
          julian = ijulian
      end if

      s1 = julian + 68569 

      n_n = floor(4*s1/146097.0)

      s2 = s1 - floor((146097*n_n + 3)/4.0)

      i_i = floor(4000*(s2 + 1)/1461001.0)

      s3 = s2 - floor(1461*i_i/4.0) + 31 

      q_q = floor(80*s3/2447.0)

      s4 = floor(q_q/11.0)

      dd = s3 - floor(2447*q_q/80.0)

      mm = q_q + 2 - 12*s4 

      yyyy = 100*(n_n - 49) + i_i + s4 

      return ! astronomical Gregorian date
      end

!$Log: not supported by cvs2svn $
!Revision 1.5  2002/09/04 20:22:17  wagner
!allow free format src compilation
!
!Revision 1.4  2002/05/02 23:14:12  fredfox
!added command line argument to call subroutine which creates a stand alone erosion input file
!based on either an input date or input simulation day. Since this erosion input is called in
!erosion, dates before the warmup years will not be called since erosion is not called. if code
!is changed to call erosion at all times, then this will work during that itme as well. Also,
!some files were cleaned up and comments and header information only changed
!
!Revision 1.3  2000/01/29 22:23:24  wjr
!moved lentrim in util/misc
!combined decini & decoinit into one file, decoinit
!removed wepdgb from weps.for
!removed grad and moved calc into getcli
!moved several vars into getwin & getcli from weps
!moved *dbug.for into respective subdirs
!modified caldat to reference am0jd if arg == -1 and removed params appropriately
!
!Revision 1.2  1999/04/26 20:16:19  wagner
!changes due to combining include files ([cdb]1glob.inc), etc
!
!Revision 1.1.1.1  1999/03/12 17:05:31  wagner
!Baseline version of WEPS with Bill Rust's modifications
!
! Revision 1.1.1.1  1995/01/18  04:20:06  wagner
! Initial checkin
!
! Revision 2.3  1992/04/03  23:24:58  wagner
! Removed extraneous test print statements.
!
! Revision 2.2  1992/04/03  23:15:54  wagner
! Added some typecasts from floats to doubles (dble)
! to hopefully eliminate some roundoff error problems
! with the MS FORTRAN 5.1 compiler.
! Later, we determined that the problem was occuring
! in the julday function and not the caldat subroutine.
! However, these changes should not bother anything
! so the changes have been kept.
!
! Revision 2.1  1992/03/27  17:22:53  wagner
! Version 2 code.
!
