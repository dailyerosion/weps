!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      integer function isamax (n, sx, incx)
!***BEGIN PROLOGUE  ISAMAX
!***PURPOSE  Find the smallest index of that component of a vector
!            having the maximum magnitude.
!***CATEGORY  D1A2
!***TYPE      SINGLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C)
!***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR
!***AUTHOR  Lawson, C. L., (JPL)
!           Hanson, R. J., (SNLA)
!           Kincaid, D. R., (U. of Texas)
!           Krogh, F. T., (JPL)
!***DESCRIPTION
!
!                B L A S  Subprogram
!    Description of Parameters
!
!     --Input--
!        n  number of elements in input vector(s)
!       sx  single precision vector with n elements
!     incx  storage spacing between elements of sx
!
!     --Output--
!   isamax  smallest index (zero if n .le. 0)
!
!     Find smallest index of maximum magnitude of single precision SX.
!     ISAMAX = first I, I = 1 to N, to maximize  ABS(SX(IX+(I-1)*INCX)),
!     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
!
!***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
!                 Krogh, Basic linear algebra subprograms for Fortran
!                 usage, Algorithm No. 539, Transactions on Mathematical
!                 Software 5, 3 (September 1979), pp. 308-323.
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   791001  DATE WRITTEN
!   861211  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900821  Modified to correct problem with a negative increment.
!           (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!   920618  Slight restructuring of code.  (RWC, WRB)
!***END PROLOGUE  ISAMAX
      real sx(*), smax, xmag
      integer i, incx, ix, n
!***first executable statement  isamax
      isamax = 0
      if (n .le. 0) return
      isamax = 1
      if (n .eq. 1) return
!
      if (incx .eq. 1) goto 20
!
!     code for increment not equal to 1.
!
      ix = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      smax = abs(sx(ix))
      ix = ix + incx
      do 10 i = 2,n
        xmag = abs(sx(ix))
        if (xmag .gt. smax) then
          isamax = i
          smax = xmag
        endif
        ix = ix + incx
   10 continue
      return
!
!     code for increments equal to 1.
!
   20 smax = abs(sx(1))
      do 30 i = 2,n
        xmag = abs(sx(i))
        if (xmag .gt. smax) then
          isamax = i
          smax = xmag
        endif
   30 continue
      return
      end
