!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine sscal (n, sa, sx, incx)
!***BEGIN PROLOGUE  SSCAL
!***PURPOSE  Multiply a vector by a constant.
!***CATEGORY  D1A6
!***TYPE      SINGLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C)
!***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR
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
!        N  number of elements in input vector(s)
!       SA  single precision scale factor
!       SX  single precision vector with N elements
!     INCX  storage spacing between elements of SX
!
!     --Output--
!       SX  single precision result (unchanged if N .LE. 0)
!
!     Replace single precision SX by single precision SA*SX.
!     For I = 0 to N-1, replace SX(IX+I*INCX) with  SA * SX(IX+I*INCX),
!     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
!
!***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
!                 Krogh, Basic linear algebra subprograms for Fortran
!                 usage, Algorithm No. 539, Transactions on Mathematical
!                 Software 5, 3 (September 1979), pp. 308-323.
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   791001  DATE WRITTEN
!   890831  Modified array declarations.  (WRB)
!   890831  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900821  Modified to correct problem with a negative increment.
!           (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  SSCAL
      real sa, sx(*)
      integer i, incx, ix, m, mp1, n
!***first executable statement  sscal
      if (n .le. 0) return
      if (incx .eq. 1) goto 20
!
!     code for increment not equal to 1.
!
      ix = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      do 10 i = 1,n
        sx(ix) = sa*sx(ix)
        ix = ix + incx
   10 continue
      return
!
!     code for increment equal to 1.
!
!     clean-up loop so remaining vector length is a multiple of 5.
!
   20 m = mod(n,5)
      if (m .eq. 0) goto 40
      do 30 i = 1,m
        sx(i) = sa*sx(i)
   30 continue
      if (n .lt. 5) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i+1) = sa*sx(i+1)
        sx(i+2) = sa*sx(i+2)
        sx(i+3) = sa*sx(i+3)
        sx(i+4) = sa*sx(i+4)
   50 continue
      return
      end
