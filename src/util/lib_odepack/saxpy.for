!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine saxpy (n, sa, sx, incx, sy, incy)
!***BEGIN PROLOGUE  SAXPY
!***PURPOSE  Compute a constant times a vector plus a vector.
!***CATEGORY  D1A7
!***TYPE      SINGLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C)
!***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
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
!       sa  single precision scalar multiplier
!       sx  single precision vector with n elements
!     incx  storage spacing between elements of sx
!       sy  single precision vector with n elements
!     incy  storage spacing between elements of sy
!
!     --Output--
!       sy  single precision result (unchanged if n .le. 0)
!
!     Overwrite single precision SY with single precision SA*SX +SY.
!     For I = 0 to N-1, replace  SY(LY+I*INCY) with SA*SX(LX+I*INCX) +
!       SY(LY+I*INCY),
!     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
!     defined in a similar way using INCY.
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
!   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
!   920501  Reformatted the REFERENCES section.  (WRB)
!   010807  Added specific declaration for all variables (FAF)
!***END PROLOGUE  SAXPY
      real sx(*), sy(*), sa
      integer incy, incx, n, ix, iy, i, m, mp1, ns
!***first executable statement  saxpy
      if (n.le.0 .or. sa.eq.0.0e0) return
      if (incx .eq. incy) if (incx-1) 5,20,60
!
!     code for unequal or nonpositive increments.
!
    5 ix = 1
      iy = 1
      if (incx .lt. 0) ix = (-n+1)*incx + 1
      if (incy .lt. 0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sy(iy) + sa*sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!     code for both increments equal to 1.
!
!     clean-up loop so remaining vector length is a multiple of 4.
!
   20 m = mod(n,4)
      if (m .eq. 0) go to 40
      do 30 i = 1,m
        sy(i) = sy(i) + sa*sx(i)
   30 continue
      if (n .lt. 4) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        sy(i) = sy(i) + sa*sx(i)
        sy(i+1) = sy(i+1) + sa*sx(i+1)
        sy(i+2) = sy(i+2) + sa*sx(i+2)
        sy(i+3) = sy(i+3) + sa*sx(i+3)
   50 continue
      return
!
!     code for equal, positive, non-unit increments.
!
   60 ns = n*incx
      do 70 i = 1,ns,incx
        sy(i) = sa*sx(i) + sy(i)
   70 continue
      return
      end
