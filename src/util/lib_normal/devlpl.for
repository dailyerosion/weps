!$Author$
!$Date$
!$Revision$
!$HeadURL$

      DOUBLE PRECISION FUNCTION devlpl(a,n,x)
!**********************************************************************
!
!     DOUBLE PRECISION FUNCTION DEVLPL(A,N,X)
!              Double precision EVALuate a PoLynomial at X
!
!
!                              Function
!
!
!     returns
!          A(1) + A(2)*X + ... + A(N)*X**(N-1)
!
!
!                              Arguments
!
!
!     A --> Array of coefficients of the polynomial.
!                                        A is DOUBLE PRECISION(N)
!
!     N --> Length of A, also degree of polynomial - 1.
!                                        N is INTEGER
!
!     X --> Point at which the polynomial is to be evaluated.
!                                        X is DOUBLE PRECISION
!
!**********************************************************************
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION x
      INTEGER n
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION a(n)
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION term
      INTEGER i
!     ..
!     .. Executable Statements ..
      term = a(n)
      DO 10,i = n - 1,1,-1
          term = a(i) + term*x
   10 CONTINUE
      devlpl = term
      RETURN

      END
