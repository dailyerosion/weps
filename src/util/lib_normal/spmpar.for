!$Author$
!$Date$
!$Revision$
!$HeadURL$

      DOUBLE PRECISION FUNCTION spmpar(i)
!-----------------------------------------------------------------------
!
!     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN
!
!        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,
!
!        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,
!
!        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!
!-----------------------------------------------------------------------
!     WRITTEN BY
!        ALFRED H. MORRIS, JR.
!        NAVAL SURFACE WARFARE CENTER
!        DAHLGREN VIRGINIA
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     MODIFIED BY BARRY W. BROWN TO RETURN DOUBLE PRECISION MACHINE
!     CONSTANTS FOR THE COMPUTER BEING USED.  THIS MODIFICATION WAS
!     MADE AS PART OF CONVERTING BRATIO TO DOUBLE PRECISION
!-----------------------------------------------------------------------
!     .. Scalar Arguments ..
      INTEGER i
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION b,binv,bm1,one,w,z
      INTEGER emax,emin,ibeta,m
!     ..
!     .. External Functions ..
      INTEGER ipmpar
      EXTERNAL ipmpar
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC dble
!     ..
!     .. Executable Statements ..
!
      IF (i.GT.1) GO TO 10
      b = ipmpar(4)
      m = ipmpar(8)
      spmpar = b** (1-m)
      RETURN
!
   10 IF (i.GT.2) GO TO 20
      b = ipmpar(4)
      emin = ipmpar(9)
      one = dble(1)
      binv = one/b
      w = b** (emin+2)
      spmpar = ((w*binv)*binv)*binv
      RETURN
!
   20 ibeta = ipmpar(4)
      m = ipmpar(8)
      emax = ipmpar(10)
!
      b = ibeta
      bm1 = ibeta - 1
      one = dble(1)
      z = b** (m-1)
      w = ((z-one)*b+bm1)/ (b*z)
!
      z = b** (emax-2)
      spmpar = ((w*z)*b)*b
      RETURN

      END
