!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine sintdy (t, k, yh, nyh, dky, iflag)
!***BEGIN PROLOGUE  SINTDY
!***SUBSIDIARY
!***PURPOSE  Interpolate solution derivatives.
!***TYPE      SINGLE PRECISION (SINTDY-S, DINTDY-D)
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
!
!  SINTDY computes interpolated values of the K-th derivative of the
!  dependent variable vector y, and stores it in DKY.  This routine
!  is called within the package with K = 0 and T = TOUT, but may
!  also be called by the user for any K up to the current order.
!  (See detailed instructions in the usage documentation.)
!
!  The computed values in DKY are gotten by interpolation using the
!  Nordsieck history array YH.  This array corresponds uniquely to a
!  vector-valued polynomial of degree NQCUR or less, and DKY is set
!  to the K-th derivative of this polynomial at T.
!  The formula for DKY is:
!               q
!   DKY(i)  =  sum  c(j,K) * (T - tn)**(j-K) * h**(-j) * YH(i,j+1)
!              j=K
!  where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, tn = TCUR, h = HCUR.
!  The quantities  nq = NQCUR, l = nq+1, N = NEQ, tn, and h are
!  communicated by COMMON.  The above sum is done in reverse order.
!  IFLAG is returned negative if either K or T is out of bounds.
!
!***SEE ALSO  SLSODE
!***ROUTINES CALLED  XERRWV
!***COMMON BLOCKS    SLS001
!***REVISION HISTORY  (YYMMDD)
!   791129  DATE WRITTEN
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
!   890503  Minor cosmetic changes.  (FNF)
!   930809  Renamed to allow single/double precision versions. (ACH)
!   010412  Reduced size of Common block /SLS001/. (ACH)
!***END PROLOGUE  SINTDY
!**End
      integer k, nyh, iflag
      integer icf, ierpj, iersl, jcur, jstart, kflag, l
      integer lyh, lewt, lacor, lsavr, lwm, liwm, meth, miter
      integer maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer i, ic, j, jb, jb2, jj, jj1, jp1
      real t, yh, dky
      real ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real c, r, s, tp
      character*80 msg
      dimension yh(nyh,*), dky(*)
      common /sls001/ ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,    &
     &   icf, ierpj, iersl, jcur, jstart, kflag, l,                     &
     &   lyh, lewt, lacor, lsavr, lwm, liwm, meth, miter,               &
     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu

      save :: /sls001/
!
!***first executable statement  sintdy
      iflag = 0
      if (k .lt. 0 .or. k .gt. nq) go to 80
      tp = tn - hu -  100.0e0*uround*(tn + hu)
      if ((t-tp)*(t-tn) .gt. 0.0e0) go to 90
!
      s = (t - tn)/h
      ic = 1
      if (k .eq. 0) go to 15
      jj1 = l - k
      do 10 jj = jj1,nq
 10     ic = ic*jj
 15   c = ic
      do 20 i = 1,n
 20     dky(i) = c*yh(i,l)
      if (k .eq. nq) go to 55
      jb2 = nq - k
      do 50 jb = 1,jb2
        j = nq - jb
        jp1 = j + 1
        ic = 1
        if (k .eq. 0) go to 35
        jj1 = jp1 - k
        do 30 jj = jj1,j
 30       ic = ic*jj
 35     c = ic
        do 40 i = 1,n
 40       dky(i) = c*yh(i,jp1) + s*dky(i)
 50     continue
      if (k .eq. 0) return
 55   r = h**(-k)
      do 60 i = 1,n
 60     dky(i) = r*dky(i)
      return
!
 80   msg = 'sintdy-  k (=i1) illegal      '
      call xerrwv (msg, 30, 51, 0, 1, k, 0, 0, 0.0e0, 0.0e0)
      iflag = -1
      return
 90   msg = 'sintdy-  t (=r1) illegal      '
      call xerrwv (msg, 30, 52, 0, 0, 0, 0, 1, t, 0.0e0)
      msg='      t not in interval tcur - hu (= r1) to tcur (=r2)      '
      call xerrwv (msg, 60, 52, 0, 0, 0, 0, 2, tp, tn)
      iflag = -2
      return
!----------------------- end of subroutine sintdy ----------------------
      end
