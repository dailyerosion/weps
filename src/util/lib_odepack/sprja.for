!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine sprja (neq, y, yh, nyh, ewt, ftem, savf, wm, iwm,      &
     &   f, jac)
      external f, jac
      integer neq, nyh, iwm
      integer icf, ierpj, iersl, jcur, jstart, kflag, l 
      integer lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter
      integer maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer jtyp, mused, mxordn, mxords
      integer i, i1, i2, ier, ii, j, j1, jj, lenp
      integer mba, mband, meb1, meband, ml, ml3, mu, np1
      real y, yh, ewt, ftem, savf, wm
      real ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real pdnorm
      real con, fac, hl0, r, r0, srur, yi, yj, yjj
      real   smnorm, sfnorm, sbnorm
      dimension neq(*), y(*), yh(nyh,*), ewt(*), ftem(*), savf(*)
      dimension   wm(*), iwm(*)
      common /sls001/ ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,    &
     &   icf, ierpj, iersl, jcur, jstart, kflag, l,                     &
     &   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter,               &
     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      common /slsa01/ pdnorm, jtyp, mused, mxordn, mxords

      save :: /sls001/, /slsa01/

!-----------------------------------------------------------------------
! SPRJA is called by SSTODA to compute and process the matrix
! P = I - H*EL(1)*J , where J is an approximation to the Jacobian.
! Here J is computed by the user-supplied routine JAC if
! MITER = 1 or 4 or by finite differencing if MITER = 2 or 5.
! J, scaled by -H*EL(1), is stored in WM.  Then the norm of J (the
! matrix norm consistent with the weighted max-norm on vectors given
! by SMNORM) is computed, and J is overwritten by P.  P is then
! subjected to LU decomposition in preparation for later solution
! of linear systems with P as coefficient matrix.  This is done
! by SGEFA if MITER = 1 or 2, and by SGBFA if MITER = 4 or 5.
!
! In addition to variables described previously, communication
! with SPRJA uses the following:
! Y     = array containing predicted values on entry.
! FTEM  = work array of length N (ACOR in SSTODA).
! SAVF  = array containing f evaluated at predicted y.
! WM    = real work space for matrices.  On output it contains the
!         LU decomposition of P.
!         Storage of matrix elements starts at WM(3).
!         WM also contains the following matrix-related data:
!         WM(1) = SQRT(UROUND), used in numerical Jacobian increments.
! IWM   = integer work space containing pivot information, starting at
!         IWM(21).   IWM also contains the band parameters
!         ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
! EL0   = EL(1) (input).
! PDNORM= norm of Jacobian matrix. (Output).
! IERPJ = output error flag,  = 0 if no trouble, .gt. 0 if
!         P matrix found to be singular.
! JCUR  = output flag = 1 to indicate that the Jacobian matrix
!         (or approximation) is now current.
! This routine also uses the Common variables EL0, H, TN, UROUND,
! MITER, N, NFE, and NJE.
!-----------------------------------------------------------------------
      nje = nje + 1
      ierpj = 0
      jcur = 1
      hl0 = h*el0
      go to (100, 200, 300, 400, 500), miter
! if miter = 1, call jac and multiply by scalar. -----------------------
 100  lenp = n*n
      do 110 i = 1,lenp
 110    wm(i+2) = 0.0e0
      call jac (neq, tn, y, 0, 0, wm(3), n)
      con = -hl0
      do 120 i = 1,lenp
 120    wm(i+2) = wm(i+2)*con
      go to 240
! if miter = 2, make n calls to f to approximate j. --------------------
 200  fac = smnorm (n, savf, ewt)
      r0 = 1000.0e0*abs(h)*uround*n*fac
      if (r0 .eq. 0.0e0) r0 = 1.0e0
      srur = wm(1)
      j1 = 2
      do 230 j = 1,n
        yj = y(j)
        r = max(srur*abs(yj),r0/ewt(j))
        y(j) = y(j) + r
        fac = -hl0/r
        call f (neq, tn, y, ftem)
        do 220 i = 1,n
 220      wm(i+j1) = (ftem(i) - savf(i))*fac
        y(j) = yj
        j1 = j1 + n
 230    continue
      nfe = nfe + n
 240  continue
! compute norm of jacobian. --------------------------------------------
      pdnorm = sfnorm (n, wm(3), ewt)/abs(hl0)
! add identity matrix. -------------------------------------------------
      j = 3
      np1 = n + 1
      do 250 i = 1,n
        wm(j) = wm(j) + 1.0e0
 250    j = j + np1
! do lu decomposition on p. --------------------------------------------
      call sgefa (wm(3), n, n, iwm(21), ier)
      if (ier .ne. 0) ierpj = 1
      return
! dummy block only, since miter is never 3 in this routine. ------------
 300  return
! if miter = 4, call jac and multiply by scalar. -----------------------
 400  ml = iwm(1)
      mu = iwm(2)
      ml3 = ml + 3
      mband = ml + mu + 1
      meband = mband + ml
      lenp = meband*n
      do 410 i = 1,lenp
 410    wm(i+2) = 0.0e0
      call jac (neq, tn, y, ml, mu, wm(ml3), meband)
      con = -hl0
      do 420 i = 1,lenp
 420    wm(i+2) = wm(i+2)*con
      go to 570
! if miter = 5, make mband calls to f to approximate j. ----------------
 500  ml = iwm(1)
      mu = iwm(2)
      mband = ml + mu + 1
      mba = min(mband,n)
      meband = mband + ml
      meb1 = meband - 1
      srur = wm(1)
      fac = smnorm (n, savf, ewt)
      r0 = 1000.0e0*abs(h)*uround*n*fac
      if (r0 .eq. 0.0e0) r0 = 1.0e0
      do 560 j = 1,mba
        do 530 i = j,n,mband
          yi = y(i)
          r = max(srur*abs(yi),r0/ewt(i))
 530      y(i) = y(i) + r
        call f (neq, tn, y, ftem)
        do 550 jj = j,n,mband
          y(jj) = yh(jj,1)
          yjj = y(jj)
          r = max(srur*abs(yjj),r0/ewt(jj))
          fac = -hl0/r
          i1 = max(jj-mu,1)
          i2 = min(jj+ml,n)
          ii = jj*meb1 - ml + 2
          do 540 i = i1,i2
 540        wm(ii+i) = (ftem(i) - savf(i))*fac
 550      continue
 560    continue
      nfe = nfe + mba
 570  continue
! compute norm of jacobian. --------------------------------------------
      pdnorm = sbnorm (n, wm(ml+3), meband, ml, mu, ewt)/abs(hl0)
! add identity matrix. -------------------------------------------------
      ii = mband + 2
      do 580 i = 1,n
        wm(ii) = wm(ii) + 1.0e0
 580    ii = ii + meband
! do lu decomposition of p. --------------------------------------------
      call sgbfa (wm(3), meband, n, ml, mu, iwm(21), ier)
      if (ier .ne. 0) ierpj = 1
      return
!----------------------- end of subroutine sprja -----------------------
      end
