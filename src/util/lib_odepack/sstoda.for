!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine sstoda (isr, neq, y, yh, nyh, yh1, ewt, savf, acor,    &
     &   wm, iwm, f, jac, pjac, slvs)

      use hydro_darcy_mod, only: sls1, slsa, stoc

      external f, jac, pjac, slvs
      integer isr, neq, nyh, iwm
!      integer icf, ierpj, iersl, jcur, jstart, kflag, l
!      integer   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter
!      integer   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!      integer jtyp, mused, mxordn, mxords
!      integer i, i1, icount, irflag, iredo, iret, j, jb, m, ncf, newq
      integer i, i1, iredo, iret, j, jb, m, ncf, newq
      integer lm1, lm1p1, lm2, lm2p1, nqm1, nqm2
!      integer ialth, ipup, lmax, nqnyh, nslp
      real y, yh, yh1, ewt, savf, acor, wm
!      real ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
!      real pdnorm
!      real conit, crate, el(13), elco(13,12), hold, rmax, tesco(3,12)
      real dcon, ddn, del, delp, dsm, dup, exdn, exsm, exup
      real   r, rh, rhdn, rhsm, rhup, told, smnorm
!      real alpha, cm1(12),cm2(5), dm1,dm2, exm1,exm2, pdest
      real alpha, dm1,dm2, exm1,exm2
!      real pdlast, pdh, pnorm, rate, ratio, rh1, rh1it, rh2, rm, sm1(12)
      real pdh, pnorm, rate, rh1, rh1it, rh2, rm
      dimension neq(*), y(*), yh(nyh,*), yh1(*), ewt(*), savf(*)
      dimension   acor(*), wm(*), iwm(*)
!      save conit, crate, el, elco, hold, rmax, tesco
!      save   ialth, ipup, lmax, nqnyh, nslp
!      save cm1, cm2, pdest, pdlast, ratio, sm1, icount, irflag
!      common /sls001/ ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,    &
!     &   icf, ierpj, iersl, jcur, jstart, kflag, l,                     &
!     &   lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter,               &
!     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
!      common /slsa01/ pdnorm, jtyp, mused, mxordn, mxords
!      data stoc(isr)%sm1/0.5e0, 0.575e0, 0.55e0, 0.45e0, 0.35e0, 0.25e0,&
!     &   0.20e0, 0.15e0, 0.10e0, 0.075e0, 0.050e0, 0.025e0/
!-----------------------------------------------------------------------
! SSTODA performs one step of the integration of an initial value
! problem for a system of ordinary differential equations.
! Note: SSTODA is independent of the value of the iteration method
! indicator MITER, when this is .ne. 0, and hence is independent
! of the type of chord method used, or the Jacobian structure.
! Communication with SSTODA is done with the following variables:
!
! Y      = an array of length .ge. N used as the Y argument in
!          all calls to F and JAC.
! NEQ    = integer array containing problem size in NEQ(1), and
!          passed as the NEQ argument in all calls to F and JAC.
! YH     = an NYH by LMAX array containing the dependent variables
!          and their approximate scaled derivatives, where
!          LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate
!          j-th derivative of y(i), scaled by H**j/factorial(j)
!          (j = 0,1,...,NQ).  On entry for the first step, the first
!          two columns of YH must be set from the initial values.
! NYH    = a constant integer .ge. N, the first dimension of YH.
! YH1    = a one-dimensional array occupying the same space as YH.
! EWT    = an array of length N containing multiplicative weights
!          for local error measurements.  Local errors in y(i) are
!          compared to 1.0/EWT(i) in various error tests.
! SAVF   = an array of working storage, of length N.
! ACOR   = a work array of length N, used for the accumulated
!          corrections.  On a successful return, ACOR(i) contains
!          the estimated one-step local error in y(i).
! WM,IWM = real and integer work arrays associated with matrix
!          operations in chord iteration (MITER .ne. 0).
! PJAC   = name of routine to evaluate and preprocess Jacobian matrix
!          and P = I - H*EL0*Jac, if a chord method is being used.
!          It also returns an estimate of norm(Jac) in PDNORM.
! SLVS   = name of routine to solve linear system in chord iteration.
! CCMAX  = maximum relative change in H*EL0 before PJAC is called.
! H      = the step size to be attempted on the next step.
!          H is altered by the error control algorithm during the
!          problem.  H can be either positive or negative, but its
!          sign must remain constant throughout the problem.
! HMIN   = the minimum absolute value of the step size H to be used.
! HMXI   = inverse of the maximum absolute value of H to be used.
!          HMXI = 0.0 is allowed and corresponds to an infinite HMAX.
!          HMIN and HMXI may be changed at any time, but will not
!          take effect until the next change of H is considered.
! TN     = the independent variable. TN is updated on each step taken.
! JSTART = an integer used for input only, with the following
!          values and meanings:
!               0  perform the first step.
!           .gt.0  take a new step continuing from the last.
!              -1  take the next step with a new value of H,
!                    N, METH, MITER, and/or matrix parameters.
!              -2  take the next step with a new value of H,
!                    but with other inputs unchanged.
!          On return, JSTART is set to 1 to facilitate continuation.
! KFLAG  = a completion code with the following meanings:
!               0  the step was succesful.
!              -1  the requested error could not be achieved.
!              -2  corrector convergence could not be achieved.
!              -3  fatal error in PJAC or SLVS.
!          A return with KFLAG = -1 or -2 means either
!          ABS(H) = HMIN or 10 consecutive failures occurred.
!          On a return with KFLAG negative, the values of TN and
!          the YH array are as of the beginning of the last
!          step, and H is the last step size attempted.
! MAXORD = the maximum order of integration method to be allowed.
! MAXCOR = the maximum number of corrector iterations allowed.
! MSBP   = maximum number of steps between PJAC calls (MITER .gt. 0).
! MXNCF  = maximum number of convergence failures allowed.
! METH   = current method.
!          METH = 1 means Adams method (nonstiff)
!          METH = 2 means BDF method (stiff)
!          METH may be reset by SSTODA.
! MITER  = corrector iteration method.
!          MITER = 0 means functional iteration.
!          MITER = JT .gt. 0 means a chord iteration corresponding
!          to Jacobian type JT.  (The SLSODA/SLSODAR argument JT is
!          communicated here as JTYP, but is not used in SSTODA
!          except to load MITER following a method switch.)
!          MITER may be reset by SSTODA.
! N      = the number of first-order differential equations.
!-----------------------------------------------------------------------
      sls1(isr)%kflag = 0
      told = sls1(isr)%tn
      ncf = 0
      sls1(isr)%ierpj = 0
      sls1(isr)%iersl = 0
      sls1(isr)%jcur = 0
      sls1(isr)%icf = 0
      delp = 0.0e0
      if (sls1(isr)%jstart .gt. 0) go to 200
      if (sls1(isr)%jstart .eq. -1) go to 100
      if (sls1(isr)%jstart .eq. -2) go to 160
!-----------------------------------------------------------------------
! on the first call, the order is set to 1, and other variables are
! initialized.  rmax is the maximum ratio by which h can be increased
! in a single step.  it is initially 1.e4 to compensate for the small
! initial h, but then is normally equal to 10.  if a failure
! occurs (in corrector convergence or error test), rmax is set at 2
! for the next increase.
! scfode is called to get the needed coefficients for both methods.
!-----------------------------------------------------------------------
      stoc(isr)%lmax = sls1(isr)%maxord + 1
      sls1(isr)%nq = 1
      sls1(isr)%l = 2
      stoc(isr)%ialth = 2
      stoc(isr)%rmax = 10000.0e0
      sls1(isr)%rc = 0.0e0
      sls1(isr)%el0 = 1.0e0
      stoc(isr)%crate = 0.7e0
      stoc(isr)%hold = sls1(isr)%h
      stoc(isr)%nslp = 0
      stoc(isr)%ipup = sls1(isr)%miter
      iret = 3
! initialize switching parameters.  meth = 1 is assumed initially. -----
      stoc(isr)%icount = 20
      stoc(isr)%irflag = 0
      stoc(isr)%pdest = 0.0e0
      stoc(isr)%pdlast = 0.0e0
      stoc(isr)%ratio = 5.0e0
      call scfode (2, stoc(isr)%elco, stoc(isr)%tesco)
      do 10 i = 1,5
 10     stoc(isr)%cm2(i) = stoc(isr)%tesco(2,i)*stoc(isr)%elco(i+1,i)
      call scfode (1, stoc(isr)%elco, stoc(isr)%tesco)
      do 20 i = 1,12
 20     stoc(isr)%cm1(i) = stoc(isr)%tesco(2,i)*stoc(isr)%elco(i+1,i)
      go to 150
!-----------------------------------------------------------------------
! the following block handles preliminaries needed when jstart = -1.
! ipup is set to miter to force a matrix update.
! if an order increase is about to be considered (ialth = 1),
! ialth is reset to 2 to postpone consideration one more step.
! if the caller has changed meth, scfode is called to reset
! the coefficients of the method.
! if h is to be changed, yh must be rescaled.
! if h or meth is being changed, ialth is reset to l = nq + 1
! to prevent further changes in h for that many steps.
!-----------------------------------------------------------------------
 100  stoc(isr)%ipup = sls1(isr)%miter
      stoc(isr)%lmax = sls1(isr)%maxord + 1
      if (stoc(isr)%ialth .eq. 1) stoc(isr)%ialth = 2
      if (sls1(isr)%meth .eq. slsa(isr)%mused) go to 160
      call scfode (sls1(isr)%meth, stoc(isr)%elco, stoc(isr)%tesco)
      stoc(isr)%ialth = sls1(isr)%l
      iret = 1
!-----------------------------------------------------------------------
! the el vector and related constants are reset
! whenever the order nq is changed, or at the start of the problem.
!-----------------------------------------------------------------------
 150  do 155 i = 1,sls1(isr)%l
 155    stoc(isr)%el(i) = stoc(isr)%elco(i,sls1(isr)%nq)
      stoc(isr)%nqnyh = sls1(isr)%nq*nyh
      sls1(isr)%rc = sls1(isr)%rc*stoc(isr)%el(1)/sls1(isr)%el0
      sls1(isr)%el0 = stoc(isr)%el(1)
      stoc(isr)%conit = 0.5e0/(sls1(isr)%nq+2)
      go to (160, 170, 200), iret
!-----------------------------------------------------------------------
! if h is being changed, the h ratio rh is checked against
! rmax, hmin, and hmxi, and the yh array rescaled.  ialth is set to
! l = nq + 1 to prevent a change of h for that many steps, unless
! forced by a convergence or error test failure.
!-----------------------------------------------------------------------
 160  if (sls1(isr)%h .eq. stoc(isr)%hold) go to 200
      rh = sls1(isr)%h/stoc(isr)%hold
      sls1(isr)%h = stoc(isr)%hold
      iredo = 3
      go to 175
 170  rh = max(rh,sls1(isr)%hmin/abs(sls1(isr)%h))
 175  rh = min(rh,stoc(isr)%rmax)
      rh = rh/max(1.0e0,abs(sls1(isr)%h)*sls1(isr)%hmxi*rh)
!-----------------------------------------------------------------------
! if meth = 1, also restrict the new step size by the stability region.
! if this reduces h, set irflag to 1 so that if there are roundoff
! problems later, we can assume that is the cause of the trouble.
!-----------------------------------------------------------------------
      if (sls1(isr)%meth .eq. 2) go to 178
      stoc(isr)%irflag = 0
      pdh = max(abs(sls1(isr)%h)*stoc(isr)%pdlast,0.000001e0)
      if (rh*pdh*1.00001e0 .lt. stoc(isr)%sm1(sls1(isr)%nq)) go to 178
      rh = stoc(isr)%sm1(sls1(isr)%nq)/pdh
      stoc(isr)%irflag = 1
 178  continue
      r = 1.0e0
      do 180 j = 2,sls1(isr)%l
        r = r*rh
        do 180 i = 1,sls1(isr)%n
 180      yh(i,j) = yh(i,j)*r
      sls1(isr)%h = sls1(isr)%h*rh
      sls1(isr)%rc = sls1(isr)%rc*rh
      stoc(isr)%ialth = sls1(isr)%l
      if (iredo .eq. 0) go to 690
!-----------------------------------------------------------------------
! this section computes the predicted values by effectively
! multiplying the yh array by the pascal triangle matrix.
! rc is the ratio of new to old values of the coefficient  h*el(1).
! when rc differs from 1 by more than ccmax, ipup is set to miter
! to force pjac to be called, if a jacobian is involved.
! in any case, pjac is called at least every msbp steps.
!-----------------------------------------------------------------------
 200  if (abs(sls1(isr)%rc-1.0e0) .gt. sls1(isr)%ccmax) then
        stoc(isr)%ipup = sls1(isr)%miter
      end if
      if (sls1(isr)%nst .ge. stoc(isr)%nslp+sls1(isr)%msbp) then
        stoc(isr)%ipup = sls1(isr)%miter
      end if
      sls1(isr)%tn = sls1(isr)%tn + sls1(isr)%h
      i1 = stoc(isr)%nqnyh + 1
      do 215 jb = 1,sls1(isr)%nq
        i1 = i1 - nyh
!dir$ ivdep
        do 210 i = i1,stoc(isr)%nqnyh
 210      yh1(i) = yh1(i) + yh1(i+nyh)
 215    continue
      pnorm = smnorm (sls1(isr)%n, yh1, ewt)
!-----------------------------------------------------------------------
! up to maxcor corrector iterations are taken.  a convergence test is
! made on the rms-norm of each correction, weighted by the error
! weight vector ewt.  the sum of the corrections is accumulated in the
! vector acor(i).  the yh array is not altered in the corrector loop.
!-----------------------------------------------------------------------
 220  m = 0
      rate = 0.0e0
      del = 0.0e0
      do 230 i = 1,sls1(isr)%n
 230    y(i) = yh(i,1)
      call f (isr, neq, sls1(isr)%tn, y, savf)
      sls1(isr)%nfe = sls1(isr)%nfe + 1
      if (stoc(isr)%ipup .le. 0) go to 250
!-----------------------------------------------------------------------
! if indicated, the matrix p = i - h*el(1)*j is reevaluated and
! preprocessed before starting the corrector iteration.  ipup is set
! to 0 as an indicator that this has been done.
!-----------------------------------------------------------------------
      call pjac (isr, neq, y, yh, nyh, ewt, acor, savf, wm, iwm, f, jac)
      stoc(isr)%ipup = 0
      sls1(isr)%rc = 1.0e0
      stoc(isr)%nslp = sls1(isr)%nst
      stoc(isr)%crate = 0.7e0
      if (sls1(isr)%ierpj .ne. 0) go to 430
 250  do 260 i = 1,sls1(isr)%n
 260    acor(i) = 0.0e0
 270  if (sls1(isr)%miter .ne. 0) go to 350
!-----------------------------------------------------------------------
! in the case of functional iteration, update y directly from
! the result of the last function evaluation.
!-----------------------------------------------------------------------
      do 290 i = 1,sls1(isr)%n
        savf(i) = sls1(isr)%h*savf(i) - yh(i,2)
 290    y(i) = savf(i) - acor(i)
      del = smnorm (sls1(isr)%n, y, ewt)
      do 300 i = 1,sls1(isr)%n
        y(i) = yh(i,1) + stoc(isr)%el(1)*savf(i)
 300    acor(i) = savf(i)
      go to 400
!-----------------------------------------------------------------------
! in the case of the chord method, compute the corrector error,
! and solve the linear system with that as right-hand side and
! p as coefficient matrix.
!-----------------------------------------------------------------------
 350  do 360 i = 1,sls1(isr)%n
 360    y(i) = sls1(isr)%h*savf(i) - (yh(i,2) + acor(i))
      call slvs (isr, wm, iwm, y, savf)
      if (sls1(isr)%iersl .lt. 0) go to 430
      if (sls1(isr)%iersl .gt. 0) go to 410
      del = smnorm (sls1(isr)%n, y, ewt)
      do 380 i = 1,sls1(isr)%n
        acor(i) = acor(i) + y(i)
 380    y(i) = yh(i,1) + stoc(isr)%el(1)*acor(i)
!-----------------------------------------------------------------------
! test for convergence.  if m .gt. 0, an estimate of the convergence
! rate constant is stored in crate, and this is used in the test.
!
! we first check for a change of iterates that is the size of
! roundoff error.  if this occurs, the iteration has converged, and a
! new rate estimate is not formed.
! in all other cases, force at least two iterations to estimate a
! local lipschitz constant estimate for adams methods.
! on convergence, form pdest = local maximum lipschitz constant
! estimate.  pdlast is the most recent nonzero estimate.
!-----------------------------------------------------------------------
 400  continue
      if (del .le. 100.0e0*pnorm*sls1(isr)%uround) go to 450
      if (m .eq. 0 .and. sls1(isr)%meth .eq. 1) go to 405
      if (m .eq. 0) go to 402
      rm = 1024.0e0
      if (del .le. 1024.0e0*delp) rm = del/delp
      rate = max(rate,rm)
      stoc(isr)%crate = max(0.2e0*stoc(isr)%crate,rm)
 402  dcon = del*min(1.0e0,1.5e0*stoc(isr)%crate)                       &
     &     / (stoc(isr)%tesco(2,sls1(isr)%nq)                           &
     &     * stoc(isr)%conit)
      if (dcon .gt. 1.0e0) go to 405
      stoc(isr)%pdest = max(stoc(isr)%pdest,rate/abs(sls1(isr)%h        &
     &                * stoc(isr)%el(1)))
      if (stoc(isr)%pdest .ne. 0.0e0) then
        stoc(isr)%pdlast = stoc(isr)%pdest
      end if
      go to 450
 405  continue
      m = m + 1
      if (m .eq. sls1(isr)%maxcor) go to 410
      if (m .ge. 2 .and. del .gt. 2.0e0*delp) go to 410
      delp = del
      call f (isr, neq, sls1(isr)%tn, y, savf)
      sls1(isr)%nfe = sls1(isr)%nfe + 1
      go to 270
!-----------------------------------------------------------------------
! the corrector iteration failed to converge.
! if miter .ne. 0 and the jacobian is out of date, pjac is called for
! the next try.  otherwise the yh array is retracted to its values
! before prediction, and h is reduced, if possible.  if h cannot be
! reduced or mxncf failures have occurred, exit with kflag = -2.
!-----------------------------------------------------------------------
 410  if (sls1(isr)%miter .eq. 0 .or. sls1(isr)%jcur .eq. 1) go to 430
      sls1(isr)%icf = 1
      stoc(isr)%ipup = sls1(isr)%miter
      go to 220
 430  sls1(isr)%icf = 2
      ncf = ncf + 1
      stoc(isr)%rmax = 2.0e0
      sls1(isr)%tn = told
      i1 = stoc(isr)%nqnyh + 1
      do 445 jb = 1,sls1(isr)%nq
        i1 = i1 - nyh
!dir$ ivdep
        do 440 i = i1,stoc(isr)%nqnyh
 440      yh1(i) = yh1(i) - yh1(i+nyh)
 445    continue
      if (sls1(isr)%ierpj .lt. 0 .or. sls1(isr)%iersl .lt. 0) go to 680
      if (abs(sls1(isr)%h) .le. sls1(isr)%hmin*1.00001e0) go to 670
      if (ncf .eq. sls1(isr)%mxncf) go to 670
      rh = 0.25e0
      stoc(isr)%ipup = sls1(isr)%miter
      iredo = 1
      go to 170
!-----------------------------------------------------------------------
! the corrector has converged.  jcur is set to 0
! to signal that the jacobian involved may need updating later.
! the local error test is made and control passes to statement 500
! if it fails.
!-----------------------------------------------------------------------
 450  sls1(isr)%jcur = 0
      if (m .eq. 0) dsm = del/stoc(isr)%tesco(2,sls1(isr)%nq)
      if (m .gt. 0) then
        dsm=smnorm(sls1(isr)%n,acor,ewt)/stoc(isr)%tesco(2,sls1(isr)%nq)
      end if
      if (dsm .gt. 1.0e0) go to 500
!-----------------------------------------------------------------------
! after a successful step, update the yh array.
! decrease icount by 1, and if it is -1, consider switching methods.
! if a method switch is made, reset various parameters,
! rescale the yh array, and exit.  if there is no switch,
! consider changing h if ialth = 1.  otherwise decrease ialth by 1.
! if ialth is then 1 and nq .lt. maxord, then acor is saved for
! use in a possible order increase on the next step.
! if a change in h is considered, an increase or decrease in order
! by one is considered also.  a change in h is made only if it is by a
! factor of at least 1.1.  if not, ialth is set to 3 to prevent
! testing for that many steps.
!-----------------------------------------------------------------------
      sls1(isr)%kflag = 0
      iredo = 0
      sls1(isr)%nst = sls1(isr)%nst + 1
      sls1(isr)%hu = sls1(isr)%h
      sls1(isr)%nqu = sls1(isr)%nq
      slsa(isr)%mused = sls1(isr)%meth
      do 460 j = 1,sls1(isr)%l
        do 460 i = 1,sls1(isr)%n
 460      yh(i,j) = yh(i,j) + stoc(isr)%el(j)*acor(i)
      stoc(isr)%icount = stoc(isr)%icount - 1
      if (stoc(isr)%icount .ge. 0) go to 488
      if (sls1(isr)%meth .eq. 2) go to 480
!-----------------------------------------------------------------------
! we are currently using an adams method.  consider switching to bdf.
! if the current order is greater than 5, assume the problem is
! not stiff, and skip this section.
! if the lipschitz constant and error estimate are not polluted
! by roundoff, go to 470 and perform the usual test.
! otherwise, switch to the bdf methods if the last step was
! restricted to insure stability (irflag = 1), and stay with adams
! method if not.  when switching to bdf with polluted error estimates,
! in the absence of other information, double the step size.
!
! when the estimates are ok, we make the usual test by computing
! the step size we could have (ideally) used on this step,
! with the current (adams) method, and also that for the bdf.
! if nq .gt. mxords, we consider changing to order mxords on switching.
! compare the two step sizes to decide whether to switch.
! the step size advantage must be at least ratio = 5 to switch.
!-----------------------------------------------------------------------
      if (sls1(isr)%nq .gt. 5) go to 488
      if (   dsm .gt. 100.0e0*pnorm*sls1(isr)%uround                    &
     & .and. stoc(isr)%pdest.ne.0.0e0 ) then
        go to 470
      end if
      if (stoc(isr)%irflag .eq. 0) go to 488
      rh2 = 2.0e0
      nqm2 = min(sls1(isr)%nq,slsa(isr)%mxords)
      go to 478
 470  continue
      exsm = 1.0e0/sls1(isr)%l
      rh1 = 1.0e0/(1.2e0*dsm**exsm + 0.0000012e0)
      rh1it = 2.0e0*rh1
      pdh = stoc(isr)%pdlast*abs(sls1(isr)%h)
      if (pdh*rh1 .gt. 0.00001e0) rh1it=stoc(isr)%sm1(sls1(isr)%nq)/pdh
      rh1 = min(rh1,rh1it)
      if (sls1(isr)%nq .le. slsa(isr)%mxords) go to 474
         nqm2 = slsa(isr)%mxords
         lm2 = slsa(isr)%mxords + 1
         exm2 = 1.0e0/lm2
         lm2p1 = lm2 + 1
         dm2 = smnorm (sls1(isr)%n, yh(1,lm2p1), ewt)                   &
     &       / stoc(isr)%cm2(slsa(isr)%mxords)
         rh2 = 1.0e0/(1.2e0*dm2**exm2 + 0.0000012e0)
         go to 476
 474  dm2 =dsm*(stoc(isr)%cm1(sls1(isr)%nq)/stoc(isr)%cm2(sls1(isr)%nq))
      rh2 = 1.0e0/(1.2e0*dm2**exsm + 0.0000012e0)
      nqm2 = sls1(isr)%nq
 476  continue
      if (rh2 .lt. stoc(isr)%ratio*rh1) go to 488
! the switch test passed.  reset relevant quantities for bdf. ----------
 478  rh = rh2
      stoc(isr)%icount = 20
      sls1(isr)%meth = 2
      sls1(isr)%miter = slsa(isr)%jtyp
      stoc(isr)%pdlast = 0.0e0
      sls1(isr)%nq = nqm2
      sls1(isr)%l = sls1(isr)%nq + 1
      go to 170
!-----------------------------------------------------------------------
! we are currently using a bdf method.  consider switching to adams.
! compute the step size we could have (ideally) used on this step,
! with the current (bdf) method, and also that for the adams.
! if nq .gt. mxordn, we consider changing to order mxordn on switching.
! compare the two step sizes to decide whether to switch.
! the step size advantage must be at least 5/ratio = 1 to switch.
! if the step size for adams would be so small as to cause
! roundoff pollution, we stay with bdf.
!-----------------------------------------------------------------------
 480  continue
      exsm = 1.0e0/sls1(isr)%l
      if (slsa(isr)%mxordn .ge. sls1(isr)%nq) go to 484
         nqm1 = slsa(isr)%mxordn
         lm1 = slsa(isr)%mxordn + 1
         exm1 = 1.0e0/lm1
         lm1p1 = lm1 + 1
         dm1 = smnorm (sls1(isr)%n, yh(1,lm1p1), ewt)                   &
     &       / stoc(isr)%cm1(slsa(isr)%mxordn)
         rh1 = 1.0e0/(1.2e0*dm1**exm1 + 0.0000012e0)
         go to 486
 484  dm1 =dsm*(stoc(isr)%cm2(sls1(isr)%nq)/stoc(isr)%cm1(sls1(isr)%nq))
      rh1 = 1.0e0/(1.2e0*dm1**exsm + 0.0000012e0)
      nqm1 = sls1(isr)%nq
      exm1 = exsm
 486  rh1it = 2.0e0*rh1
      pdh = slsa(isr)%pdnorm*abs(sls1(isr)%h)
      if (pdh*rh1 .gt. 0.00001e0) rh1it = stoc(isr)%sm1(nqm1)/pdh
      rh1 = min(rh1,rh1it)
      rh2 = 1.0e0/(1.2e0*dsm**exsm + 0.0000012e0)
      if (rh1*stoc(isr)%ratio .lt. 5.0e0*rh2) go to 488
      alpha = max(0.001e0,rh1)
      dm1 = (alpha**exm1)*dm1
      if (dm1 .le. 1000.0e0*sls1(isr)%uround*pnorm) go to 488
! the switch test passed.  reset relevant quantities for adams. --------
      rh = rh1
      stoc(isr)%icount = 20
      sls1(isr)%meth = 1
      sls1(isr)%miter = 0
      stoc(isr)%pdlast = 0.0e0
      sls1(isr)%nq = nqm1
      sls1(isr)%l = sls1(isr)%nq + 1
      go to 170
!
! no method switch is being made.  do the usual step/order selection. --
 488  continue
      stoc(isr)%ialth = stoc(isr)%ialth - 1
      if (stoc(isr)%ialth .eq. 0) go to 520
      if (stoc(isr)%ialth .gt. 1) go to 700
      if (sls1(isr)%l .eq. stoc(isr)%lmax) go to 700
      do 490 i = 1,sls1(isr)%n
 490    yh(i,stoc(isr)%lmax) = acor(i)
      go to 700
!-----------------------------------------------------------------------
! the error test failed.  kflag keeps track of multiple failures.
! restore tn and the yh array to their previous values, and prepare
! to try the step again.  compute the optimum step size for this or
! one lower order.  after 2 or more failures, h is forced to decrease
! by a factor of 0.2 or less.
!-----------------------------------------------------------------------
 500  sls1(isr)%kflag = sls1(isr)%kflag - 1
      sls1(isr)%tn = told
      i1 = stoc(isr)%nqnyh + 1
      do 515 jb = 1,sls1(isr)%nq
        i1 = i1 - nyh
!dir$ ivdep
        do 510 i = i1,stoc(isr)%nqnyh
 510      yh1(i) = yh1(i) - yh1(i+nyh)
 515    continue
      stoc(isr)%rmax = 2.0e0
      if (abs(sls1(isr)%h) .le. sls1(isr)%hmin*1.00001e0) go to 660
      if (sls1(isr)%kflag .le. -3) go to 640
      iredo = 2
      rhup = 0.0e0
      go to 540
!-----------------------------------------------------------------------
! regardless of the success or failure of the step, factors
! rhdn, rhsm, and rhup are computed, by which h could be multiplied
! at order nq - 1, order nq, or order nq + 1, respectively.
! in the case of failure, rhup = 0.0 to avoid an order increase.
! the largest of these is determined and the new order chosen
! accordingly.  if the order is to be increased, we compute one
! additional scaled derivative.
!-----------------------------------------------------------------------
 520  rhup = 0.0e0
      if (sls1(isr)%l .eq. stoc(isr)%lmax) go to 540
      do 530 i = 1,sls1(isr)%n
 530    savf(i) = acor(i) - yh(i,stoc(isr)%lmax)
      dup = smnorm (sls1(isr)%n, savf, ewt)                             &
     &    / stoc(isr)%tesco(3,sls1(isr)%nq)
      exup = 1.0e0/(sls1(isr)%l+1)
      rhup = 1.0e0/(1.4e0*dup**exup + 0.0000014e0)
 540  exsm = 1.0e0/sls1(isr)%l
      rhsm = 1.0e0/(1.2e0*dsm**exsm + 0.0000012e0)
      rhdn = 0.0e0
      if (sls1(isr)%nq .eq. 1) go to 550
      ddn = smnorm (sls1(isr)%n, yh(1,sls1(isr)%l), ewt)                &
     &    / stoc(isr)%tesco(1,sls1(isr)%nq)
      exdn = 1.0e0/sls1(isr)%nq
      rhdn = 1.0e0/(1.3e0*ddn**exdn + 0.0000013e0)
! if meth = 1, limit rh according to the stability region also. --------
 550  if (sls1(isr)%meth .eq. 2) go to 560
      pdh = max(abs(sls1(isr)%h)*stoc(isr)%pdlast,0.000001e0)
      if (sls1(isr)%l .lt. stoc(isr)%lmax) then
        rhup = min(rhup,stoc(isr)%sm1(sls1(isr)%l)/pdh)
      end if
      rhsm = min(rhsm,stoc(isr)%sm1(sls1(isr)%nq)/pdh)
      if (sls1(isr)%nq .gt. 1) then
        rhdn = min(rhdn,stoc(isr)%sm1(sls1(isr)%nq-1)/pdh)
      end if
      stoc(isr)%pdest = 0.0e0
 560  if (rhsm .ge. rhup) go to 570
      if (rhup .gt. rhdn) go to 590
      go to 580
 570  if (rhsm .lt. rhdn) go to 580
      newq = sls1(isr)%nq
      rh = rhsm
      go to 620
 580  newq = sls1(isr)%nq - 1
      rh = rhdn
      if (sls1(isr)%kflag .lt. 0 .and. rh .gt. 1.0e0) rh = 1.0e0
      go to 620
 590  newq = sls1(isr)%l
      rh = rhup
      if (rh .lt. 1.1e0) go to 610
      r = stoc(isr)%el(sls1(isr)%l)/sls1(isr)%l
      do 600 i = 1,sls1(isr)%n
 600    yh(i,newq+1) = acor(i)*r
      go to 630
 610  stoc(isr)%ialth = 3
      go to 700
! if meth = 1 and h is restricted by stability, bypass 10 percent test.
 620  if (sls1(isr)%meth .eq. 2) go to 622
      if (rh*pdh*1.00001e0 .ge. stoc(isr)%sm1(newq)) go to 625
 622  if (sls1(isr)%kflag .eq. 0 .and. rh .lt. 1.1e0) go to 610
 625  if (sls1(isr)%kflag .le. -2) rh = min(rh,0.2e0)
!-----------------------------------------------------------------------
! if there is a change of order, reset nq, l, and the coefficients.
! in any case h is reset according to rh and the yh array is rescaled.
! then exit from 690 if the step was ok, or redo the step otherwise.
!-----------------------------------------------------------------------
      if (newq .eq. sls1(isr)%nq) go to 170
 630  sls1(isr)%nq = newq
      sls1(isr)%l = sls1(isr)%nq + 1
      iret = 2
      go to 150
!-----------------------------------------------------------------------
! control reaches this section if 3 or more failures have occured.
! if 10 failures have occurred, exit with kflag = -1.
! it is assumed that the derivatives that have accumulated in the
! yh array have errors of the wrong order.  hence the first
! derivative is recomputed, and the order is set to 1.  then
! h is reduced by a factor of 10, and the step is retried,
! until it succeeds or h reaches hmin.
!-----------------------------------------------------------------------
 640  if (sls1(isr)%kflag .eq. -10) go to 660
      rh = 0.1e0
      rh = max(sls1(isr)%hmin/abs(sls1(isr)%h),rh)
      sls1(isr)%h = sls1(isr)%h*rh
      do 645 i = 1,sls1(isr)%n
 645    y(i) = yh(i,1)
      call f (isr, neq, sls1(isr)%tn, y, savf)
      sls1(isr)%nfe = sls1(isr)%nfe + 1
      do 650 i = 1,sls1(isr)%n
 650    yh(i,2) = sls1(isr)%h*savf(i)
      stoc(isr)%ipup = sls1(isr)%miter
      stoc(isr)%ialth = 5
      if (sls1(isr)%nq .eq. 1) go to 200
      sls1(isr)%nq = 1
      sls1(isr)%l = 2
      iret = 3
      go to 150
!-----------------------------------------------------------------------
! all returns are made through this section.  h is saved in hold
! to allow the caller to change h on the next step.
!-----------------------------------------------------------------------
 660  sls1(isr)%kflag = -1
      go to 720
 670  sls1(isr)%kflag = -2
      go to 720
 680  sls1(isr)%kflag = -3
      go to 720
 690  stoc(isr)%rmax = 10.0e0
 700  r = 1.0e0/stoc(isr)%tesco(2,sls1(isr)%nqu)
      do 710 i = 1,sls1(isr)%n
 710    acor(i) = acor(i)*r
 720  stoc(isr)%hold = sls1(isr)%h
      sls1(isr)%jstart = 1
      return
!----------------------- end of subroutine sstoda ----------------------
      end
