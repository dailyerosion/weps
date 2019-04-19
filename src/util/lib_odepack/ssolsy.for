!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine ssolsy (isr, wm, iwm, x, tem)

      use hydro_darcy_mod, only: sls1

!***BEGIN PROLOGUE  SSOLSY
!***SUBSIDIARY
!***PURPOSE  ODEPACK linear system solver.
!***TYPE      SINGLE PRECISION (SSOLSY-S, DSOLSY-D)
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
!
!  This routine manages the solution of the linear system arising from
!  a chord iteration.  It is called if MITER .ne. 0.
!  If MITER is 1 or 2, it calls SGESL to accomplish this.
!  If MITER = 3 it updates the coefficient h*EL0 in the diagonal
!  matrix, and then computes the solution.
!  If MITER is 4 or 5, it calls SGBSL.
!  Communication with SSOLSY uses the following variables:
!  WM    = real work space containing the inverse diagonal matrix if
!          MITER = 3 and the LU decomposition of the matrix otherwise.
!          Storage of matrix elements starts at WM(3).
!          WM also contains the following matrix-related data:
!          WM(1) = SQRT(UROUND) (not used here),
!          WM(2) = HL0, the previous value of h*EL0, used if MITER = 3.
!  IWM   = integer work space containing pivot information, starting at
!          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band
!          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
!  X     = the right-hand side vector on input, and the solution vector
!          on output, of length N.
!  TEM   = vector of work space of length N, not used in this version.
!  IERSL = output flag (in COMMON).  IERSL = 0 if no trouble occurred.
!          IERSL = 1 if a singular matrix arose with MITER = 3.
!  This routine also uses the COMMON variables EL0, H, MITER, and N.
!
!***SEE ALSO  SLSODE
!***ROUTINES CALLED  SGBSL, SGESL
!***COMMON BLOCKS    SLS001
!***REVISION HISTORY  (YYMMDD)
!   791129  DATE WRITTEN
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
!   890503  Minor cosmetic changes.  (FNF)
!   930809  Renamed to allow single/double precision versions. (ACH)
!   010412  Reduced size of Common block /SLS001/. (ACH)
!***END PROLOGUE  SSOLSY
!**End
      integer :: isr
      integer iwm
!      integer icf, ierpj, iersl, jcur, jstart, kflag, l
!      integer   lyh, lewt, lacor, lsavr, lwm, liwm, meth, miter
!      integer   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu
      integer i, meband, ml, mu
      real wm, x, tem
!      real ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround
      real di, hl0, phl0, r
      dimension wm(*), iwm(*), x(*), tem(*)
!      common /sls001/ ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround,    &
!     &   icf, ierpj, iersl, jcur, jstart, kflag, l,                     &
!     &   lyh, lewt, lacor, lsavr, lwm, liwm, meth, miter,               &
!     &   maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu

!      save :: /sls001/
!
!***first executable statement  ssolsy
      sls1(isr)%iersl = 0
      go to (100, 100, 300, 400, 400), sls1(isr)%miter
 100  call sgesl (wm(3), sls1(isr)%n, sls1(isr)%n, iwm(21), x, 0)
      return
!
 300  phl0 = wm(2)
      hl0 = sls1(isr)%h*sls1(isr)%el0
      wm(2) = hl0
      if (hl0 .eq. phl0) go to 330
      r = hl0/phl0
      do 320 i = 1,sls1(isr)%n
        di = 1.0e0 - r*(1.0e0 - 1.0e0/wm(i+2))
        if (abs(di) .eq. 0.0e0) go to 390
 320    wm(i+2) = 1.0e0/di
 330  do 340 i = 1,sls1(isr)%n
 340    x(i) = wm(i+2)*x(i)
      return
 390  sls1(isr)%iersl = 1
      return
!
 400  ml = iwm(1)
      mu = iwm(2)
      meband = 2*ml + mu + 1
      call sgbsl (wm(3), meband, sls1(isr)%n, ml, mu, iwm(21), x, 0)
      return
!----------------------- end of subroutine ssolsy ----------------------
      end
