!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      subroutine scfode (meth, elco, tesco)
!***BEGIN PROLOGUE  SCFODE
!***SUBSIDIARY
!***PURPOSE  Set ODE integrator coefficients.
!***TYPE      SINGLE PRECISION (SCFODE-S, DCFODE-D)
!***AUTHOR  Hindmarsh, Alan C., (LLNL)
!***DESCRIPTION
!
!  SCFODE is called by the integrator routine to set coefficients
!  needed there.  The coefficients for the current method, as
!  given by the value of METH, are set for all orders and saved.
!  The maximum order assumed here is 12 if METH = 1 and 5 if METH = 2.
!  (A smaller value of the maximum order is also allowed.)
!  SCFODE is called once at the beginning of the problem,
!  and is not called again unless and until METH is changed.
!
!  The ELCO array contains the basic method coefficients.
!  The coefficients el(i), 1 .le. i .le. nq+1, for the method of
!  order nq are stored in ELCO(i,nq).  They are given by a genetrating
!  polynomial, i.e.,
!      l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq.
!  For the implicit Adams methods, l(x) is given by
!      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0.
!  For the BDF methods, l(x) is given by
!      l(x) = (x+1)*(x+2)* ... *(x+nq)/K,
!  where         K = factorial(nq)*(1 + 1/2 + ... + 1/nq).
!
!  The TESCO array contains test constants used for the
!  local error test and the selection of step size and/or order.
!  At order nq, TESCO(k,nq) is used for the selection of step
!  size at order nq - 1 if k = 1, at order nq if k = 2, and at order
!  nq + 1 if k = 3.
!
!***SEE ALSO  SLSODE
!***ROUTINES CALLED  (NONE)
!***REVISION HISTORY  (YYMMDD)
!   791129  DATE WRITTEN
!   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
!   890503  Minor cosmetic changes.  (FNF)
!   930809  Renamed to allow single/double precision versions. (ACH)
!***END PROLOGUE  SCFODE
!**End
      integer meth
      integer i, ib, nq, nqm1, nqp1
      real elco, tesco
      real agamq, fnq, fnqm1, pc, pint, ragq
      real rqfac, rq1fac, tsign, xpin
      dimension elco(13,12), tesco(3,12)
      dimension pc(12)
!
!***first executable statement  scfode
      go to (100, 200), meth
!
 100  elco(1,1) = 1.0e0
      elco(2,1) = 1.0e0
      tesco(1,1) = 0.0e0
      tesco(2,1) = 2.0e0
      tesco(1,2) = 1.0e0
      tesco(3,12) = 0.0e0
      pc(1) = 1.0e0
      rqfac = 1.0e0
      do 140 nq = 2,12
!-----------------------------------------------------------------------
! the pc array will contain the coefficients of the polynomial
!     p(x) = (x+1)*(x+2)*...*(x+nq-1).
! initially, p(x) = 1.
!-----------------------------------------------------------------------
        rq1fac = rqfac
        rqfac = rqfac/nq
        nqm1 = nq - 1
        fnqm1 = nqm1
        nqp1 = nq + 1
! form coefficients of p(x)*(x+nq-1). ----------------------------------
        pc(nq) = 0.0e0
        do 110 ib = 1,nqm1
          i = nqp1 - ib
 110      pc(i) = pc(i-1) + fnqm1*pc(i)
        pc(1) = fnqm1*pc(1)
! compute integral, -1 to 0, of p(x) and x*p(x). -----------------------
        pint = pc(1)
        xpin = pc(1)/2.0e0
        tsign = 1.0e0
        do 120 i = 2,nq
          tsign = -tsign
          pint = pint + tsign*pc(i)/i
 120      xpin = xpin + tsign*pc(i)/(i+1)
! store coefficients in elco and tesco. --------------------------------
        elco(1,nq) = pint*rq1fac
        elco(2,nq) = 1.0e0
        do 130 i = 2,nq
 130      elco(i+1,nq) = rq1fac*pc(i)/i
        agamq = rqfac*xpin
        ragq = 1.0e0/agamq
        tesco(2,nq) = ragq
        if (nq .lt. 12) tesco(1,nqp1) = ragq*rqfac/nqp1
        tesco(3,nqm1) = ragq
 140    continue
      return
!
 200  pc(1) = 1.0e0
      rq1fac = 1.0e0
      do 230 nq = 1,5
!-----------------------------------------------------------------------
! the pc array will contain the coefficients of the polynomial
!     p(x) = (x+1)*(x+2)*...*(x+nq).
! initially, p(x) = 1.
!-----------------------------------------------------------------------
        fnq = nq
        nqp1 = nq + 1
! form coefficients of p(x)*(x+nq). ------------------------------------
        pc(nqp1) = 0.0e0
        do 210 ib = 1,nq
          i = nq + 2 - ib
 210      pc(i) = pc(i-1) + fnq*pc(i)
        pc(1) = fnq*pc(1)
! store coefficients in elco and tesco. --------------------------------
        do 220 i = 1,nqp1
 220      elco(i,nq) = pc(i)/pc(2)
        elco(2,nq) = 1.0e0
        tesco(1,nq) = rq1fac
        tesco(2,nq) = nqp1/elco(1,nq)
        tesco(3,nq) = (nq+2)/elco(1,nq)
        rq1fac = rq1fac/fnq
 230    continue
      return
!----------------------- end of subroutine scfode ----------------------
      end
