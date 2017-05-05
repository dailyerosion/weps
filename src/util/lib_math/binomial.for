!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/math/binomial.for,v 1.2 2002-09-04 20:22:19 wagner Exp $

      real function bino (n,k,p)

!     + + + PURPOSE + + +
!     Addition to the bico function which returns binomial coefficient
!     as a floating point number.
!
!     Modification based on:
!     'NUMERICAL RECIPES - The Art of Scientific Computing',
!     W.H. Press, B.P. Flannery, S.A. Teukolsky, W.T. Vetterling
!     Cambridge University Press, 1986
!     pg 158
!
!     + + + KEYWORDS + + +
!     binomial function
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer  n,k
      real     p
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!     n,k    - inputs for computing binomial coefficient
!     p      - probability value
!
!     + + + LOCAL VARIABLES + + +
      real    factln
      real    bico, lnbico
      real    lnbino
      real    lnfact1
      real    lnfact2

      real eps, lnsmall, lnfpmin

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     bico   - computed binomial coefficient
!
!     + + + END SPECIFICATIONS + + +

      eps = epsilon(eps)
      lnsmall = log(tiny(eps))
      lnfpmin = log(tiny(eps)/eps)

      bico = anint(exp(factln(n) - factln(k) - factln(n-k)))
      lnbico = log(bico)

      !fact1 = (p**dble(k))
      lnfact1 = k * log(p)

      !fact2 = ((1.0-p)**dble(n-k))
      lnfact2 = (n-k) * log(1.0-p)

      lnbino = lnbico + lnfact1 + lnfact2

      if ( lnbino .lt. lnsmall ) then
        write(*,*) 'UNDERFLOW: ', n, k, p, lnbino, lnsmall
        bino = 0.0
      else
        bino = exp(lnbino)
      end if

      !write(*,*)'BINO: ',bico*(p**dble(k))*((1.0-p)**dble(n-k)), bino
      !bino = bico*(p**dble(k))*((1.0-p)**dble(n-k))

      return
      end
