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
!
      real    factln
      real    bico
!
!     + + + LOCAL VARIABLE DEFINITIONS + + +
!
!     bico   - computed binomial coefficient
!
!     + + + END SPECIFICATIONS + + +
!
      bico = anint(exp(factln(n) - factln(k) - factln(n-k)))
      bino = bico*(p**dble(k))*((1.0-p)**dble(n-k))

      return
      end
