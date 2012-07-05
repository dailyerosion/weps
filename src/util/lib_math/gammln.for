!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/math/gammln.for,v 1.2 2002-09-04 20:22:19 wagner Exp $

      real function gammln (xx)

!     + + + PURPOSE + + +
!     Computes the ln of the gamma function for xx > 0
!     Full accuracy is obtained for xx > 1
!     For 0 < xx < 1, the reflection formula (6.1.4) can be used first.
!
!     Based on:
!     'NUMERICAL RECIPES - The Art of Scientific Computing',
!     W.H. Press, B.P. Flannery, S.A. Teukolsky, W.T. Vetterling
!     Cambridge University Press, 1986
!     pg 157
!
!     + + + KEYWORDS + + +
!     gamma function
!
!     + + + ARGUMENT DECLARATIONS + + +
      real   xx
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!     xx     - real value for values > 0
!
!     + + + LOCAL VARIABLES + + +
      double precision cof(6), stp
      double precision half, one, fpf
      double precision x, tmp, ser
      integer j
!
      data cof, stp / 76.18009173D0, -86.50532033D0,                    &
     &                24.01409822D0,  -1.231739516D0,                   &
     &                 0.120858003D-2,-0.536382D-5,                     &
     &                 2.50662827465D0 /
      data half, one, fpf / 0.5D0, 1.0D0, 5.5D0 /
!
!
!     + + + END SPECIFICATIONS + + +
!
      x = dble(xx)-one
      tmp = x+fpf
      tmp = (x+half)*log(tmp)-tmp
      ser=one
      do 100 j=1,6
          x = x+one
          ser = ser+cof(j)/x
100   continue
      gammln = tmp+log(stp*ser)

      return
      end
