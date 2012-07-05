!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!$Header: /weru/cvs/weps/weps.src/util/math/factln.for,v 1.3 2002-09-04 20:22:19 wagner Exp $

      real function factln (n)
 
!     + + + PURPOSE + + +
!     Computes the ln(n!) for n > 0
!
!     Based on:
!     'NUMERICAL RECIPES - The Art of Scientific Computing',
!     W.H. Press, B.P. Flannery, S.A. Teukolsky, W.T. Vetterling
!     Cambridge University Press, 1986
!     pg 159
!
!     + + + KEYWORDS + + +
!     factorial
!
!     + + + ARGUMENT DECLARATIONS + + +
      integer n
!
!
!     + + + ARGUMENT DEFINITIONS + + +
!     n      - real value for values >= 0
!
!     + + + LOCAL VARIABLES + + +
!
      real   gammln
      real   a(100)
!
      data a / 100*-1 /
!
!     + + + END SPECIFICATIONS + + +
!
      if (n .lt. 0) then
          write (*,*) 'In function factln(n): negative factorial!!'
      endif
      if (n .le. 99) then
          if (a(n+1) .lt. 0.0) a(n+1) = gammln(n+1.0)
          factln = a(n+1)
      else
          factln = gammln(n+1.0)
      endif

      return
      end
