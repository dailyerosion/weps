!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!     file: sdst.for

      subroutine sdst (x,dg,dg1,i)

!     + + + PURPOSE + + +
!     This subroutine initializes residue, nitorgen, and phosphorous amounts in
!     layers

!     + + + KEYWORDS + + +
!     Initialization

!     + + + LOCAL VARIABLES + + +
      integer i
      real dg, dg1, x(*)

!     + + + LOCAL DEFINITIONS + + +
!     dg    -
!     dg1   -
!     i     -
!     x     -

!      dimension x(10)

!     + + + END SPECIFICATIONS + + +

!     DATA E/'E'/
      if (x(i) .gt. 0.) go to 4
      if (i .gt. 1) go to 2
      x(1) = 1.
      go to 4
    2 x(i) = x(i-1) * dg * exp(-.01*dg)/dg1
!   3 EST(J,I)=E
    4 return
      end
