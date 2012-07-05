!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      real function sfnorm (n, a, w)
!-----------------------------------------------------------------------
! This function computes the norm of a full N by N matrix,
! stored in the array A, that is consistent with the weighted max-norm
! on vectors, with weights stored in the array W:
!   SFNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) )
!-----------------------------------------------------------------------
      integer n,   i, j
      real a,   w, an, sum
      dimension a(n,n), w(n)
      an = 0.0e0
      do 20 i = 1,n
        sum = 0.0e0
        do 10 j = 1,n
 10       sum = sum + abs(a(i,j))/w(j)
        an = max(an,sum*w(i))
 20     continue
      sfnorm = an
      return
!----------------------- end of function sfnorm ------------------------
      end
