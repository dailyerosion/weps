!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      real function sbnorm (n, a, nra, ml, mu, w)
!-----------------------------------------------------------------------
! This function computes the norm of a banded N by N matrix,
! stored in the array A, that is consistent with the weighted max-norm
! on vectors, with weights stored in the array W.
! ML and MU are the lower and upper half-bandwidths of the matrix.
! NRA is the first dimension of the A array, NRA .ge. ML+MU+1.
! In terms of the matrix elements a(i,j), the norm is given by:
!   SBNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) )
!-----------------------------------------------------------------------
      integer n, nra, ml, mu
      integer i, i1, jlo, jhi, j
      real a, w
      real an, sum
      dimension a(nra,n), w(n)
      an = 0.0e0
      do 20 i = 1,n
        sum = 0.0e0
        i1 = i + mu + 1
        jlo = max(i-ml,1)
        jhi = min(i+mu,n)
        do 10 j = jlo,jhi
 10       sum = sum + abs(a(i1-j,j))/w(j)
        an = max(an,sum*w(i))
 20     continue
      sbnorm = an
      return
!----------------------- end of function sbnorm ------------------------
      end
