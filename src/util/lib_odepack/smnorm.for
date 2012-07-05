!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
      real function smnorm (n, v, w)
!-----------------------------------------------------------------------
! This function routine computes the weighted max-norm
! of the vector of length N contained in the array V, with weights
! contained in the array w of length N:
!   SMNORM = MAX(i=1,...,N) ABS(V(i))*W(i)
!-----------------------------------------------------------------------
      integer n,   i
      real v, w,   vm
      dimension v(n), w(n)
      vm = 0.0e0
      do 10 i = 1,n
 10     vm = max(vm,abs(v(i))*w(i))
      smnorm = vm
      return
!----------------------- end of function smnorm ------------------------
      end
