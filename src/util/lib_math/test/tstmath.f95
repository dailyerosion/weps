!$Author$
!$Date$
!$Revision$
!$HeadURL$

program tstmath

!  use binomial_mod, only: bico, init_buffer, store_bico
  use binomial_mod, only: bino1, init_buffer

      integer  n,k

!      double precision :: bico1, bico2
      real :: bino_old, bino_new
      integer, parameter :: nmax = 67

      real bino

      call init_buffer(nmax)

      do n = 1, nmax
        do k = 1, n

!          bico1 = bico(n-1, k-1)
!          bico2 = store_bico(n-1,k-1)
!          write(*,*) 'BICO: ', n-1, k-1, bico1, bico2, store_bico(n-1,k-1)

          bino_old = bino(n-1, k-1, 0.5)
          bino_new = bino1(n-1, k-1, 0.5)
          write(*,*) 'BINO: ', n-1, k-1, bino_old, bino_new

        end do
      end do

      stop
end program tstmath

