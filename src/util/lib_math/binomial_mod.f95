!$Author:$
!$Date:$
!$Revision:$
!$HeadURL:$

module binomial_mod

  ! Routines to return points of the binomial distribution or
  ! binary coefficients. When only one point is needed, or initialization
  ! is not done, routine uses bico to find the binary coefficient.
  ! If a large number of points or coefficients are needed, the
  ! initialization routine should be run before the first call.

  ! Tested in gfortran and values for n up to 67. For n greater than 67,
  ! mid range k values will not be correct.

  use, intrinsic :: iso_fortran_env

  ! stored values of binomial coefficient (Pascal's Triangle)
  integer(kind=int64), dimension(:,:), allocatable :: store_bico

  contains

    ! binomial distribution: the discrete probability distribution of
    ! the number of successes in a sequence of n independent experiments
    ! based on https://en.wikipedia.org/wiki/Binomial_distribution
    function bino (n,k,p) result(res_bino)
      integer :: n  ! number of independent Bernoulli trials
      integer :: k  ! Maximum possible successes
      real :: p     ! success probability
      real :: res_bino  ! probability of getting exactly k successes in n trials

      real    lntiny  ! smallest value of real which will not give a denormal answer
      double precision res_bico

      if( .not. allocated(store_bico) ) then
        res_bico = bico(n, k)
      else
        res_bico = store_bico(n, k)
      end if

      ! this prevents bino from returning a denormal number
      lntiny = log(tiny(lntiny))
      if ( ((n-k)*log(1-p)) .lt. lntiny ) then
        !write(*,*) 'UNDERFLOW: ', n, k, p, lntiny, ((n-k)*log(1-p))
        res_bino = 0.0
      else
        res_bino = res_bico * (p**dble(k)) * ((1.0d0-p)**dble(n-k))
      end if

    end function bino

    ! find number of possible ways to select size k subset from a larger set of size n
    ! see https://en.wikipedia.org/wiki/Binomial_coefficient
    function bico(n, k) result(binom_coeff)
      integer :: n   ! size of a large set
      integer :: k   ! size of subset
      double precision :: binom_coeff            ! resultant binomial coefficient

      integer(kind=int64) :: c_sum
      integer(kind=int64) :: idx
      integer(kind=int64) :: n_loc
      integer(kind=int64) :: k_loc
      integer(kind=int64) :: max_val

      max_val = huge(max_val)

      c_sum = 1
      n_loc = n
      k_loc = k
      if( k_loc .gt. (n_loc - k_loc) ) then
        k_loc = n_loc - k_loc
      end if

      do idx = 1, k_loc
        if( (c_sum/idx) .gt. (max_val/n_loc) ) then
          c_sum = 0
          exit
        else
          c_sum = c_sum/idx * n_loc + mod(c_sum, idx) * n_loc / idx
          n_loc = n_loc - 1
        end if
      end do

      binom_coeff = c_sum

    end function bico

    ! initialize binomial coefficiant storage buffer with coefficients using Pascal's Triangle
    ! this is an alternative to bico shown above (and is faster if most of the numbers are needed)
    ! see https://en.wikipedia.org/wiki/Binomial_coefficient
    subroutine init_buffer(buf_size)
      integer :: buf_size

      integer alloc_stat     ! return value for allocation
      integer ndx            ! array index for loop
      integer kdx            ! array index for loop

      allocate( store_bico(0:buf_size,0:buf_size), stat = alloc_stat )
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'ERROR: unable to allocate memory in binomial/init_buffer'
        stop 1
      end if

      do ndx = 0, buf_size
        do kdx = 0, buf_size
          if( (ndx .eq. 0) .or. (kdx .eq. 0) .or. (ndx .eq. kdx) ) then
            store_bico(ndx,kdx) = 1
          else if( ndx .gt. kdx ) then
            store_bico(ndx,kdx) = store_bico(ndx-1,kdx-1) + store_bico(ndx-1,kdx)
          else
            store_bico(ndx,kdx) = -1
          end if
        end do
      end do

    end subroutine init_buffer

end module binomial_mod
