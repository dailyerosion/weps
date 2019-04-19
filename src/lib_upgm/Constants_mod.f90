module constants
    use, intrinsic :: iso_fortran_env, only : isodp => real64, isoint=> int32 
    implicit none
    integer, parameter :: int32 = isoint
    integer, parameter :: dp    = isodp

    real(dp), parameter :: u_pi = 4.0_dp * datan (1.0_dp)          ! value of pi in radians

    real(dp), parameter :: u_mmtom    = 1.0_dp/1000_dp  ! millimeter to meter
    real(dp), parameter :: u_mgtokg   = 1.0_dp/1000000_dp ! milligram to kilogram

    real(dp), parameter :: u_hatom2 = 10000.0_dp  ! hectare to square meters

    ! defined variables for portable checking of precision related numbers
    ! variables are initialized in main to make available to all subprograms

    real(dp) :: u_max_arg_exp  !  maximum value allowed for argument to exponential
                       !  function without overflowing
    real(dp) :: u_max_real     !  maximum real number allowed

  contains

    subroutine precision_init()

      ! initialize math precision global variables
      ! the factor here is due to the implementation of the EXP function
      ! apparently, the limit is not the real number limit, but something else
      ! this works in Lahey, but I cannot attest to it's portability

      u_max_real = huge(1.0_dp) * 0.999150_dp
      u_max_arg_exp = log(u_max_real)

    end   subroutine precision_init

    function check_return( var_name, succ ) result(result_val)
      character(*) :: var_name
      logical :: succ
      logical :: result_val

      ! returns true if the value is successfully retireved
      ! also resets the succ value to false for next test
      if( succ ) then
        succ = .false.
        result_val = .true.
      else
        write(*,*) var_name, " input missing. Failed to run"
        result_val = .false.
      end if

    end function check_return

end module constants
