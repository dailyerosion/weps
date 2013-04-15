!$Author$
!$Date$
!$Revision$
!$HeadURL$

Module lin_interp_mod

contains

  function lin_interp(low_index, frac_dist, values) result(val_interp)

    ! linearly interpolates a value between two locations in an array

    ! arguments
    integer, intent(in) :: low_index    ! the lower index into the value arraypnt
    real, intent(in) :: frac_dist       ! the fraction of distance from low_index to next_index (interpolate to here)
    real, dimension(:), intent(in) :: values  ! array of values between which interpolation will be done
    real :: val_interp                  ! the interpolated value

    ! local variables
    integer :: hi_index    ! index of second point in values array
    real :: val_diff       ! difference in value between the two points
    real :: val_prop       ! proportion of difference to be added to low_index value

    ! define second point in values array
    hi_index = low_index + 1

    ! find difference in value between the two points
    val_diff = values(hi_index) - values(low_index)

    ! find difference proportion
    val_prop = val_diff * frac_dist

    ! interpolated value
    val_interp = values(low_index) + val_prop

  end function lin_interp

end module lin_interp_mod

