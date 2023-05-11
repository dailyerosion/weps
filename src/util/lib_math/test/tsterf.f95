!$Author$
!$Date$
!$Revision$
!$HeadURL$

program tsterf

  use erf_mod, only: erf1, rerf

    real :: x
    real :: value_intrinsic
    real :: value_hagen
    real :: c_value_intrinsic
    real :: r_value_hagen

    x = 0.17
    value_intrinsic = erf(x)
    value_hagen = erf1(x)

    write(*,*) 'ERF: ', value_intrinsic, value_hagen

    c_value_intrinsic = erfc(value_intrinsic)
    r_value_hagen = rerf(value_hagen)

    write(*,*) 'ERFC: ', c_value_intrinsic, r_value_hagen

    stop
end program tsterf

