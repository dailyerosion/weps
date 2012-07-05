!$Author$
!$Date$
!$Revision$
!$HeadURL$

! This subroutine calculates the confidence interval for the annual 
! erosion rate averaged over one rotation, using each time throught the 
! rotation as one number. No confidence interval can be obtained until
! three rotation cycles have been completed.

subroutine confidence_interval(ci, nrot_yrs, n1cycles, ci_year)

    use pd_report_vars
    use pd_var_tables

    implicit none

    include 'file.inc'
 

    real,    intent (in) :: ci ! confidence interval value (decimal)
    integer, intent (in) :: nrot_yrs ! number of year in a rotation cycle
    integer, intent (in) :: n1cycles ! one more than the number of rotation cycles completed
    integer, intent (inout) :: ci_year ! indicates how many years of data have been printed into ci.out
    integer :: ncycles      ! the number of rotation cycles completed
    integer :: idy          ! local loop variable
    integer :: nrot         ! index of which time through the rotation
    integer :: rot_yr_cnt   ! number of values accumuated for this time through the rotation
    integer :: nzero, ngtz  ! count of erosion amounts in zero and greater than zero classes
    real :: rot_yr_sum      ! total of erosion accumuated for this time through the rotation
    real, allocatable :: eros_yr(:)    ! total erosion for one simulation year (array)
    real, allocatable :: eros_rot(:)   ! average annual total erosion for one rotation cycle (array)
    real :: mean, ci_low, ci_hi
    integer :: cycle_print  ! cycle number which matches the simulation year being printed
                            ! since printing does not start until ncycle = 4.

    INTEGER :: alloc_status = 0         ! Local allocate status return

    ! find number of cycles completed
    ncycles = n1cycles - 1

    ! check for a sufficient number of data values before first calculation
    if( ncycles .le. 3 ) then
       return
    end if

    ! Just using this as a way to determine if this is the first time into the file for writing - LEW
    if( ci_year .eq. 0 ) then
        write(UNIT=luoci,FMT="(a)",ADVANCE="NO")  "nrot_yrs | ncycles |   yr  | #events |  yr_total  | "
        write(UNIT=luoci,FMT="(a,2(f5.2,a))",ADVANCE="NO") "  yrly_ave   |  Low_", ci*100, "% | High_", ci*100, "% |"
    END IF

    ! Note that this code is expected to allocate the arrays for each
    ! independent call to the subroutine.  Newer releases of at least
    ! the Lahey compiler require the variables to first be "de-allocated"
    ! prior to any re-allocation - LEW
    IF (ALLOCATED (eros_yr) .eqv. .TRUE.)  DEALLOCATE (eros_yr)
    IF (ALLOCATED (eros_rot) .eqv. .TRUE.) DEALLOCATE (eros_rot)

    ! allocate array for year by year erosion values
    !allocate (eros_yr(1:nrot_yrs*ncycles)) 
    ! allocate array for rotation cycle by rotation cycle erosion values
    IF (ALLOCATED (eros_yr) .neqv. .TRUE.) then
        ALLOCATE (eros_yr(1:nrot_yrs*ncycles), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
           write(0,*) "Error allocating eros_yr(nrot_yrs*ncycles):", alloc_status
           call exit (1)
        END IF
    END IF

    IF (ALLOCATED (eros_rot) .neqv. .TRUE.) then
        ALLOCATE (eros_rot(1:ncycles), STAT = alloc_status) 
        IF (alloc_status /= 0) THEN
           write(0,*) "Error allocating eros_rot(ncycles):", alloc_status
           call exit (1)
        END IF
    END IF

    ! create annual erosion value array and rotation cycle array
    nrot = 0
    rot_yr_cnt = 0
    rot_yr_sum = 0.0
    nzero = 0
    ngtz = 0

    write(*,*) "nrot_yrs, ncycles ", nrot_yrs, ncycles
    do idy = 1, nrot_yrs*ncycles

       if (yr_report(N_eros_events,idy)%val > 0.0) then
          ! net loss per erosion by year (make positive for statistics)
          eros_yr(idy) = - yr_report(Eros_loss,idy)%val
       else
          eros_yr(idy) = 0.0
       end if

       if( ci_year .lt. idy ) then
           ci_year = idy
           ! make ncycles printed out to match the simulation year being printed
           cycle_print = (idy-1)/nrot_yrs + 1
           write(UNIT=luoci,FMT="(a)",ADVANCE="YES") ''
           write(UNIT=luoci,FMT="(i8,a,i7,a,i5,a)",ADVANCE="NO")  nrot_yrs," | ", cycle_print," | ", idy," | "
           write(UNIT=luoci,FMT="(f7.2,a3)",ADVANCE="NO")  yr_report(N_eros_events,idy)%val," | "
           write(UNIT=luoci,FMT="(f10.5,a3, f10.5)",ADVANCE="NO") eros_yr(idy)," | "
!            write(UNIT=luoci,FMT="(a,i5,i5)",ADVANCE="NO")  "nrot_yrs, ncycles ", nrot_yrs, ncycles
!!           write(UNIT=luoci,FMT="(a,i5,a,i5,a,f10.5)",ADVANCE="NO")  "#events/yr ", idy, &
!            write(UNIT=luoci,FMT="(a,i5)",ADVANCE="NO")  " #events in yr ", idy
!            write(UNIT=luoci,FMT="(a,f5.2)",ADVANCE="NO")  " is ", yr_report(N_eros_events,idy)%val
!            write(UNIT=luoci,FMT="(a,f10.5)",ADVANCE="NO") " with total erosion of ", eros_yr(idy)
       end if

       !write(*,*) "Number of events in year ", idy, "is", yr_report(N_eros_events,idy)%val, "with total erosion of ", eros_yr(idy)

       rot_yr_cnt = rot_yr_cnt + 1
       rot_yr_sum = rot_yr_sum + eros_yr(idy)

       if( rot_yr_cnt .eq. nrot_yrs ) then
          ! completed rotation, add to count 
          nrot = nrot + 1
          ! find average annual erosion for rotation
          eros_rot(nrot) = rot_yr_sum / rot_yr_cnt
          ! reset for next time through rotation
          rot_yr_cnt = 0
          rot_yr_sum = 0.0
       end if

    end do

    call ci_select(eros_rot, ncycles, ci, mean, ci_hi, ci_low)
    write(*,*) ci*100,"% Confidence Interval: ", ci_low, ci_hi
    !ci_hi = min( ci_hi, 99999999.0 )
    write(UNIT=luoci,FMT="(f12.5,a,f12.5,a,g16.5,a)",ADVANCE="NO") -yrly_report(Eros_loss,0)%val, " | ", ci_low, " | ", ci_hi, " | "
!    write(UNIT=luoci,FMT="(f6.2,a,f10.5,f10.5)",ADVANCE="NO") ci*100,"% Confidence Interval: ", ci_low, ci_hi

end subroutine confidence_interval
