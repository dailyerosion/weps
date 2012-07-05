!$Author$
!$Date$
!$Revision$
!$HeadURL$

! opens a file of values and computes the mean and confidence intervals
! starting with the first three values, then 4 then 5 ... till all are included

! input consists of and integer followed by a real numbers, in a single column
! the interger indicates the nquantity fo real numbers to be read in.

integer :: idx   ! loop counter
integer :: nval  ! the cound the count of numbers to be read
integer :: ios   ! iostat return value
real, allocatable :: value(:) ! values read in
real :: prob  ! confidence interval probability level, 90% interval specified as 0.9
real :: mean, ci_hi, ci_low   ! mean and confidence interval values
real :: temp

! count number of data values on stdin
nval = 0
read(*,*, iostat=ios) temp
do while(ios .eq. 0)
    nval = nval + 1
    read(*,*, iostat=ios) temp
end do
!write(*,*) "nval = ", nval

! reset stdin to beginning
rewind(5)

! allocate array for values
allocate (value(1:nval)) 

! read in values
do idx = 1, nval
    read(*,*) value(idx)
    !write(*,*) "Value = ", value(idx)
    !write(*,*) value(idx)
end do

! test various probability levels
do prob = 0.9, 0.99, 0.01
    write(*,*) "# confidence level = ", prob
    !Separate levels with blank lines
    write(*,*)
    write(*,*)
    ! find confidence intervals
    do idx = 1, nval
        call ci_select(value, idx, prob, mean, ci_hi, ci_low)
        !write(*,*) idx, mean, ci_low, ci_hi
        write(*,*) idx, mean, ci_low, ci_hi
    end do
end do

end