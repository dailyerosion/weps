!$Author$
!$Date$
!$Revision$
!$HeadURL$

integer function lcm_n( n, input )

    ! extended lcm below to n input numbers
    ! by recursively comparing the lcm for 2 numbers
    ! against the next number until the list is completed.

    ! arguments
    integer :: n, input(n)

    ! local variables
    integer idx

    ! function declaration
    integer lcm

    if( n .eq. 1 ) then
        lcm_n = input(1)
    else
        idx = 2
        lcm_n = lcm( input(idx-1), input(idx) )
        idx = idx + 1
        do while ( idx .le. n )
            lcm_n = lcm( lcm_n, input(idx) )
            idx = idx + 1
        end do
    end if

    return
end

integer function lcm(a, b)

    ! arguments
    integer :: a, b

    ! function declaration
    integer gcd

    if( ( a .ne. 0) .and. (b .ne. 0) ) then
        lcm = (a * b / gcd(a, b))
    else
        write(*,*) 'LCM: Least common multiple of zero is undefined'
        lcm = 0
    end if

    return
end

integer function gcd(a, b)
    
    ! find the greatest common denominator
    ! of two integers a and b using the
    ! Euclidean algorithm.

    ! note: this function returns an answer for any pair
    ! in any order including 0, 0

    ! arguments
    integer :: a, b

    ! local variables
    integer :: temp, loc_a, loc_b

    ! preserve the input values unchanged
    loc_a = a
    loc_b = b

    do while (loc_b .ne. 0)
        temp = loc_b
        loc_b = mod(loc_a, loc_b)
        loc_a = temp
    end do

    gcd = loc_a

    return
end


! while very nice, the functions above are cleaner.

!integer function lcm( input1, input2 )
!
!    ! http://jonlandrum.com/2012/03/02/finding-the-least-common-multiple-using-euclids-algorithm-and-cpp/
!    ! translated from C++ to fortran
!    ! http://en.wikipedia.org/wiki/Least_common_multiple
!
!    ! arguments
!    integer :: input1, input2
!
!    ! local variables
!    integer :: smaller, larger, remainder
!
!    ! Find the smaller of the two
!    if( input1 - input2 .gt. 0 ) then
!        smaller = input2
!        larger  = input1
!    else
!        smaller = input1
!        larger  = input2
!    end if
!     
!    if( mod(larger, smaller) .eq. 0) then
!        lcm = larger
!    else
!        remainder = mod(larger, smaller)
!        do while (remainder .ne. 0) {
!            larger = smaller;
!            smaller = remainder;
!            remainder = mod(larger, smaller)
!        end do
!        lcm = (input1 * input2) / smaller
!    end if
!
!    return
!end

