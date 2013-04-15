!$Author$
!$Date$
!$Revision$
!$HeadURL$

! http://rosettacode.org/wiki/Closest_pair_problem/Fortran

module Points_Mod
  use p1unconv_mod, only: degtorad
  implicit none
 
  type point
     real :: x, y
  end type point
 
  interface operator (-)
     module procedure pt_sub
  end interface
 
  interface slen
     module procedure pt_len
     module procedure pt_slen
  end interface
 
  interface translate
     module procedure pt_translate_1
     module procedure pt_translate_n
  end interface

  interface rotate
     module procedure pt_rotate_1
     module procedure pt_rotate_n
  end interface

  public :: point
  private :: pt_sub, pt_len, pt_slen
 
contains
 
  function pt_sub(a, b) result(c)
    type(point), intent(in) :: a, b
    type(point) :: c
 
    c = point(a%x - b%x, a%y - b%y)
  end function pt_sub
 
  function pt_len(a) result(l)
    type(point), intent(in) :: a
    real :: l
 
    l = sqrt((a%x)**2 + (a%y)**2)
  end function pt_len
 
  function pt_slen(a, b) result(l)
    type(point), intent(in) :: a, b
    real :: l

    type(point) :: b_a0

    ! translate segment so a is at origin 
    b_a0 = translate(a, b)

    l = pt_len(b_a0)
  end function pt_slen

  function pt_translate_1(a, b) result(b_a0)
    type(point), intent(in) :: a, b
    type(point) :: b_a0

    ! translate point so a is at origin 
    b_a0 = b - a
  end function pt_translate_1

  function pt_translate_n(a, b) result(b_a0)
    type(point), intent(in) :: a
    type(point), dimension(:), intent(in) :: b
    type(point), dimension(1:size(b)) :: b_a0

    integer idx

    do idx = 1, size(b)
      ! translate point so a is at origin 
      b_a0(idx) = b(idx) - a
    end do
  end function pt_translate_n

  function pt_rotate_1(angle, b) result(b_a0)
    ! rotates points angle degrees around the origin
    real, intent(in) :: angle
    type(point), intent(in) :: b
    type(point) :: b_a0

    real :: radangle  ! angle in radians
    real :: cosangle, sinangle  ! conputed cosine and sine of angle

    radangle = angle * degtorad
    cosangle = cos(radangle)
    sinangle = sin(radangle)

    ! rotate point around origin 
    b_a0%x = b%x * cosangle - b%y * sinangle
    b_a0%y = b%y * cosangle - b%x * sinangle

  end function pt_rotate_1

  function pt_rotate_n(angle, b) result(b_a0)
    ! rotates points angle degrees (counterclock wise is positive) around the origin

    real, intent(in) :: angle
    type(point), dimension(:), intent(in) :: b
    type(point), dimension(1:size(b)) :: b_a0

    integer :: idx    ! index for stepping through array
    real :: radangle  ! angle in radians
    real :: cosangle, sinangle  ! conputed cosine and sine of angle

    radangle = angle * degtorad
    cosangle = cos(radangle)
    sinangle = sin(radangle)

    do idx = 1, size(b)
      ! rotate point around origin 
      b_a0(idx)%x = b(idx)%x * cosangle - b(idx)%y * sinangle
      b_a0(idx)%y = b(idx)%y * cosangle - b(idx)%x * sinangle
    end do
  end function pt_rotate_n

end module Points_Mod
