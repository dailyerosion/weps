!$Author$
!$Date$
!$Revision$
!$HeadURL$

module read_write_xml_mod

  implicit none
  private

  integer, parameter :: MAX_NAME_LEN  = 40

  integer :: indent  ! indent level of tag being written
  integer, parameter :: INDENT_SPACES = 1

  interface read_param
    module procedure read_param_real_1
    module procedure read_param_real_2
    module procedure read_param_int_1
    module procedure read_param_int_2
    module procedure read_param_int_3
    module procedure read_param_delim_int_3
    module procedure read_param_char
  end interface

  interface w_begin_tag
    module procedure w_begin_tag_a0
    module procedure w_begin_tag_a1
    module procedure w_begin_tag_a2
  end interface

  interface w_whole_tag
    module procedure w_whole_tag_a0_real
    module procedure w_whole_tag_a0_integer
    module procedure w_whole_tag_a0_char
    module procedure w_whole_tag_a1
    module procedure w_whole_tag_a2_real
    module procedure w_whole_tag_a2_integer
  end interface

  public :: read_param
  public :: w_begin_tag
  public :: w_end_tag
  public :: w_whole_tag

contains

  subroutine read_param_real_1(tag_name, param_string, val)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    real, intent(out) :: val
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', trim(tag_name), ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_real_1

  subroutine read_param_real_2(tag_name, param_string, val_1, val_2)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    real, intent(out) :: val_1
    real, intent(out) :: val_2
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val_1, val_2
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', trim(tag_name), ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_real_2

  subroutine read_param_int_1(tag_name, param_string, val)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    integer, intent(out) :: val
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', trim(tag_name), ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_1

  subroutine read_param_int_2(tag_name, param_string, val_1, val_2)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    integer, intent(out) :: val_1
    integer, intent(out) :: val_2
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val_1, val_2
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', trim(tag_name), ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_2

  subroutine read_param_int_3(tag_name, param_string, val_1, val_2, val_3)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    integer, intent(out) :: val_1
    integer, intent(out) :: val_2
    integer, intent(out) :: val_3
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val_1, val_2, val_3
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', trim(tag_name), ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_3

  subroutine read_param_delim_int_3(tag_name, param_string, sepchr, val_1, val_2, val_3)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    character, intent(in) :: sepchr  ! separation character
    integer, intent(out) :: val_1
    integer, intent(out) :: val_2
    integer, intent(out) :: val_3

    integer :: delim1  ! location of first sepchr in date string
    integer :: delim2  ! location of second sepchr in date string
    integer :: sum_stat
    integer :: read_stat

    delim1 = index( param_string, sepchr )
    delim2 = index( param_string(delim1+1:), sepchr ) + delim1

    sum_stat = 0
    read(param_string(:delim1-1),*,iostat=read_stat) val_1
    sum_stat = sum_stat + read_stat
    read(param_string(delim1+1:delim2-1),*,iostat=read_stat) val_2
    sum_stat = sum_stat + read_stat
    read(param_string(delim2+1:),*,iostat=read_stat) val_3
    sum_stat = sum_stat + read_stat
    if (sum_stat .gt. 0) then
      write(*,*) 'Error reading ', trim(tag_name), ' Value: ', param_string
      call exit(1)
    end if

  end subroutine read_param_delim_int_3

  subroutine read_param_char(tag_name, param_string, val)
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: param_string
    character(len=*), intent(out) :: val

    val = trim(param_string)

  end subroutine read_param_char

  subroutine w_spaces( luo_saeinp )
    integer, intent(in) :: luo_saeinp      ! output unit number

    integer :: idx

    do idx = 1, indent
      write(luo_saeinp,'(g0)',advance='no') achar(9)
    end do
  end subroutine w_spaces

  ! write beginning tag with zero attributes
  subroutine w_begin_tag_a0( luo_saeinp, tag_name )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name

    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(3a)") '<', trim(tag_name), '>'

    indent = indent + INDENT_SPACES
  end subroutine w_begin_tag_a0

  ! write beginning tag with one attribute
  subroutine w_begin_tag_a1( luo_saeinp, tag_name, attrib1, attr1_value )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: attrib1
    integer, intent(in) :: attr1_value

    character(len=MAX_NAME_LEN) :: attr1_str

    write(attr1_str, '(i0)') attr1_value
    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(7a)") '<', trim(tag_name), &
                        ' ', trim(attrib1), '="', trim(adjustl(attr1_str)), '">'

    indent = indent + INDENT_SPACES
  end subroutine w_begin_tag_a1

  ! write beginning tag with two attributes
  subroutine w_begin_tag_a2( luo_saeinp, tag_name, attrib1, attr1_value, attrib2, attr2_value )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: attrib1
    integer, intent(in) :: attr1_value
    character(len=*), intent(in) :: attrib2
    integer, intent(in) :: attr2_value

    character(len=MAX_NAME_LEN) :: attr1_str
    character(len=MAX_NAME_LEN) :: attr2_str

    write(attr1_str, '(i0)') attr1_value
    write(attr2_str, '(i0)') attr2_value
    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(12a)") '<', trim(tag_name), &
                        ' ', trim(attrib1), '="', trim(adjustl(attr1_str)), '"', &
                        ' ', trim(attrib2), '="', trim(adjustl(attr2_str)), '">'

    indent = indent + INDENT_SPACES
  end subroutine w_begin_tag_a2

  ! write ending tag
  subroutine w_end_tag( luo_saeinp, tag_name )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name

    indent = indent - INDENT_SPACES

    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(3a)") '</', trim(tag_name), '>'
  end subroutine w_end_tag

  ! write whole tag with zero attributes and real number value
  subroutine w_whole_tag_a0_real( luo_saeinp, tag_name, in_val )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    real, intent(in) :: in_val

    character(len=MAX_NAME_LEN) :: real_str

    write(real_str, '(g39.15)') in_val
    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(7a)") '<', trim(tag_name), '>', &
                         trim(adjustl(real_str)), &
                        '</', trim(tag_name), '>'
  end subroutine w_whole_tag_a0_real

  ! write whole tag with zero attributes and integer number value
  subroutine w_whole_tag_a0_integer( luo_saeinp, tag_name, in_val )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    integer, intent(in) :: in_val

    character(len=MAX_NAME_LEN) :: integer_str

    write(integer_str, '(i0)') in_val
    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(7a)") '<', trim(tag_name), '>', &
                         trim(adjustl(integer_str)), &
                        '</', trim(tag_name), '>'
  end subroutine w_whole_tag_a0_integer

  ! write whole tag with zero attributes and character string
  subroutine w_whole_tag_a0_char( luo_saeinp, tag_name, in_val )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: in_val

    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(7a)") '<', trim(tag_name), '>', &
                         trim(adjustl(in_val)), &
                        '</', trim(tag_name), '>'
  end subroutine w_whole_tag_a0_char

  ! write whole tag with one attribute
  subroutine w_whole_tag_a1( luo_saeinp, tag_name, attrib1, attr1_value, in_val )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: attrib1
    integer, intent(in) :: attr1_value
    real, intent(in) :: in_val

    character(len=MAX_NAME_LEN) :: attr1_str
    character(len=MAX_NAME_LEN) :: real_str

    write(attr1_str, '(i0)') attr1_value
    write(real_str, '(g39.15)') in_val

    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(11a)") '<', trim(tag_name), &
                        ' ', trim(attrib1), '="', trim(adjustl(attr1_str)), '">', &
                        trim(adjustl(real_str)), &
                        '</', trim(tag_name), '>'
  end subroutine w_whole_tag_a1

  ! write whole tag with two attributes and real value
  subroutine w_whole_tag_a2_real( luo_saeinp, tag_name, attrib1, attr1_value, attrib2, attr2_value, in_val )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: attrib1
    integer, intent(in) :: attr1_value
    character(len=*), intent(in) :: attrib2
    integer, intent(in) :: attr2_value
    real, intent(in) :: in_val

    character(len=MAX_NAME_LEN) :: attr1_str
    character(len=MAX_NAME_LEN) :: attr2_str
    character(len=MAX_NAME_LEN) :: real_str

    write(attr1_str, '(i0)') attr1_value
    write(attr2_str, '(i0)') attr2_value
    write(real_str, '(g39.15)') in_val

    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(16a)") '<', trim(tag_name), &
                        ' ', trim(attrib1), '="', trim(adjustl(attr1_str)), '"', &
                        ' ', trim(attrib2), '="', trim(adjustl(attr2_str)), '">', &
                        trim(adjustl(real_str)), &
                        '</', trim(tag_name), '>'
  end subroutine w_whole_tag_a2_real

  ! write whole tag with two attributes and integer value
  subroutine w_whole_tag_a2_integer( luo_saeinp, tag_name, attrib1, attr1_value, attrib2, attr2_value, in_val )
    integer, intent(in) :: luo_saeinp      ! output unit number
    character(len=*), intent(in) :: tag_name
    character(len=*), intent(in) :: attrib1
    integer, intent(in) :: attr1_value
    character(len=*), intent(in) :: attrib2
    integer, intent(in) :: attr2_value
    integer, intent(in) :: in_val

    character(len=MAX_NAME_LEN) :: attr1_str
    character(len=MAX_NAME_LEN) :: attr2_str
    character(len=MAX_NAME_LEN) :: integer_str

    write(attr1_str, '(i0)') attr1_value
    write(attr2_str, '(i0)') attr2_value
    write(integer_str, '(i0)') in_val

    call w_spaces( luo_saeinp )
    write(luo_saeinp,"(16a)") '<', trim(tag_name), &
                        ' ', trim(attrib1), '="', trim(adjustl(attr1_str)), '"', &
                        ' ', trim(attrib2), '="', trim(adjustl(attr2_str)), '">', &
                        trim(adjustl(integer_str)), &
                        '</', trim(tag_name), '>'
  end subroutine w_whole_tag_a2_integer

end module read_write_xml_mod



