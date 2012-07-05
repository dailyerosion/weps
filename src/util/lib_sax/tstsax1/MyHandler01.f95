!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
module myhandler01

  use flib_sax

contains

  subroutine start_element(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    write(*,*) "Start Tag: ", name

  end subroutine start_element

end module myhandler01
