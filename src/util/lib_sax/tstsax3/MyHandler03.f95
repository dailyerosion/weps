!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
! The Attribute Interface
!
module myhandler03

  use flib_sax

contains

  subroutine start_element(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes
    
    character(len=100)  :: value
    character(len=100)  :: aname
    integer :: n, i, status
    
    n = len(attributes)
    
    write(*,*) "Start Tag: ", name
    do i=1,n
       call get_value(attributes,i,value,status)
       call get_key(attributes,i,aname,status)
       write(*,*) "  ", trim(aname), " ==> ", trim(value)
    enddo
    
  end subroutine start_element

end module myhandler03
