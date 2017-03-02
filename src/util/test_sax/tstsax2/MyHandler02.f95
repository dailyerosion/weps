!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
! Other Handlers
!
module myhandler02

  use flib_sax

  integer, save :: indentation

contains


  ! START DOC
  subroutine start_document()
    write(*,*) "===> Document has Begun <==="    
    indentation = 0
  end subroutine start_document


  ! START ELEMENT
  subroutine start_element(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes
    call indent(indentation)
    write(*,*) "OPEN: ", name
    indentation = indentation + 2;
  end subroutine start_element


  ! END ELEMENT
  subroutine end_element(name)
    character(len=*), intent(in)   :: name
    indentation = indentation - 2;
    call indent(indentation);
    write(*,*) "SHUT: ", name;
  end subroutine end_element


  ! END DOC
  subroutine end_document()
    write(*,*) "===> Document has Ended <==="    
  end subroutine end_document

  
  ! non SAX Method for indenting the output
  subroutine indent(ind)
    integer :: ind, i
    do i=1,ind
       write(*,'(a)', advance="no") " "
    enddo
  end subroutine indent

end module myhandler02
