!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
!
! Printing text (and keeping track of context)
!
module myhandler04

  use flib_sax

  logical, save :: foundProperty
  logical, save :: foundScalar
  data foundProperty, foundScalar/.false.,.false./
  
contains

  subroutine start_element(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: status
    character(len=20) :: value

    foundScalar = .false.
   
    if (foundProperty .and. name .eq. "scalar") then
       foundScalar = .true.
       foundProperty = .false.
    else
       call get_value(attributes,"dictRef",value,status)
       if (name .eq. "property" .and. trim(value) .eq. "gulp:Etot") then
          foundProperty = .true.
       endif
    endif
    
  end subroutine start_element


  subroutine characters(pcdata)
    character(len=*), intent(in) :: pcdata
    if (foundScalar) then
       if (len_trim(pcdata) .ne. 0) then
          write(*,*)
          write(*,*) "     Total Energy = ", trim(pcdata)
       endif
    endif
  end subroutine characters
  
end module myhandler04
