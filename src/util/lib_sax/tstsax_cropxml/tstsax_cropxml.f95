!$Author$
!$Date$
!$Revision$
!$HeadURL$



program tstsax_cropxml
 
! Example driver for reading an XML formated WEPS/MCREW crop record

use flib_sax
use m_handlers_cropxml      ! Defines begin_element, end_element, pcdata_chunk

  integer :: iostat
  type(xml_t)  :: fxml

  character(len=512)  :: cropname

  write(*,*) 'Enter XML crop record filename: '
  read(*,*) cropname

  call open_xmlfile(trim(cropname),fxml,iostat)
!  call open_xmlfile("carrot.crop",fxml,iostat)
  if (iostat /= 0) stop "Cannot open file."

  call xml_parse(fxml, &
               begin_element_handler = begin_element_handler, &
               end_element_handler = end_element_handler, &
               pcdata_chunk_handler = pcdata_chunk_handler, &
               verbose = .false.)

end program tstsax_cropxml

