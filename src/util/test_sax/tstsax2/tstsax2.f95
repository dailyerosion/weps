!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
program example02

  use flib_sax
  use MyHandler02

  type(xml_t) :: fxml
  integer     :: iostat


  call open_xmlfile("test.xml",fxml,iostat)
  if (iostat /= 0) stop "Cannot open file."

  call xml_parse(fxml,           &
                 begin_element_handler=start_element, &
                 end_element_handler=end_element,     &
                 start_document=start_document,       &
                 end_document=end_document            &
                 )

end program example02


