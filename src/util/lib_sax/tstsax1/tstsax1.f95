!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
program example01

  use flib_sax
  use MyHandler01

  type(xml_t) :: fxml
  integer     :: iostat


  call open_xmlfile("test.xml",fxml,iostat)
  if (iostat /= 0) stop "Cannot open file."

  call xml_parse(fxml, &
                 start_element &
                 )

end program example01


