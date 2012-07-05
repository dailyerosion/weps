!
!$Author$
!$Date$
!$Revision$
!$HeadURL$
!
program example04

  use flib_sax
  use MyHandler04

  type(xml_t) :: fxml
  integer     :: iostat


  call open_xmlfile("gulp.xml",fxml,iostat)
  if (iostat /= 0) stop "Cannot open file."

  call xml_parse(fxml, &
                 begin_element_handler=start_element, &
                 pcdata_chunk_handler=characters &
                 )
                 
end program example04
