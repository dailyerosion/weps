!simple program just reads the first argument as xml




program xmltest
        
    use xml
          
    type(Node), pointer :: doc, root
        
    ! file name read from arguments
    character *256 :: file
    ! io error status flag
    integer :: ioe
    
    ! version attribute value
    real :: value = 0
    
    
    ! get the file name to open from the command arguments
    call getarg(1, file)

    ! parse the file into the document tree
    doc => parseFile(file, iostat=ioe)
    
    ! check if the xml file was opened
    if (ioe /= 0) then
        print *, "Could not open XML file", file
        call exit (1)
    end if

    ! get the root element of the document
    root => getDocumentElement(doc)
        
          
    call xml_get(root, "/*", generic_element_callback)
    call xml_get(root, "*", generic_element_callback)    
    call xml_get(root, "/mock/parameters/parameter", generic_element_callback)
    
!    n => xml_xpath(root, "sub/thing")
    
    print *, "value: ", value

    !cleanup memory used to parse the xml
    call destroy(doc)
        
    
    !exit with success
    stop 0
    
contains
    subroutine generic_element_callback (parent, element, index, count)     
        use FoX_DOM
        type(Node), pointer :: parent, element        
        integer :: index, count

        print *, index, count, "(", trim(getNodeName(parent)),") ", trim(getNodeName(element))

    end subroutine generic_element_callback

end program



   



