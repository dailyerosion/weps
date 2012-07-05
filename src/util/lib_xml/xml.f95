module xml

    use FoX_DOM
    
    implicit none

    ! default scope is private, requires explicit public statements to open up to the world.
    private

    ! expose the methods of this module
    public :: xml_xpath, xml_get
    
    ! expose FoX methods and types required for the typical use case
    public :: Node, parseFile, destroy, getDocumentElement
    
    interface xml_get
        module procedure get_integer
        module procedure get_real
        module procedure get_double
        module procedure get_logical
        module procedure get_characters
        module procedure get_elements
    end interface xml_get
            

contains   

    !
    ! XML XPATH
    ! 
    ! Resolve basic xpath expressions into the node object in the dom.
    !
    recursive function xml_xpath(n, path) result(p)
        ! parameters
        type(Node), pointer, intent(in) :: n
        type(Node), pointer :: p
        character(len=*), intent (in) :: path
        ! variables
        integer :: i, length
        character :: c
        type(NodeList), pointer :: children => null()
                
!        print *, "xpath: (", getNodeName(n), ") ", path        
            
        
        ! default to assuming relative to this
        p => n
        
        length = len(path)
        
        if( length <= 0) then
            return
        end if
                        
        do i=1, length
            c=path(i:i)
            select case (c)
                case (".")
                    ! current context
                    if(length > 1) then
                        p => xml_xpath(n, path(i+1:length))
                    end if                                                                
                    return
                           
                case ("/")     
                    if(len(path(1:i-1)) == 0) then
                        ! find the root                        
                        p=> xml_xpath(getOwnerDocument(n), path(i+1:length))
                        return
                    else
                        ! find the named element                                                
                        p => xml_xpath(xml_xpath(n, path(1:i-1)), path(i+1:length))
                        return
                    end if    
                    
                case ("@")
                    ! attribute                                                            
                    p => getAttributeNode(xml_xpath(n, path(1:i-1)), path(i+1:length))                    
                    return  
            end select
        end do
        
        ! default is to use the full path as a child element name
        if(length > 0) then            
            p => xml_element(n, path)                       
        end if
                
    end function xml_xpath
    
     
    function xml_element(n, name) result(p)
        ! parameters
        type(Node), pointer, intent(in) :: n
        type(Node), pointer :: p
        character(len=*), intent (in) :: name
        ! variables        
        type(Node), pointer :: this, child
        type(NodeList), pointer :: children
        integer :: i
                
        ! default to nothing
        this => n
                        
!        print *, "element: (", getNodeName(this), ") ", name
        
        p => null()
        
        ! special handling for working from the root document
        if(getNodeType(this) == DOCUMENT_NODE) then            
            this => getDocumentElement(this)
            if(getNodeName(this) == name) then
                p=>this
                return
            else
                p=> null()
                return
            end if
        end if
        
        ! normal handling, iterate over the list of child nodes looking for a matching name
        children => getChildNodes(this)
        if(.not.associated(children)) then
            ! node has no children
            p=> null()
            return                
        end if

        ! the fox xml impl is zero based index
        do i=0, getLength(children) - 1
            child => item(children, i)
            if(getNodeType(child) == ELEMENT_NODE .and. getNodeName(child) == name) then                
                p=> child
                return
            end if            
        end do
                
    end function
    
    ! return a node list with all the direct child nodes with a matching name, supports single * as wildcard
    function xml_elements(n, name) result(list)
        ! parameters
        type(Node), pointer, intent(in) :: n
        type(NodeList), pointer :: list        
        character(len=*), intent (in) :: name
        ! variables        
        type (Node), pointer :: this, child
        type(NodeList), pointer :: children 
        integer :: i
                        
        this => n
        
!        print *, "elements: (", getNodeName(this),") ", name
                
        allocate(list)
        
        ! special handling for working from the root document
        if(getNodeType(this) == DOCUMENT_NODE) then            
            this => getDocumentElement(this)
            if(getNodeName(this) == name .or. name == "*") then
                call append(list, this)
                return
            else
                ! return empty list
                return
            end if
        end if
        
        ! normal handling, iterate over the list of child nodes looking for a matching name
        children => getChildNodes(this)        
        if(.not.associated(children)) then
            ! node has no children, return with empty list            
            return                
        end if
                
        ! the fox xml impl is zero based index
        do i=0, getLength(children) - 1
            child => item(children, i)            
            ! match the name or wildcard
            if(getNodeType(child) == ELEMENT_NODE .and. (getNodeName(child) == name .or. name == "*")) then                
                call append(list, child)
            end if            
        end do
        
        
    end function xml_elements



    subroutine get_integer(n, path, value)
        ! parameters
        type(Node), pointer :: n
        character(len=*) :: path
        integer, intent(out) :: value
        ! variables
        type(Node), pointer :: p
        
        ! resolve the path to the correct node
        p => xml_xpath(n, path)
        
        ! check that the node was resolved
        if(.not.associated(p)) then
            print *, "Unable to resolve xpath: ", path
            call exit (1)
        end if
        
        ! extract the data from the node into the value
        call extractDataContent(p, value)
        
    end subroutine get_integer
    
    subroutine get_real(n, path, value)
        ! parameters
        type(Node), pointer :: n
        character(len=*) :: path
        real, intent(out) :: value
        ! variables
        type(Node), pointer :: p
        
        ! resolve the path to the correct node
        p => xml_xpath(n, path)
        
        ! check that the node was resolved
        if(.not.associated(p)) then
            print *, "Unable to resolve xpath: ", path
            call exit (1)
        end if
        
        ! extract the data from the node into the value
        call extractDataContent(p, value)
        
    end subroutine get_real
    
    subroutine get_double(n, path, value)
        ! parameters
        type(Node), pointer :: n
        character(len=*) :: path
        double precision, intent(out) :: value
        ! variables
        type(Node), pointer :: p
        
        ! resolve the path to the correct node
        p => xml_xpath(n, path)
        
        ! check that the node was resolved
        if(.not.associated(p)) then
            print *, "Unable to resolve xpath: ", path
            call exit (1)
        end if
        
        ! extract the data from the node into the value
        call extractDataContent(p, value)
        
    end subroutine get_double
    
    subroutine get_logical(n, path, value)
        ! parameters
        type(Node), pointer :: n
        character(len=*) :: path
        logical, intent(out) :: value
        ! variables
        type(Node), pointer :: p
        
        ! resolve the path to the correct node
        p => xml_xpath(n, path)
        
        ! check that the node was resolved
        if(.not.associated(p)) then
            print *, "Unable to resolve xpath: ", path
            call exit (1)
        end if
        
        ! extract the data from the node into the value
        call extractDataContent(p, value)
        
    end subroutine get_logical
    
    subroutine get_characters(n, path, value)
        ! parameters
        type(Node), pointer :: n
        character(len=*) :: path
        character(len=*), intent(out) :: value
        ! variables
        type(Node), pointer :: p
        
        ! resolve the path to the correct node
        p => xml_xpath(n, path)
        
        ! check that the node was resolved
        if(.not.associated(p)) then
            print *, "Unable to resolve xpath: ", path
            call exit (1)
        end if
        
        ! extract the data from the node into the value
        call extractDataContent(p, value)
        
    end subroutine get_characters
    
    subroutine get_elements(n, path, element_callback)
        ! parameters
        type(Node), pointer, intent(in) :: n
        character(len=*) :: path
        
        ! variables       
        type (Node), pointer :: parent
        type (NodeList), pointer :: elements
        integer :: i, count
        
        ! element callback
        interface
            subroutine element_callback(parent, element, index, count)
                use FoX_DOM
                type(Node), pointer :: parent, element                
                integer :: index, count
            end subroutine element_callback
        end interface
        
        ! variables
        type(Node), pointer :: p
                
        parent => n
                
        
        ! find the last / character
        i = index(path, "/", .true.)
!        print *, "split: ", path,  i                
                
        if(i ==  1) then            
            ! root, keep the slash
            parent => xml_xpath(parent, path(1:i))            
        else if (i > 1) then
            ! remove the trailing slash
            parent => xml_xpath(parent, path(1:i-1))        
        end if
        
!        print *, "split: (", getNodeName(parent), ") ", path(i+1:)
        
        elements => xml_elements(parent, path(i+1:))
        
        count = getLength(elements)
        
        ! iterate over the elements and call the callback: 1-based index
        do i=1, count
            call element_callback(parent, item(elements, i-1), i, count)
        end do
        
        ! clean up the temporary nodes list
        call destroy(elements)
        
     end subroutine get_elements
        
     

end module