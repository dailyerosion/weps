!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_xml_mod

  use flib_sax
  use manage_data_struct_defs, only: manFile, operation_date, elemCreate

  integer, parameter :: MAX_NAME_LEN  = 40

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: man_tag
  integer :: max_tags

  integer, parameter, public :: rotationyears = 1
  integer, parameter, public :: wepsmanvalue = 2
  integer, parameter, public :: date = 3
  integer, parameter, public :: operationDB = 4
  integer, parameter, public :: operationname = 5
  integer, parameter, public :: actionvalue = 6
  integer, parameter, public :: identity = 7
  integer, parameter, public :: code = 8
  integer, parameter, public :: id = 9
  integer, parameter, public :: param = 10
  integer, parameter, public :: p_name = 11
  integer, parameter, public :: value = 12
  integer, parameter, public :: version = 13
  integer, parameter, public :: wepsmanDB = 14

  integer :: int_cnt     ! count of integer values to be read into an operation, group or process, for allocation
  integer :: real_cnt    ! count of real values to be read into an operation, group or process, for allocation

  integer :: isub ! current subregion number used in a routines in this module
  type(operation_date) :: t_operDate
  character(len=80) :: t_operName
  character(len=3) :: t_code
  integer :: t_id

  logical :: all_wepsmanvalues
  logical :: all_operationDBs
  logical :: all_actionvalues
  logical :: all_params
  logical :: manfile_complete ! indicator that a complete manfile was read

contains

  subroutine init_man_xml( isubr )
    integer, intent(in) :: isubr

    integer :: idx
    integer :: alloc_stat

    ! set subregion index used with manFile
    isub = isubr

    max_tags = 14   ! count of unique tags needed from management files
    allocate( man_tag(max_tags), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., input_tag'
    end if

    ! assign defaults to flag status values
    do idx = 1, max_tags
      man_tag(idx)%acquired = .false.
      man_tag(idx)%in_tag = .false.
    end do

    ! assign tag names
    man_tag(1)%name = "rotationyears"
    man_tag(2)%name = "wepsmanvalue"
    man_tag(3)%name = "date"
    man_tag(4)%name = "operationDB"
    man_tag(5)%name = "operationname"
    man_tag(6)%name = "actionvalue"
    man_tag(7)%name = "identity"
    man_tag(8)%name = "code"
    man_tag(9)%name = "id"
    man_tag(10)%name = "param"
    man_tag(11)%name = "name"
    man_tag(12)%name = "value"
    man_tag(13)%name = "version"
    man_tag(14)%name = "wepsmanDB"

    all_wepsmanvalues = .true.  ! .true. indicates that no values are required
    all_operationDBs = .true.  ! .false. indicates that a value is required
    all_actionvalues = .true.
    all_params = .true.

  end subroutine init_man_xml

!  subroutine read_manage_xml()

!    operType = 0
!    operFirst => elemCreate( operFirst, operType, real_cnt )
!    oper => operFirst

!    grpType = 0
!    oper%grpFirst => elemCreate( oper%grpFirst, grpType, real_cnt )
!    grp => oper%grpFirst
!    procType = 0
!    grp%procFirst => elemCreate( grp%procFirst, procType, int_cnt, real_cnt )
!    proc => grp%procFirst
!    do procType = 1, 5
!      proc%procNext => elemCreate( proc%procNext, procType, int_cnt, real_cnt )
!      proc => proc%procNext
!    end do
!    nullify( proc%procNext )
!    do grpType = 1, 5
!      grp => elemCreate( grp%grpNext, grpType )
!      procType = 0
!      grp%procFirst => elemCreate( grp%procFirst, procType )
!      proc => grp%procFirst
!      do procType = 1, 5
!        proc%procNext => elemCreate( proc%procNext, procType )
!        proc => proc%procNext
!      end do
!      nullify( proc%procNext )
!    end do
!    nullify( grp%grpNext )

!    do operType = 1, 5
!      oper => elemCreate( oper%operNext, operType )
!      grpType = 0
!      oper%grpFirst => elemCreate( oper%grpFirst, grpType )
!      grp => oper%grpFirst
!      procType = 0
!      grp%procFirst => elemCreate( grp%procFirst, procType )
!      proc => grp%procFirst
!      do procType = 1, 5
!        proc%procNext => elemCreate( proc%procNext, procType )
!        proc => proc%procNext
!      end do
!      nullify( proc%procNext )
!      do grpType = 1, 5
!        grp => elemCreate( grp%grpNext, grpType )
!        procType = 0
!        grp%procFirst => elemCreate( grp%procFirst, procType )
!        proc => grp%procFirst
!        do procType = 1, 5
!          proc%procNext => elemCreate( proc%procNext, procType )
!          proc => proc%procNext
!        end do
!        nullify( proc%procNext )
!      end do
!      nullify( grp%grpNext )
!    end do

!    oper%operNext => operFirst

!    oper => operFirst
!    operType = 0
!    do while( operType .lt. 12 )
!      write(*,'(a,i0)') 'OPER: ', oper%operType
!      grp => oper%grpFirst
!      do while( associated(grp) )
!        write(*,'(a,i0)') '  GRP: ', grp%grpType
!        proc => grp%procFirst
!        do while( associated(proc) )
!          write(*,'(a,i0)') '    PROC: ', proc%procType
!          proc => proc%procNext
!        end do
!        grp => grp%grpNext
!      end do
!      oper => oper%operNext
!      operType = operType + 1
!    end do

!  end subroutine read_manage_xml

  subroutine begin_man_element_handler(name,attributes)
    character(len=*), intent(in) :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx

    do idx = 1, size(man_tag)
      if( man_tag(idx)%name .eq. name ) then
        man_tag(idx)%in_tag = .true.
        !write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

  end subroutine begin_man_element_handler

  subroutine end_man_element_handler(name)
    character(len=*), intent(in) :: name

    integer :: idx

    do idx = 1, size(man_tag)
      if( man_tag(idx)%name .eq. name ) then
        man_tag(idx)%in_tag = .false.
        !write(*,*) 'Out tag ', trim(name)

        if ( man_tag(idx)%acquired ) then
          write(*,*) 'ACQUIRED: ', man_tag(idx)%name!, man_tag(idx)%acquired
        end if

        exit  ! found tag, no need to look further
      end if
    end do

    if (idx .eq. wepsmanDB) then

      if ( man_tag(rotationyears)%acquired &
        .and. man_tag(version)%acquired &
        .and. all_wepsmanvalues ) then
        manfile_complete = .true.
      else
        manfile_complete = .false.
      end if

    else if (idx .eq. wepsmanvalue) then
      if( man_tag(date)%acquired &
        .and. all_operationDBs ) then
        man_tag(date)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_wepsmanvalues = (all_wepsmanvalues .and. .true. )
      else
        all_wepsmanvalues = .false.
      end if

    else if (idx .eq. date) then

    else if (idx .eq. operationDB) then
      if( man_tag(operationname)%acquired &
        .and. all_actionvalues ) then
        man_tag(operationname)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_operationDBs = (all_operationDBs .and. .true. )
      else
        all_operationDBs = .false.
      end if

    else if (idx .eq. actionvalue) then
      if( man_tag(identity)%acquired &
        .and. all_params &
        ) then
        man_tag(identity)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_actionvalues = (all_actionvalues .and. .true. )
      else
        all_actionvalues = .false.
      end if

    else if (idx .eq. identity) then
      if( man_tag(code)%acquired &
        .and. man_tag(id)%acquired &
        ) then
        man_tag(code)%acquired = .false.
        man_tag(id)%acquired = .false.
        man_tag(identity)%acquired = .true.
      end if

    else if (idx .eq. param) then
      if( man_tag(p_name)%acquired &
        .and. man_tag(value)%acquired &
        ) then
        man_tag(p_name)%acquired = .false.
        man_tag(value)%acquired = .false.
        ! stays .true. if all previous values have been true
        all_params = (all_params .and. .true. )
      else
        all_params = .false.
      end if

    end if

  end subroutine end_man_element_handler

  subroutine pcdata_man_chunk_handler(chunk)
    use read_write_xml_mod, only: read_param
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value

    character(len=3) :: operID
    character(len=3) :: grpID
    character(len=3) :: procID

    param_value = trim(chunk)

    !write(*,*) 'CHUNK: ', trim(chunk)

    if (man_tag(wepsmanDB)%in_tag) then
      if (man_tag(version)%in_tag) then
        call read_param(man_tag(version)%name, param_value, manFile(isub)%mversion)
        man_tag(version)%acquired = .true.
      else if (man_tag(rotationyears)%in_tag) then
        call read_param(man_tag(rotationyears)%name, param_value, manFile(isub)%mperod)
        man_tag(rotationyears)%acquired = .true.
      else if (man_tag(wepsmanvalue)%in_tag) then
        if (man_tag(date)%in_tag) then
          call read_param(man_tag(date)%name, param_value, t_operDate)
          man_tag(date)%acquired = .true.
        else if (man_tag(operationDB)%in_tag ) then
          if (man_tag(operationname)%in_tag ) then
            t_operName = trim(param_value)
            man_tag(operationname)%acquired = .true.
          else if (man_tag(actionvalue)%in_tag ) then
            if (man_tag(identity)%in_tag ) then
              if (man_tag(code)%in_tag ) then
                t_code = trim(param_value)
                if (   t_code .eq. 'O' &
                  .or. t_code .eq. 'G' &
                  .or. t_code .eq. 'P' &
                  ) then
                  man_tag(code)%acquired = .true.
                  operID = ''
                  grpID = ''
                  procID = ''
                else
                  write(*,*) 'Unknown Identity code: "', trim(t_code), '" found in ', trim(manFile(isub)%tinfil)
                end if
              else if (man_tag(id)%in_tag ) then
                call read_param(man_tag(id)%name, param_value, t_id)
                man_tag(id)%acquired = .true.
                select case (t_code)
                case ('O')
                  operID = trim(param_value)
                  select case (operID)
                  case ('01')
                    real_cnt = 5
                  case ('03')
                    real_cnt = 7
                  case ('04')
                    real_cnt = 2
                  case default
                    real_cnt = 0
                  end select
                  if ( .not. associated(manFile(isub)%operFirst) ) then
                    manFile(isub)%operFirst => elemCreate( manFile(isub)%operFirst, operID, real_cnt )
                  else
                  end if
                case ('G')
                  grpID = trim(param_value)
                case ('P')
                  procID = trim(param_value)
                end select
              end if
            else if (man_tag(param)%in_tag ) then
              if (man_tag(p_name)%in_tag ) then
                
              else if (man_tag(value)%in_tag ) then

              end if
            end if
          end if
        end if


        select case (grpID)
        case ('01')
          real_cnt = 6
        case ('02')
          real_cnt = 1
        case default
          real_cnt = 0
        end select

        select case (procID)
        case ('02')
          int_cnt = 1
          real_cnt = 1
        case ('05')
          int_cnt = 1
          real_cnt = 5
        case ('11')
          int_cnt = 0
          real_cnt = 2
        case ('12')
          int_cnt = 0
          real_cnt = 1
        case ('13')
          int_cnt = 0
          real_cnt = 1
        case ('24')
          int_cnt = 1
          real_cnt = 5
        case ('25')
          int_cnt = 1
          real_cnt = 5
        case ('26')
          int_cnt = 0
          real_cnt = 5
        case ('30')
          int_cnt = 1
          real_cnt = 0
        case ('31')
          int_cnt = 1
          real_cnt = 0
        case ('32')
          int_cnt = 1
          real_cnt = 4
        case ('33')
          int_cnt = 0
          real_cnt = 4
        case ('34')
          int_cnt = 1
          real_cnt = 10
        case ('37')
          int_cnt = 0
          real_cnt = 4
        case ('38')
          int_cnt = 0
          real_cnt = 4
        case ('42')
          int_cnt = 5
          real_cnt = 4
        case ('43')
          int_cnt = 4
          real_cnt = 4
        case ('47')
          int_cnt = 4
          real_cnt = 4
        case ('48')
          int_cnt = 4
          real_cnt = 4
        case ('50')
          int_cnt = 1
          real_cnt = 18
        case ('51')
          int_cnt = 9
          real_cnt = 61
        case ('61')
          int_cnt = 2
          real_cnt = 5
        case ('62')
          int_cnt = 7
          real_cnt = 5
        case ('65')
          int_cnt = 1
          real_cnt = 18
        case ('66')
          int_cnt = 1
          real_cnt = 20
        case ('71')
          int_cnt = 1
          real_cnt = 1
        case ('72')
          int_cnt = 1
          real_cnt = 7
        case ('73')
          int_cnt = 0
          real_cnt = 4
        case ('91')
          int_cnt = 0
          real_cnt = 5
        case default
          int_cnt = 0
          real_cnt = 0
        end select

      end if
    end if

  end subroutine pcdata_man_chunk_handler

end module manage_xml_mod

