!$Author$
!$Date$
!$Revision$
!$HeadURL$

module manage_mod

  private

  type operation_date
    integer :: day
    integer :: month
    integer :: year
  end type operation_date

  type process
    integer :: procType
    type(process), pointer :: procNext
    integer, dimension(:), allocatable :: i_params
    real, dimension(:), allocatable :: r_params
    character(len=80) :: s_param
  end type process

  type group
    integer :: grpType
    type(group), pointer :: grpNext
    type(process), pointer :: procFirst
    real, dimension(:), allocatable :: r_params
    character(len=80) :: s_param
  end type group

  type operation
    type(operation_date) :: operDate
    integer :: operType
    type(operation), pointer :: operNext
    type(group), pointer :: grpFirst
    real, dimension(:), allocatable :: r_params
    character(len=80) :: s_param
  end type operation

  type(operation), pointer :: operFirst, oper
  type(group), pointer :: grp
  type(process), pointer :: proc

  interface elemCreate
    module procedure operCreate
    module procedure grpCreate
    module procedure procCreate
  end interface

  public :: read_manage_xml
  public :: elemCreate

  contains

      function operCreate(operPntr, operType) result(operNew)
        type(operation), pointer, intent(inout) :: operPntr
        integer, intent(in) :: operType
        type(operation), pointer :: operNew

        integer :: alloc_stat
        integer :: real_cnt

        allocate(operPntr, stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,'(a,i0)') 'Unable to allocate Operation pointer: P ', operType
        end if
        operPntr%operType = operType
        alloc_stat = 0
        select case (operType)
        case (1)
          real_cnt = 5
        case (3)
          real_cnt = 7
        case (4)
          real_cnt = 2
        case default
          real_cnt = 0
        end select
        allocate(operPntr%r_params(real_cnt), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,'(a,i0)') 'Unable to allocate Operation params: P ', operType
        end if
        operNew =>operPntr
        
      end function operCreate

      function grpCreate(grpPntr, grpType) result(grpNew)
        type(group), pointer, intent(inout) :: grpPntr
        integer, intent(in) :: grpType
        type(group), pointer :: grpNew

        integer :: alloc_stat
        integer :: real_cnt

        allocate(grpPntr, stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,'(a,i0)') 'Unable to allocate Group pointer: G ', grpType
        end if
        grpPntr%grpType = grpType
        alloc_stat = 0
        select case (grpType)
        case (1)
          real_cnt = 6
        case (2)
          real_cnt = 1
        case default
          real_cnt = 0
        end select
        allocate(grpPntr%r_params(real_cnt), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,'(a,i0)') 'Unable to allocate Group params: G ', grpType
        end if
        grpNew =>grpPntr
        
      end function grpCreate

      function procCreate(procPntr, procType) result(procNew)
        type(process), pointer, intent(inout) :: procPntr
        integer, intent(in) :: procType
        type(process), pointer :: procNew

        integer :: alloc_stat
        integer :: sum_stat
        integer :: int_cnt
        real :: real_cnt

        allocate(procPntr, stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,'(a,i0)') 'Unable to allocate Process pointer: P ', procType
        end if
        procPntr%procType = procType
        select case (procType)
        case (2)
          int_cnt = 1
          real_cnt = 1
        case (5)
          int_cnt = 1
          real_cnt = 5
        case (11)
          int_cnt = 0
          real_cnt = 2
        case (12)
          int_cnt = 0
          real_cnt = 1
        case (13)
          int_cnt = 0
          real_cnt = 1
        case (24)
          int_cnt = 1
          real_cnt = 5
        case (25)
          int_cnt = 1
          real_cnt = 5
        case (26)
          int_cnt = 0
          real_cnt = 5
        case (30)
          int_cnt = 1
          real_cnt = 0
        case (31)
          int_cnt = 1
          real_cnt = 0
        case (32)
          int_cnt = 1
          real_cnt = 4
        case (33)
          int_cnt = 0
          real_cnt = 4
        case (34)
          int_cnt = 1
          real_cnt = 10
        case (37)
          int_cnt = 0
          real_cnt = 4
        case (38)
          int_cnt = 0
          real_cnt = 4
        case (42)
          int_cnt = 5
          real_cnt = 4
        case (43)
          int_cnt = 4
          real_cnt = 4
        case (47)
          int_cnt = 4
          real_cnt = 4
        case (48)
          int_cnt = 4
          real_cnt = 4
        case (50)
          int_cnt = 1
          real_cnt = 18
        case (51)
          int_cnt = 9
          real_cnt = 61
        case (61)
          int_cnt = 2
          real_cnt = 5
        case (62)
          int_cnt = 7
          real_cnt = 5
        case (65)
          int_cnt = 1
          real_cnt = 18
        case (66)
          int_cnt = 1
          real_cnt = 20
        case (71)
          int_cnt = 1
          real_cnt = 1
        case (72)
          int_cnt = 1
          real_cnt = 7
        case (73)
          int_cnt = 0
          real_cnt = 4
        case (91)
          int_cnt = 0
          real_cnt = 5
        case default
          int_cnt = 0
          real_cnt = 0
        end select
        sum_stat = 0
        allocate(procPntr%i_params(int_cnt), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(procPntr%r_params(real_cnt), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        if( sum_stat .gt. 0 ) then
          write(*,'(a,i0)') 'Unable to allocate Process params: P ', procType
        end if
        procNew =>procPntr
        
      end function procCreate

      subroutine read_manage_xml()

        integer :: idx, jdx, kdx

        idx = 0
        operFirst => elemCreate( operFirst, idx )
        oper => operFirst

        jdx = 0
        oper%grpFirst => elemCreate( oper%grpFirst, jdx )
        grp => oper%grpFirst
        kdx = 0
        grp%procFirst => elemCreate( grp%procFirst, kdx )
        proc => grp%procFirst
        do kdx = 1, 5
          proc%procNext => elemCreate( proc%procNext, kdx )
          proc => proc%procNext
        end do
        nullify( proc%procNext )
        do jdx = 1, 5
          grp => elemCreate( grp%grpNext, jdx )
          kdx = 0
          grp%procFirst => elemCreate( grp%procFirst, kdx )
          proc => grp%procFirst
          do kdx = 1, 5
            proc%procNext => elemCreate( proc%procNext, kdx )
            proc => proc%procNext
          end do
          nullify( proc%procNext )
        end do
        nullify( grp%grpNext )

        do idx = 1, 5
          oper => elemCreate( oper%operNext, idx )
          jdx = 0
          oper%grpFirst => elemCreate( oper%grpFirst, jdx )
          grp => oper%grpFirst
          kdx = 0
          grp%procFirst => elemCreate( grp%procFirst, kdx )
          proc => grp%procFirst
          do kdx = 1, 5
            proc%procNext => elemCreate( proc%procNext, kdx )
            proc => proc%procNext
          end do
          nullify( proc%procNext )
          do jdx = 1, 5
            grp => elemCreate( grp%grpNext, jdx )
            kdx = 0
            grp%procFirst => elemCreate( grp%procFirst, kdx )
            proc => grp%procFirst
            do kdx = 1, 5
              proc%procNext => elemCreate( proc%procNext, kdx )
              proc => proc%procNext
            end do
            nullify( proc%procNext )
          end do
          nullify( grp%grpNext )
        end do

        oper%operNext => operFirst

        oper => operFirst
        idx = 0
        do while( idx .lt. 12 )
          write(*,'(a,i0)') 'OPER: ', oper%operType
          grp => oper%grpFirst
          do while( associated(grp) )
            write(*,'(a,i0)') '  GRP: ', grp%grpType
            proc => grp%procFirst
            do while( associated(proc) )
              write(*,'(a,i0)') '    PROC: ', proc%procType
              proc => proc%procNext
            end do
            grp => grp%grpNext
          end do
          oper => oper%operNext
          idx = idx + 1
        end do

      end subroutine read_manage_xml

end module manage_mod

