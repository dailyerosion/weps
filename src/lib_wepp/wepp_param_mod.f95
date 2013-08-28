!$Author: $
!$Date: $
!$Revision: $
!$HeadURL: $

module wepp_param_mod
  implicit none

  type wepp_param
     real :: totalRunoff
     real :: totalPrecip
     real :: totalSnowrunoff
     integer :: runoffEvents
     integer :: precipEvents
     integer :: snowmeltEvents
     real :: rkecum
     real :: prev_crust_frac
     real, dimension(:), pointer :: saxwp
     real, dimension(:), pointer :: saxfc
     real, dimension(:), pointer :: saxenp
     real, dimension(:), pointer :: saxpor
     real, dimension(:), pointer :: saxA
     real, dimension(:), pointer :: saxB
     real, dimension(:), pointer :: saxks
  end type wepp_param

contains

  function create_wepp_param(nsoillay) result(wp)
     integer, intent(in) :: nsoillay
     type(wepp_param) :: wp

     ! local variable
     integer :: alloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     allocate( wp%saxwp(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     allocate( wp%saxfc(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     allocate( wp%saxenp(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     allocate( wp%saxpor(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     allocate( wp%saxA(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     allocate( wp%saxB(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     allocate( wp%saxks(nsoillay), stat=alloc_stat )
     sum_stat = sum_stat + alloc_stat
     if( sum_stat .gt. 0 ) then
        Write(*,*) 'ERROR: unable to allocate wepp_param'
     end if
  end function create_wepp_param

  subroutine destroy_wepp_param(wp)
     type(wepp_param), intent(inout) :: wp

     !local variable
     integer :: dealloc_stat
     integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed

     sum_stat = 0
     deallocate( wp%saxwp, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     deallocate( wp%saxfc, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     deallocate( wp%saxenp, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     deallocate( wp%saxpor, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     deallocate( wp%saxA, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     deallocate( wp%saxB, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     deallocate( wp%saxks, stat=dealloc_stat )
     sum_stat = sum_stat + dealloc_stat
     if( sum_stat .gt. 0 ) then
        Write(*,*) 'ERROR: unable to deallocate wepp_param'
     end if
  end subroutine destroy_wepp_param

end module wepp_param_mod
