
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Barriers
!!

MODULE Barriers

  IMPLICIT NONE

!! Methods defined in this module

    PUBLIC :: Load_Barrier_File

!! Methods to be defined in this module

!    PUBLIC :: Dummy

!! Data structures defined in this module

    TYPE, PUBLIC :: Barrier
    integer :: id
	integer :: x1					!! left boundary of barrier
	integer :: y1					!! top boundary of barrier
	integer :: x2					!! right boundary of barrier
	integer :: y2					!! bottom boundary of barrier
	real :: height
	real :: porosity
	real :: width
	type (Barrier), pointer :: nxtBarrier		!! pointer to next cell in order of creation
    END TYPE

  CONTAINS
	
    FUNCTION Load_Barrier_File (filNam) result (Barrier_Ptr)
    
	character (len=*), intent(in) :: filNam
	type (Barrier), pointer :: Barrier_Ptr
	integer :: eofFlg
	integer :: cnt
	integer :: x1, y1, x2, y2

	type (Barrier), pointer :: basp
	type (Barrier), pointer :: curp
	type (Barrier), pointer :: prvp

        open(11, file=filNam, status="old", action="read")
	read(11, fmt=*) ! skip header line

	allocate(basp)
	prvp => basp
	cnt = 1
	do
	    allocate(curp)
	    curp%id = cnt
	    cnt = cnt + 1
	    prvp%nxtBarrier => curp
	    read(unit=11, fmt=*, iostat=eofFlg) curp%x1, curp%y1, curp%x2, curp%y2, &
	        curp%height, curp%porosity, curp%width
	    if (eofFlg<0) then
	        exit
	    end if
!	    write(*,*) 'read ', curp%x1
	    prvp => curp
	end do
	
	curp%x1 = -1
	close(11)
	Barrier_Ptr => basp%nxtBarrier
	
    END FUNCTION Load_Barrier_File


END MODULE Barriers
