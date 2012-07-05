
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Grid routines
!!

MODULE Wind

  IMPLICIT NONE

!! Methods defined in this module

    PRIVATE
    PUBLIC :: LoadWindFile
    PUBLIC :: MakeWindList
    PUBLIC :: PrintWindFile
    PUBLIC :: PrintWindList

!! Methods to be defined in this module

!    PUBLIC :: Dummy

!! Data structures defined in this module

    TYPE, PUBLIC :: WindRec
	integer :: id
	integer :: day
	integer :: month
	integer :: year
	real :: direction
	real, dimension(24) :: speed
	type (WindRec), pointer :: nxtPtr
    END TYPE 

    TYPE, PUBLIC :: WindList
	integer :: id
	integer :: day
	integer :: month
	integer :: year
	integer :: hour
	integer :: duration
	real :: direction
	real :: speed
	type (WindList), pointer :: nxtPtr
    END TYPE 

    CONTAINS

    FUNCTION LoadWindFile (filNam) result (Wind_Ptr)
    
	character (len=*), intent(in) :: filNam
	type (WindRec), pointer :: Wind_Ptr
	integer :: eofFlg
	integer :: cnt
	integer :: idx

	type (WindRec), pointer :: basp
	type (WindRec), pointer :: curp
	type (WindRec), pointer :: prvp

        open(11, file=filNam, status="old", action="read")
	do idx = 1, 7
	    read(11, fmt=*) ! skip header lines
	end do

	allocate(basp)
	prvp => basp
	cnt = 1
	do
	    allocate(curp)
	    curp%id = cnt
	    cnt = cnt + 1
	    prvp%nxtPtr => curp
!	    read(unit=11, fmt='(3 i2 25 f5.1)', iostat=eofFlg) curp%day, curp%month, curp%year, &
!		curp%direction, (curp%speed(idx), idx=1,24)
	    read(unit=11, fmt=*, iostat=eofFlg) curp%day, curp%month, curp%year, &
		curp%direction, (curp%speed(idx), idx=1,24)
	    if (eofFlg<0) then
	        exit
	    end if
!	    write(*,*) 'read ', curp%x1
	    prvp => curp
	end do
	
	curp%id = -1
	close(11)
	write(*,*) 'wind records read ', cnt
	Wind_Ptr => basp%nxtPtr
	
    END FUNCTION LoadWindFile

    SUBROUTINE PrintWindFile(luo, bwinp, limit)

	integer, INTENT (IN) :: luo
	type (WindRec), pointer, INTENT (IN) :: bwinp
	integer, INTENT (IN) :: limit

	type (WindRec), pointer :: curp
	integer :: idx
	integer :: cnt

	cnt = limit
	curp => bwinp
	do while ((cnt /= 0).and.(associated(curp)))
!	    write(*, *) curp%day, curp%month, curp%year, &
!		curp%direction, (curp%speed(idx), idx=1,24)
	    write(luo, '(3i3,25F6.1)') curp%day, curp%month, curp%year, &
		curp%direction, (curp%speed(idx), idx=1,24)
	    curp => curp%nxtPtr
	    cnt = cnt - 1
	end do
    END SUBROUTINE PrintWindFile

    FUNCTION MakeWindList(bwinp, sday, eday) result (blstp)

	type (WindRec), pointer :: bwinp
	type (WindList), pointer :: blstp
	integer :: sday
	integer :: eday

	type (WindList), pointer :: clstp
	type (WindRec), pointer :: cwinp

	integer :: idx
	integer :: cday			!current day for selecting days in list
	cwinp => bwinp
	allocate(blstp)
	clstp => blstp

	do while (associated(cwinp))
	    cday = 10000 * cwinp%year + 100 * cwinp%month + cwinp%day
	    if (cday.ge.sday.and.cday.le.eday) then
	    do idx = 1, 24
	        allocate(clstp%nxtPtr)
		clstp => clstp%nxtPtr
		clstp%id = cwinp%id
		clstp%day = cwinp%day
		clstp%month = cwinp%month
		clstp%year = cwinp%year
		clstp%hour = idx
		clstp%duration = 3600
		clstp%direction = cwinp%direction
		clstp%speed = cwinp%speed(idx)
	    end do
	    endif
	    cwinp => cwinp%nxtPtr
	end do
        clstp => blstp
	blstp => blstp%nxtPtr
	deallocate(clstp)
		
    END FUNCTION MakeWindList

    SUBROUTINE PrintWindList(luo, bwinp, limit)

	integer, INTENT (IN) :: luo
	type (WindList), pointer, INTENT (IN) :: bwinp
	integer, INTENT (IN) :: limit

	type (WindList), pointer :: curp
	integer :: idx
	integer :: cnt

	cnt = limit
	curp => bwinp
	do while ((cnt /= 0).and.(associated(curp)))
!	    write(*, *) curp%day, curp%month, curp%year, &
!		curp%direction, (curp%speed(idx), idx=1,24)
	    write(luo, '(4i3,i5,2F6.1)') curp%day, curp%month, curp%year, &
		curp%hour, curp%duration, curp%direction, curp%speed
	    curp => curp%nxtPtr
	    cnt = cnt - 1
	end do
    END SUBROUTINE PrintWindList

END MODULE Wind
