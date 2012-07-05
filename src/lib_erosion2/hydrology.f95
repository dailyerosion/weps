
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Grid routines
!!

MODULE Hydrology

  IMPLICIT NONE

!! Methods defined in this module

    PUBLIC :: Load_Hydrology_File

!! Methods to be defined in this module

!    PUBLIC :: Dummy

!! Data structures defined in this module

    TYPE, PUBLIC :: HydroRec
	integer :: id
	real :: snowDepth
	real, dimension(:), allocatable :: layerWiltingPointWaterContent
	real, dimension(:), allocatable :: layerWaterContent
	real :: surfaceLayerWaterContent
	type (HydroRec), pointer :: nxtHydroRec
    END TYPE 

    CONTAINS

    FUNCTION Load_Hydrology_File (filNam) result (Hydrology_Ptr)
    
	character (len=*), intent(in) :: filNam
	type (HydroRec), pointer :: Hydrology_Ptr
	integer :: eofFlg
	integer :: cnt
	integer :: x1, y1, x2, y2

	type (HydroRec), pointer :: basp
	type (HydroRec), pointer :: curp
	type (HydroRec), pointer :: prvp

        open(11, file=filNam, status="old", action="read")
	read(11, fmt=*) ! skip header line

	allocate(basp)
	prvp => basp
	cnt = 1
	do
	    allocate(curp)
	    curp%id = cnt
	    cnt = cnt + 1
	    prvp%nxtHydroRec => curp
!	    read(unit=11, fmt=*, iostat=eofFlg) curp%x1, curp%y1, curp%x2, curp%y2, &
!	        curp%height, curp%porosity, curp%width
	    if (eofFlg<0) then
	        exit
	    end if
!	    write(*,*) 'read ', curp%x1
	    prvp => curp
	end do
	
	curp%id = -1
	close(11)
	Hydrology_Ptr => basp%nxtHydroRec
	
    END FUNCTION Load_Hydrology_File


END MODULE Hydrology
