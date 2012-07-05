
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Grid routines
!!

MODULE SoilLayers

  IMPLICIT NONE

!! Methods defined in this module

    PUBLIC :: Load_SoilLayer_File

!! Methods to be defined in this module

!    PUBLIC :: Dummy

!! Data structures defined in this module

    TYPE, PUBLIC :: SoilLayer
	integer :: id
	real :: thickness
	real :: bulkDensity
	real :: sand
	real :: sandVeryFine
	real :: silt
	real :: clay
	real :: rockVolume
	real :: aggregateDensity
	real :: aggregateStability
	real :: gmd
	real :: aggregateSizeMinimum
	real :: aggregateSizeMaximum
	real :: gsd
	type (SoilLayer), pointer :: nxtSoilLayer
    END TYPE

    CONTAINS
  
    FUNCTION Load_SoilLayer_File (filNam) result (SoilLayer_Ptr)
    
	character (len=*), intent(in) :: filNam
	type (SoilLayer), pointer :: SoilLayer_Ptr
	integer :: eofFlg
	integer :: cnt
	integer :: x1, y1, x2, y2

	type (SoilLayer), pointer :: basp
	type (SoilLayer), pointer :: curp
	type (SoilLayer), pointer :: prvp

        open(11, file=filNam, status="old", action="read")
	read(11, fmt=*) ! skip header line

	allocate(basp)
	prvp => basp
	cnt = 1
	do
	    allocate(curp)
	    curp%id = cnt
	    cnt = cnt + 1
	    prvp%nxtSoilLayer => curp
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
	SoilLayer_Ptr => basp%nxtSoilLayer
	
    END FUNCTION Load_SoilLayer_File
	
END MODULE SoilLayers
