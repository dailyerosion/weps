
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Grid routines
!!

MODULE Biomass

  IMPLICIT NONE

!! Methods defined in this module

    PUBLIC :: Load_Biomass_File

!! Methods to be defined in this module

!    PUBLIC :: Dummy

!! Data structures defined in this module

    TYPE, PUBLIC :: BiomassRec
	integer :: id
	real :: cropHeight
	real :: cropSAI
	real :: cropLAI
	real :: residueSAI
	real :: residueLAI
	real :: cropRowSpacing
	integer :: cropSeedPlacement
	type (BiomassRec), pointer :: nxtBiomass
    END TYPE

    CONTAINS

    FUNCTION Load_Biomass_File (filNam) result (Biomass_Ptr)
    
	character (len=*), intent(in) :: filNam
	type (BiomassRec), pointer :: Biomass_Ptr
	integer :: eofFlg
	integer :: cnt
	integer :: x1, y1, x2, y2

	type (BiomassRec), pointer :: basp
	type (BiomassRec), pointer :: curp
	type (BiomassRec), pointer :: prvp

        open(11, file=filNam, status="old", action="read")
	read(11, fmt=*) ! skip header line

	allocate(basp)
	prvp => basp
	cnt = 1
	do
	    allocate(curp)
	    curp%id = cnt
	    cnt = cnt + 1
	    prvp%nxtBiomass => curp
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
	Biomass_Ptr => basp%nxtBiomass
	
    END FUNCTION Load_Biomass_File
	
	
END MODULE Biomass
