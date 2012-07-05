
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: grid.f95,v $
!-----------------------------------------------------------------------------------
!! Cell 
!!

MODULE Cell

    USE Biomass
    USE Hydrology
    USE SoilSurface
    USE SoilLayers
    USE Util

    IMPLICIT NONE

!! Methods defined in this module

!    PUBLIC :: LoadCellFile
    PRIVATE :: InitStuffMoving
    PUBLIC :: CloneCell

!! Methods to be defined in this module

!    PUBLIC :: Dummy

    REAL, PARAMETER :: MaxAeroRougRand = 30.
    REAL, PARAMETER :: MinAeroRougRand = 0.5

!! Data structures defined in this module

    TYPE, PUBLIC :: StuffMoving
    	real :: pm10
	    real :: salt
	    real :: creep
    END TYPE

    TYPE, PUBLIC :: CellRec
	    integer :: id
	    integer :: bdx, sdx, ldx, hdx ! adjacent cell indices; in for debugging only; remove later
	    integer :: xlen, ylen
	    type (BiomassRec), pointer :: biop
	    type (SoilSurfRec), pointer :: surp
	    type (SoilLayer), pointer :: layp
	    type (HydroRec), pointer :: hydp
	    type (StuffMoving) :: curMoving
	    type (StuffMoving) :: lstMoving
	    type (CellRec), pointer :: nxtPtr		! next cell in list
	    type (StuffMoving) :: availableStuff
	    real :: ridgHght
	    real :: windDir
	    real :: ridgDir
	    real :: ridgSpac
	    real :: dikeSpac
	    real :: randRoug
	    real :: ridgSpacPara
	    real :: aeroRougRand
	    real :: aeroRougRidg
	    real :: aeroRoug
	    real :: aeroRougBio 
	    real :: bioDrag
        real :: resiLAI
        real :: resiSAI
        real :: cropLAI
        real :: cropSAI 
        integer :: cropSeedPlac
        real :: cropRowSpac
	    real :: cropHght
	    integer :: anemStaFlg
	    real :: aeroRoughAnemSta
    	real :: soilFrac84
	    real :: soilFrac84Init
        real :: aggrDistGMD
        real :: aggrDistGSD
        real :: aggrDistMin
        real :: aggrDistMax
        real :: soilDiam
		real :: thrsFricVelAnem
		real :: thrsFricVelRand 
		real :: thrsFricVelRidg 
		real :: thrsFricVelBio 
		real :: thrsFricVel 
		real :: anemHght
		real :: anemRoug
		real :: windSped
		real :: aeroRougAnem
    END TYPE

    TYPE, PUBLIC :: Summary
        type (StuffMoving) :: outTop
        type (StuffMoving) :: outBot
        type (StuffMoving) :: outRig
        type (StuffMoving) :: outLef
    END TYPE

    TYPE, PUBLIC :: SurfCond
	    real :: soilMoblMassCrusSurf		! soil mobile mass on crusted surface (kg/m^2), SMlos
	    real :: soilFracSurfCovrCrus		! soil fraction of surface crust cover (unitless), SFcr
	    real :: soilFracLoosCovrCrus		! soil fraction loose cover on crust (unitless), SFlos
	    real :: soilDeptCrus			! soil depth of crust (consolidated zone) (mm), SZcr
	    real :: soilMoblMassAggrSurf		! soil mobile mass on aggregated surface (kg/m^2), SMAGlos
	    real :: soilMassFrac084			! soil mass fraction < 0.84 mm on aggregated surface, SF84
		    				!   and mobile cover fraction on aggregated surface (unitless)
        real :: soilMassFrac200			! soil mass fraction < 2.00 mm on aggregated surface, SF200					
        real :: soilMassFrac010			! soil mass fraction < 0.10 mm on aggregated surface, SF10					
	    real :: soilVoluRock			! soil volume rock > 2.0 mm diameter (unitless), SVroc
	    real :: ridgHght			! ridge height (mm), SZrg
	    real :: randRougHght			! random roughness height (standard deviation) (mm), SZrr
    END TYPE

    CONTAINS

    FUNCTION LoadCellFile (filNam) result (CellPtr)
    
		character (len=*), intent(in) :: filNam
		type (CellRec), pointer :: CellPtr
		integer :: eofFlg
		integer :: x1, y1, x2, y2

		type (CellRec), pointer :: basp
		type (CellRec), pointer :: curp
		type (CellRec), pointer :: prvp
	
		write(*,*) 'reading cell file'
        open(11, file=filNam, status="old", action="read")
		read(11, fmt=*) ! skip header line

		allocate(basp)
		prvp => basp
		do
			allocate(curp)
			prvp%nxtPtr => curp
			read(unit=11, fmt=*, iostat=eofFlg) curp%id, curp%bdx, curp%sdx, curp%ldx, curp%hdx
			write(*,*) curp%id, curp%bdx, curp%sdx, curp%ldx, curp%hdx
			if (eofFlg<0) then
				exit
			end if
			call InitStuffMoving(curp%curMoving, 0.0, 0.0, 0.0)
			call InitStuffMoving(curp%lstMoving, 0.0, 0.0, 0.0)
			!	    write(*,*) 'read ', curp%x1
			prvp => curp
		end do
	
		curp%id = -1
		close(11)
		CellPtr => basp%nxtPtr
	
    END FUNCTION LoadCellFile

	SUBROUTINE InitStuffMoving(sm, pm10, salt, creep)
    
		type (StuffMoving), INTENT (IN OUT) :: sm
		real :: pm10, salt, creep
		sm%pm10 = pm10
		sm%salt = salt
		sm%creep = creep
	
	END SUBROUTINE InitStuffMoving

    SUBROUTINE PrintCellFile (luo, basp)

		integer, INTENT (IN) :: luo
		type (CellRec), pointer, INTENT (IN) :: basp

		type (CellRec), pointer :: curp

		write(luo,*) 'ID  BIO SRF LAY HYD'

		curp => basp

		do while (curp%id /= -1)
			write(luo, '(5i4)') curp%id, curp%bdx, curp%sdx, curp%ldx, curp%hdx
			curp => curp%nxtPtr
		end do

    END SUBROUTINE PrintCellFile

    FUNCTION CloneCell(inCell) result (outCell)

		type (CellRec), pointer :: inCell
		type (CellRec), pointer :: outCell
		type (CellRec), pointer :: tmpCell

		allocate(tmpCell)

		tmpCell%id = inCell%id
		tmpCell%xlen = inCell%xlen
		tmpCell%ylen = inCell%ylen
		tmpCell%biop => inCell%biop
		tmpCell%layp => inCell%layp
		tmpCell%surp => inCell%surp
		tmpCell%hydp => inCell%hydp
		call InitStuffMoving(tmpCell%curMoving, 0.0, 0.0, 0.0)
		call InitStuffMoving(tmpCell%lstMoving, 0.0, 0.0, 0.0)
		call InitStuffMoving(tmpCell%availableStuff, 0.0, 0.0, 0.0)

		outCell => tmpCell
    END FUNCTION CloneCell

    FUNCTION CheckForErosion(inCell) result (rtnCod)

		type (CellRec), pointer :: inCell	!cell to calculate
		integer :: rtnCod			!return code, 0 == success, otherwise failure type

		real :: sina				!sin of angles

		!calc the ridge spacing parallel to the wind, set spacing to 1m if the ridges are less than 5mm
		if (inCell%ridgHght > 5) then
			!calc ridge spacing parallel the wind
			sina = abs(sin(inCell%windDir - inCell%ridgDir))	!eq. E-2a
			sina = min(sina, 0.1)				!eq. E-2b
			inCell%ridgSpacPara = inCell%ridgSpac / sina		!eq. E-2c
			if (inCell%dikeSpac > inCell%ridgSpac / 3) then
				inCell%ridgSpacPara = min(inCell%ridgSpacPara, inCell%dikeSpac) !eq. E-2e
			else 
				inCell%ridgSpacPara = 1000					!eq. E-2d
			endif

			!calc aero
			inCell%aeroRougRidg = inCell%ridgHght / (-65.1 + 135.5 * inCell%ridgHght / inCell%ridgSpacPara + &
				20.84 * sqrt(inCell%ridgHght / inCell%ridgSpacPara))	!eq. E-1
		else 
			inCell%aeroRougRidg = 0			!not in doc; assume this is correct
		endif
	
		inCell%aeroRougRand = 0.3 * inCell%randRoug			!eq. E-3
		inCell%aeroRougRand = min(MaxAeroRougRand, inCell%aeroRougRand)
		inCell%aeroRougRand = max(MinAeroRougRand, inCell%aeroRougRand)

		inCell%aeroRoug = max(inCell%aeroRougRand, inCell%aeroRougRidg)		!eq. E-4

		inCell%bioDrag = calcBioDrag (inCell%resiLAI, inCell%resiSAI, inCell%cropLAI, &
	        inCell%cropSAI, inCell%cropSeedPlac, inCell%cropRowSpac, &
	        inCell%cropHght, inCell%ridgHght)


		! calculate roughness length of canopy ( in mm)
		if (inCell%bioDrag .gt. 0.1) then
			inCell%aeroRougBio = inCell%cropHght * 1/(17.27-(1.254*alog(inCell%bioDrag)/inCell%bioDrag)-(3.714/inCell%bioDrag))	!eq. OE-68
		else if( (inCell%cropHght .gt. 5.0) .and. (inCell%bioDrag .gt. 0.001) ) then
			inCell%aeroRougBio = inCell%cropHght*(inCell%aeroRoug/inCell%cropHght+	&			!eq. OE-69
				((0.11-inCell%aeroRoug/inCell%cropHght)/4.60517)*alog(inCell%bioDrag/0.001))
		else
            inCell%aeroRougBio = 0.0
		endif

      ! choose the maximum of canopy or surface roughness
        inCell%aeroRougBio = max(inCell%aeroRougBio, inCell%aeroRoug)

        if (inCell%anemStaFlg .eq. 1) then
			inCell%aeroRoughAnemSta = inCell%aeroRougBio		         ! anemom. in field set awzzo to wzzov
		endif
      
		inCell%soilFrac84 = calcSoilMassFrac(inCell%aggrDistGMD, inCell%aggrDistGSD, inCell%aggrDistMin, inCell%aggrDistMax, inCell%soilDiam)

        inCell%soilFrac84Init = inCell%soilFrac84

		inCell%thrsFricVelAnem = calcFricVelc(inCell%anemHght, inCell%anemRoug, inCell%windSped, inCell%aeroRougAnem, 0.0)

		inCell%thrsFricVelRand = calcFricVelc(inCell%anemHght, inCell%anemRoug, inCell%windSped, inCell%aeroRougAnem, 0.0)

		inCell%thrsFricVelRidg = calcFricVelc(inCell%anemHght, inCell%anemRoug, inCell%windSped, inCell%aeroRougAnem, 0.0)

		inCell%thrsFricVelBio = calcFricVelc(inCell%anemHght, inCell%anemRoug, inCell%windSped, inCell%aeroRoug, inCell%bioDrag)

		inCell%thrsFricVel = calcFricVelc(inCell%anemHght, inCell%anemRoug, inCell%windSped, inCell%aeroRougAnem, inCell%bioDrag)

    END FUNCTION CheckForErosion
   
END MODULE Cell
