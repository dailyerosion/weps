
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Program to test grid routines
!!

MODULE Simulation

    USE Grid
    USE Cell
    USE Wind

    IMPLICIT NONE

    PUBLIC DoSimulation
    PRIVATE FinalizeCell

    TYPE, PUBLIC :: SimState
	integer :: stepDuration		! length of current simulation step (always <= 3600 seconds)
	integer :: windSpeedX		! index into wind speed hour (hour currently being run)
	type (Summary), dimension(5) :: summaries
	real :: thresholdSpeed
    END TYPE 

    CONTAINS

    SUBROUTINE DoSimulation(gridp, cellp, windp)

    type (WindRec), pointer, INTENT (IN) :: windp
	type (GridCell), pointer, INTENT (IN) :: gridp
	type (CellRec), pointer, INTENT (IN) :: cellp

	type (WindRec), pointer :: cwindp
	type (SimState) :: simStat

	logical :: updFlg ! flag indicating that erosion has occurred
	integer :: wdx

	simStat%thresholdSpeed = 15.0 ! 15 m/s totally arbitrary for testing purposes

	cwindp => windp

	do while (cwindp%day /= -1)
!	    do simStat%windSpeedX = 1, 24
	    do wdx = 1, 24
		simStat%windSpeedX = wdx
	        if (cwindp%speed(wdx) >= simStat%thresholdSpeed) then
		    updFlg = UpdateSurface(gridp, cwindp, simStat)
		endif
	    end do
	    cwindp => cwindp%nxtPtr
	end do
	
    END SUBROUTINE DoSimulation

    FUNCTION UpdateSurface(gridp, cwindp, simStat) result (updFlg)

	type (GridCell), pointer, INTENT (IN) :: gridp
	type (WindRec), pointer, INTENT (IN) :: cwindp
	type (SimState), INTENT (IN OUT) :: simStat

	integer :: wdx

	type (GridCell), pointer :: cgridp
	real :: direction, speed

	logical :: updFlg, celFlg

	wdx = simStat%windSpeedX
	updFlg = .false.

	cgridp => gridp
	do while (cgridp%id /= -1)
	    celFlg = UpdateCell(cgridp, cwindp%direction, cwindp%speed(wdx))
	    if (celFlg) then
	        updFlg = .true.
	    endif
	    cgridp => cgridp%nxtPtr
	end do
	if (updFlg) then
	    cgridp => gridp
	    do while (cgridp%id /= -1)
		call FinalizeCell(cgridp, cwindp, simStat)
		cgridp => cgridp%nxtPtr
	    end do
	endif
	    
    END FUNCTION UpdateSurface

    FUNCTION UpdateCell(cgridp, direction, speed) result (updFlg)
	
	type (GridCell), pointer :: cgridp
	real :: direction, speed
	logical :: updFlg

	type (CellRec), pointer :: cellp

	cellp => cgridp%cellp
	
	
        updFlg = .true.
    END FUNCTION UpdateCell

    SUBROUTINE FinalizeCell(cgridp, cwindp, simStat)
	
	type (GridCell), pointer, INTENT (IN) :: cgridp
	type (WindRec), pointer, INTENT (IN) :: cwindp
	type (SimState), INTENT (IN OUT) :: simStat


    END SUBROUTINE

END MODULE Simulation