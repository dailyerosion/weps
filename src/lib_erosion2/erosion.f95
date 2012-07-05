
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Program to test grid routines
!!

MODULE Erosion

    USE Grid
    USE Cell
    USE Wind
    USE Soil
    USE Misc

    IMPLICIT NONE

    PRIVATE
    PUBLIC DoErosion
    PRIVATE UpdateSurface
    PRIVATE UpdateCell
    PRIVATE FinalizeCell
    PRIVATE CalcFricVel

    TYPE, PUBLIC :: SimState
	integer :: stepDuration		! length of current simulation step (always <= 3600 seconds)
	integer :: windSpeedX		! index into wind speed hour (hour currently being run)
	type (Summary), dimension(5) :: summaries
	real :: thresholdSpeed
    END TYPE 

    CONTAINS

    SUBROUTINE DoErosion(bgridp, bcellp, bwindp, binitp, bnerop)

    type (WindList), pointer, INTENT (IN) :: bwindp		! first element in list of wind speeds
	type (GridCell), pointer, INTENT (IN) :: bgridp		! upper left element in grid
	type (CellRec), pointer, INTENT (IN) :: bcellp		! first element in list of cells
	type (InitCond), pointer, INTENT (IN) :: binitp		! miscellaneous info
	type (NoErosion), pointer, INTENT (OUT) :: bnerop	! info on why no erosion occurred in step

	type (WindList), pointer :: cwindp
	type (SimState) :: simStat

	logical :: updFlg ! flag indicating that erosion has occurred
	integer :: wdx
	integer, parameter :: SNOWDEPTH = 20			! No erosion when snow depth >= 20mm

	
	! check snow depth
	if (IsSnowTooDeepForErosion(bcellp, SNOWDEPTH)) then
	    bnerop%snowFlg = 1
	    bnerop%erosFlg = 0
	    return
	endif

    END SUBROUTINE DoErosion

!**********************************************************************

    FUNCTION IsSnowTooDeepForErosion(bcellp, snowDepth) result (snowFlg)

	type (CellRec), pointer, INTENT (IN) :: bcellp		! first element in list of cells
	integer :: snowDepth
	logical :: snowFlg

	type (CellRec), pointer :: ccellp		! current element in list of cells

	snowFlg = .false. 

	ccellp => bcellp

	do while (associated(ccellp))
	    if (ccellp%hydp%snowDepth.lt.snowDepth) then
	        return
	    endif
        end do

	snowFlg = .true.

    END FUNCTION IsSnowTooDeepForErosion
!**********************************************************************

    FUNCTION UpdateSurface(gridp, cwindp, simStat) result (updFlg)

	type (GridCell), pointer, INTENT (IN) :: gridp
	type (WindList), pointer, INTENT (IN) :: cwindp
	type (SimState), INTENT (IN OUT) :: simStat

	integer :: wdx

	type (GridCell), pointer :: cgridp
	real :: direction, speed

	logical :: updFlg, celFlg

	wdx = simStat%windSpeedX
	updFlg = .false.

	cgridp => gridp
	do while (cgridp%id /= -1)
	    celFlg = UpdateCell(cgridp, cwindp%direction, cwindp%speed)
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

!**********************************************************************

    FUNCTION UpdateCell(cgridp, direction, speed) result (updFlg)
	
	type (GridCell), pointer :: cgridp
	real :: direction, speed
	logical :: updFlg

	type (CellRec), pointer :: cellp

	cellp => cgridp%cellp
	
	
        updFlg = .true.
    END FUNCTION UpdateCell

!**********************************************************************

    SUBROUTINE FinalizeCell(cgridp, cwindp, simStat)
	
	type (GridCell), pointer, INTENT (IN) :: cgridp
	type (WindList), pointer, INTENT (IN) :: cwindp
	type (SimState), INTENT (IN OUT) :: simStat


    END SUBROUTINE


!**********************************************************************
!     subroutine sbwus
!**********************************************************************
!      subroutine sbwus (anemht, awzzo, awu, wzzov, brcd, wus)
      FUNCTION CalcFricVel(anemHght, surfAeroRoug, windSpeed, cellAreoRoug, bioDragCoef) result (fricVel)
      
!
!     +++ PURPOSE +++
!     To calculate subregion, friction velocity, given station
!     anemometer height, surface roughness, wind speed; and subregion
!     aerodynamic roughness.
!
!     if standing biomass present, then calculate friction velocity
!     at surface below the canopy (fricVel).
!
!     +++ ARGUMENT DECLARATIONS +++
      real  anemHght, surfAeroRoug, windSpeed, cellAreoRoug
      real  bioDragCoef, fricVel
!
!     +++ ARGUMENT DEFINITIONS +++
!
!     anemHght - parameter, anemometer height of input wind speed (m).
!     surfAeroRoug - parameter, surface aerodynamic roughness at input wind
!             speed location (mm).
!     windSpeed - input wind speed driving EROSION submodel (m/s).
!     cellAreoRoug - subregion aerodynamic roughness (mm).
!     bioDragCoef - biomass drag coefficient
!     fricVel - subregion soil surface friction velocity (m/s)
!           i.e. below canopy, if one exists.
!
!     +++ LOCAL VARIABLES +++
      real wusst, wusv
!
!     +++ END SPECIFICATIONS +++
!     note:  in BLOCK.FOR wzoflg should be set to 1 and anemomht
!             set to correct height if anemometer is at field site
!             to obtain correct values from SBWUS or read as
!             input data in stand-alone EROSION.
!
!     Calc station (input wind speed location) friction velocity
      wusst = windSpeed*0.4/alog(anemHght*1000./surfAeroRoug)			![E-70]
!
!     calc subregion friction velocity
      fricVel = wusst * (cellAreoRoug/surfAeroRoug)**0.067			![E-71]
!
!     if standing biomass, calculate wus below canopy
      if (bioDragCoef .gt. 0.0001 ) then
         wusv = fricVel
!
!        calculate friction velocity below canopy

        if( bioDragCoef.gt.2.56) then       !check to avoid underflow
            fricVel = wusv * 0.25*exp(-bioDragCoef/0.356)
        else
            fricVel = wusv*(0.86*exp(-bioDragCoef/0.0298)+0.25*exp(-bioDragCoef/0.356))	![E-73]
        endif
         fricVel = amin1(fricVel,wusv)
      endif
!
      return
!      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

END FUNCTION 



END MODULE Erosion