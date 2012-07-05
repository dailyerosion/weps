
!$Author: wjr $
!$Date: 2011-11-06 $
!$Revison: 0.1 $
!$Source: testerode.f95,v $
!-----------------------------------------------------------------------------------
!! Module to hold miscellaneous things -- things that need to move from one
!!  sub-model to another but don't have a real place
!!

MODULE Misc

    TYPE, PUBLIC :: InitCond
    END TYPE
    
    TYPE, PUBLIC :: NoErosion
	integer :: erosFlg		! Erosion flag -- 1 erosion code executed, 0 not executed
	integer :: snowFlg		! Snow flag -- 1 snow prevents erosion, 0 does not prevent
					! all velocities are for critical no erosion condition
	real :: anenFricVel		! anemometer located friction velocity
	real :: randRougFricVel		! site surface random roughness adjusted friction velocity
	real :: ridgeFricVel		! site surface oriented roughness adjusted friction velocity
	real :: bioDragFricVel		! site biodrag adjusted friction velocity
	real :: fricVel			! friction velocity
	
	real :: bare			! bare friction veolocity greater
	real :: flatCov			! flat cover increases threshold
	real :: surfWet			! surface wetness increases threshold
	real :: aggDen			! ag density increases threshold
	real :: fricVel2		! resultant threshold friction velocity

	real :: fracSurfMatr084		! fraction of the surface material less than 0.84 mm in diameter
	real :: fracSurfMatrRock	! fraction of the surface matherial greater than 2 mm in diameter
	real :: areoRougCanopy		! aerodynamic roughness length of the soil surface below canopy (mm)
	real :: fracNonEmit		! fraction of soil surface which is non emitting
    END TYPE

    CONTAINS

END MODULE Misc
	