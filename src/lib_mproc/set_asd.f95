!$Author:Larry Wagner $
!$Date: 2017-02-06 13:38:00 -0600 (Mon, 06 Feb 2017) $
!$Revision:  $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion/src/lib_manage/set_asd.f95 $
!
! $Header: /weru/cvs/weps/weps.src/manage/tillay.for,v 1.3 2002-09-04 20:21:49 wagner Exp $
!
!     + + + PURPOSE + + +
! This subroutine assigns the ASD modified lognormal parameters,
! e.g., the modified lognormal (transformed) GMD and GSD values
! as well as the GMDmin and GMDmax values
! to all soil layers within the specified depth.
!
! If the user is interested in setting different ASD values to different
! soil layers (depths) they should call this process repeatedly with
! smaller and smaller soil depths specified.
!
! Currently assumes we have "logcas = 3" condition (mnot != 0, minf != infinity)

!     + + + KEYWORDS + + +
!     soil layer, asd, gmd, gsd

SUBROUTINE set_asd (gmdx, gsdx, mnot, minf, nlay, soil)

  USE soil_data_struct_defs, only: soil_def
  TYPE(soil_def), INTENT(INOUT) :: soil
!      REAL,DIMENSION(:), intent(inout) :: soil%aslagm, soil%as0ags  ! for this subregion only

!     + + + ARGUMENT DECLARATIONS + + +
  REAL, INTENT (IN)    :: gmdx, gsdx
  REAL, INTENT (IN)    :: mnot, minf
  INTEGER, INTENT (IN) :: nlay


! + + + ARGUMENT DEFINITIONS + + +
! gmdx    - geometric mean diameter of aggregate size distribution
!          (or transformed gmd for "modified" lognormal cases)
! gsdx    - geometric standard deviation of aggregate size distribution
!          (or transformed gsd for "modified" lognormal cases)
! mnot    - minimum aggregate size in aggregate size distribution
!          (for "modified" lognormal cases)
! minf    - maximum aggregate size in aggregate size distribution
!          (for "modified" lognormal cases)
! nlay   - number of soil layers used


! + + + LOCAL VARIABLES + + +
  INTEGER :: j

! + + + LOCAL VARIABLE DEFINITIONS + + +
! j      - loop variable for soil layers

  IF (nlay .ge. 1) THEN    !for each soil layer
     DO j=1,nlay
        soil%aslagm(j) = gmdx
        soil%as0ags(j) = gsdx
        soil%aslagn(j) = mnot
        soil%aslagx(j) = minf
     END DO
  ELSE
     write (0,*) "Depth specified is negative, ASD values not assigned."
  END IF

END SUBROUTINE set_asd