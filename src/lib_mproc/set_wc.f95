!$Author:Larry Wagner $
!$Date: 2017-02-06 13:38:00 -0600 (Mon, 06 Feb 2017) $
!$Revision:  $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion/src/lib_manage/set_asd.f95 $
!
! $Header: /weru/cvs/weps/weps.src/manage/tillay.for,v 1.3 2002-09-04 20:21:49 wagner Exp $
!
!     + + + PURPOSE + + +
! This subroutine assigns the water content values,
! to all soil layers within the specified depth.
!
! If the user is interested in setting different water content values to different
! soil layers (depths) they should call this process repeatedly with
! smaller and smaller soil depths specified.

!     + + + KEYWORDS + + +
!     soil layer, wc

SUBROUTINE set_wc (wc, nlay, soil)

  USE soil_data_struct_defs, only: soil_def
  TYPE(soil_def), INTENT(INOUT) :: soil
!      REAL,DIMENSION(:), intent(inout) :: soil%ahrwc   ! for this subregion only

!     + + + ARGUMENT DECLARATIONS + + +
  REAL, INTENT (IN)    :: wc
  INTEGER, INTENT (IN) :: nlay


! + + + ARGUMENT DEFINITIONS + + +
! wc      - water content (Mg/Mg)
! nlay   - number of soil layers used


! + + + LOCAL VARIABLES + + +
  INTEGER :: j

! + + + LOCAL VARIABLE DEFINITIONS + + +
! j      - loop variable for soil layers

  IF (nlay .ge. 1) THEN    !for each soil layer
     DO j=1,nlay
        soil%ahrwc(j) = wc
     END DO
  ELSE
     write (0,*) "Depth specified is negative, water content values not assigned."
  END IF

END SUBROUTINE set_wc