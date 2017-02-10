!$Author:Larry Wagner $
!$Date: 2017-02-06 13:38:00 -0600 (Mon, 06 Feb 2017) $
!$Revision:  $
!$HeadURL: https://infosys.ars.usda.gov/svn/code/weps1/branches/weps.src.subregion/src/lib_manage/set_asd.f95 $
!
! $Header: /weru/cvs/weps/weps.src/manage/tillay.for,v 1.3 2002-09-04 20:21:49 wagner Exp $
!
!     + + + PURPOSE + + +
! This subroutine assigns the ASD modified lognormal parameters,
! e.g., the modified lognormal (transformed) GMD and GSD values,
! (GMDmax and GMDmin are excluded - the WEPS values are assumed)
! to all soil layers within the tillage depth.
!
! If the user is interested in setting different ASD values to different
! soil layers (depths) they should call this process repeatedly with
! smaller and smaller tillage depths specified with the "tillage" group process.
!
! Currently assumes we have "logcas = 3" condition (mnot != 0, minf != infinity)

!     + + + KEYWORDS + + +
!     soil layer, asd, mgmd, mgsd

      subroutine set_asd (mgmd, mgsd, nlay, soil)

      use soil_data_struct_defs, only: soil_def

!     + + + ARGUMENT DECLARATIONS + + +
      real       mgmd 
      real       mgsd
      integer    nlay
      type(soil_def), intent(inout) :: soil
!      type(soil_def), intent(inout) :: soil%aslagm, soil%as0ags  ! for this subregion only

!     + + + ARGUMENT DEFINITIONS + + +
!     mgmd    - geometric mean diameter of aggregate size distribution
!              (or transformed gmd for "modified" lognormal cases)
!     mgsd    - geometric standard deviation of aggregate size distribution
!              (or transformed gsd for "modified" lognormal cases)
!     nlay   - number of soil layers used

!     + + + LOCAL VARIABLES + + +
      integer j

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     j      - loop variable for soil layers

      if (nlay .ge. 1) then
!         for each soil layer
          do 100 j=1,nlay
              soil%aslagm(j) = mgmd
              soil%as0ags(j) = mgsd
100       continue
      else
          write (0,*) "Tillage depth is negative, ASD values not assigned."
      end if

      return
      end