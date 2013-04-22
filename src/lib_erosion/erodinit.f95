!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
      subroutine erodinit( noerod, cellstate )

!     +++ PURPOSE +++
!
!     Controls calls to subroutines that:
!       initialize Erosion submodel output array to zero (sbigrd).
!       calculate normalized effect of hills on friction velocity 
!        on grid for each wind direction (not activated)
!       initialize reporting variables that need to have a value even
!        when erosion is not being called.

!     + + + Modules Used + + +
      use grid_mod, only: sbigrd, init_regions_grid
      use subregions_mod
      use erosion_data_struct_defs, only: threshold, cellsurfacestate, am0eif

!     +++ ARGUMENT DECLARATIONS +++
      type(threshold), dimension(:), intent(inout) :: noerod                 ! report values to show which factors prevented erosion
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ SUBROUTINES CALLED +++
!     sbigrd
!     sbhill (not activated)

!     +++ LOCAL VARIABLES +++
      integer :: sr    ! do loop index
      integer :: nsubr ! total number of subregions

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     nbr  = number of barriers

!     +++ END SPECIFICATIONS +++

      nsubr = size(subr_poly)

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         call init_regions_grid( cellstate )

         ! set grid cell output arrays to zero
         call sbigrd( cellstate )

         ! check for hills - sbhill not implemented
!        if (nhill .gt. 0) then
!        call sbhill
!        endif

         ! check for barriers - moved to erosion to use actual wind angles

         ! Turn off grid creation flag
         am0eif = .false.
      endif

      do sr = 1, nsubr
           ! initalize erosion threshold trigger variables
           noerod(sr)%erosion = 0
           noerod(sr)%snowdepth = 0

           noerod(sr)%wus_anemom = 0
           noerod(sr)%wus_random = 0
           noerod(sr)%wus_ridge = 0
           noerod(sr)%wus_biodrag = 0
           noerod(sr)%wus = 0

           noerod(sr)%bare = 0
           noerod(sr)%flat_cov = 0
           noerod(sr)%surf_wet = 0
           noerod(sr)%ag_den = 0
           noerod(sr)%wust = 0

           noerod(sr)%sfd84 = 0
           noerod(sr)%asvroc = 0
           noerod(sr)%wzzo = 0
           noerod(sr)%sfcv = 0
      end do

      return
      end

