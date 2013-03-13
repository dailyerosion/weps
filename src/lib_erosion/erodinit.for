!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
      subroutine erodinit( noerod )

!     +++ PURPOSE +++
!
!     Controls calls to subroutines that:
!       create the Erosion submodel grid (sbgrid)    
!       initialize Erosion submodel output array to zero (sbigrd).
!       calculate normalized effect of hills on friction velocity 
!        on grid for each wind direction (not activated)
!       calculate normalized effect of barriers on friction velocity
!        on grid for each wind direction (sbbr)
!       initialize reporting variables that need to have a value even
!        when erosion is not being called.

!     + + + Modules Used + + +
      use weps_interface_defs
      use Points_Mod
      use Polygons_Mod
      use pnpoly_mod
      use subregions_mod
      use erosion_data_struct_defs

!     + + + GLOBAL COMMON BLOCKS + + +
      include  'p1werm.inc'
      include  'm1flag.inc'
      include  'm1geo.inc'
      include  'm1subr.inc'
      include  'erosion/m2geo.inc'
      include  'erosion/e2grid.inc'  !needed for initialization of csr(*,*)
      include  's1surf.inc'
      
!     +++ ARGUMENT DECLARATIONS +++
      type(threshold), dimension(:), intent(out) :: noerod                 ! report values to show which factors prevented erosion

!     +++ SUBROUTINES CALLED +++
!     sbgrid
!     sbigrd
!     sbhill (not activated)
!     sbbr

!     +++ LOCAL VARIABLES +++
      integer i, j, sr
      type(point) :: centroid

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     nbr  = number of barriers (from m1geo.inc)

!     +++ END SPECIFICATIONS +++

      ! Grid is created at least once.
      if (am0eif .eqv. .true.) then
         ! check to see if grid dimensions specified via cmdline args
         if ((xgdpt > 0) .and. (ygdpt > 0)) then
           imax = xgdpt + 1
           jmax = ygdpt + 1
           ix = (amxsim(1,2) - amxsim(1,1)) / xgdpt
           jy = (amxsim(2,2) - amxsim(2,1)) / ygdpt
         else          !use Hagen's grid dimensioning as the default
           call sbgrid
         endif

         ! assign subregion number to each grid cell
         ! code lifted from sbgrid because it is initialized there - LEW
         do j = 1, jmax-1
           do i = 1, imax-1
             ! The grid cell is assumed rectangular. Use centroid of grid cell
             ! with subregion polygon to select grid cell subregion
             centroid%x = 0.5 * (i-1+i) * ix
             centroid%y = 0.5 * (j-1+j) * jy
             do sr = 1,nsubr
               ! Check if it is inside subregion polygon
               if( pnpoly(centroid, subr_poly(sr)) .ge. 0) then
                  ! centroid of grid cell is inside or on edge of subregion polygon
                  ! set subregion index
                  csr(i,j) = sr
                  ! default to first polygon if on edge by exiting the subregion do loop
                  exit
               end if
             end do
             ! check final status
             if( csr(i,j) .eq. 0 ) then
                 ! this grid cell not assigned to a subregion
                 write(*,*) 'ERROR: no subregion for grid cell ',i,':',j
                 write(*,*) 'Subregion coverage is not complete'
                 stop
             end if
           end do          
        end do

         ! set grid cell output arrays to zero
         call sbigrd

         ! check for hills - sbhill not implemented
!        if (nhill .gt. 0) then
!        call sbhill
!        endif

         ! check for barriers
         if (nbr .gt. 0) then
         call sbbr
         endif

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

           ! initialize surface condition reporting values
           acanag = 0
           acancr = 0
      end do

      return
      end

