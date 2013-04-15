!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbgrid
!**********************************************************************
      subroutine sbgrid

!     +++ PURPOSE +++
!     to calculate grid size and spacing for EROSION.
!     grid size assumes outer points are outside field boundary
!      a distance ix/2
!     to calculate number of grid points for EROSION.
!     A max 'interior' square grid of 29X29 is assigned-no barriers
!     A max 'interior' rectangular grid of 59X59 is assigned barriers
!     to assign subregion index no. to each grid point.

      use grid_geo_def, only: imax, jmax, ix, jy, amxsim,               &
     &                        N_G_DPT, B_G_DPT, MIN_GRID_SP
      use barriers_mod

!     +++ LOCAL DEFINITIONS +++
!     imax  - no. grid intervals in x-direction
!     jmax  - no. grid intervals in y-direction.
!     ix    - grid interval in x-direction (m)
!     jy    - grid interval in y-direction (m)
!     dxmin - minimum grid interval (m)
!     i     - do loop index

!     + + + GLOBAL COMMON BLOCKS + + +

      include  'p1werm.inc'

!     +++ LOCAL VARIABLES +++
      integer  ngdpt
      integer  i, nbr
      real     dxmin, lx, ly
      real :: minht    ! minimum height of barrier along it's length.

!     +++ END SPECIFICATIONS +++

       ! set min grid spacing
       dxmin = MIN_GRID_SP

       ! set max no. of grid points with no barrier
       ngdpt = N_G_DPT

       if( allocated(barrier) ) then
         ! barriers exist
         nbr = size(barrier)
         ! find shortest barrier to determine dxmin
         do 5 i = 1, nbr
            minht =                                                     &
     &          minval(barrier(i)%param(1:size(barrier(i)%param))%amzbr)
            if( minht > 0.0 ) then    !Check for zero height barriers
               dxmin = min(dxmin, 5.0*minht)
            endif
    5    continue
         ngdpt = B_G_DPT  !default to this value if a barrier exists
       endif

!     calculate max grid intervals
!       calc. lx and ly sides of field
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y

      write(*,*) 'SBGRID: ly, ly: ', lx, ly

!     increase grid points on large field
!      if((lx .gt. 200) .or. (ly  .gt. 200)) then
!         ngdpt = B_G_DPT
!      endif

      ! case where lx > ly
      if ( lx .gt. ly)then
        imax  = int ( lx / dxmin)
        imax = min(imax,ngdpt)
        imax = max(imax,2)
        ! calculate spacing for square or with barriers a rectangular grid
        ix  = lx / (imax - 1)

         if (nbr .gt. 0) then
           jmax  = int (ly / dxmin)
           jmax  = min(jmax, ngdpt)
         else
           jmax = nint(ly/ix) + 1
         endif

        jmax = max(jmax,2)
        jy   = ly / (jmax - 1)

      ! case where lx = ly or lx < ly
      else
        jmax  = int (ly / dxmin)
        jmax = min(jmax,ngdpt)
        jmax = max(jmax,2)
        jy   = ly / (jmax - 1)

        if (nbr .gt. 0) then
           imax  = int (lx / dxmin)
           imax  = min(imax,ngdpt)
        else
           imax = nint(lx/jy) + 1
        endif
        imax = max(imax,2)
        ix = lx/(imax-1)

      endif

      return
      end

