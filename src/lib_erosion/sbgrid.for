!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbgrid
!**********************************************************************
      subroutine sbgrid
!
!     +++ PURPOSE +++
!     to calculate grid size and spacing for EROSION.
!     grid size assumes outer points are outside field boundary
!      a distance ix/2
!     to calculate number of grid points for EROSION.
!     A max 'interior' square grid of 29X29 is assigned-no barriers
!     A max 'interior' rectangular grid of 59X59 is assigned barriers
!     to assign subregion index no. to each grid point.
!
!     +++ ARGUMENT DECLARATION +++

!     +++ LOCAL DEFINITIONS +++
!     imax  - no. grid intervals in x-direction
!     jmax  - no. grid intervals in y-direction.
!     ix    - grid interval in x-direction (m)
!     jy    - grid interval in y-direction (m)
!     dxmin - minimum grid interval (m)
!     csr   - current subr. index at grid point i,j
!     icsr  - same as csr but not an array
!     i,j   - do loop indexes
!
!     + + + GLOBAL COMMON BLOCKS + + +

      include  'p1werm.inc'
      include  'm1geo.inc'
      include  'm1subr.inc'
!
!     + + + LOCAL COMMON BLOCKS + + +
      include  'erosion/m2geo.inc'
      include  'erosion/e2grid.inc'
!
!     +++ LOCAL VARIABLES +++
      integer  ngdpt
      integer  icsr, i, j
      real     dxmin, lx, ly
!
!     +++ END SPECIFICATIONS +++
!
!     set min grid spacing
       dxmin = MIN_GRID_SP
!     set max no. of grid points with no barrier

       ngdpt = N_G_DPT
!     barriers?
       if (nbr .gt. 0) then
!        find shortest barrier to determine dxmin
         do 5 i=1,nbr
            if (amzbr(i) > 0.0) then    !Check for zero height barriers
               dxmin = min(dxmin, 5.0*amzbr(i))
            endif
    5    continue
         ngdpt = B_G_DPT  !default to this value if a barrier exists
       endif

!     calculate max grid intervals
!       calc. lx and ly sides of field
      lx = amxsim (1,2)-amxsim(1,1)
      ly = amxsim (2,2)-amxsim(2,1)
     
! Change lx and ly into subregion length here by JG 
 !      lx = amxsr(1,2,isr)-amxsr(1,1,isr)
 !      ly = amxsr(2,2,isr)-amxsr(2,1,isr)

!
!^^^tmp out
!      write(*,*) 'tmp out from sbgrid, line 69'
!     write (*,*) 'lx=', lx, 'ly=',ly
! ^^^end tmp
!
!     increase grid points on large field
!      if((lx .gt. 200) .or. (ly  .gt. 200)) then
!         ngdpt = B_G_DPT
!      endif
!
! change imax and jmax size into subregion size by JG
!        case where lx > ly
      if ( lx .gt. ly)then
        imax  = int ( lx / dxmin)
        imax = min(imax,ngdpt)
        imax = max(imax,2)
!     calculate spacing for square or with barriers a rectangular grid
! Why do we need to minus 1 from max? JG
        ix  = lx / (imax - 1)

         if (nbr .gt. 0) then
           jmax  = int (ly / dxmin)
           jmax  = min(jmax, ngdpt)
         else
           jmax = anint(ly/ix) + 1
         endif

        jmax = max(jmax,2)
        jy   = ly/(jmax - 1)

!        case where lx = ly or lx < ly
      else
        jmax  = int (ly / dxmin)
        jmax = min(jmax,ngdpt)
        jmax = max(jmax,2)
        jy   = ly / (jmax - 1)

        if (nbr .gt. 0) then
           imax  = int (lx / dxmin)
           imax  = min(imax,ngdpt)
        else
           imax = anint(lx/jy) + 1
        endif
        imax = max(imax,2)
        ix = lx/(imax-1)

      endif
!
!     determine subregion of each grid point
!     for a single subregion now
      icsr = 1
      do  icsr = 1, nsubr
      do 20 j = 0, jmax
      do 10 i = 0, imax
!
!     for multiple subregions
!     Modified by JG on 7/8/12 
      
!     for multiple subregions
      if (i*ix .gt. amxsr(1,1,icsr) .and. i*ix .lt. amxsr(1,2,icsr)      &
     & .and. j*jy .gt. amxsr(2,1,icsr).and.j*jy.lt.amxsr(2,2,icsr)) then
            csr(i,j) = icsr
      end if
   10 continue
   20 continue
      end do
      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
