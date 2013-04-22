!$Author$
!$Date$
!$Revision$
!$HeadURL$
!***********************************************************************
!*     subroutine sbwind
!***********************************************************************
      subroutine sbwind( wustfl, awu, ntstep, intstep, rusust, subrsurf, cellstate)

!     +++ PURPOSE +++
!     to update wzzo at each grid point;
!     To update  soil friction velocity on each grid point
!     and modify it for barriers and hills;
!     To initialize en thresh. and cp thresh. fr. velocites on grid;
!     To calculate max ratios of friction velocity to threshold
!     friction velocity

      use weps_interface_defs
      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, anemht, awzzo, wzoflg
      use grid_mod, only: kbr, imax, jmax
      use barriers_mod, only: barrier

!     +++ ARGUMENT DECLARATIONS +++
      integer wustfl,intstep, ntstep
      real awu, rusust
      type(subregionsurfacestate), dimension(:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ ARGUMENT DEFINITIONS +++
!     intstep  - current index of ntstep thru time
!     ntstep   - max. no. of time steps in day
!     icsr     - index of current subregion.
!     wusp     - subregion soil threshold friction vel. trans. cap. (m/s)
!     rusust    - max ratio of friction velocity to thresh. friction vel.
!     imax     - no. grid intervals in x-direction.
!     jmax     - no. grid intervals in y-direction.
!     wus      - soil friction velocity at grid points corrected for
!                hills and barriers (m/s).
!     wust     - threshold fr. vel. for en. at grid points
!     wusp     - threshold fr. vel. for trans. cap. at grid points

!     +++ LOCAL VARIABLES +++
      integer i,j, icsr,k
      real wzorg, wzorr, wzzo, wzzov
      real at, rintstep, brcd
      real wubsts, wucsts, wucwts, wucdts, sfcv ! these are placeholders in call to sbwust are are not used anywhere else.
!
!     +++ SUBROUTINES CALLED
!     sbzo
!     sbwus
!     sbwust
!
!     + + + END SPECIFICATIONS + + +

      rusust = 0.1

!     loop through grid interior to update
      do 40 i = 1, imax-1
      do 30 j = 1, jmax-1

      ! assign subregion index for grid point
      icsr = cellstate(i,j)%csr

!     update aerodynamic roughness
! ^^^ tmp out
!     write (*,*) 'in sbwind, call to sbzo'

      call sbzo( subrsurf(icsr)%sxprg, cellstate(i,j)%szrgh, cellstate(i,j)%slrr, &
           wzoflg, subrsurf(icsr)%adrlaitot, subrsurf(icsr)%adrsaitot, subrsurf(icsr)%abzht,  &
           subrsurf(icsr)%acrlai, subrsurf(icsr)%acrsai, subrsurf(icsr)%aczht, &
           subrsurf(icsr)%acxrow, subrsurf(icsr)%ac0rg, wzorg, wzorr, &
           wzzo, wzzov, awzzo, brcd)

! ^^^ tmp out
!      write (*,*) 'in sbwind, call to sbwus'
!     update surface (below canopy) friction velocity
 
      call sbwus (anemht, awzzo, awu, wzzov, brcd, cellstate(i,j)%wus )

!     correct friction velocity for hills
!      if (nhill .ne. 0 ) then
!           cellstate(i,j)%wus = cellstate(i,j)%wus * w0hill(i,j,kbr)
!        endif

      ! correct friction velocity for barriers
      if ( allocated(barrier) ) then
         cellstate(i,j)%wus = cellstate(i,j)%wus * cellstate(i,j)%w0br(1)
      endif

      if (wustfl .eq. 1) then
        ! update threshold friction velocities
        ! calculate hour k for surface water content
        rintstep = intstep
        k = int(rintstep*23.75/ntstep) + 1

        call sbwust( cellstate(i,j)%sf84, subrsurf(icsr)%bsl(1)%asdagd, cellstate(i,j)%sfcr, cellstate(i,j)%svroc, &
             cellstate(i,j)%sflos, subrsurf(icsr)%abffcv, wzzo, subrsurf(icsr)%ahrwc0(k), subrsurf(icsr)%bsl(1)%ahrwcw, &
             cellstate(i,j)%wus, subrsurf(icsr)%sf84ic, subrsurf(icsr)%bsl(1)%asvroc, cellstate(i,j)%dmlos, & 
             cellstate(i,j)%wust, cellstate(i,j)%wusp, cellstate(i,j)%wusto, cellstate(i,j)%sf84mn, cellstate(i,j)%smaglos, &
             cellstate(i,j)%smaglosmx, wubsts, wucsts, wucwts, wucdts, sfcv)

! ^^^ tmp out
!      if( cellstate(i,j)%wust .le. 0.0 ) then
!       write(*,*) "sbwind: i,j", i, j
!       write(*,*) "sbwind: cellstate(i,j)%wus, cellstate(i,j)%wust, rusust",                &
!     &            cellstate(i,j)%wus, wust(i,j), rusust
!       write(*,*) "sf84(i,j) = ", cellstate(i,j)%sf84
!       write(*,*) "subrsurf(icsr)%bsl(1)%asdagd", subrsurf(icsr)%bsl(1)%asdagd
!       write(*,*) "sfcr(i,j)", cellstate(i,j)%sfcr
!       write(*,*) "svroc(i,j)", cellstate(i,j)%svroc
!       write(*,*) "sflos(i,j) ", cellstate(i,j)%sflos
!       write(*,*) "subrsurf(icsr)%abffcv", subrsurf(icsr)%abffcv
!       write(*,*) "wzzo", wzzo
!       write(*,*) "subrsurf(icsr)%ahrwc0(k)", subrsurf(icsr)%ahrwc0(k)
!       write(*,*) "subrsurf(icsr)%bsl(1)%ahrwcw", subrsurf(icsr)%bsl(1)%ahrwcw
!       write(*,*) "cellstate(i,j)%wus", cellstate(i,j)%wus
!       write(*,*) "sf84ic", subrsurf(icsr)%sf84ic
!       write(*,*) "rusust", rusust
!       write(*,*) "asvroc(1,1)", asvroc(1,1)
!       write(*,*) "dmlos(i,j)", cellstate(i,j)%dmlos
!       write(*,*) "cellstate(i,j)%wust", cellstate(i,j)%wust
!       write(*,*) "cellstate(i,j)%wusp", cellstate(i,j)%wusp
!       write(*,*) "cellstate(i,j)%sf84mn", cellstate(i,j)%sf84mn
!       write(*,*) "cellstate(i,j)%smaglos", cellstate(i,j)%smaglos
!       stop
!      end if

      endif

      at = cellstate(i,j)%wus/cellstate(i,j)%wust
      rusust = amax1(rusust, at)

   30 continue
   40 continue

!     write (*,*) 'at exit sbwind rusust =', rusust
!     write (*,*) ' cellstate(3,3)%wus, cellstate(3,3)%wust', cellstate(3,3)%wus, cellstate(3,3)%wust

      return
      end
!++++ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
