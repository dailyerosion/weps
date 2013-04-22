!$Author$
!$Date$
!$Revision$
!$HeadURL$
!**********************************************************************
!     subroutine sbinit
!**********************************************************************

      subroutine sbinit( subrsurf, cellstate )

!     +++ purpose +++
!     Input subregion values of variables from other submodels
!     to the grid points of the erosion submodel which erosion changes
!     Calc. soil fraction of 4 dia. from asd, & rr shelter angles

      use weps_interface_defs
      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, awzypt
      use grid_mod, only: imax, jmax
      use p1erode_def, only: SLRR_MIN, SLRR_MAX

!     + + + ARGUEMENT DECLARATIONS + + +
      type(subregionsurfacestate), dimension(:), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(out) :: cellstate     ! initialized grid cell state values

!     + + + LOCAL VARIABLES + + +
      integer  icsr, i, j

!     + + + LOCAL VARIABLE DEFINITIONS + + +
!     icsr  = index of current subregion
!     i,j   = grid cell x,y coordinates

!     + + + SUBROUTINES CALLED + + +
!     sbsfdi
!     sbpm10
!     + + + END SPECIFICATION + + +

!     calculate abrasion and pm10 parameters    edit LH 3-4-05
      do icsr = 1, size(subrsurf)
         call sbpm10( subrsurf(icsr)%bsl(1)%aseags, subrsurf(icsr)%asecr, subrsurf(icsr)%bsl(1)%asfcla, &
              subrsurf(icsr)%bsl(1)%asfsan, awzypt, subrsurf(icsr)%acanag, subrsurf(icsr)%acancr, &
              subrsurf(icsr)%asf10an, subrsurf(icsr)%asf10en, subrsurf(icsr)%asf10bk )

         ! calculate fraction less than diameter from asd
         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 0.01, subrsurf(icsr)%sfd1 )
         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 0.1, subrsurf(icsr)%sfd10 )
         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 0.84, subrsurf(icsr)%sfd84 )
         ! store initial sf84
         subrsurf(icsr)%sf84ic = subrsurf(icsr)%sfd84
         subrsurf(icsr)%sf84ic = min(0.9999, max(subrsurf(icsr)%sf84ic,0.0001))            !set limits
         ! store initial sf10
         subrsurf(icsr)%sf10ic = subrsurf(icsr)%sfd10

         call sbsfdi( subrsurf(icsr)%bsl(1)%aslagm, subrsurf(icsr)%bsl(1)%as0ags, &
              subrsurf(icsr)%bsl(1)%aslagn, subrsurf(icsr)%bsl(1)%aslagx, 2.0, subrsurf(icsr)%sfd200 )
      end do

      do 20 j = 1, jmax-1
      do 10 i = 1, imax-1

!     determine subregion
      icsr = cellstate(i,j)%csr
!     input variables to grid cells
      cellstate(i,j)%sf1 = subrsurf(icsr)%sfd1
      cellstate(i,j)%sf10 = subrsurf(icsr)%sfd10
      cellstate(i,j)%sf84 = subrsurf(icsr)%sfd84
      cellstate(i,j)%sf200 = subrsurf(icsr)%sfd200
!     edit ljh - 1-22-04
      cellstate(i,j)%svroc = subrsurf(icsr)%bsl(1)%asvroc    ! if ifc has surface rock, 1st index maybe 0.
!
      cellstate(i,j)%szcr = subrsurf(icsr)%aszcr
      cellstate(i,j)%sfcr = subrsurf(icsr)%asfcr
      cellstate(i,j)%smlos = subrsurf(icsr)%asmlos
      cellstate(i,j)%sflos = subrsurf(icsr)%asflos
!
      cellstate(i,j)%szrgh = subrsurf(icsr)%aszrgh

      !initialize RR values for each grid cell
      cellstate(i,j)%slrr = subrsurf(icsr)%aslrr

      if (cellstate(i,j)%slrr < SLRR_MIN) then
          cellstate(i,j)%slrr = SLRR_MIN
      else if (cellstate(i,j)%slrr > SLRR_MAX) then
          cellstate(i,j)%slrr = SLRR_MAX
      endif

      cellstate(i,j)%dmlos = 0.0
      cellstate(i,j)%smaglos = 0.0
      cellstate(i,j)%smaglosmx = 0.0
      cellstate(i,j)%sf84mn = 0.0

   10 continue
   20 continue

      return
      end
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
