!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sberod_mod

  contains

    subroutine sberod (time, SURF_UPD_FLG, subrsurf, cellstate)

!     To calc loss/dep of saltation/creep, susp. and PM-10 at cells
!     To call sbqout to calc. qcso, qsso, q10o for each cell
!     To calc. deposition in the boundary cells of sim. region
!     To update the threshold friction velocity as the loose material
!         depletes upwind and increases downwind

      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate
      use grid_mod, only: i1, i2, i3, i4, i5, i6, sin_awa, cos_awa, tan_awa, imax, jmax, ix, jy
      use timer_mod, only: timer, TIMSBEROD, TIMSBQOUT, TIMSTART, TIMSTOP
      use process_mod, only: sbqout

!     +++ ARGUMENT DECLARATIONS +++
      real      time            ! time interval (seconds)
      integer   SURF_UPD_FLG    ! Surface update flag (1=on, 0=off)
      type(subregionsurfacestate), dimension(0:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ LOCAL VARIABLES +++
      integer :: i, j  ! grid cell x,y coordinates
      integer :: icsr  ! index of current subregion

      real :: la  ! used for length geometry
      real :: lb  ! used for length geometry
      real :: ld  ! used for length geometry

      real :: lx  ! effective field length in x direction
      real :: ly  ! effective field length in y direction

      real :: aa  ! discharge scalar
      real :: bb  ! discharge scalar
      real :: dd  ! discharge scalar

      ! these define values for each cell
      !real :: qcsi   ! input discharge (creep/saltation)
      !real :: qssi   ! input discharge (suspension)
      !real :: q10i   ! input discharge (pm10)
      !real :: qcso   ! output discharge (creep/saltation)
      !real :: qsso   ! output discharge (suspension)
      !real :: q10o   ! output discharge (pm10)
      real :: eg     ! accumulation (total), loss is negative
      real :: egcs   ! accumulation (creep/saltation), loss is negative
      real :: egss   ! accumulation (suspension), loss is negative
      real :: eg10   ! accumulation (pm10), loss is negative
      real :: qcsx(0:imax, 0:jmax)  ! discharge in x direction (creep/saltation)
      real :: qcsy(0:imax, 0:jmax)  ! discharge in y direction (creep/saltation)
      real :: qssx(0:imax, 0:jmax)  ! discharge in x direction (suspension)
      real :: qssy(0:imax, 0:jmax)  ! discharge in y direction (suspension)
      real :: q10x(0:imax, 0:jmax)  ! discharge in x direction (pm10)
      real :: q10y(0:imax, 0:jmax)  ! discharge in y direction (pm10)

      ! pm2.5 is a fixed fraction of pm10
      ! ref: Hongli Lia, John Tatarko, Matthew Kucharski, and Zhi Dong. 2014.
      ! PM-2.5 and PM-10 Emission from Agricultural Soils by Wind Erosion, Aeolian Research (Draft)
      real, parameter :: pm2_5_pm10 = 0.1693


!     +++ END SPECIFICATIONS +++

      ! set initial conditions to zero
      do 50 j = 0, jmax
      do 45 i = 0, imax
        qcsx(i,j)    = 0.
        qcsy(i,j)    = 0.
        qssx(i,j)  = 0.
        qssy(i,j)  = 0.
        q10x(i,j)  = 0.
        q10y(i,j)  = 0.
   45 continue
   50 continue

!     set a correction term
!      cc = (jy - ix)/ix
!*    set field length
!      lx = ix/(abs(sin_awa)+0.001)
!      if (lx .gt. max(ix,jy))then
!          lx = max(ix,jy)
!      endif

      ! grid length (lx): revised by LH 9-22-00
      if (abs(tan_awa) .le. (ix/jy)) then
         la = jy
         lb = abs(tan_awa*jy)
         ld = abs(jy/cos_awa)
      else
         ld = abs(ix/sin_awa)
         lb = ix
         la = sqrt(ld*ld - lb*lb)
      endif
       lx = ld*(1.0 - 0.292893*la*lb/(ix*jy))
       ly = ix*jy/lx

      ! update interior grid cells:
      do i = i1, i2, i3
        do j = i4, i5, i6

          ! calculate input discharge
          cellstate(i,j)%qcsi = (qcsx(i-i3,j)*jy + qcsy(i,j-i6)*ix)/ly
          cellstate(i,j)%qssi = (qssx(i-i3,j)*jy + qssy(i,j-i6)*ix)/ly
          cellstate(i,j)%q10i = (q10x(i-i3,j)*jy + q10y(i,j-i6)*ix)/ly

          ! calc. output discharge
          icsr = cellstate(i,j)%csr

          call timer(TIMSBEROD,TIMSTOP)
          call timer(TIMSBQOUT,TIMSTART)

          call sbqout (SURF_UPD_FLG, &
           cellstate(i,j)%wus, cellstate(i,j)%wust, cellstate(i,j)%wusp, cellstate(i,j)%sf10, cellstate(i,j)%sf84, &
           cellstate(i,j)%sf200, cellstate(i,j)%szcr, cellstate(i,j)%sfcr, cellstate(i,j)%sflos, cellstate(i,j)%smlos, &
           cellstate(i,j)%szrgh, subrsurf(icsr)%asxrgs, subrsurf(icsr)%sxprg, cellstate(i,j)%slrr, &
           subrsurf(icsr)%bsl(1)%asfcla, subrsurf(icsr)%bsl(1)%asfsan, &
           subrsurf(icsr)%bsl(1)%asfvfs, cellstate(i,j)%svroc, subrsurf(icsr)%abrsai, subrsurf(icsr)%abzht, &  !edit ljh 1-22-05  
           subrsurf(icsr)%abffcv, time, &
           subrsurf(icsr)%acanag, subrsurf(icsr)%acancr, subrsurf(icsr)%asf10an, &
           subrsurf(icsr)%asf10en, subrsurf(icsr)%asf10bk, &
           lx, cellstate(i,j)%qcsi, cellstate(i,j)%qssi, cellstate(i,j)%q10i, &
           cellstate(i,j)%dmlos, cellstate(i,j)%sf84mn, subrsurf(icsr)%sf84ic, subrsurf(icsr)%sf10ic, &  !edit ljh 1-22-05
           subrsurf(icsr)%bsl(1)%asvroc, cellstate(i,j)%smaglosmx, &
           cellstate(i,j)%qcso, cellstate(i,j)%qsso, cellstate(i,j)%q10o )

          call timer(TIMSBQOUT,TIMSTOP)
          call timer(TIMSBEROD,TIMSTART)

          ! update output accumulation arrays
          ! soil loss is negative:
          egcs = -time*(cellstate(i,j)%qcso - cellstate(i,j)%qcsi)/lx
          egss = -time*(cellstate(i,j)%qsso - cellstate(i,j)%qssi)/lx
          eg10 = -time*(cellstate(i,j)%q10o - cellstate(i,j)%q10i)/lx
          cellstate(i,j)%egt   = cellstate(i,j)%egt + egcs + egss
          cellstate(i,j)%egtcs = cellstate(i,j)%egtcs + egcs
          cellstate(i,j)%egtss = cellstate(i,j)%egtss + egss
          cellstate(i,j)%egt10 = cellstate(i,j)%egt10 + eg10
          cellstate(i,j)%egt2_5 = pm2_5_pm10 * cellstate(i,j)%egt10

          !* update discharge scalars
          aa = abs(-ix*cos_awa)
          bb = abs(-jy*sin_awa)
          dd = abs(aa)+abs(bb)

          qcsx(i,j) = cellstate(i,j)%qcso*ly*bb/(jy*dd)
          qcsy(i,j) = cellstate(i,j)%qcso*ly*aa/(ix*dd)
          qssx(i,j) = cellstate(i,j)%qsso*ly*bb/(jy*dd)
          qssy(i,j) = cellstate(i,j)%qsso*ly*aa/(ix*dd)
          q10x(i,j) = cellstate(i,j)%q10o*ly*bb/(jy*dd)
          q10y(i,j) = cellstate(i,j)%q10o*ly*aa/(ix*dd)

!          qcsx(i,j) = -qcso*sin_awa
!          qcsy(i,j)   = -(qcso + (qcso - qcsi)*cc)*cos_awa
!          qssx(i,j) = -qsso*sin_awa
!          qssy(i,j) = -(qsso + (qsso -qssi)*cc)*cos_awa
!          q10x(i,j) = -q10o*sin_awa
!          q10y(i,j) = -(q10o + (q10o - q10i)*cc)*cos_awa

          ! update salt/creep, suspension & pm-10 crossing boundary
          ! note the units are kg/m and different than interior cells and
          ! the meaning also differs.
          ! egt   = total discharge
          ! egtcs = salt/creep discharge
          ! egtss = suspension discharge
          ! egt10 = pm-10 discharge

          ! calculate scalar discharge crossing borders
          if (i .eq. i2) then
            cellstate(i2+i3,j)%egt = cellstate(i2+i3,j)%egt + time * (qcsx(i2, j) + qssx(i2, j))
            cellstate(i2+i3,j)%egtcs = cellstate(i2+i3,j)%egtcs + time*qcsx(i2, j)
            cellstate(i2+i3,j)%egtss = cellstate(i2+i3,j)%egtss + time*qssx(i2, j)
            cellstate(i2+i3,j)%egt10 = cellstate(i2+i3,j)%egt10 + time*q10x(i2, j)
            cellstate(i2+i3,j)%egt2_5 = pm2_5_pm10 * cellstate(i2+i3,j)%egt10
          endif
          if (j .eq. i5) then
            cellstate(i,i5+i6)%egt = cellstate(i,i5+i6)%egt + time * (qcsy(i,i5) + qssy(i,i5))
            cellstate(i,i5+i6)%egtcs = cellstate(i,i5+i6)%egtcs + time*qcsy(i,i5)
            cellstate(i,i5+i6)%egtss = cellstate(i,i5+i6)%egtss + time*qssy(i,i5)
            cellstate(i,i5+i6)%egt10 = cellstate(i,i5+i6)%egt10 + time*q10y(i,i5)
            cellstate(i,i5+i6)%egt2_5 = pm2_5_pm10 * cellstate(i,i5+i6)%egt10
          endif

        end do
      end do

    end subroutine sberod

    subroutine sbinit( subrsurf, cellstate )

!     +++ purpose +++
!     Input subregion values of variables from other submodels
!     to the grid points of the erosion submodel which erosion changes
!     Calc. soil fraction of 4 dia. from asd, & rr shelter angles

      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, awzypt
      use grid_mod, only: imax, jmax
      use p1erode_def, only: SLRR_MIN, SLRR_MAX
      use process_mod, only: sbpm10, sbsfdi

!     + + + ARGUEMENT DECLARATIONS + + +
      type(subregionsurfacestate), dimension(0:), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     + + + LOCAL VARIABLES + + +
      integer :: i, j  ! grid cell x,y coordinates
      integer :: icsr  ! index of current subregion

!     + + + END SPECIFICATION + + +

      ! set abrasion and particle size parameters
      do icsr = 1, size(subrsurf)-1
         call sbsfdall( subrsurf(icsr) )
      end do

      do j = 1, jmax-1
        do i = 1, imax-1

          ! determine subregion
          icsr = cellstate(i,j)%csr

          ! input variables to grid cells
          cellstate(i,j)%sf1 = subrsurf(icsr)%sfd1
          cellstate(i,j)%sf10 = subrsurf(icsr)%sfd10
          cellstate(i,j)%sf84 = subrsurf(icsr)%sfd84
          cellstate(i,j)%sf200 = subrsurf(icsr)%sfd200
          ! edit ljh - 1-22-04
          cellstate(i,j)%svroc = subrsurf(icsr)%bsl(1)%asvroc    ! if ifc has surface rock, 1st index maybe 0.

          cellstate(i,j)%szcr = subrsurf(icsr)%aszcr
          cellstate(i,j)%sfcr = subrsurf(icsr)%asfcr
          cellstate(i,j)%smlos = subrsurf(icsr)%asmlos
          cellstate(i,j)%sflos = subrsurf(icsr)%asflos

          cellstate(i,j)%szrgh = subrsurf(icsr)%aszrgh

          ! initialize RR values for each grid cell
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

        end do
      end do

    end subroutine sbinit

    subroutine sbwind( wustfl, awu, ntstep, intstep, rusust, subrsurf, cellstate)

!     +++ PURPOSE +++
!     to update wzzo at each grid point;
!     To update  soil friction velocity on each grid point
!     and modify it for barriers and hills;
!     To initialize en thresh. and cp thresh. fr. velocites on grid;
!     To calculate max ratios of friction velocity to threshold
!     friction velocity

      use erosion_data_struct_defs, only: subregionsurfacestate, cellsurfacestate, anemht, awzzo, wzoflg
      use grid_mod, only: imax, jmax
      use barriers_mod, only: barrier
      use wind_mod, only: biodrag, sbzo, sbwus
      use process_mod, only: sbwust, sbaglos

!     +++ ARGUMENT DECLARATIONS +++
      integer, intent(in) :: wustfl
      integer, intent(in) :: intstep  ! current index of ntstep thru time
      integer, intent(in) :: ntstep   ! max. no. of time steps in day
      real, intent(in) :: awu       ! input wind speed driving EROSION submodel (m/s).
      real, intent(out) :: rusust    ! max ratio of friction velocity to thresh. friction vel.
      type(subregionsurfacestate), dimension(0:), intent(in) :: subrsurf  ! subregion surface conditions (erosion specific set)
      type(cellsurfacestate), dimension(0:,0:), intent(inout) :: cellstate     ! initialized grid cell state values

!     +++ LOCAL VARIABLES +++
      integer i,j, k
      integer :: icsr     ! index of current subregion.
      integer :: ipool    ! index of brcdInput pool
      real wzorg, wzorr, wzzo, wzzov
      real at, rintstep, brcd
      real wubsts, wucsts, wucwts, wucdts, sfcv ! these are placeholders in call to sbwust are are not used anywhere else.

!     + + + END SPECIFICATIONS + + +

      rusust = 0.1

      ! loop through grid interior to update
      do i = 1, imax-1
        do j = 1, jmax-1

          ! assign subregion index for grid point
          icsr = cellstate(i,j)%csr

          ! accumulate biodrag components
          brcd = 0.0
          do ipool = 1, subrsurf(icsr)%npools
            ! calculate "effective" biomass drag coefficient
            brcd = brcd + biodrag( 0.0, 0.0, subrsurf(icsr)%brcdInput(ipool)%rlai, subrsurf(icsr)%brcdInput(ipool)%rsai, &
                                   subrsurf(icsr)%brcdInput(ipool)%rg, subrsurf(icsr)%brcdInput(ipool)%xrow, &
                                   subrsurf(icsr)%brcdInput(ipool)%zht, cellstate(i,j)%szrgh )
          end do

          call sbzo( subrsurf(icsr)%sxprg, cellstate(i,j)%szrgh, cellstate(i,j)%slrr, subrsurf(icsr)%abzht, brcd, &
                     wzoflg, wzorg, wzorr, wzzo, wzzov, awzzo )

          ! update surface (below canopy) friction velocity
          cellstate(i,j)%wus = sbwus( anemht, awzzo, awu, wzzov, brcd )

          ! correct friction velocity for hills
          ! if (nhill .ne. 0 ) then
          !   cellstate(i,j)%wus = cellstate(i,j)%wus * w0hill(i,j)
          ! endif

          ! correct friction velocity for barriers
          if ( allocated(barrier) ) then
            cellstate(i,j)%wus = cellstate(i,j)%wus * cellstate(i,j)%w0br
          endif

          if (wustfl .eq. 1) then
            ! update threshold friction velocities and loose erodible material
            ! calculate hour k for surface water content
            rintstep = intstep
            k = int(rintstep*23.75/ntstep) + 1

            call sbwust( cellstate(i,j)%sf84, subrsurf(icsr)%bsl(1)%asdagd, cellstate(i,j)%sfcr, cellstate(i,j)%svroc, &
                 cellstate(i,j)%sflos, subrsurf(icsr)%abffcv, wzzo, subrsurf(icsr)%ahrwc0(k), subrsurf(icsr)%bsl(1)%ahrwcw, &
                 subrsurf(icsr)%sf84ic, subrsurf(icsr)%bsl(1)%asvroc, & 
                 cellstate(i,j)%wust, cellstate(i,j)%wusp, cellstate(i,j)%wusto, &
                 wubsts, wucsts, wucwts, wucdts, sfcv)

            ! calculate: smaglosmx; update: smaglos, sf84mn
            call sbaglos( cellstate(i,j)%wus, cellstate(i,j)%wust, cellstate(i,j)%wusto, &
                          subrsurf(icsr)%sf84ic, subrsurf(icsr)%bsl(1)%asvroc, &
                          cellstate(i,j)%smaglosmx, cellstate(i,j)%smaglos, cellstate(i,j)%sf84mn, cellstate(i,j)%sf84 )

          endif

          at = cellstate(i,j)%wus/cellstate(i,j)%wust
          rusust = max(rusust, at)

        end do
      end do

    end subroutine sbwind

    subroutine sbsfdall( subrsurf )

      ! Set the subregion values of dependent variables for abrasion and particle sizess

      use erosion_data_struct_defs, only: subregionsurfacestate, awzypt
      use process_mod, only: sbpm10, sbsfdi

!     + + + ARGUEMENT DECLARATIONS + + +
      type(subregionsurfacestate), intent(inout) :: subrsurf  ! subregion surface conditions (erosion specific set)

!     + + + END SPECIFICATION + + +

      ! calculate abrasion and pm10 parameters    edit LH 3-4-05
      call sbpm10( subrsurf%bsl(1)%aseags, subrsurf%asecr, subrsurf%bsl(1)%asfcla, &
           subrsurf%bsl(1)%asfsan, awzypt, subrsurf%acanag, subrsurf%acancr, &
           subrsurf%asf10an, subrsurf%asf10en, subrsurf%asf10bk )

      ! calculate fraction less than diameter from asd
      call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
           subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 0.01, subrsurf%sfd1 )
      ! store initial sf1
      subrsurf%sf1ic = subrsurf%sfd1

      call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
           subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 0.1, subrsurf%sfd10 )
      ! store initial sf10
      subrsurf%sf10ic = subrsurf%sfd10

      call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
           subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 0.84, subrsurf%sfd84 )
      ! store initial sf84
      subrsurf%sf84ic = subrsurf%sfd84
      subrsurf%sf84ic = min(0.9999, max(subrsurf%sf84ic,0.0001))            !set limits

      call sbsfdi( subrsurf%bsl(1)%aslagm, subrsurf%bsl(1)%as0ags, &
           subrsurf%bsl(1)%aslagn, subrsurf%bsl(1)%aslagx, 2.0, subrsurf%sfd200 )
      ! store initial sf200
      subrsurf%sf200ic = subrsurf%sfd200
      subrsurf%sf200ic = min(0.9999, max(subrsurf%sf200ic,0.0001))            !set limits

    end subroutine sbsfdall

end module sberod_mod





