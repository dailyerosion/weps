!$Author$
!$Date$
!$Revision$
!$HeadURL$

module grid_mod
    use Points_Mod, only: point
    use flib_sax
    use read_write_xml_mod, only: read_param
    use erosion_data_struct_defs, only: create_cellsurfacestate, cellstate

    implicit none

    integer :: i1, i2, i3  ! do loop parameters defining grid update sequence, i1..i3 defines second update directions
    integer :: i4, i5, i6  ! do loop parameters defining grid update sequence, i4..i6 defines first update direction

    real :: awa            ! wind angle across simulation region relative to Y-axis
    real :: sin_awa        ! precomputed sin of awa angle
    real :: cos_awa        ! precomputed cos of awa angle
    real :: tan_awa        ! precomputed tan of awa angle

    integer :: imax             ! Number of grid intervals in x direction on EROSION grid
    integer :: jmax             ! Number of grid intervals in y direction on EROSION grid
    real :: lencell_x                  ! grid interval in x-direction (m) (range from 7.0 to 3000)
    real :: lencell_y                  ! grid interval in y-direction (m) (range from 7.0 to 3000)

    ! added for command line or alternate specification of grid dimensions
    integer :: xgdpt            ! specified # of grid data points in the x-dir
    integer :: ygdpt            ! specified # of grid data points in the y-dir

    ! original Hagen grid dimensions
    integer, parameter :: N_G_DPT = 30      ! # of grid data points under no barrier cond.
    integer, parameter :: B_G_DPT = 60      ! # of grid data points to use if barrier exists
    real, parameter :: MIN_GRID_SP = 7.0    ! minimum targeted grid spacing (m)

    type(point), dimension(2) :: amxsim ! Coordinates of two diagonally opposite points for a rectangular simulation region.
    real :: sim_area            ! sim_area - Simulation Region area (m^2)
    real :: amasim              ! Field angle (degrees) (0 to 360) the angle of the simulation region boundary relative to north.

    character(len=512) :: gridfile  ! name of the grid input file
    logical :: griddata_complete
    logical, dimension(:,:), allocatable :: gridnum_complete
    logical, dimension(:,:), allocatable :: subrnum_complete
    logical, dimension(:,:), allocatable :: elevnum_complete
    integer :: igx       ! index for grid x direction
    integer :: jgy       ! index for grid y direction
    integer :: count_complete

    integer, parameter :: MAX_NAME_LEN  = 40

    type :: tag_def
      character(len=MAX_NAME_LEN)  :: name   ! tag name
      logical :: required                    ! .true. if tag is required
      logical :: acquired                    ! .true. if tag has been read
      logical :: in_tag                      ! .true. if inside tag now
    end type tag_def

    type(tag_def), dimension(:), allocatable :: grid_tag
    integer :: max_tags
  
    integer, parameter :: gridData = 1
    integer, parameter :: SCI_XGrid = 2 
    integer, parameter :: SCI_YGrid = 3
    integer, parameter :: SCI_Xindex = 4
    integer, parameter :: SCI_Yindex = 5
    integer, parameter :: SCI_SubrNum = 6
    integer, parameter :: SCI_Elevation = 7

  contains

    subroutine init_grid_xml()

      integer :: idx
      integer :: alloc_stat

      max_tags = 7   ! count of unique tags needed from all dtd files
      allocate( grid_tag(max_tags), stat=alloc_stat)
      if( alloc_stat .gt. 0 ) then
        write(*,*) 'ERROR: memory alloc., grid_tag'
      end if

      ! assign defaults to flag status values
      do idx = 1, max_tags
        grid_tag(idx)%required = .true.
        grid_tag(idx)%acquired = .false.
        grid_tag(idx)%in_tag = .false.
      end do

      ! assign tag names
      grid_tag(1)%name = "gridData"
      grid_tag(2)%name = "SCI_XGrid"
      grid_tag(3)%name = "SCI_YGrid"
      grid_tag(4)%name = "SCI_Xindex"
      grid_tag(5)%name = "SCI_Yindex"
      grid_tag(6)%name = "SCI_SubrNum"
      grid_tag(7)%name = "SCI_Elevation"

    end subroutine init_grid_xml

    subroutine write_grid( daypath )
      use file_io_mod, only: fopenk
      use read_write_xml_mod, only: w_begin_tag, w_end_tag, w_whole_tag
      character(len=*) :: daypath

      integer :: idx
      integer :: jdy
      integer :: dealloc_stat
      integer :: luo_saeinp     ! output unit number

      call init_grid_xml()  ! creates grid_tag array

      ! write single subregion grid array file
      call fopenk (luo_saeinp, (trim(daypath) // trim(gridfile)), 'unknown')

      ! write XML header
      write(luo_saeinp,"(a)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
      write(luo_saeinp,"(a)") '<!DOCTYPE sweepData SYSTEM "grid.dtd">'

      ! these are the actual values used in the simulation, not same as in .sweep file
      call w_begin_tag( luo_saeinp, grid_tag(gridData)%name, &
                                    grid_tag(SCI_XGrid)%name, imax-1, &
                                    grid_tag(SCI_YGrid)%name, jmax-1 )
        do idx = 1, imax - 1
          do jdy = 1, jmax - 1
            ! NOTE: indexes adjusted to zero based
            call w_whole_tag( luo_saeinp, grid_tag(SCI_SubrNum)%name, &
                                    grid_tag(SCI_Xindex)%name, idx-1, &
                                    grid_tag(SCI_Yindex)%name, jdy-1, &
                                    cellstate(idx,jdy)%csr-1 )
            call w_whole_tag( luo_saeinp, grid_tag(SCI_Elevation)%name, &
                                    grid_tag(SCI_Xindex)%name, idx-1, &
                                    grid_tag(SCI_Yindex)%name, jdy-1, &
                                    cellstate(idx,jdy)%elevation )
          end do
        end do
      call w_end_tag( luo_saeinp, grid_tag(gridData)%name )

      close(luo_saeinp)

      ! deallocate Tag array
      deallocate( grid_tag, stat=dealloc_stat)
      if( dealloc_stat .gt. 0 ) then
        ! deallocation failed
        write(*,*) "ERROR: unable to deallocate memory for Grid Tag array"
      end if

    end subroutine write_grid

    subroutine begin_griddata_element_handler(name,attributes)
      character(len=*), intent(in)   :: name
      type(dictionary_t), intent(in) :: attributes

      integer :: idx
      integer :: jdx
      integer :: kdx
      character(len=80) :: param_value
      integer :: ret_stat
      integer :: sum_stat
      integer :: alloc_stat

      !write(*,*) ">>Begin Element: ", name
      !write(*,*) "--- ", len(attributes), " attributes:"
      !call print_dict(attributes)

      do idx = 1, size(grid_tag)
        if( grid_tag(idx)%name .eq. name ) then
          grid_tag(idx)%in_tag = .true.
          ! write(*,*) 'In tag ', trim(name)
          exit  ! found tag, no need to look further
        end if
      end do

      if (   (idx .eq. gridData) ) then
        if ( has_key(attributes, grid_tag(SCI_XGrid)%name) ) then
          call get_value(attributes, grid_tag(SCI_XGrid)%name, param_value, ret_stat)
          call read_param(grid_tag(SCI_XGrid)%name, param_value, imax)
          ! erod.sweep is read first so amxsim values are set
          lencell_x = (amxsim(2)%x - amxsim(1)%x) / imax
          ! grid array has boundary cell on edges, so with dimension goes from 0 to XGrid + 1
          imax = imax + 1
          !write(*,*) 'XGrid Index: ', imax
        else
          write(*,*) 'SCI_XGrid attribute required for each ', trim(grid_tag(idx)%name), ' Tag.'
          call exit(1)
        end if
        if ( has_key(attributes, grid_tag(SCI_YGrid)%name) ) then
          call get_value(attributes, grid_tag(SCI_YGrid)%name, param_value, ret_stat)
          call read_param(grid_tag(SCI_YGrid)%name, param_value, jmax)
          ! erod.sweep is read first so amxsim values are set
          lencell_y = (amxsim(2)%y - amxsim(1)%y) / jmax
          ! grid array has boundary cell on edges, so with dimension goes from 0 to XGrid + 1
          jmax = jmax + 1
          !write(*,*) 'YGrid Index: ', jmax

          ! allocate gridnum_complete
          sum_stat = 0
          allocate(gridnum_complete(imax-1,jmax-1), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(subrnum_complete(imax-1,jmax-1), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(elevnum_complete(imax-1,jmax-1), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          if( sum_stat .gt. 0 ) then
            Write(*,*) 'ERROR: unable to allocate enough memory for gridnum_complete data array'
          else
            do kdx = 1, imax - 1
              do jdx = 1, jmax - 1
                gridnum_complete(kdx,jdx) = .false.
                subrnum_complete(kdx,jdx) = .false.
                elevnum_complete(kdx,jdx) = .false.
              end do
            end do
          end if
          ! allocate cellstate
          call create_cellsurfacestate( imax, jmax )
        else
          write(*,*) 'SCI_YGrid attribute required for each ', trim(grid_tag(idx)%name), ' Tag.'
          call exit(1)
        end if

      else if ( idx .eq. SCI_SubrNum ) then
        if ( has_key(attributes, grid_tag(SCI_Xindex)%name) ) then
          call get_value(attributes, grid_tag(SCI_Xindex)%name, param_value, ret_stat)
          call read_param(grid_tag(SCI_Xindex)%name, param_value, igx)
          ! adjust from 0 based array to 1 based array
          igx = igx + 1
          !write(*,*) 'SubrNum Xindex: ', igx
        else
          write(*,*) 'SCI_Xindex attribute required for each ', trim(grid_tag(idx)%name), ' Tag.'
          call exit(1)
        end if
        if ( has_key(attributes, grid_tag(SCI_Yindex)%name) ) then
          call get_value(attributes, grid_tag(SCI_Yindex)%name, param_value, ret_stat)
          call read_param(grid_tag(SCI_Yindex)%name, param_value, jgy)
          ! adjust from 0 based array to 1 based array
          jgy = jgy + 1
          !write(*,*) 'SubrNum Yindex: ', jgy
        else
          write(*,*) 'SCI_Yindex attribute required for each ', trim(grid_tag(idx)%name), ' Tag.'
          call exit(1)
        end if

      else if ( idx .eq. SCI_Elevation ) then
        if ( has_key(attributes, grid_tag(SCI_Xindex)%name) ) then
          call get_value(attributes, grid_tag(SCI_Xindex)%name, param_value, ret_stat)
          call read_param(grid_tag(SCI_Xindex)%name, param_value, igx)
          ! adjust from 0 based array to 1 based array
          igx = igx + 1
          !write(*,*) 'SubrNum Xindex: ', igx
        else
          write(*,*) 'SCI_Xindex attribute required for each ', trim(grid_tag(idx)%name), ' Tag.'
          call exit(1)
        end if
        if ( has_key(attributes, grid_tag(SCI_Yindex)%name) ) then
          call get_value(attributes, grid_tag(SCI_Yindex)%name, param_value, ret_stat)
          call read_param(grid_tag(SCI_Yindex)%name, param_value, jgy)
          ! adjust from 0 based array to 1 based array
          jgy = jgy + 1
          !write(*,*) 'SubrNum Yindex: ', jgy
        else
          write(*,*) 'SCI_Yindex attribute required for each ', trim(grid_tag(idx)%name), ' Tag.'
          call exit(1)
        end if
      end if

    end subroutine begin_griddata_element_handler

    subroutine end_griddata_element_handler(name)
      character(len=*), intent(in)     :: name

      integer :: idx
      integer :: jdx
      integer :: kdx
      integer :: sum_stat
      integer :: dealloc_stat

      do idx = 1, size(grid_tag)
        if( grid_tag(idx)%name .eq. name ) then
          grid_tag(idx)%in_tag = .false.
          ! write(*,*) 'In tag ', trim(name)

          if (idx .eq. gridData) then

            ! check for acquisition of all required elements
            count_complete = 0
            do kdx = 1, imax - 1
              do jdx = 1, jmax - 1
                if (subrnum_complete(kdx,jdx)) then
!                  if  (elevnum_complete(kdx,jdx)) then
                    gridnum_complete(kdx,jdx) = .true.
!                  else
!                    write(*,'(3a,i0,a,i0,a)') 'Tag ', trim(grid_tag(SCI_Elevation)%name), &
!                                    ' SCI_Xindex="', kdx-1, &
!                                    ' SCI_Yindex="', jdx-1, ' is missing from input file.'
!                  end if
                else
                  write(*,'(3a,i0,a,i0,a)') 'Tag ', trim(grid_tag(SCI_SubrNum)%name), &
                                  ' SCI_Xindex="', kdx-1, &
                                  ' SCI_Yindex="', jdx-1, ' is missing from input file.'
                end if
                

                if (gridnum_complete(kdx,jdx)) then
                  count_complete = count_complete + 1
                end if
              end do
            end do
            ! deallocate _complete array
            sum_stat = 0
            deallocate(gridnum_complete, stat=dealloc_stat)
            sum_stat = sum_stat + dealloc_stat
            deallocate(subrnum_complete, stat=dealloc_stat)
            sum_stat = sum_stat + dealloc_stat
            deallocate(elevnum_complete, stat=dealloc_stat)
            sum_stat = sum_stat + dealloc_stat
            if( sum_stat .gt. 0 ) then
              ! deallocation failed
              write(*,*) "ERROR: unable to deallocate memory for gridnum_complete arrays"
            end if

            if( count_complete .eq. (imax-1)*(jmax-1) ) then
              griddata_complete = .true.
            else
              griddata_complete = .false.
            end if
            ! deallocate Tag array
            deallocate( grid_tag, stat=dealloc_stat)
            if( dealloc_stat .gt. 0 ) then
              ! deallocation failed
              write(*,*) "ERROR: unable to deallocate memory for Tag array"
            end if
          else if( (idx .eq. SCI_SubrNum) ) then
            if ( grid_tag(SCI_SubrNum)%acquired ) then
              grid_tag(SCI_SubrNum)%acquired = .false.
              subrnum_complete(igx,jgy) = .true.
            else
              subrnum_complete(igx,jgy) = .false.
            end if
          else if( (idx .eq. SCI_Elevation) ) then
            if ( grid_tag(SCI_Elevation)%acquired ) then
              grid_tag(SCI_Elevation)%acquired = .false.
              elevnum_complete(igx,jgy) = .true.
            else
              elevnum_complete(igx,jgy) = .false.
            end if
          end if

          exit  ! found tag, no need to look further

        end if
      end do

    end subroutine end_griddata_element_handler

    subroutine pcdata_griddata_chunk_handler(chunk)
      character(len=*), intent(in) :: chunk

      character(len=80) :: param_value

      param_value = trim(chunk)

      if (grid_tag(gridData)%in_tag) then
        if (grid_tag(SCI_SubrNum)%in_tag) then
          call read_param(grid_tag(SCI_SubrNum)%name, param_value, cellstate(igx,jgy)%csr)
          ! adjust from 0 based to 1 based array
          cellstate(igx,jgy)%csr = cellstate(igx,jgy)%csr + 1
          !write(*,*) 'Subregion igx, iy, Number: ', igx, iy, cellstate(igx,jgy)%csr
          grid_tag(SCI_SubrNum)%acquired = .true.
        else if (grid_tag(SCI_Elevation)%in_tag) then
          call read_param(grid_tag(SCI_Elevation)%name, param_value, cellstate(igx,jgy)%elevation)
          !write(*,*) 'Subregion igx, iy, Number: ', igx, iy, cellstate(igx,jgy)%elevation
          grid_tag(SCI_Elevation)%acquired = .true.
        end if
      end if

    end subroutine pcdata_griddata_chunk_handler

    subroutine sbgrid( minht, elevation )

      ! +++ PURPOSE +++
      ! to calculate grid size and spacing for EROSION.
      ! grid size assumes outer points are outside field boundary a distance lencell_x/2
      ! to calculate number of grid points for EROSION.
      ! A max 'interior' square grid of 29X29 is assigned-no barriers
      ! A max 'interior' rectangular grid of 59X59 is assigned barriers

      ! +++ ARGUMENTS +++
      real :: minht    ! minimum height of barrier along it's length.
      real :: elevation ! elevation of simulation region

      ! +++ LOCAL VARIABLES +++
      integer :: ngdpt ! number of grid points along a direction
      real :: dxmin    ! minimum grid interval (m)
      real :: lx, ly   ! x-axis and y-axis lengths of simulation region
      logical :: bar_exist  ! .true if barriers exist
      integer :: alloc_stat
      integer :: idx
      integer :: jdx

      ! +++ END SPECIFICATIONS +++

      ! calc. lx and ly sides of field
      lx = amxsim(2)%x - amxsim(1)%x
      ly = amxsim(2)%y - amxsim(1)%y

      ! write(*,*) 'SBGRID: lx, ly: ', lx, ly

      ! check to see if grid dimensions specified via cmdline args
      if ((xgdpt > 0) .and. (ygdpt > 0)) then
        imax = xgdpt + 1
        jmax = ygdpt + 1
        lencell_x = (amxsim(2)%x - amxsim(1)%x) / xgdpt
        lencell_y = (amxsim(2)%y - amxsim(1)%y) / ygdpt
      else
        ! use Hagen's grid dimensioning as the default

        ! set min grid spacing
        dxmin = MIN_GRID_SP

        ! set max no. of grid points with no barrier
        ngdpt = N_G_DPT

        if( minht > 0.0 ) then    !Check for zero height barriers
          ! at least one barrier exists
          dxmin = min(dxmin, 5.0*minht)
          ngdpt = B_G_DPT  !default to this value if a barrier exists
          bar_exist = .true.
        else
          bar_exist = .false.
        endif

        ! calculate max grid intervals
        ! case where lx > ly
        if ( lx .gt. ly)then
          imax  = int ( lx / dxmin)
          imax = min(imax,ngdpt)
          imax = max(imax,2)
          ! calculate spacing for square or with barriers a rectangular grid
          lencell_x  = lx / (imax - 1)

        if( bar_exist ) then
          jmax  = int (ly / dxmin)
          jmax  = min(jmax, ngdpt)
        else
          jmax = nint(ly/lencell_x) + 1
        endif

        jmax = max(jmax,2)
        lencell_y   = ly / (jmax - 1)

        ! case where lx = ly or lx < ly
        else
          jmax  = int (ly / dxmin)
          jmax = min(jmax,ngdpt)
          jmax = max(jmax,2)
          lencell_y   = ly / (jmax - 1)

          if( bar_exist ) then
            imax  = int (lx / dxmin)
            imax  = min(imax,ngdpt)
          else
            imax = nint(lx/lencell_y) + 1
          endif
          imax = max(imax,2)
          lencell_x = lx/(imax-1)
        end if

      endif

      ! allocate cellstate
      call create_cellsurfacestate( imax, jmax )

      ! this only happens with old single subregion file
      ! set to subregion 1
      ! set elevation to cligen elevation
      do jdx = 1, jmax-1
        do idx = 1, imax-1
          cellstate(idx,jdx)%csr = 1
          cellstate(idx,jdx)%elevation = elevation
        end do
      end do

    end subroutine sbgrid

    subroutine sbigrd( )
      use erosion_data_struct_defs, only: subregionsurfacestate

      ! + + + PURPOSE + + +
      ! To set the grid output arrays to zero

      ! + + + LOCAL VARIABLES + + +
      integer :: idx
      integer :: jdx

      ! + + + END SPECIFICATIONS + + +

      ! Set the grid output arrays to zero
      do jdx = 0, jmax
         do idx = 0, imax
            cellstate(idx,jdx)%egt = 0.0
            cellstate(idx,jdx)%egtcs = 0.0
            cellstate(idx,jdx)%egtss = 0.0
            cellstate(idx,jdx)%egt10 = 0.0
            cellstate(idx,jdx)%egt2_5 = 0.0
         end do
      end do

    end subroutine sbigrd     

    subroutine init_regions_grid( )

      ! +++ PURPOSE +++
      ! Set the subregion and accounting region associations with each grid cell

      ! + + + Modules Used + + +
      use pnpoly_mod, only: pnpoly
      use subregions_mod, only: subr_poly, acct_poly

      ! +++ LOCAL VARIABLES +++
      integer i, j, sr
      type(point) :: centroid

      ! +++ END SPECIFICATIONS +++

      ! assign subregion number to each grid cell
      ! code lifted from sbgrid because it is initialized there - LEW
      do j = 1, jmax-1
        do i = 1, imax-1
          ! The grid cell is assumed rectangular. Use centroid of grid cell
          ! with subregion polygon to select grid cell subregion
          centroid%x = 0.5 * (i-1+i) * lencell_x
          centroid%y = 0.5 * (j-1+j) * lencell_y
          do sr = 1, size(subr_poly)
            ! Check if it is inside subregion polygon
            if( pnpoly(centroid, subr_poly(sr)) .ge. 0) then
               ! centroid of grid cell is inside or on edge of subregion polygon
               ! set subregion index
               cellstate(i,j)%csr = sr
               ! default to first polygon if on edge by exiting the subregion do loop
               exit
            end if
          end do
          ! check final status
          if( cellstate(i,j)%csr .eq. 0 ) then
              ! this grid cell not assigned to a subregion
              write(*,*) 'ERROR: no subregion for grid cell ',i,':',j
              !write(*,*) 'Subregion coverage is not complete'
              !stop
          end if
          ! do same assignment check for accounting regions
          do sr = 1, size(acct_poly)
            ! Check if it is inside accounting region polygon
            if( pnpoly(centroid, acct_poly(sr)) .ge. 0) then
               ! centroid of grid cell is inside or on edge of accounting region polygon
               ! set accounting region index
               cellstate(i,j)%car = sr
               ! default to first polygon if on edge by exiting the accounting region do loop
               exit
            end if
          end do
        end do          
      end do

    end subroutine init_regions_grid

    subroutine sbdirini(wind_dir)

!     +++ purpose +++
!     Calc. wind angle on the sim. region
!     Calc. sweep sequence for update of grid cells

      use p1unconv_mod, only: degtorad

!     +++ ARGUMENT DECLARATIONS +++
      real wind_dir  ! direction of the wind in degrees from north

!     + + + END SPECIFICATION + + +

!     calc wind angle relative to the field Y-axis (+, - 45 deg. range)
      awa = wind_dir - amasim
      if (awa .lt. 0.0 ) awa = awa + 360.0
      if (awa .gt. 360.0) awa = awa - 360.0

      sin_awa = sin(dble(awa)*degtorad)
      cos_awa = cos(dble(awa)*degtorad)
      tan_awa = tan(dble(awa)*degtorad)

!     find wind quadrant relative to sim region & select sweep sequence

      If (awa .ge. 0.0 .and. awa .lt. 90.0) then
        i1 = imax - 1
        i2 = 1
        i3 = -1
        i4 = jmax - 1
        i5 = 1
        i6 = -1

      elseif (awa .ge. 90.0 .and. awa .lt. 180.0) then
        i1 = imax - 1
        i2 = 1
        i3 = -1
        i4 = 1
        i5 = jmax - 1
        i6 = 1

      elseif (awa .ge. 180.0 .and. awa .lt. 270.0) then
        i1 = 1
        i2 = imax - 1
        i3 = 1
        i4 = 1
        i5 = jmax - 1
        i6 = 1

      else
        i1 = 1
        i2 = imax - 1
        i3 = 1
        i4 = jmax - 1
        i5 = 1
        i6 = -1

      endif

    end subroutine sbdirini

end module grid_mod
