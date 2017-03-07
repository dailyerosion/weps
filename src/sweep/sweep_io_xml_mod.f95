!$Author$
!$Date$
!$Revision$
!$HeadURL$

module sweep_io_xml_mod
  ! It defines the routines that are called by the XML parser in response
  ! to particular events.

  ! A module such as this could use "utility routines" to convert pcdata
  ! to numerical arrays, and to populate specific data structures.

  use flib_sax
  use Polygons_Mod, only: polygon, create_polygon, destroy_polygon, set_area_polygon
  use Points_Mod, only: point
  use subregions_mod, only: acct_poly, subr_poly
  use erosion_data_struct_defs, only: subregionsurfacestate, create_subregionsurfacestate, &
                                          awzypt, awdair, anemht, awzzo, wzoflg, &
                                          ntstep, awadir, awudmx, subday, am0eif, subrsurf
  use p1erode_def, only: SLRR_MIN, SLRR_MAX, WZZO_MIN, WZZO_MAX
  use barriers_mod, only: create_barrier, barrier, barseas
  use barriers_mod, only: barrier_day_state, barrier_params, barrier_climate
  use grid_mod, only: amasim, amxsim, xgdpt, ygdpt
  use sae_in_out_mod, only: saeinp
 
  integer, parameter :: MAX_NAME_LEN  = 40

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: required                    ! .true. if tag is required
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: input_tag
  integer :: max_tags
  
  interface read_param
    module procedure read_param_real_1
    module procedure read_param_real_2
    module procedure read_param_int_1
    module procedure read_param_int_2
    module procedure read_param_int_3
  end interface

  integer, parameter :: SCI_AccNo = 1
  integer, parameter :: SCI_Account = 2
  integer, parameter :: SCI_Barrier = 7
  integer, parameter :: SCI_BarrierNo = 8
  integer, parameter :: SCI_coordinate = 14
  integer, parameter :: SCI_coordinates = 15
  integer, parameter :: SCI_coord = 16
  integer, parameter :: SCI_coordI = 17
  integer, parameter :: SCI_coordNo = 18
  integer, parameter :: SCI_Description = 23
  integer, parameter :: SCI_height = 30
  integer, parameter :: SCI_index = 32
  integer, parameter :: SCI_Number = 36
  integer, parameter :: SCI_pointBarCli = 37
  integer, parameter :: SCI_porosity = 38
  integer, parameter :: SCI_RegionAngle = 39
  integer, parameter :: inputData = 40
  integer, parameter :: SCI_Subregion = 47
  integer, parameter :: SCI_SubregionNo = 48
  integer, parameter :: SCI_TimeSteps = 51
  integer, parameter :: SCI_width = 53
  integer, parameter :: SCI_XGrid = 55
  integer, parameter :: SCI_XLength = 56
  integer, parameter :: SCI_XOrigin = 57
  integer, parameter :: SCI_x = 58
  integer, parameter :: SCI_YGrid = 59
  integer, parameter :: SCI_YLength = 60
  integer, parameter :: SCI_YOrigin = 61
  integer, parameter :: SCI_y = 62

  integer, parameter :: max_simyear = 100000  ! value used to test simulation year input range
  integer :: nacctr   ! Number of accounting regions
  integer :: nsubr    ! Number of subregions
  integer :: nbr      ! number of barriers
  integer :: seas_flg ! barrier season flag
  integer :: ntm_seas ! number of time marks for seasonal barrier
  integer :: poly_np  ! number of points in polygon or polyline
  integer :: isr      ! index for subregion reading
  integer :: iar      ! index for accounting region reading
  integer :: ibr      ! index for barrier reading
  integer :: ipol     ! index for polygon reading
  integer :: iseas    ! index for barrier season reading
  logical, dimension(:), allocatable :: subregion_complete
  logical, dimension(:), allocatable :: points_complete
  logical, dimension(:,:), allocatable :: clipar_complete
  integer :: count_complete
  ! temporary holder for array elements until index is read
  type(polygon) :: t_polygon
  type(point) :: t_point
  type(barrier_day_state) :: t_day_state
  type(barrier_params) :: t_params
  type(barrier_climate) :: t_climate
  logical, dimension(2) :: inputfile_complete

contains

  subroutine begin_element_handler(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx

    !write(*,*) ">>Begin Element: ", name
    !write(*,*) "--- ", len(attributes), " attributes:"
    !call print_dict(attributes)

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .true.
        ! write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

  end subroutine begin_element_handler

  subroutine end_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: alloc_stat

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. inputData) then
            !write(*,*) 'Tags' &
            !           , input_tag(SCI_TimeSteps)%acquired &
            !           , input_tag(SCI_RegionAngle)%acquired &
            !           , input_tag(SCI_XOrigin)%acquired &
            !           , input_tag(SCI_YOrigin)%acquired &
            !           , input_tag(SCI_XLength)%acquired &
            !           , input_tag(SCI_YLength)%acquired &
            !           , input_tag(SCI_XGrid)%acquired &
            !           , input_tag(SCI_YGrid)%acquired &
            !           , input_tag(SCI_AccNo)%acquired &
            !           , input_tag(SCI_Account)%acquired &
            !           , input_tag(SCI_SubregionNo)%acquired &
            !           , input_tag(SCI_Subregion)%acquired &
            !           , input_tag(SCI_BarrierNo)%acquired &
            !           , input_tag(SCI_Barrier)%acquired

          if (input_tag(SCI_AccNo)%acquired) then
            if (nacctr .le. 0) then
              inputfile_complete(1) = .true.
            else
              if (input_tag(SCI_Account)%acquired) then
                inputfile_complete(1) = .true.
              end if
            end if
          else
            inputfile_complete(1) = .true.
            ! create array of accounting region polygons (zero size array allowed)
            allocate(acct_poly(0), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory alloc., accounting region polygons'
            end if
          end if

          if (input_tag(SCI_BarrierNo)%acquired) then
            if (nbr .le. 0) then
              inputfile_complete(2) = .true.
            else
              if (input_tag(SCI_Barrier)%acquired) then
                inputfile_complete(2) = .true.
              end if
            end if
          else
            inputfile_complete(2) = .true.
            ! allocate structure for barriers (zero size array allowed)
            allocate(barrier(0), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory alloc., barrier'
            end if
            allocate(barseas(0), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory alloc., seasonal barrier'
            end if
          end if

          ! check for acquisition of all required elements
          if (    input_tag(SCI_TimeSteps)%acquired &
            .and. input_tag(SCI_RegionAngle)%acquired &
            .and. input_tag(SCI_XOrigin)%acquired &
            .and. input_tag(SCI_YOrigin)%acquired &
            .and. input_tag(SCI_XLength)%acquired &
            .and. input_tag(SCI_YLength)%acquired &
            .and. input_tag(SCI_XGrid)%acquired &
            .and. input_tag(SCI_YGrid)%acquired &
            .and. input_tag(SCI_SubregionNo)%acquired &
            .and. input_tag(SCI_Subregion)%acquired &
            .and. inputfile_complete(1) &
            .and. inputfile_complete(2) &
            ) then
            input_tag(inputData)%acquired = .true.
            ! always true on sweep runs
            am0eif = .true.
          end if

        end if

        exit  ! found tag, no need to look further

      end if
    end do

  end subroutine end_element_handler

  subroutine init_input_xml()

    integer :: idx
    integer :: alloc_stat

    max_tags = 62   ! count of unique tags needed from all dtd files
    allocate( input_tag(max_tags), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., input_tag'
    end if

    ! assign defaults to flag status values
    do idx = 1, max_tags
      input_tag(idx)%required = .true.
      input_tag(idx)%acquired = .false.
      input_tag(idx)%in_tag = .false.
    end do

    ! set optional items
    input_tag(SCI_AccNo)%required = .false.
    input_tag(SCI_Account)%required = .false.
    input_tag(SCI_BarrierNo)%required = .false.
    input_tag(SCI_Barrier)%required = .false.

    ! assign tag names
    input_tag(1)%name = "SCI_AccNo"
    input_tag(2)%name = "SCI_Account"
    input_tag(7)%name = "SCI_Barrier"
    input_tag(8)%name = "SCI_BarrierNo"
    input_tag(14)%name = "SCI_coordinate"
    input_tag(15)%name = "SCI_coordinates"
    input_tag(16)%name = "SCI_coord"
    input_tag(17)%name = "SCI_coordI"
    input_tag(18)%name = "SCI_coordNo"
    input_tag(23)%name = "SCI_Description"
    input_tag(30)%name = "SCI_height"
    input_tag(32)%name = "SCI_index"
    input_tag(36)%name = "SCI_Number"
    input_tag(37)%name = "SCI_pointBarCli"
    input_tag(38)%name = "SCI_porosity"
    input_tag(39)%name = "SCI_RegionAngle"
    input_tag(40)%name = "inputData"
    input_tag(47)%name = "SCI_Subregion"
    input_tag(48)%name = "SCI_SubregionNo"
    input_tag(51)%name = "SCI_TimeSteps"
    input_tag(53)%name = "SCI_width"
    input_tag(55)%name = "SCI_XGrid"
    input_tag(56)%name = "SCI_XLength"
    input_tag(57)%name = "SCI_XOrigin"
    input_tag(58)%name = "SCI_x"
    input_tag(59)%name = "SCI_YGrid"
    input_tag(60)%name = "SCI_YLength"
    input_tag(61)%name = "SCI_YOrigin"
    input_tag(62)%name = "SCI_y"

    ! create integer variable names for tags and assign index number.
    ! makes chunk code more understandable.

  end subroutine init_input_xml

  subroutine pcdata_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value
    integer :: sum_stat, alloc_stat, dealloc_stat

    param_value = trim(chunk)

    if (input_tag(inputData)%in_tag) then
      if (input_tag(SCI_TimeSteps)%in_tag) then
        call read_param(SCI_TimeSteps, param_value, ntstep)
        input_tag(SCI_TimeSteps)%acquired = .true.

        ! allocate wind direction and speed array
        allocate(subday(ntstep), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., wind direction and speed'
        end if

      else if (input_tag(SCI_RegionAngle)%in_tag) then
        call read_param(SCI_RegionAngle, param_value, amasim)
        input_tag(SCI_RegionAngle)%acquired = .true.

      else if (input_tag(SCI_XOrigin)%in_tag) then
        call read_param(SCI_XOrigin, param_value, amxsim(1)%x)
        input_tag(SCI_XOrigin)%acquired = .true.

      else if (input_tag(SCI_YOrigin)%in_tag) then
        call read_param(SCI_YOrigin, param_value, amxsim(1)%y)
        input_tag(SCI_YOrigin)%acquired = .true.

      else if (input_tag(SCI_XLength)%in_tag) then
        call read_param(SCI_XLength, param_value, amxsim(2)%x)
        input_tag(SCI_XLength)%acquired = .true.

      else if (input_tag(SCI_YLength)%in_tag) then
        call read_param(SCI_YLength, param_value, amxsim(2)%y)
        input_tag(SCI_YLength)%acquired = .true.

      else if (input_tag(SCI_XGrid)%in_tag) then
        call read_param(SCI_XGrid, param_value, xgdpt)
        input_tag(SCI_XGrid)%acquired = .true.

      else if (input_tag(SCI_YGrid)%in_tag) then
        call read_param(SCI_YGrid, param_value, ygdpt)
        input_tag(SCI_YGrid)%acquired = .true.

      else if (input_tag(SCI_AccNo)%in_tag) then
        call read_param(SCI_AccNo, param_value, nacctr)
        input_tag(SCI_AccNo)%acquired = .true.
        if (nacctr .gt. 0) then
          ! set counter iar for reading in Accounting Regions
          iar = 1
        end if
        ! create array of accounting region polygons (zero size array allowed)
        allocate(acct_poly(nacctr), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory alloc., accounting region polygons'
        end if

      else if (input_tag(SCI_Account)%in_tag) then
        ! Accounting region SCI_coordinates
        if (input_tag(SCI_AccNo)%required) then
          if (input_tag(SCI_coordinates)%in_tag) then
            !SCI_coordinates
            if (input_tag(SCI_Number)%in_tag) then
              call read_param(SCI_Number, param_value, poly_np)
              if (poly_np .gt. 0) then
                ! create polygon point storage
                acct_poly(iar) = create_polygon(poly_np)
                ! initialize polygon point counter
                ipol = 1
              end if
              input_tag(SCI_Number)%acquired = .true.
            else if (input_tag(SCI_coordinate)%in_tag) then
              if (input_tag(SCI_Number)%acquired) then
                call read_param(SCI_coordinate, param_value, acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y)
                ipol = ipol + 1
                if (ipol .gt. poly_np) then
                  ! finished with this accounting region
                  call set_area_polygon( acct_poly(iar) )
                  iar = iar + 1
                end if
              else
                write(*,*) 'Error: Number of coordinates must be specified before reading in SCI_coordinates'
              end if
            end if
          end if
          if (iar .gt. nacctr) then
            input_tag(SCI_AccNo)%acquired = .true.
            input_tag(SCI_Account)%acquired = .true.
            input_tag(SCI_Number)%acquired = .false.
          end if
        else
          write(*,*) 'Error: Number of accounting regions must be specified before reading in accounting region data'
        end if

      else if (input_tag(SCI_SubregionNo)%in_tag) then
        call read_param(SCI_SubregionNo, param_value, nsubr)
        if (nsubr .lt. 1) then
          write(*,*) 'Error, subregion count must be 1 or greater. Value: ', nsubr
          call exit(1)
        end if
        input_tag(SCI_SubregionNo)%acquired = .true.

      ! create data array to hold input and derived values for each subregion
      sum_stat = 0
      allocate(subrsurf(0:nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      ! create subregion polygon array
      allocate(subr_poly(nsubr), stat=alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
         Write(*,*) 'ERROR: memory allocation, subrsurf, subr_poly'
      end if

      else if (input_tag(SCI_Subregion)%in_tag) then
        ! SCI_Subregion
        if (input_tag(SCI_SubregionNo)%acquired) then
          if (input_tag(SCI_index)%in_tag) then
            call read_param(SCI_index, param_value, isr)
            ! adjust from base 0 to base 1 arrays
            isr = isr + 1
            input_tag(SCI_index)%acquired = .true.
          else if (input_tag(SCI_coordinates)%in_tag) then
            !SCI_coordinates
            if (input_tag(SCI_Number)%in_tag) then
              call read_param(SCI_Number, param_value, poly_np)
              if (poly_np .gt. 0) then
                ! create polygon point storage
                t_polygon = create_polygon(poly_np)
                ! initialize polygon point counter
                ipol = 1
              end if
              input_tag(SCI_Number)%acquired = .true.
            else if (input_tag(SCI_coordinate)%in_tag) then
              if (input_tag(SCI_Number)%acquired) then
                call read_param(SCI_coordinate, param_value, t_polygon%points(ipol)%x, t_polygon%points(ipol)%y)
                ipol = ipol + 1
                if (ipol .gt. poly_np) then
                  input_tag(SCI_coordinates)%acquired = .true.
                  input_tag(SCI_Number)%acquired = .false.
                end if
              else
                write(*,*) 'Error: Number of coordinates must be specified before reading in SCI_coordinates'
              end if
            end if
          end if
          if (    input_tag(SCI_index)%acquired &
            .and. input_tag(SCI_coordinates)%acquired) then
            input_tag(SCI_index)%acquired = .false.
            subr_poly(isr) = t_polygon
            ! polygon complete
            call set_area_polygon(subr_poly(isr))
            call destroy_polygon(t_polygon)
            subregion_complete(isr) = .true.
            count_complete = 0
            do isr = 1, nsubr
              if (subregion_complete(isr)) then
                count_complete = count_complete + 1
              end if
            end do
            if (count_complete .ge. nsubr) then
              input_tag(SCI_Subregion)%acquired = .true.
            end if
          end if
        else
          write(*,*) 'Error: Number of subregions must be specified before reading in subregion data'
        end if
      else if (input_tag(SCI_BarrierNo)%in_tag) then
        call read_param(SCI_BarrierNo, param_value, nbr)
        input_tag(SCI_BarrierNo)%acquired = .true.
        if (nbr .gt. 0) then
          ! set counter ibr for reading in Barriers Regions
          ibr = 1
        end if
        ! allocate structure for barriers (nbr .lt. 1 gives zero size array)
        allocate(barrier(nbr), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory alloc., barrier'
        end if
        allocate(barseas(nbr), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory alloc., seasonal barrier'
        end if

      else if (input_tag(SCI_Barrier)%in_tag) then
        !  These barriers as entered are considered to be thin, having no real
        !  area effect such as erodible material source or deposition area.
        !  The polyline entered is the "effective location".

        !  Barriers wider than anything approaching the scale of a cell (1/10th
        !  a cell width)should probably be entered as subregions and the erosion
        !  submodel changed to consider their wind shadow effect on adjoining cells

        !  Note: the barrier point number must be read first and the barrier storage
        !  allocated, then the barrier level data populated. (hence the barrier type
        !  string now comes last)

        !  Note: When seas_flg = 2 is specified, it is required that two points (no
        !  more no less) in time be provided, the first date being when it can be
        !  guaranteed that leaves are at a minimum, and the data values for porosity
        !  correspond to that. The second date is when it can be quaranteed that
        !  leaves are at a maximum and the data values for porosity correspond to that.

        ! read in barrier info
        if (input_tag(SCI_BarrierNo)%acquired) then
          if (input_tag(SCI_Description)%in_tag) then
            barseas(ibr)%amzbt = trim(param_value)
            barrier(ibr)%amzbt = barseas(ibr)%amzbt
            input_tag(SCI_Description)%acquired = .true.

          else if (input_tag(SCI_coordNo)%in_tag) then
            call read_param(SCI_coordNo, param_value, poly_np)
            input_tag(SCI_coordNo)%acquired = .true.
            ! create storage for point and barrier data
            ! this also sets values for barr%np and barr%ntm
            ntm_seas = 1
            seas_flg = 0
            call create_barrier(barrier(ibr), poly_np)
            call create_barrier(barseas(ibr), poly_np,ntm_seas,seas_flg)
            ! create storage for season, points and climate parameter index tracking
            sum_stat = 0
            allocate(points_complete(poly_np), stat = alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(clipar_complete(poly_np, ntm_seas), stat = alloc_stat)
            sum_stat = sum_stat + alloc_stat
            if( sum_stat .gt. 0 ) then
              ! deallocation failed
              write(*,*) "ERROR: unable to allocate memory for _complete arrays"
            end if
            ! initialize _complete arrays to false
            do ipol = 1, poly_np
              points_complete(ipol) = .false.
            end do
            do iseas = 1, ntm_seas
              do ipol = 1, poly_np
                clipar_complete(ipol, iseas) = .false.
              end do
            end do

          else if (input_tag(SCI_coord)%in_tag) then
            !SCI_coord
            if (input_tag(SCI_index)%in_tag) then
              call read_param(SCI_index, param_value, ipol)
              ! adjust from base 0 to base 1 arrays
              ipol = ipol + 1
              input_tag(SCI_index)%acquired = .true.
            else if (input_tag(SCI_x)%in_tag) then
              call read_param(SCI_x, param_value, t_point%x)
              input_tag(SCI_x)%acquired = .true.
            else if (input_tag(SCI_y)%in_tag) then
              call read_param(SCI_x, param_value, t_point%y)
              input_tag(SCI_y)%acquired = .true.
            end if
            if(     input_tag(SCI_index)%acquired &
              .and. input_tag(SCI_x)%acquired &
              .and. input_tag(SCI_y)%acquired &
              ) then
              input_tag(SCI_index)%acquired = .false.
              input_tag(SCI_x)%acquired = .false.
              input_tag(SCI_y)%acquired = .false.
              barseas(ibr)%points(ipol) = t_point
              !  also place in fixed barrier structure
              barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)
              points_complete(ipol) = .true.
              count_complete = 0
              do ipol = 1, poly_np
                if (points_complete(ipol)) then
                  count_complete = count_complete + 1
                end if
              end do
              if (count_complete .ge. poly_np) then
                input_tag(SCI_coord)%acquired = .true.
              end if
            end if
          else if (input_tag(SCI_pointBarCli)%in_tag) then
            ! SCI_pointBarCli
             if (input_tag(SCI_coordI)%in_tag) then
              call read_param(SCI_coordI, param_value, ipol) 
              ! adjust from base 0 to base 1 arrays
              ipol = ipol + 1
              input_tag(SCI_coordI)%acquired = .true.
              iseas = 1
            else if (input_tag(SCI_height)%in_tag) then
              call read_param(SCI_height, param_value, t_params%amzbr)
              input_tag(SCI_height)%acquired = .true.
            else if (input_tag(SCI_width)%in_tag) then
              call read_param(SCI_width, param_value, t_params%amxbrw)
              input_tag(SCI_width)%acquired = .true.
            else if (input_tag(SCI_porosity)%in_tag) then
              call read_param(SCI_porosity, param_value, t_params%ampbr)
              input_tag(SCI_porosity)%acquired = .true.
            end if
            if (    input_tag(SCI_coordI)%acquired &
              .and. input_tag(SCI_height)%acquired &
              .and. input_tag(SCI_width)%acquired &
              .and. input_tag(SCI_porosity)%acquired &
              ) then
              if( t_params%amzbr .le. 0.0 ) then
                write(*,*) 'ERROR: Barrier height must be > 0'
                write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol, 'Season #: ', iseas
                call exit(40)
              end if
              input_tag(SCI_coordI)%acquired = .false.
              input_tag(SCI_height)%acquired = .false.
              input_tag(SCI_width)%acquired = .false.
              input_tag(SCI_porosity)%acquired = .false.
              barseas(ibr)%param(ipol,iseas) = t_params
              clipar_complete(ipol,iseas) = .true.
              count_complete = 0
              do ipol = 1, poly_np
                do iseas = 1, ntm_seas
                  if (clipar_complete(ipol,iseas)) then
                    count_complete = count_complete + 1
                  end if
                end do
              end do
              if (count_complete .ge. poly_np*ntm_seas) then
                input_tag(SCI_pointBarCli)%acquired = .true.
              end if
            end if
          end if

          if (    input_tag(SCI_Description)%acquired &
            .and. input_tag(SCI_coordNo)%acquired &
            .and. input_tag(SCI_coord)%acquired &
            .and. input_tag(SCI_pointBarCli)%acquired &
            ) then
            input_tag(SCI_Description)%acquired = .false.
            input_tag(SCI_coordNo)%acquired = .false.
            input_tag(SCI_coord)%acquired = .false.
            input_tag(SCI_pointBarCli)%acquired = .false.
            ibr = ibr + 1
            if (ibr .gt. nbr) then
              input_tag(SCI_Barrier)%acquired = .true.
            end if

            sum_stat = 0
            deallocate(points_complete, stat=dealloc_stat)
            sum_stat = sum_stat + dealloc_stat
            deallocate(clipar_complete, stat=dealloc_stat)
            sum_stat = sum_stat + dealloc_stat
            if( sum_stat .gt. 0 ) then
              ! deallocation failed
              write(*,*) "ERROR: unable to deallocate memory for _complete arrays"
            end if

          end if
        else
          write(*,*) 'Error: Number of barriers must be specified before reading in barrier data'
        end if

      end if

    end if

  end subroutine pcdata_chunk_handler

  subroutine read_param_real_1(tag, param_string, val)
    integer, intent(in) :: tag
    character(len=80), intent(in) :: param_string
    real, intent(out) :: val
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', input_tag(tag)%name, ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_real_1

  subroutine read_param_real_2(tag, param_string, val_1, val_2)
    integer, intent(in) :: tag
    character(len=80), intent(in) :: param_string
    real, intent(out) :: val_1
    real, intent(out) :: val_2
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val_1, val_2
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', input_tag(tag)%name, ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_real_2

  subroutine read_param_int_1(tag, param_string, val)
    integer, intent(in) :: tag
    character(len=80), intent(in) :: param_string
    integer, intent(out) :: val
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', input_tag(tag)%name, ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_1

  subroutine read_param_int_2(tag, param_string, val_1, val_2)
    integer, intent(in) :: tag
    character(len=80), intent(in) :: param_string
    integer, intent(out) :: val_1
    integer, intent(out) :: val_2
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val_1, val_2
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', input_tag(tag)%name, ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_2

  subroutine read_param_int_3(tag, param_string, val_1, val_2, val_3)
    integer, intent(in) :: tag
    character(len=80), intent(in) :: param_string
    integer, intent(out) :: val_1
    integer, intent(out) :: val_2
    integer, intent(out) :: val_3
    integer :: read_stat
    read(param_string,*,iostat=read_stat) val_1, val_2, val_3
    if (read_stat .gt. 0) then
      write(*,*) 'Error reading ', input_tag(tag)%name, ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_3

end module sweep_io_xml_mod
