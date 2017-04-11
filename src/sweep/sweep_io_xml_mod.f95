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
  use erosion_data_struct_defs, only: subregionsurfacestate, create_subregionsoillayers, create_subregionsurfacewet, &
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
  integer, parameter :: SCI_AerodynamicRoughness = 3
  integer, parameter :: SCI_AggregateDensity = 4
  integer, parameter :: SCI_AggregateGMD = 5
  integer, parameter :: SCI_AggregateGSD = 6
  integer, parameter :: SCI_AggregateMAX = 7
  integer, parameter :: SCI_AggregateMIN = 8
  integer, parameter :: SCI_AggregateStability = 9
  integer, parameter :: SCI_AirDensity = 10
  integer, parameter :: SCI_AnemometerFlag = 11
  integer, parameter :: SCI_AnemometerHeight = 12
  integer, parameter :: SCI_AverageAnnualPrecipitation = 13
  integer, parameter :: SCI_BarPoint = 14
  integer, parameter :: SCI_Barrier = 15
  integer, parameter :: SCI_BarrierNo = 16
  integer, parameter :: SCI_BiomassFlatCover = 17
  integer, parameter :: SCI_BulkDensity = 18
  integer, parameter :: SCI_Clay = 19
  integer, parameter :: SCI_coord = 20
  integer, parameter :: SCI_coordinate = 21
  integer, parameter :: SCI_coordinates = 22
  integer, parameter :: SCI_coordNo = 23
  integer, parameter :: SCI_CropHeight = 24
  integer, parameter :: SCI_CropLAI = 25
  integer, parameter :: SCI_CropRowSpacing = 26
  integer, parameter :: SCI_CropSAI = 27
  integer, parameter :: SCI_CropSeedPlace = 28
  integer, parameter :: SCI_CrustCover = 29
  integer, parameter :: SCI_CrustDensity = 30
  integer, parameter :: SCI_CrustFracCoverLoose = 31
  integer, parameter :: SCI_CrustMassCoverLoose = 32
  integer, parameter :: SCI_CrustStability = 33
  integer, parameter :: SCI_CrustThick = 34
  integer, parameter :: SCI_DikeSpacing = 35
  integer, parameter :: SCI_height = 36
  integer, parameter :: SCI_index = 37
  integer, parameter :: SCI_LayerThickness = 38
  integer, parameter :: SCI_Number = 39
  integer, parameter :: SCI_PointNo = 40
  integer, parameter :: SCI_porosity = 41
  integer, parameter :: SCI_RandomRoughness = 42
  integer, parameter :: SCI_RegionAngle = 43
  integer, parameter :: SCI_ResidueHeight = 44
  integer, parameter :: SCI_ResidueLAI = 45
  integer, parameter :: SCI_ResidueSAI = 46
  integer, parameter :: SCI_RidgeHeight = 47
  integer, parameter :: SCI_RidgeOrientation = 48
  integer, parameter :: SCI_RidgeSpacing = 49
  integer, parameter :: SCI_RidgeWidth = 50
  integer, parameter :: SCI_RockVolume = 51
  integer, parameter :: SCI_Sand = 52
  integer, parameter :: SCI_Silt = 53
  integer, parameter :: SCI_SnowDepth = 54
  integer, parameter :: SCI_SoilLay = 55
  integer, parameter :: SCI_SoilLayNo = 56
  integer, parameter :: SCI_Subregion = 57
  integer, parameter :: SCI_SubregionNo = 58
  integer, parameter :: SCI_SurfaceSubDayWater = 59
  integer, parameter :: SCI_SurfaceSubDayWaterNo = 60
  integer, parameter :: SCI_VeryFineSand = 61
  integer, parameter :: SCI_WaterContent = 62
  integer, parameter :: SCI_width = 63
  integer, parameter :: SCI_WiltingPoint = 64
  integer, parameter :: SCI_WindDirection = 65
  integer, parameter :: SCI_WindSpeed = 66
  integer, parameter :: SCI_WindTimeSteps = 67
  integer, parameter :: SCI_x = 68
  integer, parameter :: SCI_XGrid = 69
  integer, parameter :: SCI_XLength = 70
  integer, parameter :: SCI_XOrigin = 71
  integer, parameter :: SCI_y = 72
  integer, parameter :: SCI_YGrid = 73
  integer, parameter :: SCI_YLength = 74
  integer, parameter :: SCI_YOrigin = 75
  integer, parameter :: sweepData = 76

! GUI_lat
! GUI_lon
! GUI_WeibullC
! GUI_WeibullCalm
! GUI_WeibullFlag
! GUI_WeibullK

  integer, parameter :: max_simyear = 100000  ! value used to test simulation year input range
  integer :: nacctr   ! Number of accounting regions
  integer :: nsubr    ! Number of subregions
  integer :: nbr      ! number of barriers
  integer :: seas_flg ! barrier season flag
  integer :: ntm_seas ! number of time marks for seasonal barrier
  integer :: poly_np  ! number of points in polygon or polyline
  integer :: isr      ! index for subregion reading
  integer :: isl      ! index for soil layer reading
  integer :: iar      ! index for accounting region reading
  integer :: ibr      ! index for barrier reading
  integer :: ipol     ! index for polygon reading
  integer :: iseas    ! index for barrier season reading
  integer :: iwind    ! index for wind speed values
  integer :: isurfwat ! index for surface water content values
  logical, dimension(:), allocatable :: subregion_complete
  logical, dimension(:), allocatable :: points_complete
  logical, dimension(:), allocatable :: soillay_complete
  logical, dimension(:), allocatable :: surfwat_complete
  logical, dimension(:), allocatable :: wind_complete
  logical, dimension(:), allocatable :: barrier_complete
  integer :: count_complete
  ! temporary holder for array elements until index is read
  type(barrier_day_state) :: t_day_state
  type(barrier_climate) :: t_climate
  logical, dimension(2) :: inputfile_complete

contains

  subroutine begin_element_handler(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx
    character(len=80) :: param_value
    integer :: ret_stat

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

    if (   (idx .eq. SCI_Subregion) &
      .or. (idx .eq. SCI_WindSpeed) &
      .or. (idx .eq. SCI_SurfaceSubDayWater) &
      .or. (idx .eq. SCI_SoilLay) &
      .or. (idx .eq. SCI_Barrier) &
      .or. (idx .eq. SCI_BarPoint) &
      .or. (idx .eq. SCI_coord) ) then
      if ( has_key(attributes, input_tag(SCI_index)%name) ) then
        call get_value(attributes, input_tag(SCI_index)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregion)
          call read_param(SCI_index, param_value, isr)
          ! adjust from base 0 to base 1 arrays
          isr = isr + 1
          !write(*,*) 'Subregion Index: ', isr
        case (SCI_WindSpeed)
          call read_param(SCI_index, param_value, iwind)
          ! adjust from base 0 to base 1 arrays
          iwind = iwind + 1
          !write(*,*) 'Wind Speed Index: ', iwind
        case (SCI_SurfaceSubDayWater)
          call read_param(SCI_index, param_value, isurfwat)
          ! adjust from base 0 to base 1 arrays
          isurfwat = isurfwat + 1
          !write(*,*) 'Surface Water Index: ', isurfwat
        case (SCI_SoilLay)
          call read_param(SCI_index, param_value, isl)
          ! adjust from base 0 to base 1 arrays
          isl = isl + 1
          !write(*,*) 'Soil Layer Index: ', isl
        case (SCI_Barrier)
          call read_param(SCI_index, param_value, ibr)
          ! adjust from base 0 to base 1 arrays
          ibr = ibr + 1
          !write(*,*) 'Barrier Index: ', ibr
        case (SCI_BarPoint)
          call read_param(SCI_index, param_value, ipol)
          ! adjust from base 0 to base 1 arrays
          ipol = ipol + 1
          !write(*,*) 'Barrier Point Index: ', ipol
        case (SCI_coord)
          call read_param(SCI_index, param_value, ipol)
          ! adjust from base 0 to base 1 arrays
          ipol = ipol + 1
          !write(*,*) 'Subregion Coordinates Point Index: ', ipol
        end select
      else
        write(*,*) 'SCI_index attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
    end if

  end subroutine begin_element_handler

  subroutine end_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: alloc_stat

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. SweepData) then
          ! intiialize flags for optional tags
          inputfile_complete(1) = .false.
          inputfile_complete(2) = .false.

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

            write(*,*) 'Tags' &
                       , input_tag(SCI_WindTimeSteps)%acquired &
                       , input_tag(SCI_RegionAngle)%acquired &
                       , input_tag(SCI_XOrigin)%acquired &
                       , input_tag(SCI_YOrigin)%acquired &
                       , input_tag(SCI_XLength)%acquired &
                       , input_tag(SCI_YLength)%acquired &
                       , input_tag(SCI_XGrid)%acquired &
                       , input_tag(SCI_YGrid)%acquired &
                       , input_tag(SCI_SubregionNo)%acquired &
                       , input_tag(SCI_Subregion)%acquired &
                       , input_tag(SCI_BarrierNo)%acquired &
                       , input_tag(SCI_Barrier)%acquired &
                       , input_tag(SCI_AirDensity)%acquired &
                       , input_tag(SCI_WindDirection)%acquired &
                       , input_tag(SCI_AnemometerHeight)%acquired &
                       , input_tag(SCI_AerodynamicRoughness)%acquired &
                       , input_tag(SCI_AnemometerFlag)%acquired &
                       , input_tag(SCI_AverageAnnualPrecipitation)%acquired &
                       , input_tag(SCI_WindTimeSteps)%acquired &
                       , input_tag(SCI_WindSpeed)%acquired &
                       , inputfile_complete(1) &
                       , inputfile_complete(2)

          ! check for acquisition of all required elements
          if (    input_tag(SCI_RegionAngle)%acquired &
            .and. input_tag(SCI_XOrigin)%acquired &
            .and. input_tag(SCI_YOrigin)%acquired &
            .and. input_tag(SCI_XLength)%acquired &
            .and. input_tag(SCI_YLength)%acquired &
            .and. input_tag(SCI_XGrid)%acquired &
            .and. input_tag(SCI_YGrid)%acquired &
            .and. input_tag(SCI_SubregionNo)%acquired &
            .and. input_tag(SCI_Subregion)%acquired &
            .and. input_tag(SCI_AirDensity)%acquired &
            .and. input_tag(SCI_WindDirection)%acquired &
            .and. input_tag(SCI_AnemometerHeight)%acquired &
            .and. input_tag(SCI_AerodynamicRoughness)%acquired &
            .and. input_tag(SCI_AnemometerFlag)%acquired &
            .and. input_tag(SCI_AverageAnnualPrecipitation)%acquired &
            .and. input_tag(SCI_WindTimeSteps)%acquired &
            .and. input_tag(SCI_WindSpeed)%acquired &
            .and. inputfile_complete(1) &
            .and. inputfile_complete(2) &
            ) then
            input_tag(SweepData)%acquired = .true.
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

    max_tags = 76   ! count of unique tags needed from all dtd files
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
    input_tag(3)%name = "SCI_AerodynamicRoughness"
    input_tag(4)%name = "SCI_AggregateDensity"
    input_tag(5)%name = "SCI_AggregateGMD"
    input_tag(6)%name = "SCI_AggregateGSD"
    input_tag(7)%name = "SCI_AggregateMAX"
    input_tag(8)%name = "SCI_AggregateMIN"
    input_tag(9)%name = "SCI_AggregateStability"
    input_tag(10)%name = "SCI_AirDensity"
    input_tag(11)%name = "SCI_AnemometerFlag"
    input_tag(12)%name = "SCI_AnemometerHeight"
    input_tag(13)%name = "SCI_AverageAnnualPrecipitation"
    input_tag(14)%name = "SCI_BarPoint"
    input_tag(15)%name = "SCI_Barrier"
    input_tag(16)%name = "SCI_BarrierNo"
    input_tag(17)%name = "SCI_BiomassFlatCover"
    input_tag(18)%name = "SCI_BulkDensity"
    input_tag(19)%name = "SCI_Clay"
    input_tag(20)%name = "SCI_coord"
    input_tag(21)%name = "SCI_coordinate"
    input_tag(22)%name = "SCI_coordinates"
    input_tag(23)%name = "SCI_coordNo"
    input_tag(24)%name = "SCI_CropHeight"
    input_tag(25)%name = "SCI_CropLAI"
    input_tag(26)%name = "SCI_CropRowSpacing"
    input_tag(27)%name = "SCI_CropSAI"
    input_tag(28)%name = "SCI_CropSeedPlace"
    input_tag(29)%name = "SCI_CrustCover"
    input_tag(30)%name = "SCI_CrustDensity"
    input_tag(31)%name = "SCI_CrustFracCoverLoose"
    input_tag(32)%name = "SCI_CrustMassCoverLoose"
    input_tag(33)%name = "SCI_CrustStability"
    input_tag(34)%name = "SCI_CrustThick"
    input_tag(35)%name = "SCI_DikeSpacing"
    input_tag(36)%name = "SCI_height"
    input_tag(37)%name = "SCI_index"
    input_tag(38)%name = "SCI_LayerThickness"
    input_tag(39)%name = "SCI_Number"
    input_tag(40)%name = "SCI_PointNo"
    input_tag(41)%name = "SCI_porosity"
    input_tag(42)%name = "SCI_RandomRoughness"
    input_tag(43)%name = "SCI_RegionAngle"
    input_tag(44)%name = "SCI_ResidueHeight"
    input_tag(45)%name = "SCI_ResidueLAI"
    input_tag(46)%name = "SCI_ResidueSAI"
    input_tag(47)%name = "SCI_RidgeHeight"
    input_tag(48)%name = "SCI_RidgeOrientation"
    input_tag(49)%name = "SCI_RidgeSpacing"
    input_tag(50)%name = "SCI_RidgeWidth"
    input_tag(51)%name = "SCI_RockVolume"
    input_tag(52)%name = "SCI_Sand"
    input_tag(53)%name = "SCI_Silt"
    input_tag(54)%name = "SCI_SnowDepth"
    input_tag(55)%name = "SCI_SoilLay"
    input_tag(56)%name = "SCI_SoilLayNo"
    input_tag(57)%name = "SCI_Subregion"
    input_tag(58)%name = "SCI_SubregionNo"
    input_tag(59)%name = "SCI_SurfaceSubDayWater"
    input_tag(60)%name = "SCI_SurfaceSubDayWaterNo"
    input_tag(61)%name = "SCI_VeryFineSand"
    input_tag(62)%name = "SCI_WaterContent"
    input_tag(63)%name = "SCI_width"
    input_tag(64)%name = "SCI_WiltingPoint"
    input_tag(65)%name = "SCI_WindDirection"
    input_tag(66)%name = "SCI_WindSpeed"
    input_tag(67)%name = "SCI_WindTimeSteps"
    input_tag(68)%name = "SCI_x"
    input_tag(69)%name = "SCI_XGrid"
    input_tag(70)%name = "SCI_XLength"
    input_tag(71)%name = "SCI_XOrigin"
    input_tag(72)%name = "SCI_y"
    input_tag(73)%name = "SCI_YGrid"
    input_tag(74)%name = "SCI_YLength"
    input_tag(75)%name = "SCI_YOrigin"
    input_tag(76)%name = "sweepData"

    ! See above:
    ! create integer variable names for tags and assign index number.
    ! makes chunk code more understandable.

  end subroutine init_input_xml

  subroutine pcdata_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value
    integer :: sum_stat, alloc_stat, dealloc_stat
    integer :: idx

    param_value = trim(chunk)

    if (input_tag(SweepData)%in_tag) then
      if (input_tag(SCI_RegionAngle)%in_tag) then
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

!      else if (input_tag(SCI_Account)%in_tag) then
!        ! Accounting region SCI_coordinates
!        if (input_tag(SCI_AccNo)%required) then
!          if (input_tag(SCI_coordinates)%in_tag) then
!            !SCI_coordinates
!            if (input_tag(SCI_Number)%in_tag) then
!              call read_param(SCI_Number, param_value, poly_np)
!              if (poly_np .gt. 0) then
!                ! create polygon point storage
!                acct_poly(iar) = create_polygon(poly_np)
!                ! initialize polygon point counter
!                ipol = 1
!              end if
!              input_tag(SCI_Number)%acquired = .true.
!            else if (input_tag(SCI_coordinate)%in_tag) then
!              if (input_tag(SCI_Number)%acquired) then
!                call read_param(SCI_coordinate, param_value, acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y)
!                ipol = ipol + 1
!                if (ipol .gt. poly_np) then
!                  ! finished with this accounting region
!                  call set_area_polygon( acct_poly(iar) )
!                  iar = iar + 1
!                end if
!              else
!                write(*,*) 'Error: Number of coordinates must be specified before reading in SCI_coordinates'
!              end if
!            end if
!          end if
!          if (iar .gt. nacctr) then
!            input_tag(SCI_AccNo)%acquired = .true.
!            input_tag(SCI_Account)%acquired = .true.
!            input_tag(SCI_Number)%acquired = .false.
!          end if
!        else
!          write(*,*) 'Error: Number of accounting regions must be specified before reading in accounting region data'
!        end if

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
        allocate(subregion_complete(nsubr), stat = alloc_stat)
        sum_stat = sum_stat + alloc_stat
        if( sum_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory allocation, subrsurf, subr_poly'
        end if
        ! initialize _complete arrays to false
        do idx = 1, nsubr
          subregion_complete(idx) = .false.
        end do

      else if (input_tag(SCI_Subregion)%in_tag) then
        ! SCI_Subregion
        if (input_tag(SCI_SubregionNo)%acquired) then
          if (input_tag(SCI_coordNo)%in_tag) then
            call read_param(SCI_coordNo, param_value, poly_np)
            if (poly_np .gt. 0) then
              ! create polygon point storage
              subr_poly(isr) = create_polygon(poly_np)
              ! create storage for points index tracking
              allocate(points_complete(poly_np), stat = alloc_stat)
              if( alloc_stat .gt. 0 ) then
                ! allocation failed
                write(*,*) "ERROR: unable to allocate memory for points_complete array"
              end if
              ! initialize _complete arrays to false
              do idx = 1, poly_np
                points_complete(idx) = .false.
              end do
            end if
            input_tag(SCI_coordNo)%acquired = .true.
          end if

          if (input_tag(SCI_coord)%in_tag) then
            !SCI_coord
            if (input_tag(SCI_coordNo)%acquired) then
              if (input_tag(SCI_x)%in_tag) then
                call read_param(SCI_x, param_value, subr_poly(isr)%points(ipol)%x)
                input_tag(SCI_x)%acquired = .true.
              else if (input_tag(SCI_y)%in_tag) then
                call read_param(SCI_y, param_value, subr_poly(isr)%points(ipol)%y)
                input_tag(SCI_y)%acquired = .true.
              end if

              if (    input_tag(SCI_x)%acquired &
                .and. input_tag(SCI_y)%acquired &
                ) then 
                input_tag(SCI_x)%acquired = .false.
                input_tag(SCI_y)%acquired = .false.
                points_complete(ipol) = .true.
                count_complete = 0
                do idx = 1, poly_np
                  if (points_complete(idx)) then
                    count_complete = count_complete + 1
                  end if
                end do
                if (count_complete .ge. poly_np) then
                  input_tag(SCI_coord)%acquired = .true.
                  ! polygon complete
                  call set_area_polygon(subr_poly(isr))
                  deallocate(points_complete, stat=dealloc_stat)
                  if( dealloc_stat .gt. 0 ) then
                    ! deallocation failed
                    write(*,*) "ERROR: unable to deallocate memory for points_complete arrays"
                  end if
                end if
              end if
            else
              write(*,*) 'Error: Number of coordinates must be specified before reading in SCI_coord'
            end if

          else if (input_tag(SCI_ResidueHeight)%in_tag) then
            call read_param(SCI_ResidueHeight, param_value, subrsurf(isr)%adzht_ave)
            input_tag(SCI_ResidueHeight)%acquired = .true.

          else if (input_tag(SCI_CropHeight)%in_tag) then
            call read_param(SCI_CropHeight, param_value, subrsurf(isr)%aczht)
            input_tag(SCI_CropHeight)%acquired = .true.

          else if (input_tag(SCI_CropSAI)%in_tag) then
            call read_param(SCI_CropSAI, param_value, subrsurf(isr)%acrsai)
            input_tag(SCI_CropSAI)%acquired = .true.

          else if (input_tag(SCI_CropLAI)%in_tag) then
            call read_param(SCI_CropLAI, param_value, subrsurf(isr)%acrlai)
            input_tag(SCI_CropLAI)%acquired = .true.

          else if (input_tag(SCI_ResidueSAI)%in_tag) then
            call read_param(SCI_ResidueSAI, param_value, subrsurf(isr)%adrsaitot)
            input_tag(SCI_ResidueSAI)%acquired = .true.

          else if (input_tag(SCI_ResidueLAI)%in_tag) then
            call read_param(SCI_ResidueLAI, param_value, subrsurf(isr)%adrlaitot)
            input_tag(SCI_ResidueLAI)%acquired = .true.

          else if (input_tag(SCI_CropRowSpacing)%in_tag) then
            call read_param(SCI_CropRowSpacing, param_value, subrsurf(isr)%acxrow)
            input_tag(SCI_CropRowSpacing)%acquired = .true.

          else if (input_tag(SCI_CropSeedPlace)%in_tag) then
            call read_param(SCI_CropSeedPlace, param_value, subrsurf(isr)%ac0rg)
            input_tag(SCI_CropSeedPlace)%acquired = .true.

          else if (input_tag(SCI_BiomassFlatCover)%in_tag) then
            call read_param(SCI_BiomassFlatCover, param_value, subrsurf(isr)%abffcv)
            input_tag(SCI_BiomassFlatCover)%acquired = .true.

          else if (input_tag(SCI_CrustCover)%in_tag) then
            call read_param(SCI_CrustCover, param_value, subrsurf(isr)%asfcr)
            input_tag(SCI_CrustCover)%acquired = .true.

          else if (input_tag(SCI_CrustThick)%in_tag) then
            call read_param(SCI_CrustThick, param_value, subrsurf(isr)%aszcr)
            input_tag(SCI_CrustThick)%acquired = .true.

          else if (input_tag(SCI_CrustFracCoverLoose)%in_tag) then
            call read_param(SCI_CrustFracCoverLoose, param_value, subrsurf(isr)%asflos)
            input_tag(SCI_CrustFracCoverLoose)%acquired = .true.

          else if (input_tag(SCI_CrustMassCoverLoose)%in_tag) then
            call read_param(SCI_CrustMassCoverLoose, param_value, subrsurf(isr)%asmlos)
            input_tag(SCI_CrustMassCoverLoose)%acquired = .true.

          else if (input_tag(SCI_CrustDensity)%in_tag) then
            call read_param(SCI_CrustDensity, param_value, subrsurf(isr)%asdcr)
            input_tag(SCI_CrustDensity)%acquired = .true.

          else if (input_tag(SCI_CrustStability)%in_tag) then
            call read_param(SCI_CrustStability, param_value, subrsurf(isr)%asecr)
            input_tag(SCI_CrustStability)%acquired = .true.

          else if (input_tag(SCI_RandomRoughness)%in_tag) then
            call read_param(SCI_RandomRoughness, param_value, subrsurf(isr)%aslrr)
            input_tag(SCI_RandomRoughness)%acquired = .true.

            !Lower and upper limits of grid cell RR allowed by erosion submodel
            if (subrsurf(isr)%aslrr < SLRR_MIN) then
              write(0,*) 'slrr: ', subrsurf(isr)%aslrr,' < ', SLRR_MIN
            end if
            if (subrsurf(isr)%aslrr > SLRR_MAX) then
              write(0,*) 'slrr: ', subrsurf(isr)%aslrr,' < ', SLRR_MIN
            end if

            !Lower and upper limits of grid cell aerodynamic roughness allowed
            !by erosion submodel (currently determined by equation used here)
            if (subrsurf(isr)%aslrr < (WZZO_MIN/0.3)) then
              write(0,*) 'slrr: ', subrsurf(isr)%aslrr
              write(0,*) 'wzzo < WZZO_MIN: ', subrsurf(isr)%aslrr*0.3,' < ', WZZO_MIN
            else if(subrsurf(isr)%aslrr > (WZZO_MAX/0.3)) then
              write(0,*) 'slrr: ', subrsurf(isr)%aslrr
              write(0,*) 'wzzo > WZZO_MAX: ', subrsurf(isr)%aslrr*0.3,' > ', WZZO_MAX
            end if

          else if (input_tag(SCI_RidgeHeight)%in_tag) then
            call read_param(SCI_RidgeHeight, param_value, subrsurf(isr)%aszrgh)
            input_tag(SCI_RidgeHeight)%acquired = .true.

          else if (input_tag(SCI_RidgeSpacing)%in_tag) then
            call read_param(SCI_RidgeSpacing, param_value, subrsurf(isr)%asxrgs)
            input_tag(SCI_RidgeSpacing)%acquired = .true.

          else if (input_tag(SCI_RidgeWidth)%in_tag) then
            call read_param(SCI_RidgeWidth, param_value, subrsurf(isr)%asxrgw)
            input_tag(SCI_RidgeWidth)%acquired = .true.

          else if (input_tag(SCI_RidgeOrientation)%in_tag) then
            call read_param(SCI_RidgeOrientation, param_value, subrsurf(isr)%asargo)
            input_tag(SCI_RidgeOrientation)%acquired = .true.

          else if (input_tag(SCI_DikeSpacing)%in_tag) then
            call read_param(SCI_DikeSpacing, param_value, subrsurf(isr)%asxdks)
            input_tag(SCI_DikeSpacing)%acquired = .true.

          else if (input_tag(SCI_SnowDepth)%in_tag) then
            call read_param(SCI_SnowDepth, param_value, subrsurf(isr)%ahzsnd)
            input_tag(SCI_SnowDepth)%acquired = .true.

          else if (input_tag(SCI_SoilLayNo)%in_tag) then
            call read_param(SCI_SoilLayNo, param_value, subrsurf(isr)%nslay)
            allocate(soillay_complete(subrsurf(isr)%nslay), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory allocation, soillay_complete'
            end if
            ! initialize _complete arrays to false
            do idx = 1, subrsurf(isr)%nslay
              soillay_complete(idx) = .false.
            end do
            ! create subrsurf soil layer arrays
            call create_subregionsoillayers(subrsurf(isr)%nslay, subrsurf(isr))
            input_tag(SCI_SoilLayNo)%acquired = .true.

          else if (input_tag(SCI_SoilLay)%in_tag) then
            if (input_tag(SCI_SoilLayNo)%acquired) then
              if (input_tag(SCI_LayerThickness)%in_tag) then
                call read_param(SCI_LayerThickness, param_value, subrsurf(isr)%bsl(isl)%aszlyt)
                input_tag(SCI_LayerThickness)%acquired = .true.
              else if (input_tag(SCI_BulkDensity)%in_tag) then
                call read_param(SCI_BulkDensity, param_value, subrsurf(isr)%bsl(isl)%asdblk)
                input_tag(SCI_BulkDensity)%acquired = .true.
              else if (input_tag(SCI_Sand)%in_tag) then
                call read_param(SCI_Sand, param_value, subrsurf(isr)%bsl(isl)%asfsan)
                input_tag(SCI_Sand)%acquired = .true.
              else if (input_tag(SCI_Silt)%in_tag) then
                call read_param(SCI_Silt, param_value, subrsurf(isr)%bsl(isl)%asfsil)
                input_tag(SCI_Silt)%acquired = .true.
              else if (input_tag(SCI_Clay)%in_tag) then
                call read_param(SCI_Clay, param_value, subrsurf(isr)%bsl(isl)%asfcla)
                input_tag(SCI_Clay)%acquired = .true.
              else if (input_tag(SCI_VeryFineSand)%in_tag) then
                call read_param(SCI_VeryFineSand, param_value, subrsurf(isr)%bsl(isl)%asfvfs)
                input_tag(SCI_VeryFineSand)%acquired = .true.
              else if (input_tag(SCI_RockVolume)%in_tag) then
                call read_param(SCI_RockVolume, param_value, subrsurf(isr)%bsl(isl)%asvroc)
                input_tag(SCI_RockVolume)%acquired = .true.
              else if (input_tag(SCI_AggregateDensity)%in_tag) then
                call read_param(SCI_AggregateDensity, param_value, subrsurf(isr)%bsl(isl)%asdagd)
                input_tag(SCI_AggregateDensity)%acquired = .true.
              else if (input_tag(SCI_AggregateStability)%in_tag) then
                call read_param(SCI_AggregateStability, param_value, subrsurf(isr)%bsl(isl)%aseags)
                input_tag(SCI_AggregateStability)%acquired = .true.
              else if (input_tag(SCI_AggregateGMD)%in_tag) then
                call read_param(SCI_AggregateGMD, param_value, subrsurf(isr)%bsl(isl)%aslagm)
                input_tag(SCI_AggregateGMD)%acquired = .true.
              else if (input_tag(SCI_AggregateGSD)%in_tag) then
                call read_param(SCI_AggregateGSD, param_value, subrsurf(isr)%bsl(isl)%as0ags)
                input_tag(SCI_AggregateGSD)%acquired = .true.
              else if (input_tag(SCI_AggregateMIN)%in_tag) then
                call read_param(SCI_AggregateMIN, param_value, subrsurf(isr)%bsl(isl)%aslagn)
                input_tag(SCI_AggregateMIN)%acquired = .true.
              else if (input_tag(SCI_AggregateMAX)%in_tag) then
                call read_param(SCI_AggregateMAX, param_value, subrsurf(isr)%bsl(isl)%aslagx)
                input_tag(SCI_AggregateMAX)%acquired = .true.
              else if (input_tag(SCI_WiltingPoint)%in_tag) then
                call read_param(SCI_WiltingPoint, param_value, subrsurf(isr)%bsl(isl)%ahrwcw)
                input_tag(SCI_WiltingPoint)%acquired = .true.
              else if (input_tag(SCI_WaterContent)%in_tag) then
                call read_param(SCI_WaterContent, param_value, subrsurf(isr)%bsl(isl)%ahrwca)
                input_tag(SCI_WaterContent)%acquired = .true.
              end if
              if (    input_tag(SCI_LayerThickness)%acquired &
                .and. input_tag(SCI_BulkDensity)%acquired &
                .and. input_tag(SCI_Sand)%acquired &
                .and. input_tag(SCI_Silt)%acquired &
                .and. input_tag(SCI_Clay)%acquired &
                .and. input_tag(SCI_VeryFineSand)%acquired &
                .and. input_tag(SCI_RockVolume)%acquired &
                .and. input_tag(SCI_AggregateDensity)%acquired &
                .and. input_tag(SCI_AggregateStability)%acquired &
                .and. input_tag(SCI_AggregateGMD)%acquired &
                .and. input_tag(SCI_AggregateGSD)%acquired &
                .and. input_tag(SCI_AggregateMIN)%acquired &
                .and. input_tag(SCI_AggregateMAX)%acquired &
                .and. input_tag(SCI_WiltingPoint)%acquired &
                .and. input_tag(SCI_WaterContent)%acquired ) then
                input_tag(SCI_LayerThickness)%acquired = .false.
                input_tag(SCI_BulkDensity)%acquired = .false.
                input_tag(SCI_Sand)%acquired = .false.
                input_tag(SCI_Silt)%acquired = .false.
                input_tag(SCI_Clay)%acquired = .false.
                input_tag(SCI_VeryFineSand)%acquired = .false.
                input_tag(SCI_RockVolume)%acquired = .false.
                input_tag(SCI_AggregateDensity)%acquired = .false.
                input_tag(SCI_AggregateStability)%acquired = .false.
                input_tag(SCI_AggregateGMD)%acquired = .false.
                input_tag(SCI_AggregateGSD)%acquired = .false.
                input_tag(SCI_AggregateMIN)%acquired = .false.
                input_tag(SCI_AggregateMAX)%acquired = .false.
                input_tag(SCI_WiltingPoint)%acquired = .false.
                input_tag(SCI_WaterContent)%acquired = .false.
                soillay_complete(isl) = .true.
                count_complete = 0
                do idx = 1, subrsurf(isr)%nslay
                  if (soillay_complete(idx)) then
                    count_complete = count_complete + 1
                  end if
                end do
                if (count_complete .ge. subrsurf(isr)%nslay) then
                  input_tag(SCI_SoilLay)%acquired = .true.
                  deallocate(soillay_complete, stat=dealloc_stat)
                  if( dealloc_stat .gt. 0 ) then
                    write(*,*) 'ERROR: memory deallocation, soillay_complete'
                    call exit(1)
                  end if
                end if
              end if
            else
              write(*,*) 'Number of soil layers must precede soil layer data input'
              call exit(1)
            end if

          else if (input_tag(SCI_SurfaceSubDayWaterNo)%in_tag) then
            call read_param(SCI_SurfaceSubDayWaterNo, param_value, subrsurf(isr)%nswet)
            allocate(surfwat_complete(subrsurf(isr)%nswet), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory allocation, surfwat_complete'
              call exit(1)
            end if
            call create_subregionsurfacewet(subrsurf(isr)%nswet, subrsurf(isr))
            input_tag(SCI_SurfaceSubDayWaterNo)%acquired = .true.
            ! initialize _complete arrays to .false.
            do idx = 1, subrsurf(isr)%nswet
              surfwat_complete(idx) = .false.
            end do

          else if (input_tag(SCI_SurfaceSubDayWater)%in_tag) then
            if (input_tag(SCI_SurfaceSubDayWaterNo)%acquired) then
              call read_param(SCI_SurfaceSubDayWater, param_value, subrsurf(isr)%ahrwc0(isurfwat))
              surfwat_complete(isurfwat) = .true.
              count_complete = 0
              do idx = 1, subrsurf(isr)%nswet
                if (surfwat_complete(idx)) then
                  count_complete = count_complete + 1
                end if
              end do
              if (count_complete .ge. subrsurf(isr)%nswet) then
                input_tag(SCI_SurfaceSubDayWater)%acquired = .true.
                deallocate(surfwat_complete, stat=dealloc_stat)
                if( dealloc_stat .gt. 0 ) then
                  write(*,*) 'ERROR: memory deallocation, surfwat_complete'
                  call exit(1)
                end if
              end if
            else
              write(*,*) 'Number of surface water content values must precede surface water value data input'
              call exit(1)
            end if
          end if

          !write(*,*) 'SubregionTags ' &
          !           , input_tag(SCI_coordNo)%acquired &
          !           , input_tag(SCI_coord)%acquired &
          !           , input_tag(SCI_ResidueHeight)%acquired &
          !           , input_tag(SCI_CropHeight)%acquired &
          !           , input_tag(SCI_CropSAI)%acquired &
          !           , input_tag(SCI_CropLAI)%acquired &
          !           , input_tag(SCI_ResidueSAI)%acquired &
          !           , input_tag(SCI_ResidueLAI)%acquired &
          !           , input_tag(SCI_CropRowSpacing)%acquired &
          !           , input_tag(SCI_CropSeedPlace)%acquired &
          !           , input_tag(SCI_BiomassFlatCover)%acquired &
          !           , input_tag(SCI_CrustCover)%acquired &
          !           , input_tag(SCI_CrustThick)%acquired &
          !           , input_tag(SCI_CrustFracCoverLoose)%acquired &
          !           , input_tag(SCI_CrustMassCoverLoose)%acquired &
          !           , input_tag(SCI_CrustDensity)%acquired &
          !           , input_tag(SCI_CrustStability)%acquired &
          !           , input_tag(SCI_RandomRoughness)%acquired &
          !           , input_tag(SCI_RidgeHeight)%acquired &
          !           , input_tag(SCI_RidgeSpacing)%acquired &
          !           , input_tag(SCI_RidgeWidth)%acquired &
          !           , input_tag(SCI_RidgeOrientation)%acquired &
          !           , input_tag(SCI_DikeSpacing)%acquired &
          !           , input_tag(SCI_SnowDepth)%acquired &
          !           , input_tag(SCI_SoilLayNo)%acquired &
          !           , input_tag(SCI_SoilLay)%acquired &
          !           , input_tag(SCI_SurfaceSubDayWaterNo)%acquired &
          !           , input_tag(SCI_SurfaceSubDayWater)%acquired

          if (    input_tag(SCI_coordNo)%acquired &
            .and. input_tag(SCI_coord)%acquired &
            .and. input_tag(SCI_ResidueHeight)%acquired &
            .and. input_tag(SCI_CropHeight)%acquired &
            .and. input_tag(SCI_CropSAI)%acquired &
            .and. input_tag(SCI_CropLAI)%acquired &
            .and. input_tag(SCI_ResidueSAI)%acquired &
            .and. input_tag(SCI_ResidueLAI)%acquired &
            .and. input_tag(SCI_CropRowSpacing)%acquired &
            .and. input_tag(SCI_CropSeedPlace)%acquired &
            .and. input_tag(SCI_BiomassFlatCover)%acquired &
            .and. input_tag(SCI_CrustCover)%acquired &
            .and. input_tag(SCI_CrustThick)%acquired &
            .and. input_tag(SCI_CrustFracCoverLoose)%acquired &
            .and. input_tag(SCI_CrustMassCoverLoose)%acquired &
            .and. input_tag(SCI_CrustDensity)%acquired &
            .and. input_tag(SCI_CrustStability)%acquired &
            .and. input_tag(SCI_RandomRoughness)%acquired &
            .and. input_tag(SCI_RidgeHeight)%acquired &
            .and. input_tag(SCI_RidgeSpacing)%acquired &
            .and. input_tag(SCI_RidgeWidth)%acquired &
            .and. input_tag(SCI_RidgeOrientation)%acquired &
            .and. input_tag(SCI_DikeSpacing)%acquired &
            .and. input_tag(SCI_SnowDepth)%acquired &
            .and. input_tag(SCI_SoilLayNo)%acquired &
            .and. input_tag(SCI_SoilLay)%acquired &
            .and. input_tag(SCI_SurfaceSubDayWaterNo)%acquired &
            .and. input_tag(SCI_SurfaceSubDayWater)%acquired &
            ) then
            input_tag(SCI_coordNo)%acquired = .false.
            input_tag(SCI_coord)%acquired = .false.
            input_tag(SCI_ResidueHeight)%acquired = .false.
            input_tag(SCI_CropHeight)%acquired = .false.
            input_tag(SCI_CropSAI)%acquired = .false.
            input_tag(SCI_CropLAI)%acquired = .false.
            input_tag(SCI_ResidueSAI)%acquired = .false.
            input_tag(SCI_ResidueLAI)%acquired = .false.
            input_tag(SCI_CropRowSpacing)%acquired = .false.
            input_tag(SCI_CropSeedPlace)%acquired = .false.
            input_tag(SCI_BiomassFlatCover)%acquired = .false.
            input_tag(SCI_CrustCover)%acquired = .false.
            input_tag(SCI_CrustThick)%acquired = .false.
            input_tag(SCI_CrustFracCoverLoose)%acquired = .false.
            input_tag(SCI_CrustMassCoverLoose)%acquired = .false.
            input_tag(SCI_CrustDensity)%acquired = .false.
            input_tag(SCI_CrustStability)%acquired = .false.
            input_tag(SCI_RandomRoughness)%acquired = .false.
            input_tag(SCI_RidgeHeight)%acquired = .false.
            input_tag(SCI_RidgeSpacing)%acquired = .false.
            input_tag(SCI_RidgeWidth)%acquired = .false.
            input_tag(SCI_RidgeOrientation)%acquired = .false.
            input_tag(SCI_DikeSpacing)%acquired = .false.
            input_tag(SCI_SnowDepth)%acquired = .false.
            input_tag(SCI_SoilLayNo)%acquired = .false.
            input_tag(SCI_SoilLay)%acquired = .false.
            input_tag(SCI_SurfaceSubDayWaterNo)%acquired = .false.
            input_tag(SCI_SurfaceSubDayWater)%acquired = .false.

            ! use crop and residue values to find the total value
            ! sum the stem area index and leaf area index values
            subrsurf(isr)%abrsai = subrsurf(isr)%acrsai + subrsurf(isr)%adrsaitot
            subrsurf(isr)%abrlai = subrsurf(isr)%acrlai + subrsurf(isr)%adrlaitot
            ! Compute the weighted average "biomass height" (residues and crop)
            ! which is used internally by the erosion code - LEW 1/26/06
            if (subrsurf(isr)%abrsai .le. 0.0) then
              subrsurf(isr)%abzht = 0.0
            else
              subrsurf(isr)%abzht = ( subrsurf(isr)%adzht_ave*subrsurf(isr)%adrsaitot &
                                  + subrsurf(isr)%aczht*subrsurf(isr)%acrsai ) / subrsurf(isr)%abrsai
            endif

            subregion_complete(isr) = .true.
            count_complete = 0
            do idx = 1, nsubr
              if (subregion_complete(idx)) then
                count_complete = count_complete + 1
              end if
            end do
            if (count_complete .ge. nsubr) then
              input_tag(SCI_Subregion)%acquired = .true.
              deallocate(subregion_complete, stat=dealloc_stat)
              if( dealloc_stat .gt. 0 ) then
                write(*,*) 'ERROR: memory deallocation, subregion_complete'
                call exit(1)
              end if
            end if

          end if
        else
          write(*,*) 'Error: Number of subregions must be specified before reading in subregion data'
          call exit(20)
        end if
      else if (input_tag(SCI_BarrierNo)%in_tag) then
        call read_param(SCI_BarrierNo, param_value, nbr)
        input_tag(SCI_BarrierNo)%acquired = .true.
        ! allocate structure for barriers (nbr .lt. 1 gives zero size array)
        sum_stat = 0
        allocate(barrier(nbr), stat = alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(barseas(nbr), stat = alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(barrier_complete(nbr), stat = alloc_stat)
        if( sum_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory alloc., barrier arrays'
        end if
        ! initialize _complete arrays to .false.
        do idx = 1, nbr
          barrier_complete(idx) = .false.
        end do


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
          if (input_tag(SCI_PointNo)%in_tag) then
            call read_param(SCI_PointNo, param_value, poly_np)
            input_tag(SCI_PointNo)%acquired = .true.
            ! create storage for point and barrier data
            ! this also sets values for barr%np and barr%ntm
            ntm_seas = 1
            iseas = 1
            seas_flg = 0
            call create_barrier(barrier(ibr), poly_np)
            call create_barrier(barseas(ibr), poly_np,ntm_seas,seas_flg)
            ! create storage for points index tracking
            allocate(points_complete(poly_np), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              ! allocation failed
              write(*,*) "ERROR: unable to allocate memory for points_complete array"
            end if
            ! initialize _complete arrays to false
            do idx = 1, poly_np
              points_complete(idx) = .false.
            end do

          else if (input_tag(SCI_BarPoint)%in_tag) then
            ! SCI_BarPoint
            if (input_tag(SCI_PointNo)%acquired) then
              if (input_tag(SCI_x)%in_tag) then
                call read_param(SCI_x, param_value, barseas(ibr)%points(ipol)%x)
                input_tag(SCI_x)%acquired = .true.
              else if (input_tag(SCI_y)%in_tag) then
                call read_param(SCI_x, param_value, barseas(ibr)%points(ipol)%y)
                input_tag(SCI_y)%acquired = .true.
              else if (input_tag(SCI_height)%in_tag) then
                call read_param(SCI_height, param_value, barseas(ibr)%param(ipol,iseas)%amzbr)
                input_tag(SCI_height)%acquired = .true.
              else if (input_tag(SCI_width)%in_tag) then
                call read_param(SCI_width, param_value, barseas(ibr)%param(ipol,iseas)%amxbrw)
                input_tag(SCI_width)%acquired = .true.
              else if (input_tag(SCI_porosity)%in_tag) then
                call read_param(SCI_porosity, param_value, barseas(ibr)%param(ipol,iseas)%ampbr)
                input_tag(SCI_porosity)%acquired = .true.
              end if
              if (    input_tag(SCI_x)%acquired &
                .and. input_tag(SCI_y)%acquired &
                .and. input_tag(SCI_height)%acquired &
                .and. input_tag(SCI_width)%acquired &
                .and. input_tag(SCI_porosity)%acquired &
                ) then
                if( barseas(ibr)%param(ipol,iseas)%amzbr .le. 0.0 ) then
                  write(*,*) 'ERROR: Barrier height must be > 0'
                  write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol, 'Season #: ', iseas
                  call exit(40)
                end if
                input_tag(SCI_x)%acquired = .false.
                input_tag(SCI_y)%acquired = .false.
                input_tag(SCI_height)%acquired = .false.
                input_tag(SCI_width)%acquired = .false.
                input_tag(SCI_porosity)%acquired = .false.
                points_complete(ipol) = .true.
                ! copy barseas into fixed barrier
                barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)
                barrier(ibr)%param(ipol) = barseas(ibr)%param(ipol,iseas)
                count_complete = 0
                do idx = 1, poly_np
                  if (points_complete(idx)) then
                    count_complete = count_complete + 1
                  end if
                end do
                if (count_complete .ge. poly_np) then
                  input_tag(SCI_BarPoint)%acquired = .true.
                  deallocate(points_complete, stat=dealloc_stat)
                  if( dealloc_stat .gt. 0 ) then
                    ! deallocation failed
                    write(*,*) "ERROR: unable to deallocate memory for points_complete arrays"
                  end if
                end if
              end if
            else
              write(*,*) 'Error: Number of barrier points must be specified before reading in barrier point data'
              call exit(20)
            end if
          end if

          if ( input_tag(SCI_BarPoint)%acquired ) then
            input_tag(SCI_BarPoint)%acquired = .false.
            barrier_complete(ibr) = .true.
            count_complete = 0
            do idx = 1, nbr
              if (barrier_complete(idx)) then
                count_complete = count_complete + 1
              end if
            end do
            if (count_complete .ge. nbr) then
              input_tag(SCI_Barrier)%acquired = .true.
              deallocate(barrier_complete, stat=dealloc_stat)
              if( dealloc_stat .gt. 0 ) then
                write(*,*) 'ERROR: memory deallocation, barrier_complete'
                call exit(1)
              end if
            end if
          end if

        else
          write(*,*) 'Error: Number of barriers must be specified before reading in barrier data'
        end if

      else if (input_tag(SCI_AirDensity)%in_tag) then
        call read_param(SCI_AirDensity, param_value, awdair)
        input_tag(SCI_AirDensity)%acquired = .true.
      else if (input_tag(SCI_WindDirection)%in_tag) then
        call read_param(SCI_WindDirection, param_value, awadir)
        input_tag(SCI_WindDirection)%acquired = .true.
      else if (input_tag(SCI_AnemometerHeight)%in_tag) then
        call read_param(SCI_AnemometerHeight, param_value, anemht)
        input_tag(SCI_AnemometerHeight)%acquired = .true.
      else if (input_tag(SCI_AerodynamicRoughness)%in_tag) then
        call read_param(SCI_AerodynamicRoughness, param_value, awzzo)
        input_tag(SCI_AerodynamicRoughness)%acquired = .true.
      else if (input_tag(SCI_AnemometerFlag)%in_tag) then
        call read_param(SCI_AnemometerFlag, param_value, wzoflg)
        input_tag(SCI_AnemometerFlag)%acquired = .true.
      else if (input_tag(SCI_AverageAnnualPrecipitation)%in_tag) then
        call read_param(SCI_AverageAnnualPrecipitation, param_value, awzypt)
        input_tag(SCI_AverageAnnualPrecipitation)%acquired = .true.
      else if (input_tag(SCI_WindTimeSteps)%in_tag) then
        call read_param(SCI_WindTimeSteps, param_value, ntstep)
        input_tag(SCI_WindTimeSteps)%acquired = .true.

        ! allocate wind accounting, direction and speed arrays
        sum_stat = 0
        allocate(wind_complete(ntstep), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(subday(ntstep), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        if( sum_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., wind direction and speed'
        end if
        ! initialize _complete arrays to false
        do idx = 1, ntstep
          wind_complete(idx) = .false.
        end do

      else if (input_tag(SCI_WindSpeed)%in_tag) then
        if (input_tag(SCI_WindTimeSteps)%acquired) then
          call read_param(SCI_WindSpeed, param_value, subday(iwind)%awu)
          wind_complete(iwind) = .true.
          count_complete = 0
          do idx = 1, ntstep
            if (wind_complete(idx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. ntstep) then
            input_tag(SCI_WindSpeed)%acquired = .true.
            deallocate(wind_complete, stat=dealloc_stat)
            if( dealloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory deallocation, wind_complete'
              call exit(1)
            end if
            ! Determine the maximum wind speed during the day
            awudmx = 0.0
            do idx = 1, ntstep
              if( awudmx .lt. subday(idx)%awu ) then
                awudmx = subday(idx)%awu
              endif
            end do
          end if
        else
          write(*,*) 'Number of wind speed values must precede wind speed data input'
          call exit(1)
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
