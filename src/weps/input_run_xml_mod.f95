!$Author$
!$Date$
!$Revision$
!$HeadURL$

module input_run_xml_mod
  ! It defines the routines that are called by the XML parser in response
  ! to particular events.

  ! A module such as this could use "utility routines" to convert pcdata
  ! to numerical arrays, and to populate specific data structures.

  use flib_sax
  use datetime_mod, only: lstday, difdat
  use Polygons_Mod, only: polygon, create_polygon, destroy_polygon, set_area_polygon
  use file_io_mod, only: fopenk, luicli, luiwin
  use climate_input_mod, only: cli_gen_fmt_flag, wind_gen_fmt_flag
  use climate_input_mod, only: amzele
  use solar_mod, only: amalat, amalon
  use erosion_data_struct_defs, only: subday, ntstep, am0efl
  use hydro_data_struct_defs, only: am0hfl, am0hdb
  use soil_data_struct_defs, only: am0sfl, am0sdb

  use manage_data_struct_defs, only: manFile, manFileAlloc
  use crop_data_struct_defs, only: am0cfl, am0cdb
  use decomp_data_struct_defs, only: am0dfl, am0ddb
  use input_soil_mod, only: soil_def, soil_in
  use Points_Mod, only: point
  use weps_main_mod, only: farmid, fieldid, rootp, run_rot_cycles, runtype, siteid, tractid, usrnam
  use weps_main_mod, only: clifil, clielevation, cliflag, clilatitude, clilongitude
  use weps_main_mod, only: climethod, clistateid, clistationname, clistationnum
  use weps_main_mod, only: winfil, winflag, wincountry, winlatitude, winlongitude
  use weps_main_mod, only: winmethod, winstate, winstationname, winstationnum
  use weps_main_mod, only: id, im, iy, ld, lm, ly
  use barriers_mod, only: create_barrier, barrier, barseas
  use barriers_mod, only: barrier_day_state, barrier_params, barrier_climate
  use read_write_xml_mod, only: read_param
  use weps_cmdline_parms, only: report_info

  integer, parameter :: MAX_NAME_LEN  = 40

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: required                    ! .true. if tag is required
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def

  type(tag_def), dimension(:), allocatable :: run_tag
  integer :: max_tags
  
  integer, parameter :: runFileData               = 1

  integer, parameter :: SCI_CycleCount            = 2
  integer, parameter :: SCI_CentrLatitude         = 3
  integer, parameter :: SCI_CentrLongitude        = 4
  integer, parameter :: SCI_Elevation             = 5
  integer, parameter :: SCI_StartDate             = 6
  integer, parameter :: SCI_EndDate               = 7
  integer, parameter :: SCI_Day                   = 8
  integer, parameter :: SCI_Month                 = 9
  integer, parameter :: SCI_Year                  = 10
  integer, parameter :: SCI_TimeSteps             = 11
  integer, parameter :: SCI_climateFile           = 12
  integer, parameter :: SCI_windFile              = 13
  integer, parameter :: SCI_ErosionSubmodelOutput = 14
  integer, parameter :: SCI_RegionAngle           = 15
  integer, parameter :: SCI_XOrigin               = 16
  integer, parameter :: SCI_YOrigin               = 17
  integer, parameter :: SCI_XLength               = 18
  integer, parameter :: SCI_YLength               = 19

  integer, parameter :: SCI_number                = 20
  integer, parameter :: SCI_index                 = 21

  integer, parameter :: SCI_Subregions            = 22
  integer, parameter :: SCI_Subregion             = 23
  integer, parameter :: SCI_SubmodelOutput        = 24
  integer, parameter :: SCI_DebugOutput           = 25
  integer, parameter :: SCI_hydro                 = 26
  integer, parameter :: SCI_soil                  = 27
  integer, parameter :: SCI_man                   = 28
  integer, parameter :: SCI_crop                  = 29
  integer, parameter :: SCI_decomp                = 30
  integer, parameter :: SCI_AverageSlope          = 31
  integer, parameter :: SCI_SoilRockFragments     = 32
  integer, parameter :: SCI_SoilFile              = 33
  integer, parameter :: SCI_ManageFile            = 34
  integer, parameter :: SCI_WaterErosionLoss      = 35

  integer, parameter :: SCI_BarrierSeasonFlag     = 36
  integer, parameter :: SCI_BarCliNumber          = 37
  integer, parameter :: SCI_CoordinateNumber      = 38
  integer, parameter :: SCI_BarCliIndex           = 39
  integer, parameter :: SCI_coordIndex            = 40

  integer, parameter :: SCI_Barriers              = 41
  integer, parameter :: SCI_Barrier               = 42
  integer, parameter :: SCI_Description           = 43
  integer, parameter :: SCI_PointBarClis          = 44
  integer, parameter :: SCI_BarCli                = 45
  integer, parameter :: SCI_TimeDesc              = 46
  integer, parameter :: SCI_TimeMark              = 47
  integer, parameter :: SCI_coord                 = 48
  integer, parameter :: SCI_x                     = 49
  integer, parameter :: SCI_y                     = 50
  integer, parameter :: SCI_PointBarCli           = 51
  integer, parameter :: SCI_height                = 52
  integer, parameter :: SCI_width                 = 53
  integer, parameter :: SCI_porosity              = 54
  integer, parameter :: SCI_BegTranFlg            = 55
  integer, parameter :: SCI_BegTranThresh         = 56
  integer, parameter :: SCI_BegTranBase           = 57
  integer, parameter :: SCI_EndTranFlg            = 58
  integer, parameter :: SCI_EndTranThresh         = 59
  integer, parameter :: SCI_EndTranBase           = 60

  integer, parameter :: GUI_UserName              = 61
  integer, parameter :: GUI_FarmId                = 62
  integer, parameter :: GUI_TractId               = 63
  integer, parameter :: GUI_FieldId               = 64
  integer, parameter :: GUI_RunTypeDisp           = 65
  integer, parameter :: GUI_Site                  = 66
  integer, parameter :: GUI_cligenFlag            = 67
  integer, parameter :: GUI_cligenMethod          = 68
  integer, parameter :: GUI_cligenLatitude        = 69
  integer, parameter :: GUI_cligenLongitude       = 70
  integer, parameter :: GUI_cligenStateId         = 71
  integer, parameter :: GUI_cligenStationNum      = 72
  integer, parameter :: GUI_cligenStationName     = 73
  integer, parameter :: GUI_cligenElevation       = 74
  integer, parameter :: GUI_windgenFlag           = 75
  integer, parameter :: GUI_windgenMethod         = 76
  integer, parameter :: GUI_windgenLatitude       = 77
  integer, parameter :: GUI_windgenLongitude      = 78
  integer, parameter :: GUI_windgenStationNum     = 79
  integer, parameter :: GUI_windgenCountry        = 80
  integer, parameter :: GUI_windgenState          = 81
  integer, parameter :: GUI_windgenStationName    = 82
  integer, parameter :: GUI_lastrun               = 83
  integer, parameter :: GUI_lat                   = 84
  integer, parameter :: GUI_lon                   = 85

  integer, parameter :: max_simyear = 100000  ! value used to test simulation year input range
  integer :: nsubr    ! Number of subregions
  integer :: nbr      ! number of barriers
  integer :: seas_flg ! barrier season flag
  integer :: ntm_seas ! number of time marks for seasonal barrier
  integer :: poly_np  ! number of points in polygon or polyline
  integer :: isr      ! index for subregion reading
  integer :: ibr      ! index for barrier reading
  integer :: ipol     ! index for polygon reading
  integer :: iseas    ! index for barrier season reading
  logical :: barriers_present
  logical, dimension(:), allocatable :: barrier_complete
  logical, dimension(:), allocatable :: subregion_complete
  logical, dimension(:), allocatable :: season_complete
  logical, dimension(:,:), allocatable :: clipar_complete
  logical :: bar_seasons
  logical :: bar_coords
  logical :: bar_params
  logical, dimension(:), allocatable :: coord_complete
  integer :: count_complete
  logical :: runfile_complete

contains

  subroutine init_run_xml()

    integer :: idx
    integer :: alloc_stat

    max_tags = 85   ! count of unique tags needed from all dtd files
    allocate( run_tag(max_tags), stat=alloc_stat)
    if( alloc_stat .gt. 0 ) then
      write(*,*) 'ERROR: memory alloc., run_tag'
    end if

    ! assign defaults to flag status values
    do idx = 1, max_tags
      run_tag(idx)%required = .true.
      run_tag(idx)%acquired = .false.
      run_tag(idx)%in_tag = .false.
    end do

    ! assign tag names
    run_tag(1)%name = "runFileData"

    run_tag(2)%name = "SCI_CycleCount"
    run_tag(3)%name = "SCI_CentrLatitude"
    run_tag(4)%name = "SCI_CentrLongitude"
    run_tag(5)%name = "SCI_Elevation"
    run_tag(6)%name = "SCI_StartDate"
    run_tag(7)%name = "SCI_EndDate"
    run_tag(8)%name = "SCI_Day"
    run_tag(9)%name = "SCI_Month"
    run_tag(10)%name = "SCI_Year"
    run_tag(11)%name = "SCI_TimeSteps"
    run_tag(12)%name = "SCI_climateFile"
    run_tag(13)%name = "SCI_windFile"
    run_tag(14)%name = "SCI_ErosionSubmodelOutput"
    run_tag(15)%name = "SCI_RegionAngle"
    run_tag(16)%name = "SCI_XOrigin"
    run_tag(17)%name = "SCI_YOrigin"
    run_tag(18)%name = "SCI_XLength"
    run_tag(19)%name = "SCI_YLength"

    run_tag(20)%name = "SCI_number"
    run_tag(21)%name = "SCI_index"

    run_tag(22)%name = "SCI_Subregions"
    run_tag(23)%name = "SCI_Subregion"
    run_tag(24)%name = "SCI_SubmodelOutput"
    run_tag(25)%name = "SCI_DebugOutput"
    run_tag(26)%name = "SCI_hydro"
    run_tag(27)%name = "SCI_soil"
    run_tag(28)%name = "SCI_man"
    run_tag(29)%name = "SCI_crop"
    run_tag(30)%name = "SCI_decomp"
    run_tag(31)%name = "SCI_AverageSlope"
    run_tag(32)%name = "SCI_SoilRockFragments"
    run_tag(33)%name = "SCI_SoilFile"
    run_tag(34)%name = "SCI_ManageFile"
    run_tag(35)%name = "SCI_WaterErosionLoss"

    run_tag(36)%name = "SCI_BarrierSeasonFlag"
    run_tag(37)%name = "SCI_BarCliNumber"
    run_tag(38)%name = "SCI_CoordinateNumber"
    run_tag(39)%name = "SCI_BarCliIndex"
    run_tag(40)%name = "SCI_coordIndex"

    run_tag(41)%name = "SCI_Barriers"
    run_tag(42)%name = "SCI_Barrier"
    run_tag(43)%name = "SCI_Description"
    run_tag(44)%name = "SCI_PointBarClis"
    run_tag(45)%name = "SCI_BarCli"
    run_tag(46)%name = "SCI_TimeDesc"
    run_tag(47)%name = "SCI_TimeMark"
    run_tag(48)%name = "SCI_coord"
    run_tag(49)%name = "SCI_x"
    run_tag(50)%name = "SCI_y"
    run_tag(51)%name = "SCI_PointBarCli"
    run_tag(52)%name = "SCI_height"
    run_tag(53)%name = "SCI_width"
    run_tag(54)%name = "SCI_porosity"
    run_tag(55)%name = "SCI_BegTranFlg"
    run_tag(56)%name = "SCI_BegTranThresh"
    run_tag(57)%name = "SCI_BegTranBase"
    run_tag(58)%name = "SCI_EndTranFlg"
    run_tag(59)%name = "SCI_EndTranThresh"
    run_tag(60)%name = "SCI_EndTranBase"

    run_tag(61)%name = "GUI_UserName"
    run_tag(62)%name = "GUI_FarmId"
    run_tag(63)%name = "GUI_TractId"
    run_tag(64)%name = "GUI_FieldId"
    run_tag(65)%name = "GUI_RunTypeDisp"
    run_tag(66)%name = "GUI_Site"
    run_tag(67)%name = "GUI_cligenFlag"
    run_tag(68)%name = "GUI_cligenMethod"
    run_tag(69)%name = "GUI_cligenLatitude"
    run_tag(70)%name = "GUI_cligenLongitude"
    run_tag(71)%name = "GUI_cligenStateId"
    run_tag(72)%name = "GUI_cligenStationNum"
    run_tag(73)%name = "GUI_cligenStationName"
    run_tag(74)%name = "GUI_cligenElevation"
    run_tag(75)%name = "GUI_windgenFlag"
    run_tag(76)%name = "GUI_windgenMethod"
    run_tag(77)%name = "GUI_windgenLatitude"
    run_tag(78)%name = "GUI_windgenLongitude"
    run_tag(79)%name = "GUI_windgenStationNum"
    run_tag(80)%name = "GUI_windgenCountry"
    run_tag(81)%name = "GUI_windgenState"
    run_tag(82)%name = "GUI_windgenStationName"
    run_tag(83)%name = "GUI_lastrun"
    run_tag(84)%name = "GUI_lat"
    run_tag(85)%name = "GUI_lon"


    ! create integer variable names for tags and assign index number.
    ! makes chunk code more understandable.

    barriers_present = .false.
    bar_seasons = .false.
    bar_coords = .false.
    bar_params = .false.

  end subroutine init_run_xml

  subroutine begin_element_handler(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx
    character(len=80) :: param_value
    integer :: ret_stat
    integer :: alloc_stat
    integer :: sum_stat

    !write(*,*) ">>Begin Element: ", name
    !write(*,*) "--- ", len(attributes), " attributes:"
    !call print_dict(attributes)

    do idx = 1, size(run_tag)
      if( run_tag(idx)%name .eq. name ) then
        run_tag(idx)%in_tag = .true.
        ! write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

    if (   (idx .eq. SCI_Subregions) &
      .or. (idx .eq. SCI_Barriers) ) then
      if ( has_key(attributes, run_tag(SCI_number)%name) ) then
        call get_value(attributes, run_tag(SCI_number)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregions)
          call read_param(run_tag(SCI_number)%name, param_value, nsubr)
          !write(*,*) 'Number of Subregions: ', nsubr
          if (nsubr .lt. 1) then
            write(*,*) 'Error, subregion count must be 1 or greater. Value: ', nsubr
            call exit(1)
          end if
          sum_stat = 0
          ! create arrays for submodel output flags
          allocate(am0hfl(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(am0sfl(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(am0cfl(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(am0dfl(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          ! create arrays for submodel debug flags
          allocate(am0hdb(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(am0sdb(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(am0cdb(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(am0ddb(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(soil_in(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(subregion_complete(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          if( sum_stat .gt. 0 ) then
            write(*,*) 'ERROR: memory alloc., subregion arrays'
          end if
          ! initialize _complete arrays to false
          do idx = 1, nsubr
            subregion_complete(idx) = .false.
          end do

          call manFileAlloc(nsubr)

        case (SCI_Barriers)
          call read_param(run_tag(SCI_number)%name, param_value, nbr)
          !write(*,*) 'Number of Barriers: ', nbr
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
          barriers_present = .true.

        end select
      else
        write(*,*) 'SCI_number attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
    else if ( (idx .eq. SCI_Subregion) &
      .or. (idx .eq. SCI_coord) &
      .or. (idx .eq. SCI_BarCli) ) then
      if ( has_key(attributes, run_tag(SCI_index)%name) ) then
        call get_value(attributes, run_tag(SCI_index)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregion)
          call read_param(run_tag(SCI_index)%name, param_value, isr)
          ! adjust from base 0 to base 1 arrays
          isr = isr + 1
          !write(*,*) 'Subregion Index: ', isr
        case (SCI_coord)
          call read_param(run_tag(SCI_index)%name, param_value, ipol)
          ! adjust from base 0 to base 1 arrays
          ipol = ipol + 1
          !write(*,*) 'Subregion Coordinates Point Index: ', ipol
        case (SCI_BarCli)
          call read_param(run_tag(SCI_index)%name, param_value, iseas)
          ! adjust from base 0 to base 1 arrays
          iseas = iseas + 1
          !write(*,*) 'Barrier Point Index: ', ipol
        end select
      else
        write(*,*) 'SCI_index attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    else if ( (idx .eq. SCI_Barrier) ) then
      if ( has_key(attributes, run_tag(SCI_index)%name) ) then
        call get_value(attributes, run_tag(SCI_index)%name, param_value, ret_stat)
        call read_param(run_tag(SCI_index)%name, param_value, ibr)
        ! adjust from base 0 to base 1 arrays
        ibr = ibr + 1
        !write(*,*) 'Barrier Index: ', ibr
      else
        write(*,*) 'SCI_index attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
      if ( has_key(attributes, run_tag(SCI_BarrierSeasonFlag)%name) ) then
        call get_value(attributes, run_tag(SCI_BarrierSeasonFlag)%name, param_value, ret_stat)
        call read_param(run_tag(SCI_BarrierSeasonFlag)%name, param_value, seas_flg)
        !write(*,*) 'Barrier Season Flag: ', seas_flg
        if (    (seas_flg .ne. 0) &
          .and. (seas_flg .ne. 1) &
          .and. (seas_flg .ne. 2) &
          ) then
          write(*,*) 'ERROR: Barrier season flag value must be 0, 1 or 2'
          write(*,FMT='(i0)') 'Input value was: ', seas_flg
          call exit(35)
        end if
      else
        write(*,*) 'SCI_BarrierSeasonFlag attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    else if ( (idx .eq. SCI_PointBarClis) ) then
      if ( has_key(attributes, run_tag(SCI_BarCliNumber)%name) ) then
        call get_value(attributes, run_tag(SCI_BarCliNumber)%name, param_value, ret_stat)
        call read_param(run_tag(SCI_BarCliNumber)%name, param_value, ntm_seas)
        !write(*,*) 'Barrier Total Time Marks: ', ntm_seas
      else
        write(*,*) 'SCI_BarCliNumber attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
      if ( has_key(attributes, run_tag(SCI_CoordinateNumber)%name) ) then
        call get_value(attributes, run_tag(SCI_CoordinateNumber)%name, param_value, ret_stat)
        call read_param(run_tag(SCI_CoordinateNumber)%name, param_value, poly_np)
        !write(*,*) 'Number of Barrier Points: ', poly_np
        if (poly_np .ge. 2) then
        else
          write(*,'(2(a,i0))') 'Tag SCI_PointBarClis SCI_CoordinateNumber="',ibr-1, &
                               '" must have at least 2 points. Only has ', poly_np
          call exit(1)
        end if
      else
        write(*,*) 'SCI_CoordinateNumber attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
      ! create storage for point and barrier data
      ! this also sets values for barr%np and barr%ntm
      call create_barrier(barrier(ibr), poly_np)
      call create_barrier(barseas(ibr), poly_np,ntm_seas,seas_flg)
      ! create storage for season, points and climate parameter index tracking
      sum_stat = 0
      allocate(season_complete(ntm_seas), stat = alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(coord_complete(poly_np), stat = alloc_stat)
      sum_stat = sum_stat + alloc_stat
      allocate(clipar_complete(poly_np, ntm_seas), stat = alloc_stat)
      sum_stat = sum_stat + alloc_stat
      if( sum_stat .gt. 0 ) then
        ! deallocation failed
        write(*,*) "ERROR: unable to allocate memory for _complete arrays"
      end if
      ! initialize _complete arrays to false
      do iseas = 1, ntm_seas
        season_complete(iseas) = .false.
      end do
      do ipol = 1, poly_np
        coord_complete(ipol) = .false.
      end do
      do iseas = 1, ntm_seas
        do ipol = 1, poly_np
          clipar_complete(ipol, iseas) = .false.
        end do
      end do

    else if ( (idx .eq. SCI_PointBarCli) ) then
      if ( has_key(attributes, run_tag(SCI_BarCliIndex)%name) ) then
        call get_value(attributes, run_tag(SCI_BarCliIndex)%name, param_value, ret_stat)
        call read_param(run_tag(SCI_BarCliIndex)%name, param_value, iseas)
        ! adjust from base 0 to base 1 arrays
        iseas = iseas + 1
        !write(*,*) 'Barrier Time Marks Index: ', iseas
      else
        write(*,*) 'SCI_BarCliIndex attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
      if ( has_key(attributes, run_tag(SCI_coordIndex)%name) ) then
        call get_value(attributes, run_tag(SCI_coordIndex)%name, param_value, ret_stat)
        call read_param(run_tag(SCI_CoordinateNumber)%name, param_value, ipol)
        ! adjust from base 0 to base 1 arrays
        ipol = ipol + 1
        !write(*,*) 'Barrier Points Index: ', ipol
      else
        write(*,*) 'SCI_coordIndex attribute required for each ', trim(run_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    end if

  end subroutine begin_element_handler

  subroutine end_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx, jdx, kdx
    integer :: sum_stat, alloc_stat, dealloc_stat

    do idx = 1, size(run_tag)
      if( run_tag(idx)%name .eq. name ) then
        run_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. runFileData) then

          if ( .not. barriers_present) then
            ! no SCI_Barriers tag found (not needed so set to true)
            run_tag(SCI_Barriers)%acquired = .true.
            ! allocate structure for barriers (zero size array allowed)
            sum_stat = 0
            allocate(barrier(0), stat = alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(barseas(0), stat = alloc_stat)
            sum_stat = sum_stat + alloc_stat
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory alloc., barriers'
            end if
          end if

          ! check for acquisition of all required elements
          if (    run_tag(SCI_CycleCount)%acquired &
            .and. run_tag(SCI_CentrLatitude)%acquired &
            .and. run_tag(SCI_CentrLongitude)%acquired &
            .and. run_tag(SCI_Elevation)%acquired &
            .and. run_tag(SCI_StartDate)%acquired &
            .and. run_tag(SCI_EndDate)%acquired &
            .and. run_tag(SCI_TimeSteps)%acquired &
            .and. run_tag(SCI_climateFile)%acquired &
            .and. run_tag(SCI_windFile)%acquired &
            .and. run_tag(SCI_ErosionSubmodelOutput)%acquired &
            .and. run_tag(SCI_RegionAngle)%acquired &
            .and. run_tag(SCI_XOrigin)%acquired &
            .and. run_tag(SCI_YOrigin)%acquired &
            .and. run_tag(SCI_XLength)%acquired &
            .and. run_tag(SCI_YLength)%acquired &
            .and. run_tag(SCI_Subregions)%acquired &
            .and. run_tag(SCI_Barriers)%acquired &
            ) then
            runfile_complete = .true.
          else
            runfile_complete = .false.
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_CycleCount, &
                    SCI_CentrLatitude, &
                    SCI_CentrLongitude, &
                    SCI_Elevation, &
                    SCI_StartDate, &
                    SCI_EndDate, &
                    SCI_TimeSteps, &
                    SCI_climateFile, &
                    SCI_windFile, &
                    SCI_ErosionSubmodelOutput, &
                    SCI_RegionAngle, &
                    SCI_XOrigin, &
                    SCI_YOrigin, &
                    SCI_XLength, &
                    SCI_YLength)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_Subregions, &
                    SCI_Barriers)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is incomplete in input file.'
                end if
              end select
            end do
            write(*,*) 'No Results Generated.'
          end if
          ! deallocate tag array
          deallocate( run_tag, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for tag array"
          end if

        else if (idx .eq. SCI_StartDate) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_Day)%acquired &
            .and. run_tag(SCI_Month)%acquired &
            .and. run_tag(SCI_Year)%acquired &
            ) then 
            run_tag(SCI_Day)%acquired = .false.
            run_tag(SCI_Month)%acquired = .false.
            run_tag(SCI_Year)%acquired = .false.
            run_tag(SCI_StartDate)%acquired = .true.
            ! check for valid date values
            if ((id .lt. 1) .or. (id .gt. lstday(im,iy))) then
              write(*,*) 'Start date day of month: ', id, ' is not valid'
              call exit(1)
            end if
            if ((im .lt. 1) .or. (im .gt. 12)) then
              write(*,*) 'Start date month of year: ', im, ' is not valid'
              call exit(1)
            end if
            if ((iy .lt. 0) .or. (iy .gt.max_simyear)) then
              write(*,*) 'Start date year: ', im, ' must be less than ', max_simyear
              call exit(1)
            end if
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_Day, &
                    SCI_Month, &
                    SCI_Year)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_EndDate) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_Day)%acquired &
            .and. run_tag(SCI_Month)%acquired &
            .and. run_tag(SCI_Year)%acquired &
            ) then 
            run_tag(SCI_Day)%acquired = .false.
            run_tag(SCI_Month)%acquired = .false.
            run_tag(SCI_Year)%acquired = .false.
            run_tag(SCI_EndDate)%acquired = .true.
            ! check for valid date values
            if ((ld .lt. 1) .or. (ld .gt. lstday(lm,ly))) then
              write(*,*) 'Start date day of month: ', ld, ' is not valid'
              call exit(1)
            end if
            if ((lm .lt. 1) .or. (lm .gt. 12)) then
              write(*,*) 'Start date month of year: ', lm, ' is not valid'
              call exit(1)
            end if
            if ((ly .lt. 0) .or. (ly .gt.max_simyear)) then
              write(*,*) 'Start date year: ', im, ' must be less than ', max_simyear
              call exit(1)
            end if
            if (difdat(id, im, iy, ld, lm, ly) .le. 0) then
              write(*,*) 'Start date must be less than end date'
              write(*,*) ''
              call exit(1)
            end if

          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_Day, &
                    SCI_Month, &
                    SCI_Year)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_TimeSteps) then
          ! allocate wind direction and speed array
          allocate(subday(ntstep), stat=alloc_stat)
          if( alloc_stat .gt. 0 ) then
             write(*,*) 'ERROR: memory alloc., wind direction and speed'
          end if

        else if (idx .eq. SCI_Subregions) then
          !write(*,*) 'SCI_Subregions end of tag, nsubr: ', nsubr
          count_complete = 0
          do jdx = 1, nsubr
            if (subregion_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. nsubr) then
            run_tag(SCI_Subregions)%acquired = .true.
          else
            do jdx = 1, nsubr
              if ( .not. subregion_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(run_tag(SCI_Subregion)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(subregion_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for tags and subregion_complete arrays"
          end if

        else if (idx .eq. SCI_Barriers) then
          count_complete = 0
          do jdx = 1, nbr
            if (barrier_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. nbr) then
            run_tag(SCI_Barriers)%acquired = .true.
          else
            do jdx = 1, size(barrier_complete)
              if ( .not. barrier_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(run_tag(SCI_Barrier)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete arrays
          deallocate(barrier_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for barrier_complete array"
          end if

        else if (idx .eq. SCI_Barrier) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_Description)%acquired &
            .and. run_tag(SCI_PointBarClis)%acquired ) then 
            run_tag(SCI_Description)%acquired = .false.
            run_tag(SCI_PointBarClis)%acquired = .false.
            barrier_complete(ibr) = .true.
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_Description)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_PointBarClis)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is incomplete or missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_PointBarClis) then
          ! check for acquisition of all required elements
          ! seasons
          count_complete = 0
          do jdx = 1, ntm_seas
            if (season_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. ntm_seas) then
            bar_seasons = .true.
          else
            do jdx = 1, ntm_seas
              if ( .not. season_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(run_tag(SCI_BarCli)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! points
          count_complete = 0
          do jdx = 1, poly_np
            if (coord_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. poly_np) then
            bar_coords = .true.
          else
            do jdx = 1, poly_np
              if ( .not. coord_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(run_tag(SCI_coord)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! barrier parameters
          count_complete = 0
          do jdx = 1, poly_np
            do kdx = 1, ntm_seas
              if (clipar_complete(jdx, kdx)) then
                count_complete = count_complete + 1
              end if
            end do
          end do
          if (count_complete .ge. poly_np*ntm_seas) then
            bar_params = .true.
          else
            do jdx = 1, poly_np
              do kdx = 1, ntm_seas
                if ( .not. clipar_complete(jdx, kdx)) then
                  write(*,'(3a,2(i0,a))') 'Tag ', trim(run_tag(SCI_PointBarCli)%name), &
                                       ' SCI_coordIndex="', jdx-1, '" SCI_BarCliIndex="',kdx-1, &
                                       '" is incomplete or missing from input file.'
                end if
              end do
            end do
          end if
          ! check for all complete
          if ( bar_seasons .and. bar_coords .and. bar_params ) then
            bar_seasons = .false.
            bar_coords = .false.
            bar_params = .false.
            run_tag(SCI_PointBarClis)%acquired = .true.
          end if
          ! deallocate _complete arrays
          sum_stat = 0
          deallocate(season_complete, stat=dealloc_stat)
          sum_stat = sum_stat + dealloc_stat
          deallocate(coord_complete, stat=dealloc_stat)
          sum_stat = sum_stat + dealloc_stat
          deallocate(clipar_complete, stat=dealloc_stat)
          sum_stat = sum_stat + dealloc_stat
          if( sum_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for _complete arrays"
          end if

        else if (idx .eq. SCI_BarCli) then
          if( seas_flg .eq. 2 ) then
            if (    run_tag(SCI_TimeMark)%acquired & 
              .and. run_tag(SCI_TimeDesc)%acquired &
              .and. run_tag(SCI_BegTranFlg)%acquired &
              .and. run_tag(SCI_BegTranThresh)%acquired &
              .and. run_tag(SCI_BegTranBase)%acquired &
              .and. run_tag(SCI_EndTranFlg)%acquired &
              .and. run_tag(SCI_EndTranThresh)%acquired &
              .and. run_tag(SCI_EndTranBase)%acquired &
              ) then
              run_tag(SCI_TimeMark)%acquired = .false.
              run_tag(SCI_TimeDesc)%acquired = .false.
              run_tag(SCI_BegTranFlg)%acquired = .false.
              run_tag(SCI_BegTranThresh)%acquired = .false.
              run_tag(SCI_BegTranBase)%acquired = .false.
              run_tag(SCI_EndTranFlg)%acquired = .false.
              run_tag(SCI_EndTranThresh)%acquired = .false.
              run_tag(SCI_EndTranBase)%acquired = .false.
              season_complete(iseas) = .true.
            else
              do jdx = 1, size(run_tag)
                select case (jdx)
                case (SCI_TimeMark, &
                      SCI_TimeDesc, &
                      SCI_BegTranFlg, &
                      SCI_BegTranThresh, &
                      SCI_BegTranBase, &
                      SCI_EndTranFlg, &
                      SCI_EndTranThresh, &
                      SCI_EndTranBase)
                  if( .not. run_tag(jdx)%acquired ) then
                    write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                  end if
                end select
              end do
            end if  
          else 
            if (    run_tag(SCI_TimeMark)%acquired & 
              .and. run_tag(SCI_TimeDesc)%acquired &
              ) then
              run_tag(SCI_TimeMark)%acquired = .false.
              run_tag(SCI_TimeDesc)%acquired = .false.
              season_complete(iseas) = .true.
            else
              do jdx = 1, size(run_tag)
                select case (jdx)
                case (SCI_TimeMark, &
                      SCI_TimeDesc)
                  if( .not. run_tag(jdx)%acquired ) then
                    write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                  end if
                end select
              end do
            end if
          end if

        else if (idx .eq. SCI_PointBarCli) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_height)%acquired &
            .and. run_tag(SCI_width)%acquired &
            .and. run_tag(SCI_porosity)%acquired ) then 
            run_tag(SCI_height)%acquired = .false.
            run_tag(SCI_width)%acquired = .false.
            run_tag(SCI_porosity)%acquired = .false.
            clipar_complete(ipol,iseas) = .true.
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_height, &
                    SCI_width, &
                    SCI_porosity)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_coord) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_x)%acquired &
            .and. run_tag(SCI_y)%acquired ) then 
            run_tag(SCI_x)%acquired = .false.
            run_tag(SCI_y)%acquired = .false.
            coord_complete(ipol) = .true.
            if ( run_tag(SCI_Barrier)%in_tag ) then
              !  also place in fixed barrier structure
              barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)
            end if
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_x, &
                    SCI_y )
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_Subregion) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_SubmodelOutput)%acquired &
            .and. run_tag(SCI_DebugOutput)%acquired &
            .and. run_tag(SCI_AverageSlope)%acquired &
            .and. run_tag(SCI_SoilRockFragments)%acquired &
            .and. run_tag(SCI_SoilFile)%acquired &
            .and. run_tag(SCI_ManageFile)%acquired &
            .and. run_tag(SCI_WaterErosionLoss)%acquired &
            ) then 
            run_tag(SCI_SubmodelOutput)%acquired = .false.
            run_tag(SCI_DebugOutput)%acquired = .false.
            run_tag(SCI_AverageSlope)%acquired = .false.
            run_tag(SCI_SoilRockFragments)%acquired = .false.
            run_tag(SCI_SoilFile)%acquired = .false.
            run_tag(SCI_ManageFile)%acquired = .false.
            run_tag(SCI_WaterErosionLoss)%acquired = .false.
            subregion_complete(isr) = .true.
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_AverageSlope, &
                    SCI_SoilRockFragments, &
                    SCI_SoilFile, &
                    SCI_ManageFile, &
                    SCI_WaterErosionLoss)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_SubmodelOutput, &
                    SCI_DebugOutput)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is incomplete or missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_SubmodelOutput) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_hydro)%acquired &
            .and. run_tag(SCI_soil)%acquired &
            .and. run_tag(SCI_man)%acquired &
            .and. run_tag(SCI_crop)%acquired &
            .and. run_tag(SCI_decomp)%acquired &
            ) then 
            run_tag(SCI_hydro)%acquired = .false.
            run_tag(SCI_soil)%acquired = .false.
            run_tag(SCI_man)%acquired = .false.
            run_tag(SCI_crop)%acquired = .false.
            run_tag(SCI_decomp)%acquired = .false.
            run_tag(SCI_SubmodelOutput)%acquired = .true.
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_hydro, &
                    SCI_soil, &
                    SCI_man, &
                    SCI_crop, &
                    SCI_decomp)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_DebugOutput) then
          ! check for acquisition of all required elements
          if (    run_tag(SCI_hydro)%acquired &
            .and. run_tag(SCI_soil)%acquired &
            .and. run_tag(SCI_man)%acquired &
            .and. run_tag(SCI_crop)%acquired &
            .and. run_tag(SCI_decomp)%acquired &
            ) then 
            run_tag(SCI_hydro)%acquired = .false.
            run_tag(SCI_soil)%acquired = .false.
            run_tag(SCI_man)%acquired = .false.
            run_tag(SCI_crop)%acquired = .false.
            run_tag(SCI_decomp)%acquired = .false.
            run_tag(SCI_DebugOutput)%acquired = .true.
          else
            do jdx = 1, size(run_tag)
              select case (jdx)
              case (SCI_hydro, &
                    SCI_soil, &
                    SCI_man, &
                    SCI_crop, &
                    SCI_decomp)
                if( .not. run_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(run_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        end if

        exit  ! found tag, no need to look further

      end if
    end do

  end subroutine end_element_handler

  subroutine pcdata_chunk_handler(chunk)
    use grid_mod, only: amasim, amxsim, sim_area
    character(len=*), intent(in) :: chunk

    character(len=256) :: param_value
    integer :: read_stat
    real :: cligen_version

    param_value = trim(chunk)

    if (run_tag(runFileData)%in_tag) then
      if (run_tag(SCI_CycleCount)%in_tag) then
        call read_param(run_tag(SCI_CycleCount)%name, param_value, run_rot_cycles)
        run_tag(SCI_CycleCount)%acquired = .true.

      else if (run_tag(SCI_CentrLatitude)%in_tag) then
        call read_param(run_tag(SCI_CentrLatitude)%name, param_value, amalat)
        if ((amalat .lt. -90.) .or. (amalat .gt. 90.)) then
           write (*,*)'ERROR: latitude is not between -90. and 90. degrees. Please check run file'
           call exit(1)
        end if
        run_tag(SCI_CentrLatitude)%acquired = .true.

      else if (run_tag(SCI_CentrLongitude)%in_tag) then
        call read_param(run_tag(SCI_CentrLongitude)%name, param_value, amalon)
        if ((amalon .lt. -180.) .or. (amalon .gt. 180.)) then
           write (*,*) 'ERROR: longitude is not between -180. and 180. degrees. Please check run file'
           call exit(1)
        end if
        run_tag(SCI_CentrLongitude)%acquired = .true.

      else if (run_tag(SCI_Elevation)%in_tag) then
        call read_param(run_tag(SCI_Elevation)%name, param_value, amzele)
        run_tag(SCI_Elevation)%acquired = .true.

      else if (run_tag(SCI_StartDate)%in_tag) then
        if (run_tag(SCI_Day)%in_tag) then
          call read_param(run_tag(SCI_Day)%name, param_value, id)
          run_tag(SCI_Day)%acquired = .true.

        else if (run_tag(SCI_Month)%in_tag) then
          call read_param(run_tag(SCI_Month)%name, param_value, im)
          run_tag(SCI_Month)%acquired = .true.

        else if (run_tag(SCI_Year)%in_tag) then
          call read_param(run_tag(SCI_Year)%name, param_value, iy)
          run_tag(SCI_Year)%acquired = .true.
        end if

      else if (run_tag(SCI_EndDate)%in_tag) then
        if (run_tag(SCI_Day)%in_tag) then
          call read_param(run_tag(SCI_Day)%name, param_value, ld)
          run_tag(SCI_Day)%acquired = .true.

        else if (run_tag(SCI_Month)%in_tag) then
          call read_param(run_tag(SCI_Month)%name, param_value, lm)
          run_tag(SCI_Month)%acquired = .true.

        else if (run_tag(SCI_Year)%in_tag) then
          call read_param(run_tag(SCI_Year)%name, param_value, ly)
          run_tag(SCI_Year)%acquired = .true.
        end if

      else if (run_tag(SCI_TimeSteps)%in_tag) then
        call read_param(run_tag(SCI_TimeSteps)%name, param_value, ntstep)
        run_tag(SCI_TimeSteps)%acquired = .true.

      else if (run_tag(SCI_climateFile)%in_tag) then
        ! read CLIGEN file name
        clifil = trim(param_value)
        ! open CLIGEN run file
        call fopenk (luicli, trim(rootp) // clifil, 'old')
        read(luicli,fmt="(a)",iostat=read_stat) param_value
        if (read_stat .gt. 0) then
          write(*,*) 'Error in file ', clifil, ' reading: ', param_value
          call exit(1)
        end if
        if (report_info >=2) then
          write(6,*) '1st cligen output line is: ', trim(param_value)
        end if

        ! I think this is pretty messy.  It was working with the Lahey compiler
        ! with a "73x,f" format but the Sun F95 compiler didn't like that, so
        ! it was changed to "73x,f6.3".  I am now assuming that the "old versions"
        ! of cligen had the version number there.  Anyway, I had to change from
        ! "f" to "f6.3" for the Sun compiler on the second read of the line string.

        ! Probably not a very robust way to do this
        read(param_value,fmt="(73x,f6.3)",iostat=read_stat) cligen_version
        if (read_stat .gt. 0) then
          write(*,*) 'Error in file ', clifil, ' reading: ', param_value
          call exit(1)
        end if
        if (cligen_version <= 5.1) then   ! assume new version of cligen
          read(param_value,fmt="(f6.3)",iostat=read_stat) cligen_version
          if (read_stat .gt. 0) then
            write(*,*) 'Error in file ', clifil, ' reading: ', param_value
            call exit(1)
          end if
        end if

        if (report_info >=1) then
          write(6,"(a17,f8.5)") 'cligen version: ', cligen_version
        end if
        if (cligen_version >= 5.110) then
          cli_gen_fmt_flag = 3
          if (report_info >=2) then
            write(6,*) 'cligen_version >= 5.110 db format'
          end if
        else if (cligen_version >= 5.101) then
          cli_gen_fmt_flag = 2
          if (report_info >=2) then
            write(6,*) 'Forest Service cligen db format'
          end if
        else
          cli_gen_fmt_flag = 1
          if (report_info >=2) then
            write(6,*) '3.1 version cligen db format'
          end if
        endif
        rewind luicli
        run_tag(SCI_climateFile)%acquired = .true.

      else if (run_tag(SCI_windFile)%in_tag) then
        ! read WINDGEN file name
        winfil = trim(param_value)
        ! open WINDGEN file
        call fopenk (luiwin, trim(rootp) // winfil, 'old')
        ! We will now check the header to determine which wind_gen data file
        ! format we are reading, either the old one (daily max and min wind
        ! speed, etc.) or the new one (24 hourly values per day).
        ! We now have a global wind_gen format flag we will set once we know.
        read(luiwin,fmt="(a80)",iostat=read_stat) param_value
        if (read_stat .gt. 0) then
          write(*,*) 'Error in file ', winfil, ' reading: ', param_value
          call exit(1)
        end if
        if (    (index(param_value,'WIND_GEN5') > 0) &
           .or. (index(param_value,'WIND_GEN4') > 0) &
           .or. (index(param_value,'WIND_GEN3') > 0) &
           .or. (index(param_value,'WIND_GEN2') > 0) ) then
           wind_gen_fmt_flag = 2
        else
           wind_gen_fmt_flag = 1
        endif
        rewind luiwin
        run_tag(SCI_windFile)%acquired = .true.

      else if (run_tag(SCI_ErosionSubmodelOutput)%in_tag) then
        call read_param(run_tag(SCI_ErosionSubmodelOutput)%name, param_value, am0efl)
        run_tag(SCI_ErosionSubmodelOutput)%acquired = .true.

      else if (run_tag(SCI_RegionAngle)%in_tag) then
        call read_param(run_tag(SCI_RegionAngle)%name, param_value, amasim)
        run_tag(SCI_RegionAngle)%acquired = .true.

      else if (run_tag(SCI_XOrigin)%in_tag) then
        call read_param(run_tag(SCI_XOrigin)%name, param_value, amxsim(1)%x)
        run_tag(SCI_XOrigin)%acquired = .true.

      else if (run_tag(SCI_YOrigin)%in_tag) then
        call read_param(run_tag(SCI_YOrigin)%name, param_value, amxsim(1)%y)
        run_tag(SCI_YOrigin)%acquired = .true.

      else if (run_tag(SCI_XLength)%in_tag) then
        call read_param(run_tag(SCI_XLength)%name, param_value, amxsim(2)%x)
        run_tag(SCI_XLength)%acquired = .true.
        ! compute the simulation area
        if (run_tag(SCI_YLength)%acquired) then
          sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
          !write(6,*) "Simulation area (m^2)", sim_area
        end if

      else if (run_tag(SCI_YLength)%in_tag) then
        call read_param(run_tag(SCI_YLength)%name, param_value, amxsim(2)%y)
        run_tag(SCI_YLength)%acquired = .true.
        ! compute the simulation area
        if (run_tag(SCI_XLength)%acquired) then
          sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
          !write(6,*) "Simulation area (m^2)", sim_area
        end if

      else if (run_tag(SCI_Subregions)%in_tag) then
        if (run_tag(SCI_Subregion)%in_tag) then
          ! SCI_Subregion
          if (run_tag(SCI_SubmodelOutput)%in_tag) then
            ! SCI_SubmodelOutput
            if (run_tag(SCI_hydro)%in_tag) then
              call read_param(run_tag(SCI_hydro)%name, param_value, am0hfl(isr))
              run_tag(SCI_hydro)%acquired = .true.
            else if (run_tag(SCI_soil)%in_tag) then
              call read_param(run_tag(SCI_soil)%name, param_value, am0sfl(isr))
              run_tag(SCI_soil)%acquired = .true.
            else if (run_tag(SCI_man)%in_tag) then
              call read_param(run_tag(SCI_man)%name, param_value, manFile(isr)%am0tfl)
              run_tag(SCI_man)%acquired = .true.
            else if (run_tag(SCI_crop)%in_tag) then
              call read_param(run_tag(SCI_crop)%name, param_value, am0cfl(isr))
              run_tag(SCI_crop)%acquired = .true.
            else if (run_tag(SCI_decomp)%in_tag) then
              call read_param(run_tag(SCI_decomp)%name, param_value, am0dfl(isr))
              run_tag(SCI_decomp)%acquired = .true.
            end if
          else if (run_tag(SCI_DebugOutput)%in_tag) then
            ! SCI_DebugOutput
            if (run_tag(SCI_hydro)%in_tag) then
              call read_param(run_tag(SCI_hydro)%name, param_value, am0hdb(isr))
              run_tag(SCI_hydro)%acquired = .true.
            else if (run_tag(SCI_soil)%in_tag) then
              call read_param(run_tag(SCI_soil)%name, param_value, am0sdb(isr))
              run_tag(SCI_soil)%acquired = .true.
            else if (run_tag(SCI_man)%in_tag) then
              call read_param(run_tag(SCI_man)%name, param_value, manFile(isr)%am0tdb)
              run_tag(SCI_man)%acquired = .true.
            else if (run_tag(SCI_crop)%in_tag) then
              call read_param(run_tag(SCI_crop)%name, param_value, am0cdb(isr))
              run_tag(SCI_crop)%acquired = .true.
            else if (run_tag(SCI_decomp)%in_tag) then
              call read_param(run_tag(SCI_decomp)%name, param_value, am0ddb(isr))
              run_tag(SCI_decomp)%acquired = .true.
            end if
          else if (run_tag(SCI_AverageSlope)%in_tag) then
            !        The new "versioned" IFC files contain a slope value
            !        which will be used if this value is set negative, 
            !        ie. not entered. It is now the only way to set a 
            !        non default slope when using the older "non-versioned"
            !        IFC files.   
            call read_param(run_tag(SCI_AverageSlope)%name, param_value, soil_in(isr)%amrslp)
            run_tag(SCI_AverageSlope)%acquired = .true.

          else if (run_tag(SCI_SoilRockFragments)%in_tag) then
            ! If this value is set to -1, the value from the ifc is used
            ! If set here to value of 0 or greater, this value is assigned to all layers
            call read_param(run_tag(SCI_SoilRockFragments)%name, param_value, soil_in(isr)%SoilRockFragments)
            run_tag(SCI_SoilRockFragments)%acquired = .true.

          else if (run_tag(SCI_SoilFile)%in_tag) then
            ! read in initial field conditions file name
            soil_in(isr)%sinfil = trim(param_value)
            run_tag(SCI_SoilFile)%acquired = .true.

          else if (run_tag(SCI_ManageFile)%in_tag) then
            ! read in management file name
            manFile(isr)%tinfil = trim(param_value)
            run_tag(SCI_ManageFile)%acquired = .true.

          else if (run_tag(SCI_WaterErosionLoss)%in_tag) then
            call read_param(run_tag(SCI_WaterErosionLoss)%name, param_value, soil_in(isr)%WaterErosion)
            run_tag(SCI_WaterErosionLoss)%acquired = .true.

          end if
        end if

      else if (run_tag(SCI_Barriers)%in_tag) then
        if (run_tag(SCI_Barrier)%in_tag) then
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
          if (run_tag(SCI_Description)%in_tag) then
            barseas(ibr)%amzbt = trim(param_value)
            barrier(ibr)%amzbt = barseas(ibr)%amzbt
            run_tag(SCI_Description)%acquired = .true.
          else if (run_tag(SCI_PointBarClis)%in_tag) then
            if (run_tag(SCI_BarCli)%in_tag) then
              ! SCI_BarCli Climate transition parameters
              if (run_tag(SCI_TimeMark)%in_tag) then
                call read_param(run_tag(SCI_TimeMark)%name, param_value, barseas(ibr)%dst(iseas)%doy)
                run_tag(SCI_TimeMark)%acquired = .true.
              else if (run_tag(SCI_TimeDesc)%in_tag) then
                barseas(ibr)%dst(iseas)%st_desc = param_value(1:80)
                run_tag(SCI_TimeDesc)%acquired = .true.
              else if (run_tag(SCI_BegTranFlg)%in_tag) then
                call read_param(run_tag(SCI_BegTranFlg)%name, param_value, barseas(ibr)%clim(iseas)%beg_flg)
                run_tag(SCI_BegTranFlg)%acquired = .true.
              else if (run_tag(SCI_BegTranThresh)%in_tag) then
                call read_param(run_tag(SCI_BegTranThresh)%name, param_value, barseas(ibr)%clim(iseas)%beg_thresh)
                run_tag(SCI_BegTranThresh)%acquired = .true.
              else if (run_tag(SCI_BegTranBase)%in_tag) then
                call read_param(run_tag(SCI_BegTranBase)%name, param_value, barseas(ibr)%clim(iseas)%beg_base)
                run_tag(SCI_BegTranBase)%acquired = .true.
              else if (run_tag(SCI_EndTranFlg)%in_tag) then
                call read_param(run_tag(SCI_EndTranFlg)%name, param_value, barseas(ibr)%clim(iseas)%end_flg)
                run_tag(SCI_EndTranFlg)%acquired = .true.
              else if (run_tag(SCI_EndTranThresh)%in_tag) then
                call read_param(run_tag(SCI_EndTranThresh)%name, param_value, barseas(ibr)%clim(iseas)%end_thresh)
                run_tag(SCI_EndTranThresh)%acquired = .true.
              else if (run_tag(SCI_EndTranBase)%in_tag) then
                call read_param(run_tag(SCI_EndTranBase)%name, param_value, barseas(ibr)%clim(iseas)%end_base)
                run_tag(SCI_EndTranBase)%acquired = .true.
              end if
            else if (run_tag(SCI_coord)%in_tag) then
              !SCI_coord
              if (run_tag(SCI_index)%in_tag) then
                call read_param(run_tag(SCI_index)%name, param_value, ipol)
                ! adjust from base 0 to base 1 arrays
                ipol = ipol + 1
                run_tag(SCI_index)%acquired = .true.
              else if (run_tag(SCI_x)%in_tag) then
                call read_param(run_tag(SCI_x)%name, param_value, barseas(ibr)%points(ipol)%x)
                run_tag(SCI_x)%acquired = .true.
              else if (run_tag(SCI_y)%in_tag) then
                call read_param(run_tag(SCI_y)%name, param_value, barseas(ibr)%points(ipol)%y)
                run_tag(SCI_y)%acquired = .true.
              end if
            else if (run_tag(SCI_PointBarCli)%in_tag) then
              ! SCI_PointBarCli
               if (run_tag(SCI_height)%in_tag) then
                call read_param(run_tag(SCI_height)%name, param_value, barseas(ibr)%param(ipol,iseas)%amzbr)
                run_tag(SCI_height)%acquired = .true.
              else if (run_tag(SCI_width)%in_tag) then
                call read_param(run_tag(SCI_width)%name, param_value, barseas(ibr)%param(ipol,iseas)%amxbrw)
                run_tag(SCI_width)%acquired = .true.
              else if (run_tag(SCI_porosity)%in_tag) then
                call read_param(run_tag(SCI_porosity)%name, param_value, barseas(ibr)%param(ipol,iseas)%ampbr)
                run_tag(SCI_porosity)%acquired = .true.
              end if
            end if
          end if
        end if

      end if

    end if

  end subroutine pcdata_chunk_handler

  subroutine write_run_xml()
    use read_write_xml_mod, only: w_begin_tag, w_end_tag, w_whole_tag
    use grid_mod, only: amasim, amxsim

    character*512 :: runxmlfil  ! xml run file name
    logical :: fexist     ! flag used to indicate result of file existence
    integer :: luo_xml     ! output unit number
    integer :: dealloc_stat

    nsubr = size(soil_in)

    runxmlfil = trim(rootp) // 'weps.runx'

    call init_run_xml()

    inquire(file = trim(runxmlfil), exist = fexist)
    if (fexist) then
      ! do not overwrite existing file
      write(*,*) 'File exists, move or delete to create new: ', trim(runxmlfil)
      return
    end if

    call fopenk (luo_xml, trim(runxmlfil), 'unknown')

    ! write XML header
    write(luo_xml,"(a)") '<?xml version="1.0" encoding="ISO-8859-1"?>'
    write(luo_xml,"(a)") '<!DOCTYPE runFileData SYSTEM "weps.run.dtd">'

    call w_begin_tag( luo_xml, run_tag(runFileData)%name )

      ! GUI tags
      call w_whole_tag( luo_xml, run_tag(GUI_UserName)%name, trim(usrnam) )
      call w_whole_tag( luo_xml, run_tag(GUI_FarmId)%name, trim(farmid) )
      call w_whole_tag( luo_xml, run_tag(GUI_TractId)%name, trim(tractid) )
      call w_whole_tag( luo_xml, run_tag(GUI_FieldId)%name, trim(fieldid) )
      call w_whole_tag( luo_xml, run_tag(GUI_RunTypeDisp)%name, trim(runtype) )
      call w_whole_tag( luo_xml, run_tag(GUI_Site)%name, trim(siteid) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenFlag)%name, trim(cliflag) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenMethod)%name, trim(climethod) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenLatitude)%name, trim(clilatitude) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenLongitude)%name, trim(clilongitude) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenStateId)%name, trim(clistateid) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenStationNum)%name, trim(clistationnum) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenStationName)%name, trim(clistationname) )
      call w_whole_tag( luo_xml, run_tag(GUI_cligenElevation)%name, trim(clielevation) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenFlag)%name, trim(winflag) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenMethod)%name, trim(winmethod) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenLatitude)%name, trim(winlatitude) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenLongitude)%name, trim(winlongitude) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenStationNum)%name, trim(winstationnum) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenCountry)%name, trim(wincountry) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenState)%name, trim(winstate) )
      call w_whole_tag( luo_xml, run_tag(GUI_windgenStationName)%name, trim(winstationname) )
      ! not recorded in old weps.run files, so make blank entry
      call w_whole_tag( luo_xml, run_tag(GUI_lastrun)%name, '' )

      call w_whole_tag( luo_xml, run_tag(SCI_CycleCount)%name, run_rot_cycles )
      call w_whole_tag( luo_xml, run_tag(SCI_CentrLatitude)%name, amalat )
      call w_whole_tag( luo_xml, run_tag(SCI_CentrLongitude)%name, amalon )
      call w_whole_tag( luo_xml, run_tag(SCI_Elevation)%name, amzele )
      call w_begin_tag( luo_xml, run_tag(SCI_StartDate)%name )
        call w_whole_tag( luo_xml, run_tag(SCI_Day)%name, id )
        call w_whole_tag( luo_xml, run_tag(SCI_Month)%name, im )
        call w_whole_tag( luo_xml, run_tag(SCI_Year)%name, iy )
      call w_end_tag( luo_xml, run_tag(SCI_StartDate)%name )
      call w_begin_tag( luo_xml, run_tag(SCI_EndDate)%name )
        call w_whole_tag( luo_xml, run_tag(SCI_Day)%name, ld )
        call w_whole_tag( luo_xml, run_tag(SCI_Month)%name, lm )
        call w_whole_tag( luo_xml, run_tag(SCI_Year)%name, ly )
      call w_end_tag( luo_xml, run_tag(SCI_EndDate)%name )
      call w_whole_tag( luo_xml, run_tag(SCI_TimeSteps)%name, ntstep )
      call w_whole_tag( luo_xml, run_tag(SCI_climateFile)%name, trim(clifil) )
      call w_whole_tag( luo_xml, run_tag(SCI_windFile)%name, trim(winfil) )
      call w_whole_tag( luo_xml, run_tag(SCI_ErosionSubmodelOutput)%name, am0efl )
      call w_whole_tag( luo_xml, run_tag(SCI_RegionAngle)%name, amasim )
      call w_whole_tag( luo_xml, run_tag(SCI_XOrigin)%name, amxsim(1)%x )
      call w_whole_tag( luo_xml, run_tag(SCI_YOrigin)%name, amxsim(1)%y )
      call w_whole_tag( luo_xml, run_tag(SCI_XLength)%name, amxsim(2)%x )
      call w_whole_tag( luo_xml, run_tag(SCI_YLength)%name, amxsim(2)%y )

      call w_begin_tag( luo_xml, run_tag(SCI_Subregions)%name, &
                                 run_tag(SCI_number)%name, nsubr)
      do isr = 1, nsubr
        call w_begin_tag( luo_xml, run_tag(SCI_Subregion)%name, &
                                   run_tag(SCI_index)%name, isr-1)
          call w_begin_tag( luo_xml, run_tag(SCI_SubmodelOutput)%name )
            call w_whole_tag( luo_xml, run_tag(SCI_hydro)%name, am0hfl(isr) )
            call w_whole_tag( luo_xml, run_tag(SCI_soil)%name, am0sfl(isr) )
            call w_whole_tag( luo_xml, run_tag(SCI_man)%name, manFile(isr)%am0tfl )
            call w_whole_tag( luo_xml, run_tag(SCI_crop)%name, am0cfl(isr) )
            call w_whole_tag( luo_xml, run_tag(SCI_decomp)%name, am0dfl(isr) )
          call w_end_tag( luo_xml, run_tag(SCI_SubmodelOutput)%name )
          call w_begin_tag( luo_xml, run_tag(SCI_DebugOutput)%name )
            call w_whole_tag( luo_xml, run_tag(SCI_hydro)%name, am0hdb(isr) )
            call w_whole_tag( luo_xml, run_tag(SCI_soil)%name, am0sdb(isr) )
            call w_whole_tag( luo_xml, run_tag(SCI_man)%name, manFile(isr)%am0tdb )
            call w_whole_tag( luo_xml, run_tag(SCI_crop)%name, am0cdb(isr) )
            call w_whole_tag( luo_xml, run_tag(SCI_decomp)%name, am0ddb(isr) )
          call w_end_tag( luo_xml, run_tag(SCI_DebugOutput)%name )
          call w_whole_tag( luo_xml, run_tag(SCI_AverageSlope)%name , soil_in(isr)%amrslp )
          call w_whole_tag( luo_xml, run_tag(SCI_SoilRockFragments)%name , soil_in(isr)%SoilRockFragments )
          call w_whole_tag( luo_xml, run_tag(SCI_SoilFile)%name , soil_in(isr)%sinfil )
          call w_whole_tag( luo_xml, run_tag(SCI_ManageFile)%name , manFile(isr)%tinfil )
          call w_whole_tag( luo_xml, run_tag(SCI_WaterErosionLoss)%name , soil_in(isr)%WaterErosion )
        call w_end_tag( luo_xml, run_tag(SCI_Subregion)%name )
      end do
      call w_end_tag( luo_xml, run_tag(SCI_Subregions)%name )

      nbr = size(barseas)
      call w_begin_tag( luo_xml, run_tag(SCI_Barriers)%name, &
                                 run_tag(SCI_number)%name, nbr)
      do ibr = 1, nbr
        call w_begin_tag( luo_xml, run_tag(SCI_Barrier)%name, &
                                   run_tag(SCI_BarrierSeasonFlag)%name, barseas(ibr)%seas_flg, &
                                   run_tag(SCI_index)%name, ibr-1)
          call w_whole_tag( luo_xml, run_tag(SCI_Description)%name, trim(barseas(ibr)%amzbt) )
            call w_begin_tag( luo_xml, run_tag(SCI_PointBarClis)%name, &
                                       run_tag(SCI_BarCliNumber)%name, barseas(ibr)%ntm, &
                                       run_tag(SCI_CoordinateNumber)%name, barseas(ibr)%np )
            do iseas = 1, barseas(ibr)%ntm
              call w_begin_tag( luo_xml, run_tag(SCI_BarCli)%name, &
                                         run_tag(SCI_index)%name, iseas-1 )
                call w_whole_tag( luo_xml, run_tag(SCI_TimeDesc)%name, trim(barseas(ibr)%dst(iseas)%st_desc) )
                call w_whole_tag( luo_xml, run_tag(SCI_TimeMark)%name, barseas(ibr)%dst(iseas)%doy )

                if( barseas(ibr)%seas_flg .eq. 2 ) then
                  ! these are used for climate based seasonal transitions
                  call w_whole_tag( luo_xml, run_tag(SCI_BegTranFlg)%name, barseas(ibr)%clim(iseas)%beg_flg )
                  call w_whole_tag( luo_xml, run_tag(SCI_BegTranThresh)%name, barseas(ibr)%clim(iseas)%beg_thresh )
                  call w_whole_tag( luo_xml, run_tag(SCI_BegTranBase)%name, barseas(ibr)%clim(iseas)%beg_base )
                  call w_whole_tag( luo_xml, run_tag(SCI_EndTranFlg)%name, barseas(ibr)%clim(iseas)%end_flg )
                  call w_whole_tag( luo_xml, run_tag(SCI_EndTranThresh)%name, barseas(ibr)%clim(iseas)%end_thresh )
                  call w_whole_tag( luo_xml, run_tag(SCI_EndTranBase)%name, barseas(ibr)%clim(iseas)%end_base )
                end if
              call w_end_tag( luo_xml, run_tag(SCI_BarCli)%name )
            end do
            do ipol = 1, barseas(ibr)%np
              call w_begin_tag( luo_xml, run_tag(SCI_coord)%name, &
                                         run_tag(SCI_index)%name, ipol-1 )
                call w_whole_tag( luo_xml, run_tag(SCI_x)%name, barseas(ibr)%points(ipol)%x )
                call w_whole_tag( luo_xml, run_tag(SCI_y)%name, barseas(ibr)%points(ipol)%y )
                !GUI_lat
                !GUI_lon
              call w_end_tag( luo_xml, run_tag(SCI_coord)%name )
            end do
            do iseas = 1, barseas(ibr)%ntm
              do ipol = 1, barseas(ibr)%np
                call w_begin_tag( luo_xml, run_tag(SCI_PointBarCli)%name, &
                                         run_tag(SCI_BarCliIndex)%name, iseas-1, &
                                         run_tag(SCI_coordIndex)%name, ipol-1 )
                  call w_whole_tag( luo_xml, run_tag(SCI_height)%name, barseas(ibr)%param(ipol,iseas)%amzbr )
                  call w_whole_tag( luo_xml, run_tag(SCI_width)%name, barseas(ibr)%param(ipol,iseas)%amxbrw )
                  call w_whole_tag( luo_xml, run_tag(SCI_porosity)%name, barseas(ibr)%param(ipol,iseas)%ampbr )
                call w_end_tag( luo_xml, run_tag(SCI_PointBarCli)%name )
              end do
            end do
            call w_end_tag( luo_xml, run_tag(SCI_PointBarClis)%name )
          call w_end_tag( luo_xml, run_tag(SCI_Barrier)%name )
      end do
      call w_end_tag( luo_xml, run_tag(SCI_Barriers)%name )

    call w_end_tag( luo_xml, run_tag(runFileData)%name )

    close (luo_xml)

    ! deallocate tag array
    deallocate( run_tag, stat=dealloc_stat)
    if( dealloc_stat .gt. 0 ) then
      ! deallocation failed
      write(*,*) "ERROR: unable to deallocate memory for tag array"
    end if

  end subroutine write_run_xml
end module input_run_xml_mod
