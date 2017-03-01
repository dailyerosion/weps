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
  use subregions_mod, only: acct_poly, subr_poly
  use file_io_mod, only: fopenk, luicli, luiwin, luolog
  use climate_input_mod, only: cli_gen_fmt_flag, wind_gen_fmt_flag, cligen_sname
  use climate_input_mod, only: amalat, amalon, amzele
  use erosion_data_struct_defs, only: subday, ntstep, am0efl
  use grid_mod, only: amasim, amxsim, sim_area, xgdpt, ygdpt
  use hydro_data_struct_defs, only: am0hfl, am0hdb
  use soil_data_struct_defs, only: am0sfl, am0sdb

  use manage_data_struct_defs, only: am0tfl, am0tdb, tinfil
  use crop_data_struct_defs, only: am0cfl, am0cdb
  use decomp_data_struct_defs, only: am0dfl, am0ddb
  use input_soil_mod, only: soil_def, soil_in
  use Points_Mod, only: point
  use weps_main_mod
  use barriers_mod, only: create_barrier, barrier, barseas
  use barriers_mod, only: barrier_day_state, barrier_params, barrier_climate

  private

  interface read_param
    module procedure read_param_real_1
    module procedure read_param_real_2
    module procedure read_param_int_1
    module procedure read_param_int_2
    module procedure read_param_int_3
  end interface

  integer, parameter, public   :: MAX_NAME_LEN  = 40

  type :: tag_def
    character(len=MAX_NAME_LEN)  :: name   ! tag name
    logical :: required                    ! .true. if tag is required
    logical :: acquired                    ! .true. if tag has been read
    logical :: in_tag                      ! .true. if inside tag now
  end type tag_def
  
  type(tag_def), dimension(:), allocatable :: run_tag
  integer :: max_tags

  integer, parameter :: SCI_AccNo = 1
  integer, parameter :: SCI_Account = 2
  integer, parameter :: SCI_AverageSlope = 3
  integer, parameter :: SCI_BarCli = 4
  integer, parameter :: SCI_BarCliI = 5
  integer, parameter :: SCI_BarCliNo = 6
  integer, parameter :: SCI_Barrier = 7
  integer, parameter :: SCI_BarrierNo = 8
  integer, parameter :: SCI_BarrierSeasonFlag = 9
  integer, parameter :: SCI_BegTranBase = 10
  integer, parameter :: SCI_BegTranFlg = 11
  integer, parameter :: SCI_BegTranThresh = 12
  integer, parameter :: SCI_climateFile = 13
  integer, parameter :: SCI_coordinate = 14
  integer, parameter :: SCI_coordinates = 15
  integer, parameter :: SCI_coord = 16
  integer, parameter :: SCI_coordI = 17
  integer, parameter :: SCI_coordNo = 18
  integer, parameter :: SCI_crop = 19
  integer, parameter :: SCI_CycleCount = 20
  integer, parameter :: SCI_DebugOutput = 21
  integer, parameter :: SCI_decomp = 22
  integer, parameter :: SCI_Description = 23
  integer, parameter :: SCI_Elevation = 24
  integer, parameter :: SCI_EndDate = 25
  integer, parameter :: SCI_EndTranBase = 26
  integer, parameter :: SCI_EndTranFlg = 27
  integer, parameter :: SCI_EndTranThresh = 28
  integer, parameter :: SCI_ErosionSubmodelOutput = 29
  integer, parameter :: SCI_height = 30
  integer, parameter :: SCI_hydro = 31
  integer, parameter :: SCI_index = 32
  integer, parameter :: SCI_LatLong = 33
  integer, parameter :: SCI_ManageFile = 34
  integer, parameter :: SCI_man = 35
  integer, parameter :: SCI_Number = 36
  integer, parameter :: SCI_pointBarCli = 37
  integer, parameter :: SCI_porosity = 38
  integer, parameter :: SCI_RegionAngle = 39
  integer, parameter :: runFileData = 40
  integer, parameter :: SCI_SoilFile = 41
  integer, parameter :: SCI_soil = 42
  integer, parameter :: SCI_SoilRockFragments = 43
  integer, parameter :: SCI_StartDate = 44
  integer, parameter :: SCI_subDailyFile = 45
  integer, parameter :: SCI_SubmodelOutput = 46
  integer, parameter :: SCI_Subregion = 47
  integer, parameter :: SCI_SubregionNo = 48
  integer, parameter :: SCI_TimeDesc = 49
  integer, parameter :: SCI_TimeMark = 50
  integer, parameter :: SCI_TimeSteps = 51
  integer, parameter :: SCI_WaterErosionLoss = 52
  integer, parameter :: SCI_width = 53
  integer, parameter :: SCI_windFile = 54
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
  logical, dimension(:), allocatable :: season_complete
  logical, dimension(:), allocatable :: points_complete
  logical, dimension(:,:), allocatable :: clipar_complete
  integer :: count_complete
  ! temporary holder for array elements until index is read
  integer :: t_am0hfl
  integer :: t_am0sfl
  integer :: t_am0tfl
  integer :: t_am0cfl
  integer :: t_am0dfl
  integer :: t_am0hdb
  integer :: t_am0sdb
  integer :: t_am0tdb
  integer :: t_am0cdb
  integer :: t_am0ddb
  type(polygon) :: t_polygon
  type(soil_def) :: t_soil
  character(len=512) :: t_tinfil
  type(point) :: t_point
  type(barrier_day_state) :: t_day_state
  type(barrier_params) :: t_params
  type(barrier_climate) :: t_climate
  logical, dimension(2) :: runfile_complete

  public :: begin_element_handler, end_element_handler, init_run_xml, pcdata_chunk_handler
  public :: runFileData, run_tag

contains

  subroutine begin_element_handler(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx

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

  end subroutine begin_element_handler

  subroutine end_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: alloc_stat

    do idx = 1, size(run_tag)
      if( run_tag(idx)%name .eq. name ) then
        run_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. runFileData) then
            !write(*,*) 'Tags', run_tag(SCI_CycleCount)%acquired &
            !           , run_tag(SCI_LatLong)%acquired &
            !           , run_tag(SCI_Elevation)%acquired &
            !           , run_tag(SCI_StartDate)%acquired &
            !           , run_tag(SCI_EndDate)%acquired &
            !           , run_tag(SCI_TimeSteps)%acquired &
            !           , run_tag(SCI_climateFile)%acquired &
            !           , run_tag(SCI_windFile)%acquired &
            !           , run_tag(SCI_ErosionSubmodelOutput)%acquired &
            !           , run_tag(SCI_RegionAngle)%acquired &
            !           , run_tag(SCI_XOrigin)%acquired &
            !           , run_tag(SCI_YOrigin)%acquired &
            !           , run_tag(SCI_XLength)%acquired &
            !           , run_tag(SCI_YLength)%acquired &
            !           , run_tag(SCI_XGrid)%acquired &
            !           , run_tag(SCI_YGrid)%acquired &
            !           , run_tag(SCI_AccNo)%acquired &
            !           , run_tag(SCI_Account)%acquired &
            !           , run_tag(SCI_SubregionNo)%acquired &
            !           , run_tag(SCI_Subregion)%acquired &
            !           , run_tag(SCI_BarrierNo)%acquired &
            !           , run_tag(SCI_Barrier)%acquired

          if (run_tag(SCI_AccNo)%acquired) then
            if (nacctr .le. 0) then
              runfile_complete(1) = .true.
            else
              if (run_tag(SCI_Account)%acquired) then
                runfile_complete(1) = .true.
              end if
            end if
          else
            runfile_complete(1) = .true.
            ! create array of accounting region polygons (zero size array allowed)
            allocate(acct_poly(0), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory alloc., accounting region polygons'
            end if
          end if

          if (run_tag(SCI_BarrierNo)%acquired) then
            if (nbr .le. 0) then
              runfile_complete(2) = .true.
            else
              if (run_tag(SCI_Barrier)%acquired) then
                runfile_complete(2) = .true.
              end if
            end if
          else
            runfile_complete(2) = .true.
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
          if (    run_tag(SCI_CycleCount)%acquired &
            .and. run_tag(SCI_LatLong)%acquired &
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
            .and. run_tag(SCI_XGrid)%acquired &
            .and. run_tag(SCI_YGrid)%acquired &
            .and. run_tag(SCI_SubregionNo)%acquired &
            .and. run_tag(SCI_Subregion)%acquired &
            .and. runfile_complete(1) &
            .and. runfile_complete(2) &
            ) then
            run_tag(runFileData)%acquired = .true.
          end if

        end if

        exit  ! found tag, no need to look further

      end if
    end do

  end subroutine end_element_handler

  subroutine init_run_xml()

    integer :: idx
    integer :: alloc_stat

    max_tags = 62   ! count of unique tags needed from all dtd files
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

    ! set optional items
    run_tag(SCI_AccNo)%required = .false.
    run_tag(SCI_Account)%required = .false.
    run_tag(SCI_BarrierNo)%required = .false.
    run_tag(SCI_Barrier)%required = .false.

    ! assign tag names
    run_tag(1)%name = "SCI_AccNo"
    run_tag(2)%name = "SCI_Account"
    run_tag(3)%name = "SCI_AverageSlope"
    run_tag(4)%name = "SCI_BarCli"
    run_tag(5)%name = "SCI_BarCliI"
    run_tag(6)%name = "SCI_BarCliNo"
    run_tag(7)%name = "SCI_Barrier"
    run_tag(8)%name = "SCI_BarrierNo"
    run_tag(9)%name = "SCI_BarrierSeasonFlag"
    run_tag(10)%name = "SCI_BegTranBase"
    run_tag(11)%name = "SCI_BegTranFlg"
    run_tag(12)%name = "SCI_BegTranThresh"
    run_tag(13)%name = "SCI_climateFile"
    run_tag(14)%name = "SCI_coordinate"
    run_tag(15)%name = "SCI_coordinates"
    run_tag(16)%name = "SCI_coord"
    run_tag(17)%name = "SCI_coordI"
    run_tag(18)%name = "SCI_coordNo"
    run_tag(19)%name = "SCI_crop"
    run_tag(20)%name = "SCI_CycleCount"
    run_tag(21)%name = "SCI_DebugOutput"
    run_tag(22)%name = "SCI_decomp"
    run_tag(23)%name = "SCI_Description"
    run_tag(24)%name = "SCI_Elevation"
    run_tag(25)%name = "SCI_EndDate"
    run_tag(26)%name = "SCI_EndTranBase"
    run_tag(27)%name = "SCI_EndTranFlg"
    run_tag(28)%name = "SCI_EndTranThresh"
    run_tag(29)%name = "SCI_ErosionSubmodelOutput"
    run_tag(30)%name = "SCI_height"
    run_tag(31)%name = "SCI_hydro"
    run_tag(32)%name = "index"
    run_tag(33)%name = "SCI_LatLong"
    run_tag(34)%name = "SCI_ManageFile"
    run_tag(35)%name = "SCI_man"
    run_tag(36)%name = "SCI_Number"
    run_tag(37)%name = "SCI_pointBarCli"
    run_tag(38)%name = "SCI_porosity"
    run_tag(39)%name = "SCI_RegionAngle"
    run_tag(40)%name = "runFileData"
    run_tag(41)%name = "SCI_SoilFile"
    run_tag(42)%name = "SCI_soil"
    run_tag(43)%name = "SCI_SoilRockFragments"
    run_tag(44)%name = "SCI_StartDate"
    run_tag(45)%name = "SCI_subDailyFile"
    run_tag(46)%name = "SCI_SubmodelOutput"
    run_tag(47)%name = "SCI_Subregion"
    run_tag(48)%name = "SCI_SubregionNo"
    run_tag(49)%name = "SCI_TimeDesc"
    run_tag(50)%name = "SCI_TimeMark"
    run_tag(51)%name = "SCI_TimeSteps"
    run_tag(52)%name = "SCI_WaterErosionLoss"
    run_tag(53)%name = "SCI_width"
    run_tag(54)%name = "SCI_windFile"
    run_tag(55)%name = "SCI_XGrid"
    run_tag(56)%name = "SCI_XLength"
    run_tag(57)%name = "SCI_XOrigin"
    run_tag(58)%name = "SCI_x"
    run_tag(59)%name = "SCI_YGrid"
    run_tag(60)%name = "SCI_YLength"
    run_tag(61)%name = "SCI_YOrigin"
    run_tag(62)%name = "SCI_y"

    ! create integer variable names for tags and assign index number.
    ! makes chunk code more understandable.

  end subroutine init_run_xml

  subroutine pcdata_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value
    integer :: sum_stat, alloc_stat, dealloc_stat
    integer :: read_stat
    real :: cligen_version

    param_value = trim(chunk)

    if (run_tag(runFileData)%in_tag) then
      if (run_tag(SCI_CycleCount)%in_tag) then
        call read_param(SCI_CycleCount, param_value, run_rot_cycles)
        run_tag(SCI_CycleCount)%acquired = .true.

      else if (run_tag(SCI_LatLong)%in_tag) then
        call read_param(SCI_LatLong, param_value, amalat, amalon)
        if (read_stat .gt. 0) then
          write(*,*) 'Error reading ', run_tag(SCI_LatLong)%name, ' Value: ', param_value
          call exit(1)
        end if
        if ((amalat .lt. -90.) .or. (amalat .gt. 90.)) then
           write (*,*)'ERROR: latitude is not between -90. and 90. degrees. Please check run file'
           call exit(1)
        end if
        if ((amalon .lt. -180.) .or. (amalon .gt. 180.)) then
           write (*,*) 'ERROR: longitude is not between -180. and 180. degrees. Please check run file'
           call exit(1)
        end if
        run_tag(SCI_LatLong)%acquired = .true.

      else if (run_tag(SCI_Elevation)%in_tag) then
        call read_param(SCI_Elevation, param_value, amzele)
        run_tag(SCI_Elevation)%acquired = .true.
      else if (run_tag(SCI_StartDate)%in_tag) then
        call read_param(SCI_StartDate, param_value, id, im, iy)
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
        run_tag(SCI_StartDate)%acquired = .true.

      else if (run_tag(SCI_EndDate)%in_tag) then
        call read_param(SCI_EndDate, param_value, ld, lm, ly)
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
        run_tag(SCI_EndDate)%acquired = .true.

      else if (run_tag(SCI_TimeSteps)%in_tag) then
        call read_param(SCI_TimeSteps, param_value, ntstep)
        run_tag(SCI_TimeSteps)%acquired = .true.

        ! allocate wind direction and speed array
        allocate(subday(ntstep), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., wind direction and speed'
        end if

      else if (run_tag(SCI_climateFile)%in_tag) then
        ! read CLIGEN file name
        clifil = rootp(1:len_trim(rootp)) // param_value(1:len_trim(param_value))
        write(luolog, *) 'clifil: ', clifil(1:len_trim(clifil))
        ! open CLIGEN run file
        call fopenk (luicli, clifil, 'old')
        write(luolog,*) 'opened cligen file to determine db format...'
        ! read 1st line of CLIGEN file

        read(luicli,fmt="(a)",iostat=read_stat) param_value
        if (read_stat .gt. 0) then
          write(*,*) 'Error in file ', clifil, ' reading: ', param_value
          call exit(1)
        end if
        write(6,*) '1st cligen output line is: ', param_value

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

        write(luolog,*) 'cligen version: ', cligen_version
        write(6,*) 'cligen version: ', cligen_version

        if (cligen_version >= 5.110) then
          cli_gen_fmt_flag = 3
        else if (cligen_version >= 5.101) then
          cli_gen_fmt_flag = 2
          write(luolog,*) 'Forest Service cligen db format'
        else
          cli_gen_fmt_flag = 1
          write(luolog,*) '3.1 version cligen db format'
        endif
        rewind luicli
        run_tag(SCI_climateFile)%acquired = .true.

      else if (run_tag(SCI_windFile)%in_tag) then
        ! read WINDGEN file name
        winfil = rootp(1:len_trim(rootp)) // param_value(1:len_trim(param_value))
        ! open WINDGEN file
        call fopenk (luiwin, winfil, 'old')
        ! We will now check the header to determine which wind_gen data file
        ! format we are reading, either the old one (daily max and min wind
        ! speed, etc.) or the new one (24 hourly values per day).
        ! We now have a global wind_gen format flag we will set once we know.
        read(luiwin,fmt="(a80)",iostat=read_stat) param_value
        if (read_stat .gt. 0) then
          write(*,*) 'Error in file ', winfil, ' reading: ', param_value
          call exit(1)
        end if
        if (index(param_value,'WIND_GEN4') > 0 ) then
           wind_gen_fmt_flag = 2
        else if (index(param_value,'WIND_GEN3') > 0 ) then
           wind_gen_fmt_flag = 2
        else if (index(param_value,'WIND_GEN2') > 0 ) then
           wind_gen_fmt_flag = 2
        else
           wind_gen_fmt_flag = 1
        endif
        rewind luiwin
        run_tag(SCI_windFile)%acquired = .true.

      ! else if (run_tag(SCI_subDailyFile)%in_tag) then

      else if (run_tag(SCI_ErosionSubmodelOutput)%in_tag) then
        call read_param(SCI_ErosionSubmodelOutput, param_value, am0efl)
        run_tag(SCI_ErosionSubmodelOutput)%acquired = .true.

      else if (run_tag(SCI_RegionAngle)%in_tag) then
        call read_param(SCI_RegionAngle, param_value, amasim)
        run_tag(SCI_RegionAngle)%acquired = .true.

      else if (run_tag(SCI_XOrigin)%in_tag) then
        call read_param(SCI_XOrigin, param_value, amxsim(1)%x)
        run_tag(SCI_XOrigin)%acquired = .true.

      else if (run_tag(SCI_YOrigin)%in_tag) then
        call read_param(SCI_YOrigin, param_value, amxsim(1)%y)
        run_tag(SCI_YOrigin)%acquired = .true.

      else if (run_tag(SCI_XLength)%in_tag) then
        call read_param(SCI_XLength, param_value, amxsim(2)%x)
        run_tag(SCI_XLength)%acquired = .true.
        ! compute the simulation area
        if (run_tag(SCI_YLength)%acquired) then
          sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
          write(6,*) "Simulation area (m^2)", sim_area
        end if

      else if (run_tag(SCI_YLength)%in_tag) then
        call read_param(SCI_YLength, param_value, amxsim(2)%y)
        run_tag(SCI_YLength)%acquired = .true.
        ! compute the simulation area
        if (run_tag(SCI_XLength)%acquired) then
          sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
          write(6,*) "Simulation area (m^2)", sim_area
        end if

      else if (run_tag(SCI_XGrid)%in_tag) then
        call read_param(SCI_XGrid, param_value, xgdpt, ygdpt)
        run_tag(SCI_XGrid)%acquired = .true.

      else if (run_tag(SCI_YGrid)%in_tag) then
        call read_param(SCI_YGrid, param_value, xgdpt, ygdpt)
        run_tag(SCI_YGrid)%acquired = .true.

      else if (run_tag(SCI_AccNo)%in_tag) then
        call read_param(SCI_AccNo, param_value, nacctr)
        run_tag(SCI_AccNo)%acquired = .true.
        if (nacctr .gt. 0) then
          ! set counter iar for reading in Accounting Regions
          iar = 1
        end if
        ! create array of accounting region polygons (zero size array allowed)
        allocate(acct_poly(nacctr), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory alloc., accounting region polygons'
        end if

      else if (run_tag(SCI_Account)%in_tag) then
        ! Accounting region SCI_coordinates
        if (run_tag(SCI_AccNo)%required) then
          if (run_tag(SCI_coordinates)%in_tag) then
            !SCI_coordinates
            if (run_tag(SCI_Number)%in_tag) then
              call read_param(SCI_Number, param_value, poly_np)
              if (poly_np .gt. 0) then
                ! create polygon point storage
                acct_poly(iar) = create_polygon(poly_np)
                ! initialize polygon point counter
                ipol = 1
              end if
              run_tag(SCI_Number)%acquired = .true.
            else if (run_tag(SCI_coordinate)%in_tag) then
              if (run_tag(SCI_Number)%acquired) then
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
            run_tag(SCI_AccNo)%acquired = .true.
            run_tag(SCI_Account)%acquired = .true.
            run_tag(SCI_Number)%acquired = .false.
          end if
        else
          write(*,*) 'Error: Number of accounting regions must be specified before reading in accounting region data'
        end if

      else if (run_tag(SCI_SubregionNo)%in_tag) then
        call read_param(SCI_SubregionNo, param_value, nsubr)
        if (nsubr .lt. 1) then
          write(*,*) 'Error, subregion count must be 1 or greater. Value: ', nsubr
          call exit(1)
        end if
        run_tag(SCI_SubregionNo)%acquired = .true.

        sum_stat = 0
        ! create array of subregion polygons
        allocate(subr_poly(nsubr), stat = alloc_stat)
        sum_stat = sum_stat + alloc_stat
        ! create arrays for submodel output flags
        allocate(am0hfl(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0sfl(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0tfl(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0cfl(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0dfl(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        ! create arrays for submodel debug flags
        sum_stat = 0
        allocate(am0hdb(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0sdb(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0tdb(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0cdb(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(am0ddb(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(soil_in(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(tinfil(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        allocate(subregion_complete(nsubr), stat=alloc_stat)
        sum_stat = sum_stat + alloc_stat
        if( sum_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., subregion arrays'
        end if

      else if (run_tag(SCI_Subregion)%in_tag) then
        ! SCI_Subregion
        if (run_tag(SCI_SubregionNo)%acquired) then
          if (run_tag(SCI_index)%in_tag) then
            call read_param(SCI_index, param_value, isr)
            ! adjust from base 0 to base 1 arrays
            isr = isr + 1
            run_tag(SCI_index)%acquired = .true.
          else if (run_tag(SCI_SubmodelOutput)%in_tag) then
            ! SCI_SubmodelOutput
            if (run_tag(SCI_hydro)%in_tag) then
              call read_param(SCI_hydro, param_value, t_am0hfl)
              run_tag(SCI_hydro)%acquired = .true.
            else if (run_tag(SCI_soil)%in_tag) then
              call read_param(SCI_soil, param_value, t_am0sfl)
              run_tag(SCI_soil)%acquired = .true.
            else if (run_tag(SCI_man)%in_tag) then
              call read_param(SCI_man, param_value, t_am0tfl)
              run_tag(SCI_man)%acquired = .true.
            else if (run_tag(SCI_crop)%in_tag) then
              call read_param(SCI_crop, param_value, t_am0cfl)
              run_tag(SCI_crop)%acquired = .true.
            else if (run_tag(SCI_decomp)%in_tag) then
              call read_param(SCI_decomp, param_value, t_am0dfl)
              run_tag(SCI_decomp)%acquired = .true.
            end if
            if (    run_tag(SCI_hydro)%acquired &
              .and. run_tag(SCI_soil)%acquired &
              .and. run_tag(SCI_man)%acquired &
              .and. run_tag(SCI_crop)%acquired &
              .and. run_tag(SCI_decomp)%acquired ) then
              run_tag(SCI_hydro)%acquired = .false.
              run_tag(SCI_soil)%acquired = .false.
              run_tag(SCI_man)%acquired = .false.
              run_tag(SCI_crop)%acquired = .false.
              run_tag(SCI_decomp)%acquired = .false.
              run_tag(SCI_SubmodelOutput)%acquired = .true.
            end if
          else if (run_tag(SCI_DebugOutput)%in_tag) then
            ! SCI_DebugOutput
            if (run_tag(SCI_hydro)%in_tag) then
              call read_param(SCI_hydro, param_value, t_am0hdb)
              run_tag(SCI_hydro)%acquired = .true.
            else if (run_tag(SCI_soil)%in_tag) then
              call read_param(SCI_soil, param_value, t_am0sdb)
              run_tag(SCI_soil)%acquired = .true.
            else if (run_tag(SCI_man)%in_tag) then
              call read_param(SCI_man, param_value, t_am0tdb)
              run_tag(SCI_man)%acquired = .true.
            else if (run_tag(SCI_crop)%in_tag) then
              call read_param(SCI_crop, param_value, t_am0cdb)
              run_tag(SCI_crop)%acquired = .true.
            else if (run_tag(SCI_decomp)%in_tag) then
              call read_param(SCI_decomp, param_value, t_am0ddb)
              run_tag(SCI_decomp)%acquired = .true.
            end if
            if (    run_tag(SCI_hydro)%acquired &
              .and. run_tag(SCI_soil)%acquired &
              .and. run_tag(SCI_man)%acquired &
              .and. run_tag(SCI_crop)%acquired &
              .and. run_tag(SCI_decomp)%acquired ) then
              run_tag(SCI_hydro)%acquired = .false.
              run_tag(SCI_soil)%acquired = .false.
              run_tag(SCI_man)%acquired = .false.
              run_tag(SCI_crop)%acquired = .false.
              run_tag(SCI_decomp)%acquired = .false.
              run_tag(SCI_DebugOutput)%acquired = .true.
            end if
          else if (run_tag(SCI_coordinates)%in_tag) then
            !SCI_coordinates
            if (run_tag(SCI_Number)%in_tag) then
              call read_param(SCI_Number, param_value, poly_np)
              if (poly_np .gt. 0) then
                ! create polygon point storage
                t_polygon = create_polygon(poly_np)
                ! initialize polygon point counter
                ipol = 1
              end if
              run_tag(SCI_Number)%acquired = .true.
            else if (run_tag(SCI_coordinate)%in_tag) then
              if (run_tag(SCI_Number)%acquired) then
                call read_param(SCI_coordinate, param_value, t_polygon%points(ipol)%x, t_polygon%points(ipol)%y)
                ipol = ipol + 1
                if (ipol .gt. poly_np) then
                  run_tag(SCI_coordinates)%acquired = .true.
                  run_tag(SCI_Number)%acquired = .false.
                end if
              else
                write(*,*) 'Error: Number of coordinates must be specified before reading in SCI_coordinates'
              end if
            end if
          else if (run_tag(SCI_AverageSlope)%in_tag) then
            !        The new "versioned" IFC files contain a slope value
            !        which will be used if this value is set negative, 
            !        ie. not entered. It is now the only way to set a 
            !        non default slope when using the older "non-versioned"
            !        IFC files.   
            call read_param(SCI_AverageSlope, param_value, t_soil%amrslp)
            run_tag(SCI_AverageSlope)%acquired = .true.

          else if (run_tag(SCI_SoilRockFragments)%in_tag) then
            call read_param(SCI_SoilRockFragments, param_value, t_soil%SoilRockFragments)
            run_tag(SCI_SoilRockFragments)%acquired = .true.

          else if (run_tag(SCI_SoilFile)%in_tag) then
            ! read in initial field conditions file name
            t_soil%sinfil = rootp(1:len_trim(rootp)) // param_value(1:len_trim(param_value))
            run_tag(SCI_SoilFile)%acquired = .true.

            write(*,*) 'SOILFILE: ', param_value(1:len_trim(param_value))

          else if (run_tag(SCI_ManageFile)%in_tag) then
            ! read in management file name
            t_tinfil = rootp(1:len_trim(rootp)) // param_value(1:len_trim(param_value))
            run_tag(SCI_ManageFile)%acquired = .true.

          else if (run_tag(SCI_WaterErosionLoss)%in_tag) then
            call read_param(SCI_WaterErosionLoss, param_value, t_soil%WaterErosion)
            run_tag(SCI_WaterErosionLoss)%acquired = .true.

          end if
          if (    run_tag(SCI_index)%acquired &
            .and. run_tag(SCI_SubmodelOutput)%acquired &
            .and. run_tag(SCI_DebugOutput)%acquired &
            .and. run_tag(SCI_coordinates)%acquired &
            .and. run_tag(SCI_AverageSlope)%acquired &
            .and. run_tag(SCI_SoilRockFragments)%acquired &
            .and. run_tag(SCI_SoilFile)%acquired &
            .and. run_tag(SCI_ManageFile)%acquired &
            .and. run_tag(SCI_WaterErosionLoss)%acquired) then
            run_tag(SCI_index)%acquired = .false.
            run_tag(SCI_SubmodelOutput)%acquired = .false.
            run_tag(SCI_DebugOutput)%acquired = .false.
            run_tag(SCI_coordinates)%acquired = .false.
            run_tag(SCI_AverageSlope)%acquired = .false.
            run_tag(SCI_SoilRockFragments)%acquired = .false.
            run_tag(SCI_SoilFile)%acquired = .false.
            run_tag(SCI_ManageFile)%acquired = .false.
            run_tag(SCI_WaterErosionLoss)%acquired = .false.
            am0hfl(isr) = t_am0hfl
            am0sfl(isr) = t_am0sfl
            am0tfl(isr) = t_am0tfl
            am0cfl(isr) = t_am0cfl
            am0dfl(isr) = t_am0dfl
            am0hdb(isr) = t_am0hdb
            am0sdb(isr) = t_am0sdb
            am0tdb(isr) = t_am0tdb
            am0cdb(isr) = t_am0cdb
            am0ddb(isr) = t_am0ddb
            subr_poly(isr) = t_polygon
            ! polygon complete
            call set_area_polygon(subr_poly(isr))
            call destroy_polygon(t_polygon)
            soil_in(isr) = t_soil
            tinfil(isr) = t_tinfil
            subregion_complete(isr) = .true.
            count_complete = 0
            do isr = 1, nsubr
              if (subregion_complete(isr)) then
                count_complete = count_complete + 1
              end if
            end do
            if (count_complete .ge. nsubr) then
              run_tag(SCI_Subregion)%acquired = .true.
            end if
          end if
        else
          write(*,*) 'Error: Number of subregions must be specified before reading in subregion data'
        end if
      else if (run_tag(SCI_BarrierNo)%in_tag) then
        call read_param(SCI_BarrierNo, param_value, nbr)
        run_tag(SCI_BarrierNo)%acquired = .true.
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

      else if (run_tag(SCI_Barrier)%in_tag) then
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
        if (run_tag(SCI_BarrierNo)%acquired) then
          if (run_tag(SCI_Description)%in_tag) then
              barseas(ibr)%amzbt = trim(param_value)
              barrier(ibr)%amzbt = barseas(ibr)%amzbt
            run_tag(SCI_Description)%acquired = .true.
          else if (run_tag(SCI_BarrierSeasonFlag)%in_tag) then
            call read_param(SCI_BarrierSeasonFlag, param_value, seas_flg)
            run_tag(SCI_BarrierSeasonFlag)%acquired = .true.
          else if (run_tag(SCI_BarCliNo)%in_tag) then
            call read_param(SCI_BarCliNo, param_value, ntm_seas)
            run_tag(SCI_BarCliNo)%acquired = .true.

          else if (run_tag(SCI_coordNo)%in_tag) then
            call read_param(SCI_coordNo, param_value, poly_np)
            if (run_tag(SCI_BarrierSeasonFlag)%acquired &
              .and. run_tag(SCI_BarCliNo)%acquired &
              ) then
              if (    (seas_flg .ne. 0) &
                .and. (seas_flg .ne. 1) &
                .and. (seas_flg .ne. 2) &
                ) then
                write(*,*) 'ERROR: Barrier season flag value must be 0, 1 or 2'
                write(*,FMT='(i0)') 'Input value was: ', seas_flg
                call exit(35)
              end if
            else
              write(*,*) 'Tags for SCI_BarrierSeasonFlag and SCI_BarCliNo are required before reading SCI_coordNo tag.'
              call exit(1)
            end if
            run_tag(SCI_coordNo)%acquired = .true.
            ! create storage for point and barrier data
            ! this also sets values for barr%np and barr%ntm
            call create_barrier(barrier(ibr), poly_np)
            call create_barrier(barseas(ibr), poly_np,ntm_seas,seas_flg)
            ! create storage for season, points and climate parameter index tracking
            sum_stat = 0
            allocate(season_complete(ntm_seas), stat = alloc_stat)
            sum_stat = sum_stat + alloc_stat
            allocate(points_complete(poly_np), stat = alloc_stat)
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
              points_complete(ipol) = .false.
            end do
            do iseas = 1, ntm_seas
              do ipol = 1, poly_np
                clipar_complete(ipol, iseas) = .false.
              end do
            end do

          else if (run_tag(SCI_BarCli)%in_tag) then
            ! SCI_BarCli Climate transition parameters
            if (run_tag(SCI_index)%in_tag) then
              call read_param(SCI_index, param_value, iseas)
              ! adjust from base 0 to base 1 arrays
              iseas = iseas + 1
              run_tag(SCI_index)%acquired = .true.
            else if (run_tag(SCI_TimeMark)%in_tag) then
              call read_param(SCI_TimeMark, param_value, t_day_state%doy)
              run_tag(SCI_TimeMark)%acquired = .true.
            else if (run_tag(SCI_TimeDesc)%in_tag) then
              t_day_state%st_desc = param_value(1:80)
              run_tag(SCI_TimeDesc)%acquired = .true.
            else if (run_tag(SCI_BegTranFlg)%in_tag) then
              call read_param(SCI_BegTranFlg, param_value, t_climate%beg_flg)
              run_tag(SCI_BegTranFlg)%acquired = .true.
            else if (run_tag(SCI_BegTranThresh)%in_tag) then
              call read_param(SCI_BegTranThresh, param_value, t_climate%beg_thresh)
              run_tag(SCI_BegTranThresh)%acquired = .true.
            else if (run_tag(SCI_BegTranBase)%in_tag) then
              call read_param(SCI_BegTranBase, param_value, t_climate%beg_base)
              run_tag(SCI_BegTranBase)%acquired = .true.
            else if (run_tag(SCI_EndTranFlg)%in_tag) then
              call read_param(SCI_EndTranFlg, param_value, t_climate%end_flg)
              run_tag(SCI_EndTranFlg)%acquired = .true.
            else if (run_tag(SCI_EndTranThresh)%in_tag) then
              call read_param(SCI_EndTranThresh, param_value, t_climate%end_thresh)
              run_tag(SCI_EndTranThresh)%acquired = .true.
            else if (run_tag(SCI_EndTranBase)%in_tag) then
              call read_param(SCI_EndTranBase, param_value, t_climate%end_base)
              run_tag(SCI_EndTranBase)%acquired = .true.
            end if
            if( seas_flg .eq. 2 ) then
              if (    run_tag(SCI_index)%acquired &
                .and. run_tag(SCI_TimeMark)%acquired & 
                .and. run_tag(SCI_TimeDesc)%acquired &
                .and. run_tag(SCI_BegTranFlg)%acquired &
                .and. run_tag(SCI_BegTranThresh)%acquired &
                .and. run_tag(SCI_BegTranBase)%acquired &
                .and. run_tag(SCI_EndTranFlg)%acquired &
                .and. run_tag(SCI_EndTranThresh)%acquired &
                .and. run_tag(SCI_EndTranBase)%acquired &
                ) then
                run_tag(SCI_index)%acquired = .false.
                run_tag(SCI_TimeMark)%acquired = .false.
                run_tag(SCI_TimeDesc)%acquired = .false.
                run_tag(SCI_BegTranFlg)%acquired = .false.
                run_tag(SCI_BegTranThresh)%acquired = .false.
                run_tag(SCI_BegTranBase)%acquired = .false.
                run_tag(SCI_EndTranFlg)%acquired = .false.
                run_tag(SCI_EndTranThresh)%acquired = .false.
                run_tag(SCI_EndTranBase)%acquired = .false.
                barseas(ibr)%dst(iseas) = t_day_state
                barseas(ibr)%clim(iseas) = t_climate
                season_complete(iseas) = .true.
                count_complete = 0
                do iseas = 1, ntm_seas
                  if (season_complete(iseas)) then
                    count_complete = count_complete + 1
                  end if
                end do
                if (count_complete .ge. ntm_seas) then
                  run_tag(SCI_BarCli)%acquired = .true.
                end if
              end if  
            else 
              if (    run_tag(SCI_index)%acquired &
                .and. run_tag(SCI_TimeMark)%acquired & 
                .and. run_tag(SCI_TimeDesc)%acquired &
                ) then
                run_tag(SCI_index)%acquired = .false.
                run_tag(SCI_TimeMark)%acquired = .false.
                run_tag(SCI_TimeDesc)%acquired = .false.
                season_complete(iseas) = .true.
                count_complete = 0
                do iseas = 1, ntm_seas
                  if (season_complete(iseas)) then
                    count_complete = count_complete + 1
                  end if
                end do
                if (count_complete .ge. ntm_seas) then
                  run_tag(SCI_BarCli)%acquired = .true.
                end if
              end if
            end if
          else if (run_tag(SCI_coord)%in_tag) then
            !SCI_coord
            if (run_tag(SCI_index)%in_tag) then
              call read_param(SCI_index, param_value, ipol)
              ! adjust from base 0 to base 1 arrays
              ipol = ipol + 1
              run_tag(SCI_index)%acquired = .true.
            else if (run_tag(SCI_x)%in_tag) then
              call read_param(SCI_EndTranBase, param_value, t_point%x)
              run_tag(SCI_x)%acquired = .true.
            else if (run_tag(SCI_y)%in_tag) then
              call read_param(SCI_EndTranBase, param_value, t_point%y)
              run_tag(SCI_y)%acquired = .true.
            end if
            if(     run_tag(SCI_index)%acquired &
              .and. run_tag(SCI_x)%acquired &
              .and. run_tag(SCI_y)%acquired &
              ) then
              run_tag(SCI_index)%acquired = .false.
              run_tag(SCI_x)%acquired = .false.
              run_tag(SCI_y)%acquired = .false.
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
                run_tag(SCI_coord)%acquired = .true.
              end if
            end if
          else if (run_tag(SCI_pointBarCli)%in_tag) then
            ! SCI_pointBarCli
             if (run_tag(SCI_coordI)%in_tag) then
              call read_param(SCI_coordI, param_value, ipol) 
              ! adjust from base 0 to base 1 arrays
              ipol = ipol + 1
              run_tag(SCI_coordI)%acquired = .true.
            else if (run_tag(SCI_BarCliI)%in_tag) then
              call read_param(SCI_BarCliI, param_value, iseas)
              ! adjust from base 0 to base 1 arrays
              iseas = iseas + 1
              run_tag(SCI_BarCliI)%acquired = .true.
            else if (run_tag(SCI_height)%in_tag) then
              call read_param(SCI_height, param_value, t_params%amzbr)
              run_tag(SCI_height)%acquired = .true.
            else if (run_tag(SCI_width)%in_tag) then
              call read_param(SCI_width, param_value, t_params%amxbrw)
              run_tag(SCI_width)%acquired = .true.
            else if (run_tag(SCI_porosity)%in_tag) then
              call read_param(SCI_porosity, param_value, t_params%ampbr)
              run_tag(SCI_porosity)%acquired = .true.
            end if
            if (    run_tag(SCI_coordI)%acquired &
              .and. run_tag(SCI_BarCliI)%acquired &
              .and. run_tag(SCI_height)%acquired &
              .and. run_tag(SCI_width)%acquired &
              .and. run_tag(SCI_porosity)%acquired &
              ) then
              if( t_params%amzbr .le. 0.0 ) then
                write(*,*) 'ERROR: Barrier height must be > 0'
                write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol, 'Season #: ', iseas
                call exit(40)
              end if
              run_tag(SCI_coordI)%acquired = .false.
              run_tag(SCI_BarCliI)%acquired = .false.
              run_tag(SCI_height)%acquired = .false.
              run_tag(SCI_width)%acquired = .false.
              run_tag(SCI_porosity)%acquired = .false.
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
                run_tag(SCI_pointBarCli)%acquired = .true.
              end if
            end if
          end if

          if (    run_tag(SCI_Description)%acquired &
            .and. run_tag(SCI_BarrierSeasonFlag)%acquired &
            .and. run_tag(SCI_BarCliNo)%acquired &
            .and. run_tag(SCI_BarCli)%acquired &
            .and. run_tag(SCI_coordNo)%acquired &
            .and. run_tag(SCI_coord)%acquired &
            .and. run_tag(SCI_pointBarCli)%acquired &
            ) then
            run_tag(SCI_Description)%acquired = .false.
            run_tag(SCI_BarrierSeasonFlag)%acquired = .false.
            run_tag(SCI_BarCliNo)%acquired = .false.
            run_tag(SCI_BarCli)%acquired = .false.
            run_tag(SCI_coordNo)%acquired = .false.
            run_tag(SCI_coord)%acquired = .false.
            run_tag(SCI_pointBarCli)%acquired = .false.
            ibr = ibr + 1
            if (ibr .gt. nbr) then
              run_tag(SCI_Barrier)%acquired = .true.
            end if

            sum_stat = 0
            deallocate(season_complete, stat=dealloc_stat)
            sum_stat = sum_stat + dealloc_stat
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
      write(*,*) 'Error reading ', run_tag(tag)%name, ' Value: ', param_string
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
      write(*,*) 'Error reading ', run_tag(tag)%name, ' Value: ', param_string
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
      write(*,*) 'Error reading ', run_tag(tag)%name, ' Value: ', param_string
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
      write(*,*) 'Error reading ', run_tag(tag)%name, ' Value: ', param_string
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
      write(*,*) 'Error reading ', run_tag(tag)%name, ' Value: ', param_string
      call exit(1)
    end if
  end subroutine read_param_int_3

end module input_run_xml_mod
