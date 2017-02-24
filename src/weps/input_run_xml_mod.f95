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

  integer, parameter :: AccNo = 1
  integer, parameter :: Account = 2
  integer, parameter :: AverageSlope = 3
  integer, parameter :: BarCli = 4
  integer, parameter :: BarCliI = 5
  integer, parameter :: BarCliNo = 6
  integer, parameter :: Barrier_tag = 7
  integer, parameter :: BarrierNo = 8
  integer, parameter :: BarrierSeasonFlag = 9
  integer, parameter :: BegTranBase = 10
  integer, parameter :: BegTranFlg = 11
  integer, parameter :: BegTranThresh = 12
  integer, parameter :: climateFile = 13
  integer, parameter :: coordinate = 14
  integer, parameter :: coordinates = 15
  integer, parameter :: coord = 16
  integer, parameter :: coordI = 17
  integer, parameter :: coordNo = 18
  integer, parameter :: crop = 19
  integer, parameter :: CycleCount = 20
  integer, parameter :: DebugOutput = 21
  integer, parameter :: decomp = 22
  integer, parameter :: Description = 23
  integer, parameter :: Elevation = 24
  integer, parameter :: EndDate = 25
  integer, parameter :: EndTranBase = 26
  integer, parameter :: EndTranFlg = 27
  integer, parameter :: EndTranThresh = 28
  integer, parameter :: ErosionSubmodelOutput = 29
  integer, parameter :: height = 30
  integer, parameter :: hydro = 31
  integer, parameter :: n_index = 32
  integer, parameter :: LatLong = 33
  integer, parameter :: ManageFile = 34
  integer, parameter :: man = 35
  integer, parameter :: Number = 36
  integer, parameter :: OriginCoord = 37
  integer, parameter :: pointBarCli = 38
  integer, parameter :: porosity = 39
  integer, parameter :: RegionAngle = 40
  integer, parameter :: runFileData = 41
  integer, parameter :: SoilFile = 42
  integer, parameter :: soil = 43
  integer, parameter :: SoilRockFragments = 44
  integer, parameter :: StartDate = 45
  integer, parameter :: subDailyFile = 46
  integer, parameter :: SubmodelOutput = 47
  integer, parameter :: Subregion = 48
  integer, parameter :: SubregionNo = 49
  integer, parameter :: TimeDesc = 50
  integer, parameter :: TimeMark = 51
  integer, parameter :: TimeSteps = 52
  integer, parameter :: WaterErosionLoss = 53
  integer, parameter :: width = 54
  integer, parameter :: windFile = 55
  integer, parameter :: XGrid = 56
  integer, parameter :: XLength = 57
  integer, parameter :: x = 58
  integer, parameter :: YGrid = 59
  integer, parameter :: YLength = 60
  integer, parameter :: y = 61

  integer :: nacctr   ! Number of accounting regions
  integer :: nbr      ! number of barriers
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
    use subregions_mod, only: acct_poly
    use barriers_mod, only: barrier, barseas
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: alloc_stat

    do idx = 1, size(run_tag)
      if( run_tag(idx)%name .eq. name ) then
        run_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. runFileData) then
            !write(*,*) 'Tags', run_tag(CycleCount)%acquired &
            !           , run_tag(LatLong)%acquired &
            !           , run_tag(Elevation)%acquired &
            !           , run_tag(StartDate)%acquired &
            !           , run_tag(EndDate)%acquired &
            !           , run_tag(TimeSteps)%acquired &
            !           , run_tag(climateFile)%acquired &
            !           , run_tag(windFile)%acquired &
            !           , run_tag(ErosionSubmodelOutput)%acquired &
            !           , run_tag(RegionAngle)%acquired &
            !           , run_tag(OriginCoord)%acquired &
            !           , run_tag(XLength)%acquired &
            !           , run_tag(YLength)%acquired &
            !           , run_tag(XGrid)%acquired &
            !           , run_tag(YGrid)%acquired &
            !           , run_tag(AccNo)%acquired &
            !           , run_tag(Account)%acquired &
            !           , run_tag(SubregionNo)%acquired &
            !           , run_tag(Subregion)%acquired &
            !           , run_tag(BarrierNo)%acquired &
            !           , run_tag(Barrier_tag)%acquired

          if (run_tag(AccNo)%acquired) then
            if (nacctr .le. 0) then
              runfile_complete(1) = .true.
            else
              if (run_tag(Account)%acquired) then
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

          if (run_tag(BarrierNo)%acquired) then
            if (nbr .le. 0) then
              runfile_complete(2) = .true.
            else
              if (run_tag(Barrier_tag)%acquired) then
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
          if (    run_tag(CycleCount)%acquired &
            .and. run_tag(LatLong)%acquired &
            .and. run_tag(Elevation)%acquired &
            .and. run_tag(StartDate)%acquired &
            .and. run_tag(EndDate)%acquired &
            .and. run_tag(TimeSteps)%acquired &
            .and. run_tag(climateFile)%acquired &
            .and. run_tag(windFile)%acquired &
            .and. run_tag(ErosionSubmodelOutput)%acquired &
            .and. run_tag(RegionAngle)%acquired &
            .and. run_tag(OriginCoord)%acquired &
            .and. run_tag(XLength)%acquired &
            .and. run_tag(YLength)%acquired &
            .and. run_tag(XGrid)%acquired &
            .and. run_tag(YGrid)%acquired &
            .and. run_tag(SubregionNo)%acquired &
            .and. run_tag(Subregion)%acquired &
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

    max_tags = 61   ! count of unique tags needed from all dtd files
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
    run_tag(AccNo)%required = .false.
    run_tag(Account)%required = .false.
    run_tag(BarrierNo)%required = .false.
    run_tag(Barrier_tag)%required = .false.

    ! assign tag names
    run_tag(1)%name = "AccNo"
    run_tag(2)%name = "Account"
    run_tag(3)%name = "AverageSlope"
    run_tag(4)%name = "BarCli"
    run_tag(5)%name = "BarCliI"
    run_tag(6)%name = "BarCliNo"
    run_tag(7)%name = "Barrier"
    run_tag(8)%name = "BarrierNo"
    run_tag(9)%name = "BarrierSeasonFlag"
    run_tag(10)%name = "BegTranBase"
    run_tag(11)%name = "BegTranFlg"
    run_tag(12)%name = "BegTranThresh"
    run_tag(13)%name = "climateFile"
    run_tag(14)%name = "coordinate"
    run_tag(15)%name = "coordinates"
    run_tag(16)%name = "coord"
    run_tag(17)%name = "coordI"
    run_tag(18)%name = "coordNo"
    run_tag(19)%name = "crop"
    run_tag(20)%name = "CycleCount"
    run_tag(21)%name = "DebugOutput"
    run_tag(22)%name = "decomp"
    run_tag(23)%name = "Description"
    run_tag(24)%name = "Elevation"
    run_tag(25)%name = "EndDate"
    run_tag(26)%name = "EndTranBase"
    run_tag(27)%name = "EndTranFlg"
    run_tag(28)%name = "EndTranThresh"
    run_tag(29)%name = "ErosionSubmodelOutput"
    run_tag(30)%name = "height"
    run_tag(31)%name = "hydro"
    run_tag(32)%name = "index"
    run_tag(33)%name = "LatLong"
    run_tag(34)%name = "ManageFile"
    run_tag(35)%name = "man"
    run_tag(36)%name = "Number"
    run_tag(37)%name = "OriginCoord"
    run_tag(38)%name = "pointBarCli"
    run_tag(39)%name = "porosity"
    run_tag(40)%name = "RegionAngle"
    run_tag(41)%name = "runFileData"
    run_tag(42)%name = "SoilFile"
    run_tag(43)%name = "soil"
    run_tag(44)%name = "SoilRockFragments"
    run_tag(45)%name = "StartDate"
    run_tag(46)%name = "subDailyFile"
    run_tag(47)%name = "SubmodelOutput"
    run_tag(48)%name = "Subregion"
    run_tag(49)%name = "SubregionNo"
    run_tag(50)%name = "TimeDesc"
    run_tag(51)%name = "TimeMark"
    run_tag(52)%name = "TimeSteps"
    run_tag(53)%name = "WaterErosionLoss"
    run_tag(54)%name = "width"
    run_tag(55)%name = "windFile"
    run_tag(56)%name = "XGrid"
    run_tag(57)%name = "XLength"
    run_tag(58)%name = "x"
    run_tag(59)%name = "YGrid"
    run_tag(60)%name = "YLength"
    run_tag(61)%name = "y"

    ! create integer variable names for tags and assign index number.
    ! makes chunk code more understandable.

  end subroutine init_run_xml

  subroutine pcdata_chunk_handler(chunk)

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

    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value
    integer :: sum_stat, alloc_stat, dealloc_stat
    integer :: read_stat
    real :: cligen_version

    integer, parameter :: max_simyear = 100000  ! value used to test simulation year input range
    integer, save :: nsubr    ! Number of subregions
    integer, save :: seas_flg ! barrier season flag
    integer, save :: ntm_seas ! number of time marks for seasonal barrier
    integer, save :: poly_np  ! number of points in polygon or polyline
    integer, save :: isr      ! index for subregion reading
    integer, save :: iar      ! index for accounting region reading
    integer, save :: ibr      ! index for barrier reading
    integer, save :: ipol     ! index for polygon reading
    integer, save :: iseas    ! index for barrier season reading
    logical, save, dimension(:), allocatable :: subregion_complete
    logical, save, dimension(:), allocatable :: season_complete
    logical, save, dimension(:), allocatable :: points_complete
    logical, save, dimension(:,:), allocatable :: clipar_complete
    integer, save :: count_complete
  ! temporary holder for array elements until index is read
    integer, save :: t_am0hfl
    integer, save :: t_am0sfl
    integer, save :: t_am0tfl
    integer, save :: t_am0cfl
    integer, save :: t_am0dfl
    integer, save :: t_am0hdb
    integer, save :: t_am0sdb
    integer, save :: t_am0tdb
    integer, save :: t_am0cdb
    integer, save :: t_am0ddb
    type(polygon), save :: t_polygon
    type(soil_def), save :: t_soil
    character(len=512), save :: t_tinfil
    type(point), save :: t_point
    type(barrier_day_state), save :: t_day_state
    type(barrier_params), save :: t_params
    type(barrier_climate), save :: t_climate
    character(len=80), save :: t_amzbt

    param_value = trim(chunk)

    if (run_tag(runFileData)%in_tag) then
      if (run_tag(CycleCount)%in_tag) then
        call read_param(CycleCount, param_value, run_rot_cycles)
        run_tag(CycleCount)%acquired = .true.

      else if (run_tag(LatLong)%in_tag) then
        call read_param(LatLong, param_value, amalat, amalon)
        if (read_stat .gt. 0) then
          write(*,*) 'Error reading ', run_tag(LatLong)%name, ' Value: ', param_value
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
        run_tag(LatLong)%acquired = .true.

      else if (run_tag(Elevation)%in_tag) then
        call read_param(Elevation, param_value, amzele)
        run_tag(Elevation)%acquired = .true.
      else if (run_tag(StartDate)%in_tag) then
        call read_param(StartDate, param_value, id, im, iy)
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
        run_tag(StartDate)%acquired = .true.

      else if (run_tag(EndDate)%in_tag) then
        call read_param(EndDate, param_value, ld, lm, ly)
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
        run_tag(EndDate)%acquired = .true.

      else if (run_tag(TimeSteps)%in_tag) then
        call read_param(TimeSteps, param_value, ntstep)
        run_tag(TimeSteps)%acquired = .true.

        ! allocate wind direction and speed array
        allocate(subday(ntstep), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: memory alloc., wind direction and speed'
        end if

      else if (run_tag(climateFile)%in_tag) then
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
        run_tag(climateFile)%acquired = .true.

      else if (run_tag(windFile)%in_tag) then
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
        run_tag(windFile)%acquired = .true.

      ! else if (run_tag(subDailyFile)%in_tag) then

      else if (run_tag(ErosionSubmodelOutput)%in_tag) then
        call read_param(ErosionSubmodelOutput, param_value, am0efl)
        run_tag(ErosionSubmodelOutput)%acquired = .true.

      else if (run_tag(RegionAngle)%in_tag) then
        call read_param(RegionAngle, param_value, amasim)
        run_tag(RegionAngle)%acquired = .true.

      else if (run_tag(OriginCoord)%in_tag) then
        call read_param(OriginCoord, param_value, amxsim(1)%x, amxsim(1)%y)
        run_tag(OriginCoord)%acquired = .true.

      else if (run_tag(XLength)%in_tag) then
        call read_param(XLength, param_value, amxsim(2)%x)
        run_tag(XLength)%acquired = .true.
        ! compute the simulation area
        if (run_tag(YLength)%acquired) then
          sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
          write(6,*) "Simulation area (m^2)", sim_area
        end if

      else if (run_tag(YLength)%in_tag) then
        call read_param(YLength, param_value, amxsim(2)%y)
        run_tag(YLength)%acquired = .true.
        ! compute the simulation area
        if (run_tag(XLength)%acquired) then
          sim_area = (amxsim(2)%x - amxsim(1)%x) * (amxsim(2)%y - amxsim(1)%y)
          write(6,*) "Simulation area (m^2)", sim_area
        end if

      else if (run_tag(XGrid)%in_tag) then
        call read_param(XGrid, param_value, xgdpt, ygdpt)
        run_tag(XGrid)%acquired = .true.

      else if (run_tag(YGrid)%in_tag) then
        call read_param(YGrid, param_value, xgdpt, ygdpt)
        run_tag(YGrid)%acquired = .true.

      else if (run_tag(AccNo)%in_tag) then
        call read_param(AccNo, param_value, nacctr)
        run_tag(AccNo)%acquired = .true.
        if (nacctr .gt. 0) then
          ! set counter iar for reading in Accounting Regions
          iar = 1
        end if
        ! create array of accounting region polygons (zero size array allowed)
        allocate(acct_poly(nacctr), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
          write(*,*) 'ERROR: memory alloc., accounting region polygons'
        end if

      else if (run_tag(Account)%in_tag) then
        ! Accounting region coordinates
        if (run_tag(AccNo)%required) then
          if (run_tag(coordinates)%in_tag) then
            !coordinates
            if (run_tag(Number)%in_tag) then
              call read_param(Number, param_value, poly_np)
              if (poly_np .gt. 0) then
                ! create polygon point storage
                acct_poly(iar) = create_polygon(poly_np)
                ! initialize polygon point counter
                ipol = 1
              end if
              run_tag(Number)%acquired = .true.
            else if (run_tag(coordinate)%in_tag) then
              if (run_tag(Number)%acquired) then
                call read_param(coordinate, param_value, acct_poly(iar)%points(ipol)%x, acct_poly(iar)%points(ipol)%y)
                ipol = ipol + 1
                if (ipol .gt. poly_np) then
                  iar = iar + 1
                end if
              else
                write(*,*) 'Error: Number of coordinates must be specified before reading in coordinates'
              end if
            end if
          end if
          if (iar .gt. nacctr) then
            run_tag(AccNo)%acquired = .true.
            run_tag(Account)%acquired = .true.
            run_tag(Number)%acquired = .false.
          end if
        else
          write(*,*) 'Error: Number of accounting regions must be specified before reading in accounting region data'
        end if

      else if (run_tag(SubregionNo)%in_tag) then
        call read_param(SubregionNo, param_value, nsubr)
        if (nsubr .lt. 1) then
          write(*,*) 'Error, subregion count must be 1 or greater. Value: ', nsubr
          call exit(1)
        end if
        run_tag(SubregionNo)%acquired = .true.

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

      else if (run_tag(Subregion)%in_tag) then
        ! Subregion
        if (run_tag(SubregionNo)%acquired) then
          if (run_tag(n_index)%in_tag) then
            call read_param(n_index, param_value, isr)
            ! adjust from base 0 to base 1 arrays
            isr = isr + 1
            run_tag(n_index)%acquired = .true.
          else if (run_tag(SubmodelOutput)%in_tag) then
            ! SubmodelOutput
            if (run_tag(hydro)%in_tag) then
              call read_param(hydro, param_value, t_am0hfl)
              run_tag(hydro)%acquired = .true.
            else if (run_tag(soil)%in_tag) then
              call read_param(soil, param_value, t_am0sfl)
              run_tag(soil)%acquired = .true.
            else if (run_tag(man)%in_tag) then
              call read_param(man, param_value, t_am0tfl)
              run_tag(man)%acquired = .true.
            else if (run_tag(crop)%in_tag) then
              call read_param(crop, param_value, t_am0cfl)
              run_tag(crop)%acquired = .true.
            else if (run_tag(decomp)%in_tag) then
              call read_param(decomp, param_value, t_am0dfl)
              run_tag(decomp)%acquired = .true.
            end if
            if (    run_tag(hydro)%acquired &
              .and. run_tag(soil)%acquired &
              .and. run_tag(man)%acquired &
              .and. run_tag(crop)%acquired &
              .and. run_tag(decomp)%acquired ) then
              run_tag(hydro)%acquired = .false.
              run_tag(soil)%acquired = .false.
              run_tag(man)%acquired = .false.
              run_tag(crop)%acquired = .false.
              run_tag(decomp)%acquired = .false.
              run_tag(SubmodelOutput)%acquired = .true.
            end if
          else if (run_tag(DebugOutput)%in_tag) then
            ! DebugOutput
            if (run_tag(hydro)%in_tag) then
              call read_param(hydro, param_value, t_am0hdb)
              run_tag(hydro)%acquired = .true.
            else if (run_tag(soil)%in_tag) then
              call read_param(soil, param_value, t_am0sdb)
              run_tag(soil)%acquired = .true.
            else if (run_tag(man)%in_tag) then
              call read_param(man, param_value, t_am0tdb)
              run_tag(man)%acquired = .true.
            else if (run_tag(crop)%in_tag) then
              call read_param(crop, param_value, t_am0cdb)
              run_tag(crop)%acquired = .true.
            else if (run_tag(decomp)%in_tag) then
              call read_param(decomp, param_value, t_am0ddb)
              run_tag(decomp)%acquired = .true.
            end if
            if (    run_tag(hydro)%acquired &
              .and. run_tag(soil)%acquired &
              .and. run_tag(man)%acquired &
              .and. run_tag(crop)%acquired &
              .and. run_tag(decomp)%acquired ) then
              run_tag(hydro)%acquired = .false.
              run_tag(soil)%acquired = .false.
              run_tag(man)%acquired = .false.
              run_tag(crop)%acquired = .false.
              run_tag(decomp)%acquired = .false.
              run_tag(DebugOutput)%acquired = .true.
            end if
          else if (run_tag(coordinates)%in_tag) then
            !coordinates
            if (run_tag(Number)%in_tag) then
              call read_param(Number, param_value, poly_np)
              if (poly_np .gt. 0) then
                ! create polygon point storage
                t_polygon = create_polygon(poly_np)
                ! initialize polygon point counter
                ipol = 1
              end if
              run_tag(Number)%acquired = .true.
            else if (run_tag(coordinate)%in_tag) then
              if (run_tag(Number)%acquired) then
                call read_param(coordinate, param_value, t_polygon%points(ipol)%x, t_polygon%points(ipol)%y)
                ipol = ipol + 1
                if (ipol .gt. poly_np) then
                  run_tag(coordinates)%acquired = .true.
                  run_tag(Number)%acquired = .false.
                end if
              else
                write(*,*) 'Error: Number of coordinates must be specified before reading in coordinates'
              end if
            end if
          else if (run_tag(AverageSlope)%in_tag) then
            !        The new "versioned" IFC files contain a slope value
            !        which will be used if this value is set negative, 
            !        ie. not entered. It is now the only way to set a 
            !        non default slope when using the older "non-versioned"
            !        IFC files.   
            call read_param(AverageSlope, param_value, t_soil%amrslp)
            run_tag(AverageSlope)%acquired = .true.

          else if (run_tag(SoilRockFragments)%in_tag) then
            call read_param(SoilRockFragments, param_value, t_soil%SoilRockFragments)
            run_tag(SoilRockFragments)%acquired = .true.

          else if (run_tag(SoilFile)%in_tag) then
            ! read in initial field conditions file name
            t_soil%sinfil = rootp(1:len_trim(rootp)) // param_value(1:len_trim(param_value))
            run_tag(SoilFile)%acquired = .true.

            write(*,*) 'SOILFILE: ', param_value(1:len_trim(param_value))

          else if (run_tag(ManageFile)%in_tag) then
            ! read in management file name
            t_tinfil = rootp(1:len_trim(rootp)) // param_value(1:len_trim(param_value))
            run_tag(ManageFile)%acquired = .true.

          else if (run_tag(WaterErosionLoss)%in_tag) then
            call read_param(WaterErosionLoss, param_value, t_soil%WaterErosion)
            run_tag(WaterErosionLoss)%acquired = .true.

          end if
          if (    run_tag(n_index)%acquired &
            .and. run_tag(SubmodelOutput)%acquired &
            .and. run_tag(DebugOutput)%acquired &
            .and. run_tag(coordinates)%acquired &
            .and. run_tag(AverageSlope)%acquired &
            .and. run_tag(SoilRockFragments)%acquired &
            .and. run_tag(SoilFile)%acquired &
            .and. run_tag(ManageFile)%acquired &
            .and. run_tag(WaterErosionLoss)%acquired) then
            run_tag(n_index)%acquired = .false.
            run_tag(SubmodelOutput)%acquired = .false.
            run_tag(DebugOutput)%acquired = .false.
            run_tag(coordinates)%acquired = .false.
            run_tag(AverageSlope)%acquired = .false.
            run_tag(SoilRockFragments)%acquired = .false.
            run_tag(SoilFile)%acquired = .false.
            run_tag(ManageFile)%acquired = .false.
            run_tag(WaterErosionLoss)%acquired = .false.
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
              run_tag(Subregion)%acquired = .true.
            end if
          end if
        else
          write(*,*) 'Error: Number of subregions must be specified before reading in subregion data'
        end if
      else if (run_tag(BarrierNo)%in_tag) then
        call read_param(BarrierNo, param_value, nbr)
        run_tag(BarrierNo)%acquired = .true.
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

      else if (run_tag(Barrier_tag)%in_tag) then
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
        if (run_tag(BarrierNo)%acquired) then
          if (run_tag(Description)%in_tag) then
            t_amzbt = trim(param_value)
            !  also place in fixed barrier structure
            t_amzbt = barseas(ibr)%amzbt
            run_tag(Description)%acquired = .true.
          else if (run_tag(BarrierSeasonFlag)%in_tag) then
            call read_param(BarrierSeasonFlag, param_value, seas_flg)
            run_tag(BarrierSeasonFlag)%acquired = .true.
          else if (run_tag(BarCliNo)%in_tag) then
            call read_param(BarCliNo, param_value, ntm_seas)
            run_tag(BarCliNo)%acquired = .true.

          else if (run_tag(coordNo)%in_tag) then
            call read_param(coordNo, param_value, poly_np)
            if (run_tag(BarrierSeasonFlag)%acquired &
              .and. run_tag(BarCliNo)%acquired &
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
              write(*,*) 'Tags for BarrierSeasonFlag and BarCliNo are required before reading coordNo tag.'
              call exit(1)
            end if
            run_tag(coordNo)%acquired = .true.
            ! create storage for point and barrier data
            ! this also sets values for barr%np and barr%ntm
            barrier(ibr) = create_barrier(poly_np)
            barseas(ibr) = create_barrier(poly_np,ntm_seas,seas_flg)
            if( run_tag(Description)%acquired ) then
              ! acquired so assign value to description
              barseas(ibr)%amzbt = t_amzbt
              barrier(ibr)%amzbt = t_amzbt
            end if
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

          else if (run_tag(BarCli)%in_tag) then
            ! BarCli Climate transition parameters
            if (run_tag(n_index)%in_tag) then
              call read_param(n_index, param_value, iseas)
              ! adjust from base 0 to base 1 arrays
              iseas = iseas + 1
              run_tag(n_index)%acquired = .true.
            else if (run_tag(TimeMark)%in_tag) then
              call read_param(TimeMark, param_value, t_day_state%doy)
              run_tag(TimeMark)%acquired = .true.
            else if (run_tag(TimeDesc)%in_tag) then
              t_day_state%st_desc = param_value(1:80)
              run_tag(TimeDesc)%acquired = .true.
            else if (run_tag(BegTranFlg)%in_tag) then
              call read_param(BegTranFlg, param_value, t_climate%beg_flg)
              run_tag(BegTranFlg)%acquired = .true.
            else if (run_tag(BegTranThresh)%in_tag) then
              call read_param(BegTranThresh, param_value, t_climate%beg_thresh)
              run_tag(BegTranThresh)%acquired = .true.
            else if (run_tag(BegTranBase)%in_tag) then
              call read_param(BegTranBase, param_value, t_climate%beg_base)
              run_tag(BegTranBase)%acquired = .true.
            else if (run_tag(EndTranFlg)%in_tag) then
              call read_param(EndTranFlg, param_value, t_climate%end_flg)
              run_tag(EndTranFlg)%acquired = .true.
            else if (run_tag(EndTranThresh)%in_tag) then
              call read_param(EndTranThresh, param_value, t_climate%end_thresh)
              run_tag(EndTranThresh)%acquired = .true.
            else if (run_tag(EndTranBase)%in_tag) then
              call read_param(EndTranBase, param_value, t_climate%end_base)
              run_tag(EndTranBase)%acquired = .true.
            end if
            if( seas_flg .eq. 2 ) then
              if (    run_tag(n_index)%acquired &
                .and. run_tag(TimeMark)%acquired & 
                .and. run_tag(TimeDesc)%acquired &
                .and. run_tag(BegTranFlg)%acquired &
                .and. run_tag(BegTranThresh)%acquired &
                .and. run_tag(BegTranBase)%acquired &
                .and. run_tag(EndTranFlg)%acquired &
                .and. run_tag(EndTranThresh)%acquired &
                .and. run_tag(EndTranBase)%acquired &
                ) then
                run_tag(n_index)%acquired = .false.
                run_tag(TimeMark)%acquired = .false.
                run_tag(TimeDesc)%acquired = .false.
                run_tag(BegTranFlg)%acquired = .false.
                run_tag(BegTranThresh)%acquired = .false.
                run_tag(BegTranBase)%acquired = .false.
                run_tag(EndTranFlg)%acquired = .false.
                run_tag(EndTranThresh)%acquired = .false.
                run_tag(EndTranBase)%acquired = .false.
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
                  run_tag(BarCli)%acquired = .true.
                end if
              end if  
            else 
              if (    run_tag(n_index)%acquired &
                .and. run_tag(TimeMark)%acquired & 
                .and. run_tag(TimeDesc)%acquired &
                ) then
                run_tag(n_index)%acquired = .false.
                run_tag(TimeMark)%acquired = .false.
                run_tag(TimeDesc)%acquired = .false.
                season_complete(iseas) = .true.
                count_complete = 0
                do iseas = 1, ntm_seas
                  if (season_complete(iseas)) then
                    count_complete = count_complete + 1
                  end if
                end do
                if (count_complete .ge. ntm_seas) then
                  run_tag(BarCli)%acquired = .true.
                end if
              end if
            end if
          else if (run_tag(coord)%in_tag) then
            !coord
            if (run_tag(n_index)%in_tag) then
              call read_param(n_index, param_value, ipol)
              ! adjust from base 0 to base 1 arrays
              ipol = ipol + 1
              run_tag(n_index)%acquired = .true.
            else if (run_tag(x)%in_tag) then
              call read_param(EndTranBase, param_value, t_point%x)
              run_tag(x)%acquired = .true.
            else if (run_tag(y)%in_tag) then
              call read_param(EndTranBase, param_value, t_point%y)
              run_tag(y)%acquired = .true.
            end if
            if(     run_tag(n_index)%acquired &
              .and. run_tag(x)%acquired &
              .and. run_tag(y)%acquired &
              ) then
              run_tag(n_index)%acquired = .false.
              run_tag(x)%acquired = .false.
              run_tag(y)%acquired = .false.
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
                run_tag(coord)%acquired = .true.
              end if
            end if
          else if (run_tag(pointBarCli)%in_tag) then
            ! pointBarCli
             if (run_tag(coordI)%in_tag) then
              call read_param(coordI, param_value, ipol) 
              ! adjust from base 0 to base 1 arrays
              ipol = ipol + 1
              run_tag(coordI)%acquired = .true.
            else if (run_tag(BarCliI)%in_tag) then
              call read_param(BarCliI, param_value, iseas)
              ! adjust from base 0 to base 1 arrays
              iseas = iseas + 1
              run_tag(BarCliI)%acquired = .true.
            else if (run_tag(height)%in_tag) then
              call read_param(height, param_value, t_params%amzbr)
              run_tag(height)%acquired = .true.
            else if (run_tag(width)%in_tag) then
              call read_param(width, param_value, t_params%amxbrw)
              run_tag(width)%acquired = .true.
            else if (run_tag(porosity)%in_tag) then
              call read_param(porosity, param_value, t_params%ampbr)
              run_tag(porosity)%acquired = .true.
            end if
            if (    run_tag(coordI)%acquired &
              .and. run_tag(BarCliI)%acquired &
              .and. run_tag(height)%acquired &
              .and. run_tag(width)%acquired &
              .and. run_tag(porosity)%acquired &
              ) then
              if( t_params%amzbr .le. 0.0 ) then
                write(*,*) 'ERROR: Barrier height must be > 0'
                write(*,FMT='(2(i0))') 'Barrier #: ', ibr, 'Point #: ', ipol, 'Season #: ', iseas
                call exit(40)
              end if
              run_tag(coordI)%acquired = .false.
              run_tag(BarCliI)%acquired = .false.
              run_tag(height)%acquired = .false.
              run_tag(width)%acquired = .false.
              run_tag(porosity)%acquired = .false.
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
                run_tag(pointBarCli)%acquired = .true.
              end if
            end if
          end if

          if (    run_tag(Description)%acquired &
            .and. run_tag(BarrierSeasonFlag)%acquired &
            .and. run_tag(BarCliNo)%acquired &
            .and. run_tag(BarCli)%acquired &
            .and. run_tag(coordNo)%acquired &
            .and. run_tag(coord)%acquired &
            .and. run_tag(pointBarCli)%acquired &
            ) then
            run_tag(Description)%acquired = .false.
            run_tag(BarrierSeasonFlag)%acquired = .false.
            run_tag(BarCliNo)%acquired = .false.
            run_tag(BarCli)%acquired = .false.
            run_tag(coordNo)%acquired = .false.
            run_tag(coord)%acquired = .false.
            run_tag(pointBarCli)%acquired = .false.
            ibr = ibr + 1
            if (ibr .gt. nbr) then
              run_tag(Barrier_tag)%acquired = .true.
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
