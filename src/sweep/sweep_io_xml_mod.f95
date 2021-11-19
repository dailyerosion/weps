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
  use sweep_io_xml_defs
  use Polygons_Mod, only: polygon, create_polygon, destroy_polygon, set_area_polygon
  use Points_Mod, only: point
  use subregions_mod, only: acct_poly, subr_poly
  use erosion_data_struct_defs, only: subregionsurfacestate, create_brcdinputpools, &
                                      create_subregionsoillayers, create_subregionsurfacewet, &
                                      awzypt, awdair, anemht, awzzo, wzoflg, &
                                      ntstep, awadir, awudmx, subday, am0eif, subrsurf
  use p1erode_def, only: SLRR_MIN, SLRR_MAX, WZZO_MIN, WZZO_MAX
  use barriers_mod, only: create_barrier, barrier, barseas
  use barriers_mod, only: barrier_day_state, barrier_params, barrier_climate
  use sae_in_out_mod, only: saeinp, subrfiles
  use grid_mod, only: gridfile
  use read_write_xml_mod, only: read_param
  use sae_in_out_mod, only: mksaeinp

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
  integer :: iwind    ! index for wind speed values
  integer :: isurfwat ! index for surface water content values
  logical :: accounts_present = .false.
  logical, dimension(:), allocatable :: account_complete
  logical, dimension(:), allocatable :: subregion_complete
  logical, dimension(:), allocatable :: coord_complete
  logical, dimension(:), allocatable :: brcdinput_complete
  logical, dimension(:), allocatable :: soillay_complete
  logical, dimension(:), allocatable :: surfwat_complete
  logical :: barriers_present = .false.
  logical, dimension(:), allocatable :: barrier_complete
  logical, dimension(:), allocatable :: barpnts_complete
  logical, dimension(:), allocatable :: wind_complete
  integer :: count_complete
  ! temporary holder for array elements until index is read
  type(barrier_day_state) :: t_day_state
  type(barrier_climate) :: t_climate
  logical :: sweepdata_complete
  logical, dimension(:), allocatable :: treatmentdata_complete
  logical, dimension(:), allocatable :: soilstate_complete

  integer :: npools   ! number of brcdInput pools
  integer :: ipool    ! index for biomass pool reading

  ! temporary holders for brcdInput data until whole record read
  real :: rlai     ! leaf area index (m^2/m^2)
  real :: rsai     ! stem area index (m^2/m^2)
  integer :: rg    ! seed placement (0 - furrow, 1 - ridge)
  real :: xrow     ! row spacing (m)
  real :: zht      ! height (m)

  integer :: isl      ! index for soil layer reading

contains

  subroutine begin_sweep_element_handler(name,attributes)
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

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .true.
        ! write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

    if (   (idx .eq. SCI_Subregions) &
      .or. (idx .eq. SCI_Barriers) &
      .or. (idx .eq. SCI_WindSpeeds) ) then
      if ( has_key(attributes, input_tag(SCI_number)%name) ) then
        call get_value(attributes, input_tag(SCI_number)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregions)
          call read_param(input_tag(SCI_number)%name, param_value, nsubr)
          ! create data array to hold input and derived values for each subregion
          sum_stat = 0
          allocate(subrsurf(1,nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(subrfiles(nsubr), stat=alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(subregion_complete(nsubr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(treatmentdata_complete(nsubr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(soilstate_complete(nsubr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          if( sum_stat .gt. 0 ) then
            write(*,*) 'ERROR: memory allocation, subrsurf, subrfiles'
          end if
          ! for all subregions
          do idx = 1, nsubr
            ! initialize _complete arrays to false
            subregion_complete(idx) = .false.
            ! nullify brcdInput pointers
          end do

        case (SCI_Barriers)
          call read_param(input_tag(SCI_number)%name, param_value, nbr)
          !write(*,*) 'Number of Barriers: ', nbr
          ! allocate structure for barriers (nbr .lt. 1 gives zero size array)
          sum_stat = 0
          allocate(barrier(nbr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(barseas(nbr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(barrier_complete(nbr), stat = alloc_stat)
          if( sum_stat .gt. 0 ) then
            write(*,*) 'ERROR: memory alisrloc., barrier arrays'
          end if
          ! initialize _complete arrays to .false.
          do idx = 1, nbr
            barrier_complete(idx) = .false.
          end do
          barriers_present = .true.

        case (SCI_WindSpeeds)
          call read_param(input_tag(SCI_number)%name, param_value, ntstep)
          !write(*,*) 'Number of Wind Speeds: ', ntstep
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

        end select
      else
        write(*,*) 'SCI_number attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
    else if ( (idx .eq. SCI_Subregion) &
      .or. (idx .eq. SCI_BarPoint) &
      .or. (idx .eq. SCI_WindSpeed) ) then
      if ( has_key(attributes, input_tag(SCI_index)%name) ) then
        call get_value(attributes, input_tag(SCI_index)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregion)
          call read_param(input_tag(SCI_index)%name, param_value, isr)
          ! adjust from base 0 to base 1 arrays
          isr = isr + 1
          subrfiles(isr)%isub = isr
          !write(*,*) 'Subregion Index: ', isr
        case (SCI_BarPoint)
          call read_param(input_tag(SCI_index)%name, param_value, ipol)
          ! adjust from base 0 to base 1 arrays
          ipol = ipol + 1
          !write(*,*) 'Barrier Point Index: ', ipol
        case (SCI_WindSpeed)
          call read_param(input_tag(SCI_index)%name, param_value, iwind)
          ! adjust from base 0 to base 1 arrays
          iwind = iwind + 1
          !write(*,*) 'Wind Speed Index: ', iwind
        end select
      else
        write(*,*) 'SCI_index attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    else if ( (idx .eq. SCI_Barrier) ) then
      if ( has_key(attributes, input_tag(SCI_index)%name) ) then
        call get_value(attributes, input_tag(SCI_index)%name, param_value, ret_stat)
        call read_param(input_tag(SCI_index)%name, param_value, ibr)
        ! adjust from base 0 to base 1 arrays
        ibr = ibr + 1
        !write(*,*) 'Barrier Index: ', ibr
      else
        write(*,*) 'SCI_index attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
      if ( has_key(attributes, input_tag(SCI_number)%name) ) then
        call get_value(attributes, input_tag(SCI_number)%name, param_value, ret_stat)
        call read_param(input_tag(SCI_number)%name, param_value, poly_np)
        !write(*,*) 'Number of Barrier Points: ', poly_np
        ! create storage for point and barrier data
        ! this also sets values for barr%np and barr%ntm
        ntm_seas = 1
        iseas = 1
        seas_flg = 0
        call create_barrier(barrier(ibr), poly_np)
        call create_barrier(barseas(ibr), poly_np,ntm_seas,seas_flg)
        ! create storage for points index tracking
        allocate(barpnts_complete(poly_np), stat = alloc_stat)
        if( alloc_stat .gt. 0 ) then
          ! allocation failed
          write(*,*) "ERROR: unable to allocate memory for barpnts_complete array"
        end if
        ! initialize _complete arrays to false
        do idx = 1, poly_np
          barpnts_complete(idx) = .false.
        end do
      else
        write(*,*) 'SCI_number attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    end if

  end subroutine begin_sweep_element_handler

  subroutine end_sweep_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: jdx
    integer :: kdx
    integer :: alloc_stat
    integer :: sum_stat
    integer :: dealloc_stat

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. SweepData) then
          if ( .not. barriers_present) then
            ! no SCI_Barriers tag found (not needed so set to true)
            input_tag(SCI_Barriers)%acquired = .true.
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
          if (    input_tag(SCI_RegionAngle)%acquired &
            .and. input_tag(SCI_XOrigin)%acquired &
            .and. input_tag(SCI_YOrigin)%acquired &
            .and. input_tag(SCI_XLength)%acquired &
            .and. input_tag(SCI_YLength)%acquired &
            .and. input_tag(SCI_AirDensity)%acquired &
            .and. input_tag(SCI_WindDirection)%acquired &
            .and. input_tag(SCI_AnemometerHeight)%acquired &
            .and. input_tag(SCI_AerodynamicRoughness)%acquired &
            .and. input_tag(SCI_AnemometerFlag)%acquired &
            .and. input_tag(SCI_AverageAnnualPrecipitation)%acquired &
            .and. input_tag(SCI_Subregions)%acquired &
            .and. input_tag(SCI_WindSpeeds)%acquired &
            .and. input_tag(SCI_Barriers)%acquired &
            .and. input_tag(SCI_GridFile)%acquired &
            ) then
            sweepdata_complete = .true.

            ! always true on sweep runs
            am0eif = .true.
          else
            sweepdata_complete = .false.
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_RegionAngle, &
                    SCI_XOrigin, &
                    SCI_YOrigin, &
                    SCI_XLength, &
                    SCI_YLength, &
                    SCI_AirDensity, &
                    SCI_WindDirection, &
                    SCI_AnemometerHeight, &
                    SCI_AerodynamicRoughness, &
                    SCI_AnemometerFlag, &
                    SCI_AverageAnnualPrecipitation, &
                    SCI_GridFile)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_Subregions, &
                    SCI_WindSpeeds, &
                    SCI_Barriers)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is incomplete in input file.'
                end if
              end select
            end do
            write(*,*) 'No Results Generated.'
          end if
          ! deallocate Tag array
          deallocate( input_tag, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for Tag array"
          end if

        else if (idx .eq. SCI_Subregions) then
          !write(*,*) 'SCI_Subregions end of tag nsubr: ', nsubr
          count_complete = 0
          do jdx = 1, nsubr
            if (subregion_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. nsubr) then
            input_tag(SCI_Subregions)%acquired = .true.
          else
            do jdx = 1, nsubr
              if ( .not. subregion_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_Subregion)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(subregion_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for subregion_complete array"
          end if

        else if (idx .eq. SCI_WindSpeeds) then
          count_complete = 0
          do jdx = 1, ntstep
            if (wind_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. ntstep) then
            input_tag(SCI_WindSpeeds)%acquired = .true.

            ! Determine the maximum wind speed during the day
            awudmx = 0.0
            do jdx = 1, ntstep
              if( awudmx .lt. subday(jdx)%awu ) then
                awudmx = subday(jdx)%awu
              endif
            end do
          else
            do jdx = 1, ntstep
              if ( .not. wind_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_WindSpeed)%name), &
                                     ' SCI_index="', jdx-1, '" is missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(wind_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for wind_complete array"
          end if

        else if (idx .eq. SCI_Subregion) then
          ! check for acquisition of all required elements
          if (    input_tag(SCI_treat)%acquired &
            .and. input_tag(SCI_soilsurf)%acquired &
            ) then
            input_tag(SCI_treat)%acquired = .false.

            input_tag(SCI_soilsurf)%acquired = .false.
            subregion_complete(isr) = .true.

            !write(*,*) 'SCI_Subregion complete'
          else
            !write(*,*) 'SCI_Subregion NOT complete'
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_treat, &
                    SCI_soilsurf)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_Barriers) then
          count_complete = 0
          do jdx = 1, nbr
            if (barrier_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. nbr) then
            input_tag(SCI_Barriers)%acquired = .true.
          else
            do jdx = 1, size(barrier_complete)
              if ( .not. barrier_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_Barrier)%name), &
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
          count_complete = 0
          do jdx = 1, poly_np
            if (barpnts_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. poly_np) then
            barrier_complete(ibr) = .true.
          else
            do kdx = 1, size(barpnts_complete)
              if ( .not. barpnts_complete(kdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_BarPoint)%name), &
                           ' SCI_index="', kdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete arrays
          deallocate(barpnts_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for barpnts_complete arrays"
          end if

        else if (idx .eq. SCI_BarPoint) then
          if (    input_tag(SCI_x)%acquired &
            .and. input_tag(SCI_y)%acquired &
            .and. input_tag(SCI_height)%acquired &
            .and. input_tag(SCI_width)%acquired &
            .and. input_tag(SCI_porosity)%acquired &
            ) then
            if( barseas(ibr)%param(ipol,iseas)%amzbr .le. 0.0 ) then
              write(*,*) 'ERROR: Barrier height must be > 0'
              write(*,FMT='(2a,i0,3a,i0)') trim(input_tag(SCI_Barrier)%name), &
                          ' SCI_index="', ibr-1, &
                          ', ', trim(input_tag(SCI_BarPoint)%name), &
                          ' SCI_index="', ipol-1
              call exit(40)
            end if
            input_tag(SCI_x)%acquired = .false.
            input_tag(SCI_y)%acquired = .false.
            input_tag(SCI_height)%acquired = .false.
            input_tag(SCI_width)%acquired = .false.
            input_tag(SCI_porosity)%acquired = .false.
            barpnts_complete(ipol) = .true.
            ! copy barseas into fixed barrier
            barrier(ibr)%points(ipol) = barseas(ibr)%points(ipol)
            barrier(ibr)%param(ipol) = barseas(ibr)%param(ipol,iseas)
          else
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_x, &
                    SCI_y, &
                    SCI_height, &
                    SCI_width, &
                    SCI_porosity)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        end if

        exit  ! found tag, no need to look further

      end if
    end do

  end subroutine end_sweep_element_handler

  subroutine pcdata_sweep_chunk_handler(chunk)
  use grid_mod, only: amasim, amxsim
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value
    type(xml_t) :: fxml   ! xml file handle structure
    integer :: iostat     ! input/output status

    param_value = trim(chunk)

    if (input_tag(SweepData)%in_tag) then
      if (input_tag(SCI_RegionAngle)%in_tag) then
        call read_param(input_tag(SCI_RegionAngle)%name, param_value, amasim)
        input_tag(SCI_RegionAngle)%acquired = .true.

      else if (input_tag(SCI_XOrigin)%in_tag) then
        call read_param(input_tag(SCI_XOrigin)%name, param_value, amxsim(1)%x)
        input_tag(SCI_XOrigin)%acquired = .true.

      else if (input_tag(SCI_YOrigin)%in_tag) then
        call read_param(input_tag(SCI_YOrigin)%name, param_value, amxsim(1)%y)
        input_tag(SCI_YOrigin)%acquired = .true.

      else if (input_tag(SCI_XLength)%in_tag) then
        call read_param(input_tag(SCI_XLength)%name, param_value, amxsim(2)%x)
        input_tag(SCI_XLength)%acquired = .true.

      else if (input_tag(SCI_YLength)%in_tag) then
        call read_param(input_tag(SCI_YLength)%name, param_value, amxsim(2)%y)
        input_tag(SCI_YLength)%acquired = .true.

      else if (input_tag(SCI_Subregions)%in_tag) then
        if (input_tag(SCI_Subregion)%in_tag) then
          ! SCI_Subregion
          if (input_tag(SCI_treat)%in_tag) then
            call read_param(input_tag(SCI_treat)%name, param_value, subrfiles(isr)%treatfil)

            ! open input file
            call open_xmlfile(trim(mksaeinp%fullpath) // trim(subrfiles(isr)%treatfil),fxml,iostat)
            if (iostat /= 0) stop "Cannot open xml input file"
            call close_xmlfile(fxml)

            input_tag(SCI_treat)%acquired = .true.

          else if (input_tag(SCI_soilsurf)%in_tag) then
            call read_param(input_tag(SCI_soilsurf)%name, param_value, subrfiles(isr)%slstfil)

            ! open input file
            call open_xmlfile(trim(mksaeinp%fullpath) // trim(subrfiles(isr)%slstfil),fxml,iostat)
            if (iostat /= 0) stop "Cannot open xml input file"
            call close_xmlfile(fxml)

            input_tag(SCI_soilsurf)%acquired = .true.

          end if

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
        if (input_tag(SCI_BarPoint)%in_tag) then
          ! SCI_BarPoint
          !write(*,*) 'ibr: ',ibr, 'ipol: ', ipol
          if (input_tag(SCI_x)%in_tag) then
            call read_param(input_tag(SCI_x)%name, param_value, barseas(ibr)%points(ipol)%x)
            input_tag(SCI_x)%acquired = .true.
          else if (input_tag(SCI_y)%in_tag) then
            call read_param(input_tag(SCI_x)%name, param_value, barseas(ibr)%points(ipol)%y)
            input_tag(SCI_y)%acquired = .true.
          else if (input_tag(SCI_height)%in_tag) then
            call read_param(input_tag(SCI_height)%name, param_value, barseas(ibr)%param(ipol,iseas)%amzbr)
            input_tag(SCI_height)%acquired = .true.
          else if (input_tag(SCI_width)%in_tag) then
            call read_param(input_tag(SCI_width)%name, param_value, barseas(ibr)%param(ipol,iseas)%amxbrw)
            input_tag(SCI_width)%acquired = .true.
          else if (input_tag(SCI_porosity)%in_tag) then
            call read_param(input_tag(SCI_porosity)%name, param_value, barseas(ibr)%param(ipol,iseas)%ampbr)
            input_tag(SCI_porosity)%acquired = .true.
          end if
        end if

      else if (input_tag(SCI_AirDensity)%in_tag) then
        call read_param(input_tag(SCI_AirDensity)%name, param_value, awdair)
        input_tag(SCI_AirDensity)%acquired = .true.
      else if (input_tag(SCI_WindDirection)%in_tag) then
        call read_param(input_tag(SCI_WindDirection)%name, param_value, awadir)
        input_tag(SCI_WindDirection)%acquired = .true.
      else if (input_tag(SCI_AnemometerHeight)%in_tag) then
        call read_param(input_tag(SCI_AnemometerHeight)%name, param_value, anemht)
        input_tag(SCI_AnemometerHeight)%acquired = .true.
      else if (input_tag(SCI_AerodynamicRoughness)%in_tag) then
        call read_param(input_tag(SCI_AerodynamicRoughness)%name, param_value, awzzo)
        input_tag(SCI_AerodynamicRoughness)%acquired = .true.
      else if (input_tag(SCI_AnemometerFlag)%in_tag) then
        call read_param(input_tag(SCI_AnemometerFlag)%name, param_value, wzoflg)
        input_tag(SCI_AnemometerFlag)%acquired = .true.
      else if (input_tag(SCI_AverageAnnualPrecipitation)%in_tag) then
        call read_param(input_tag(SCI_AverageAnnualPrecipitation)%name, param_value, awzypt)
        input_tag(SCI_AverageAnnualPrecipitation)%acquired = .true.
      else if (input_tag(SCI_WindSpeeds)%in_tag) then
        if (input_tag(SCI_WindSpeed)%in_tag) then
          call read_param(input_tag(SCI_WindSpeed)%name, param_value, subday(iwind)%awu)
          wind_complete(iwind) = .true.
        end if
      else if (input_tag(SCI_GridFile)%in_tag) then
        call read_param(input_tag(SCI_GridFile)%name, param_value, gridfile)
        input_tag(SCI_GridFile)%acquired = .true.
      end if

    end if

  end subroutine pcdata_sweep_chunk_handler

  subroutine begin_soilstate_element_handler(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx
    character(len=80) :: param_value
    integer :: ret_stat
    integer :: alloc_stat

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

    if (   (idx .eq. SCI_SoilLays) ) then
      if ( has_key(attributes, input_tag(SCI_number)%name) ) then
        call get_value(attributes, input_tag(SCI_number)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_SoilLays)
          call read_param(input_tag(SCI_number)%name, param_value, subrsurf(1,isr)%nslay)
          !write(*,*) 'Number of Soil Layers: ', subrsurf(1,isr)%nslay
          allocate(soillay_complete(subrsurf(1,isr)%nslay), stat=alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'ERROR: memory allocation, soillay_complete'
          end if
          ! initialize _complete arrays to false
          do idx = 1, subrsurf(1,isr)%nslay
            soillay_complete(idx) = .false.
          end do
          ! create subrsurf soil layer arrays
          call create_subregionsoillayers(subrsurf(1,isr)%nslay, subrsurf(1,isr))
        end select
      else
        write(*,*) 'SCI_number attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
    else if ( (idx .eq. SCI_SoilLay) ) then
      if ( has_key(attributes, input_tag(SCI_index)%name) ) then
        call get_value(attributes, input_tag(SCI_index)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_SoilLay)
          call read_param(input_tag(SCI_index)%name, param_value, isl)
          ! adjust from base 0 to base 1 arrays
          isl = isl + 1
          !write(*,*) 'Soil Layer Index: ', isl
        end select
      else
        write(*,*) 'SCI_index attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    end if

  end subroutine begin_soilstate_element_handler

  subroutine end_soilstate_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: jdx
    integer :: dealloc_stat

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. SoilState) then

          ! check for acquisition of all required elements
          if ( input_tag(SCI_SoilLays)%acquired &
            ) then
            soilstate_complete(isr) = .true.

            ! always true on sweep runs
            am0eif = .true.
          else
            soilstate_complete(isr) = .false.
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_SoilLays)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is incomplete in input file.'
                end if
              end select
            end do
            write(*,*) 'No Results Generated.'
          end if
          ! deallocate Tag array
          deallocate( input_tag, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for Tag array"
          end if

        else if (idx .eq. SCI_SoilLays) then
          count_complete = 0
          do jdx = 1, subrsurf(1,isr)%nslay
            if (soillay_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. subrsurf(1,isr)%nslay) then
            input_tag(SCI_SoilLays)%acquired = .true.
          else
            do jdx = 1, size(soillay_complete)
              if ( .not. soillay_complete(jdx)) then
                 write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_SoilLay)%name), &
                                      ' SCI_index=', jdx-1, ' is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(soillay_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for soillay_complete array"
          end if

        else if (idx .eq. SCI_SoilLay) then
          ! check for acquisition of all required elements
          if (    input_tag(SCI_LayerThickness)%acquired &
            .and. input_tag(SCI_Sand)%acquired &
            .and. input_tag(SCI_Silt)%acquired &
            .and. input_tag(SCI_Clay)%acquired &
            .and. input_tag(SCI_RockVolume)%acquired &
            .and. input_tag(SCI_VeryFineSand)%acquired &
            .and. input_tag(SCI_AggregateGMD)%acquired &
            .and. input_tag(SCI_AggregateGSD)%acquired &
            .and. input_tag(SCI_AggregateMAX)%acquired &
            .and. input_tag(SCI_AggregateMIN)%acquired &
            .and. input_tag(SCI_AggregateDensity)%acquired &
            .and. input_tag(SCI_AggregateStability)%acquired &
            .and. input_tag(SCI_BulkDensity)%acquired &
            .and. input_tag(SCI_WaterContent)%acquired &
            .and. input_tag(SCI_WiltingPoint)%acquired ) then
            input_tag(SCI_LayerThickness)%acquired = .false.
            input_tag(SCI_Sand)%acquired = .false.
            input_tag(SCI_Silt)%acquired = .false.
            input_tag(SCI_Clay)%acquired = .false.
            input_tag(SCI_RockVolume)%acquired = .false.
            input_tag(SCI_VeryFineSand)%acquired = .false.
            input_tag(SCI_AggregateGMD)%acquired = .false.
            input_tag(SCI_AggregateGSD)%acquired = .false.
            input_tag(SCI_AggregateMAX)%acquired = .false.
            input_tag(SCI_AggregateMIN)%acquired = .false.
            input_tag(SCI_AggregateDensity)%acquired = .false.
            input_tag(SCI_AggregateStability)%acquired = .false.
            input_tag(SCI_BulkDensity)%acquired = .false.
            input_tag(SCI_WaterContent)%acquired = .false.
            input_tag(SCI_WiltingPoint)%acquired = .false.
            soillay_complete(isl) = .true.
          else
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_LayerThickness, &
                    SCI_Sand, &
                    SCI_Silt, &
                    SCI_Clay, &
                    SCI_RockVolume, &
                    SCI_VeryFineSand, &
                    SCI_AggregateGMD, &
                    SCI_AggregateGSD, &
                    SCI_AggregateMAX, &
                    SCI_AggregateMIN, &
                    SCI_AggregateDensity, &
                    SCI_AggregateStability, &
                    SCI_BulkDensity, &
                    SCI_WaterContent, &
                    SCI_WiltingPoint)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,*) 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if


        end if

        exit  ! found tag, no need to look further

      end if
    end do

  end subroutine end_soilstate_element_handler

  subroutine pcdata_soilstate_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value

    param_value = trim(chunk)

    if (input_tag(SoilState)%in_tag) then
      if (input_tag(SCI_SoilLays)%in_tag) then
        if (input_tag(SCI_SoilLay)%in_tag) then
          if (input_tag(SCI_LayerThickness)%in_tag) then
            call read_param(input_tag(SCI_LayerThickness)%name, param_value, subrsurf(1,isr)%bsl(isl)%aszlyt)
            input_tag(SCI_LayerThickness)%acquired = .true.
          else if (input_tag(SCI_Sand)%in_tag) then
            call read_param(input_tag(SCI_Sand)%name, param_value, subrsurf(1,isr)%bsl(isl)%asfsan)
            input_tag(SCI_Sand)%acquired = .true.
          else if (input_tag(SCI_Silt)%in_tag) then
            call read_param(input_tag(SCI_Silt)%name, param_value, subrsurf(1,isr)%bsl(isl)%asfsil)
            input_tag(SCI_Silt)%acquired = .true.
          else if (input_tag(SCI_Clay)%in_tag) then
            call read_param(input_tag(SCI_Clay)%name, param_value, subrsurf(1,isr)%bsl(isl)%asfcla)
            input_tag(SCI_Clay)%acquired = .true.
          else if (input_tag(SCI_RockVolume)%in_tag) then
            call read_param(input_tag(SCI_RockVolume)%name, param_value, subrsurf(1,isr)%bsl(isl)%asvroc)
            input_tag(SCI_RockVolume)%acquired = .true.
          else if (input_tag(SCI_VeryFineSand)%in_tag) then
            call read_param(input_tag(SCI_VeryFineSand)%name, param_value, subrsurf(1,isr)%bsl(isl)%asfvfs)
            input_tag(SCI_VeryFineSand)%acquired = .true.
          else if (input_tag(SCI_AggregateGMD)%in_tag) then
            call read_param(input_tag(SCI_AggregateGMD)%name, param_value, subrsurf(1,isr)%bsl(isl)%aslagm)
            input_tag(SCI_AggregateGMD)%acquired = .true.
          else if (input_tag(SCI_AggregateGSD)%in_tag) then
            call read_param(input_tag(SCI_AggregateGSD)%name, param_value, subrsurf(1,isr)%bsl(isl)%as0ags)
            input_tag(SCI_AggregateGSD)%acquired = .true.
          else if (input_tag(SCI_AggregateMAX)%in_tag) then
            call read_param(input_tag(SCI_AggregateMAX)%name, param_value, subrsurf(1,isr)%bsl(isl)%aslagx)
            input_tag(SCI_AggregateMAX)%acquired = .true.
          else if (input_tag(SCI_AggregateMIN)%in_tag) then
            call read_param(input_tag(SCI_AggregateMIN)%name, param_value, subrsurf(1,isr)%bsl(isl)%aslagn)
            input_tag(SCI_AggregateMIN)%acquired = .true.
          else if (input_tag(SCI_AggregateDensity)%in_tag) then
            call read_param(input_tag(SCI_AggregateDensity)%name, param_value, subrsurf(1,isr)%bsl(isl)%asdagd)
            input_tag(SCI_AggregateDensity)%acquired = .true.
          else if (input_tag(SCI_AggregateStability)%in_tag) then
            call read_param(input_tag(SCI_AggregateStability)%name, param_value, subrsurf(1,isr)%bsl(isl)%aseags)
            input_tag(SCI_AggregateStability)%acquired = .true.
          else if (input_tag(SCI_BulkDensity)%in_tag) then
            call read_param(input_tag(SCI_BulkDensity)%name, param_value, subrsurf(1,isr)%bsl(isl)%asdblk)
            input_tag(SCI_BulkDensity)%acquired = .true.
          else if (input_tag(SCI_WaterContent)%in_tag) then
            call read_param(input_tag(SCI_WaterContent)%name, param_value, subrsurf(1,isr)%bsl(isl)%ahrwca)
            input_tag(SCI_WaterContent)%acquired = .true.
          else if (input_tag(SCI_WiltingPoint)%in_tag) then
            call read_param(input_tag(SCI_WiltingPoint)%name, param_value, subrsurf(1,isr)%bsl(isl)%ahrwcw)
            input_tag(SCI_WiltingPoint)%acquired = .true.
          end if
        end if

      end if

    end if

  end subroutine pcdata_soilstate_chunk_handler

  subroutine begin_treatment_element_handler(name,attributes)
    character(len=*), intent(in)   :: name
    type(dictionary_t), intent(in) :: attributes

    integer :: idx
    character(len=80) :: param_value
    integer :: ret_stat
    integer :: alloc_stat

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

    if (   (idx .eq. SCI_brcdInputs) &
      .or. (idx .eq. SCI_SurfaceSubDayWaters) ) then
      if ( has_key(attributes, input_tag(SCI_number)%name) ) then
        call get_value(attributes, input_tag(SCI_number)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_brcdInputs)
          call read_param(input_tag(SCI_number)%name, param_value, npools)
          !write(*,*) 'Number of Biomass Pools: ', npools
          allocate(brcdinput_complete(npools), stat=alloc_stat)
          if( alloc_stat .gt. 0 ) then
            write(*,*) 'ERROR: memory allocation, brcdinput_complete'
          end if
          ! initialize _complete arrays to false
          do idx = 1, npools
            brcdinput_complete(idx) = .false.
          end do
          ! create brcdInput arrays
          subrsurf(1,isr)%npools = npools
          call create_brcdinputpools( npools, subrsurf(1,isr) )

        case (SCI_SurfaceSubDayWaters)
          call read_param(input_tag(SCI_number)%name, param_value, subrsurf(1,isr)%nswet)
          !write(*,*) 'Number of Surface Water Sub Day values: ', subrsurf(1,isr)%nswet
            allocate(surfwat_complete(subrsurf(1,isr)%nswet), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory allocation, surfwat_complete'
              call exit(1)
            end if
            call create_subregionsurfacewet(subrsurf(1,isr)%nswet, subrsurf(1,isr))
            ! initialize _complete arrays to .false.
            do idx = 1, subrsurf(1,isr)%nswet
              surfwat_complete(idx) = .false.
            end do

        end select
      else
        write(*,*) 'SCI_number attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
    else if ( (idx .eq. SCI_brcdInput) &
      .or. (idx .eq. SCI_SurfaceSubDayWater) ) then
      if ( has_key(attributes, input_tag(SCI_index)%name) ) then
        call get_value(attributes, input_tag(SCI_index)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_brcdInput)
          call read_param(input_tag(SCI_index)%name, param_value, ipool)
          ! adjust from base 0 to base 1 arrays
          ipool = ipool + 1
          !write(*,*) 'Soil Layer Index: ', isl
        case (SCI_SurfaceSubDayWater)
          call read_param(input_tag(SCI_index)%name, param_value, isurfwat)
          ! adjust from base 0 to base 1 arrays
          isurfwat = isurfwat + 1
          !write(*,*) 'Surface Water Index: ', isurfwat
        end select
      else
        write(*,*) 'SCI_index attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if

    end if

  end subroutine begin_treatment_element_handler

  subroutine end_treatment_element_handler(name)
    character(len=*), intent(in)     :: name

    integer :: idx
    integer :: jdx
    integer :: dealloc_stat

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .false.
        ! write(*,*) 'In tag ', trim(name)

        if (idx .eq. TreatmentData) then
          ! check for acquisition of all required elements
          if (    input_tag(SCI_brcdInputs)%acquired &
            .and. input_tag(SCI_BiomassFlatCover)%acquired &
            .and. input_tag(SCI_CrustThick)%acquired &
            .and. input_tag(SCI_CrustDensity)%acquired &
            .and. input_tag(SCI_CrustStability)%acquired &
            .and. input_tag(SCI_CrustCover)%acquired &
            .and. input_tag(SCI_CrustMassCoverLoose)%acquired &
            .and. input_tag(SCI_CrustFracCoverLoose)%acquired &
            .and. input_tag(SCI_RandomRoughness)%acquired &
            .and. input_tag(SCI_RidgeOrientation)%acquired &
            .and. input_tag(SCI_RidgeHeight)%acquired &
            .and. input_tag(SCI_RidgeSpacing)%acquired &
            .and. input_tag(SCI_RidgeWidth)%acquired &
            .and. input_tag(SCI_DikeSpacing)%acquired &
            .and. input_tag(SCI_SnowDepth)%acquired &
            .and. input_tag(SCI_SurfaceSubDayWaters)%acquired &
            ) then
            input_tag(SCI_brcdInputs)%acquired = .false.
            input_tag(SCI_BiomassFlatCover)%acquired = .false.
            input_tag(SCI_CrustThick)%acquired = .false.
            input_tag(SCI_CrustDensity)%acquired = .false.
            input_tag(SCI_CrustStability)%acquired = .false.
            input_tag(SCI_CrustCover)%acquired = .false.
            input_tag(SCI_CrustMassCoverLoose)%acquired = .false.
            input_tag(SCI_CrustFracCoverLoose)%acquired = .false.
            input_tag(SCI_RandomRoughness)%acquired = .false.
            input_tag(SCI_RidgeOrientation)%acquired = .false.
            input_tag(SCI_RidgeHeight)%acquired = .false.
            input_tag(SCI_RidgeSpacing)%acquired = .false.
            input_tag(SCI_RidgeWidth)%acquired = .false.
            input_tag(SCI_DikeSpacing)%acquired = .false.
            input_tag(SCI_SnowDepth)%acquired = .false.
            input_tag(SCI_SurfaceSubDayWaters)%acquired = .false.
            treatmentdata_complete(isr) = .true.

            !write(*,*) 'TreatmentData complete'
          else
            !write(*,*) 'TreatmentData NOT complete'
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_BiomassFlatCover, &
                    SCI_CrustThick, &      
                    SCI_CrustDensity, &      
                    SCI_CrustStability, &      
                    SCI_CrustCover, &      
                    SCI_CrustMassCoverLoose, &      
                    SCI_CrustFracCoverLoose, &      
                    SCI_RandomRoughness, &      
                    SCI_RidgeOrientation, &      
                    SCI_RidgeHeight, &      
                    SCI_RidgeSpacing, &      
                    SCI_RidgeWidth, &      
                    SCI_DikeSpacing, &      
                    SCI_SnowDepth)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_brcdInputs, &
                    SCI_SurfaceSubDayWaters)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is incomplete or missing from input file.'
                end if
              end select
            end do
          end if
          ! deallocate Tag array
          deallocate( input_tag, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for Tag array"
          end if

        else if (idx .eq. SCI_brcdInputs) then
          count_complete = 0
          do jdx = 1, npools
            if (brcdinput_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. npools) then
            input_tag(SCI_brcdInputs)%acquired = .true.

            ! assign values to subrsurf variables based on brcdInputs
            ! sum the stem area index and leaf area index values
            subrsurf(1,isr)%abrsai = 0.0
            do jdx = 1, npools
              subrsurf(1,isr)%abrsai = subrsurf(1,isr)%abrsai + subrsurf(1,isr)%brcdInput(jdx)%rsai
              subrsurf(1,isr)%abrlai = subrsurf(1,isr)%abrlai + subrsurf(1,isr)%brcdInput(jdx)%rlai 
            end do

            ! Compute the weighted average "biomass height" (residues and crop)
            subrsurf(1,isr)%abzht = 0.0
            if (subrsurf(1,isr)%abrsai .gt. 0.0) then
              do jdx = 1, npools
                subrsurf(1,isr)%abzht = &
                              subrsurf(1,isr)%abzht + (subrsurf(1,isr)%brcdInput(jdx)%zht * subrsurf(1,isr)%brcdInput(jdx)%rsai)
              end do
              subrsurf(1,isr)%abzht = subrsurf(1,isr)%abzht / subrsurf(1,isr)%abrsai
            endif

          else
            do jdx = 1, size(brcdinput_complete)
             if ( .not. brcdinput_complete(jdx)) then
                 write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_brcdInput)%name), &
                                      ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(brcdinput_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for brcdinput_complete array"
          end if

        else if (idx .eq. SCI_SurfaceSubDayWaters) then
          count_complete = 0
          do jdx = 1, subrsurf(1,isr)%nswet
            if (surfwat_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. subrsurf(1,isr)%nswet) then
            input_tag(SCI_SurfaceSubDayWaters)%acquired = .true.
          else
            do jdx = 1, subrsurf(1,isr)%nswet
              if ( .not. surfwat_complete(jdx)) then
                 write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_SurfaceSubDayWater)%name), &
                                      ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(surfwat_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory surfwat_complete array"
          end if

        else if (idx .eq. SCI_brcdInput) then
          ! check for acquisition of all required elements
          if (    input_tag(SCI_brcdBname)%acquired &
            .and. input_tag(SCI_brcdRlai)%acquired &
            .and. input_tag(SCI_brcdRsai)%acquired &
            .and. input_tag(SCI_brcdRg)%acquired &
            .and. input_tag(SCI_brcdXrow)%acquired &
            .and. input_tag(SCI_brcdZht)%acquired ) then
            input_tag(SCI_brcdBname)%acquired = .false.
            input_tag(SCI_brcdRlai)%acquired = .false.
            input_tag(SCI_brcdRsai)%acquired = .false.
            input_tag(SCI_brcdRg)%acquired = .false.
            input_tag(SCI_brcdXrow)%acquired = .false.
            input_tag(SCI_brcdZht)%acquired = .false.
            brcdinput_complete(ipool) = .true.
          else
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_brcdBname, & 
                    SCI_brcdRlai, & 
                    SCI_brcdRsai, &
                    SCI_brcdRg, &
                    SCI_brcdXrow, &
                    SCI_brcdZht)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,*) 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        end if

        exit  ! found tag, no need to look further

      end if
    end do

  end subroutine end_treatment_element_handler

  subroutine pcdata_treatment_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value

    param_value = trim(chunk)

    if (input_tag(TreatmentData)%in_tag) then
      if (input_tag(SCI_brcdInputs)%in_tag) then
        if (input_tag(SCI_brcdInput)%in_tag) then
          if (input_tag(SCI_brcdBname)%in_tag) then
            call read_param(input_tag(SCI_brcdBname)%name, param_value, subrsurf(1,isr)%brcdInput(ipool)%bname)
            input_tag(SCI_brcdBname)%acquired = .true.
          else if (input_tag(SCI_brcdRlai)%in_tag) then
            call read_param(input_tag(SCI_brcdRlai)%name, param_value, subrsurf(1,isr)%brcdInput(ipool)%rlai)
            input_tag(SCI_brcdRlai)%acquired = .true.
          else if (input_tag(SCI_brcdRsai)%in_tag) then
            call read_param(input_tag(SCI_brcdRsai)%name, param_value, subrsurf(1,isr)%brcdInput(ipool)%rsai)
            input_tag(SCI_brcdRsai)%acquired = .true.
          else if (input_tag(SCI_brcdRg)%in_tag) then
            call read_param(input_tag(SCI_brcdRg)%name, param_value, subrsurf(1,isr)%brcdInput(ipool)%rg)
            input_tag(SCI_brcdRg)%acquired = .true.
          else if (input_tag(SCI_brcdXrow)%in_tag) then
            call read_param(input_tag(SCI_brcdXrow)%name, param_value, subrsurf(1,isr)%brcdInput(ipool)%xrow)
            input_tag(SCI_brcdXrow)%acquired = .true.
          else if (input_tag(SCI_brcdZht)%in_tag) then
            call read_param(input_tag(SCI_brcdZht)%name, param_value, subrsurf(1,isr)%brcdInput(ipool)%zht)
            input_tag(SCI_brcdZht)%acquired = .true.
          end if
        end if

      else if (input_tag(SCI_BiomassFlatCover)%in_tag) then
        call read_param(input_tag(SCI_BiomassFlatCover)%name, param_value, subrsurf(1,isr)%abffcv)
        input_tag(SCI_BiomassFlatCover)%acquired = .true.

      else if (input_tag(SCI_CrustThick)%in_tag) then
        call read_param(input_tag(SCI_CrustThick)%name, param_value, subrsurf(1,isr)%aszcr)
        input_tag(SCI_CrustThick)%acquired = .true.

      else if (input_tag(SCI_CrustDensity)%in_tag) then
        call read_param(input_tag(SCI_CrustDensity)%name, param_value, subrsurf(1,isr)%asdcr)
        input_tag(SCI_CrustDensity)%acquired = .true.

      else if (input_tag(SCI_CrustStability)%in_tag) then
        call read_param(input_tag(SCI_CrustStability)%name, param_value, subrsurf(1,isr)%asecr)
        input_tag(SCI_CrustStability)%acquired = .true.

      else if (input_tag(SCI_CrustCover)%in_tag) then
        call read_param(input_tag(SCI_CrustCover)%name, param_value, subrsurf(1,isr)%asfcr)
        input_tag(SCI_CrustCover)%acquired = .true.

      else if (input_tag(SCI_CrustMassCoverLoose)%in_tag) then
        call read_param(input_tag(SCI_CrustMassCoverLoose)%name, param_value, subrsurf(1,isr)%asmlos)
        input_tag(SCI_CrustMassCoverLoose)%acquired = .true.

      else if (input_tag(SCI_CrustFracCoverLoose)%in_tag) then
        call read_param(input_tag(SCI_CrustFracCoverLoose)%name, param_value, subrsurf(1,isr)%asflos)
        input_tag(SCI_CrustFracCoverLoose)%acquired = .true.

      else if (input_tag(SCI_RandomRoughness)%in_tag) then
        call read_param(input_tag(SCI_RandomRoughness)%name, param_value, subrsurf(1,isr)%aslrr)
        input_tag(SCI_RandomRoughness)%acquired = .true.

        !Lower and upper limits of grid cell RR allowed by erosion submodel
        if (subrsurf(1,isr)%aslrr < SLRR_MIN) then
          write(0,*) 'slrr: ', subrsurf(1,isr)%aslrr,' < ', SLRR_MIN
        end if
        if (subrsurf(1,isr)%aslrr > SLRR_MAX) then
          write(0,*) 'slrr: ', subrsurf(1,isr)%aslrr,' < ', SLRR_MIN
        end if

        !Lower and upper limits of grid cell aerodynamic roughness allowed
        !by erosion submodel (currently determined by equation used here)
        if (subrsurf(1,isr)%aslrr < (WZZO_MIN/0.3)) then
          write(0,*) 'slrr: ', subrsurf(1,isr)%aslrr
          write(0,*) 'wzzo < WZZO_MIN: ', subrsurf(1,isr)%aslrr*0.3,' < ', WZZO_MIN
        else if(subrsurf(1,isr)%aslrr > (WZZO_MAX/0.3)) then
          write(0,*) 'slrr: ', subrsurf(1,isr)%aslrr
          write(0,*) 'wzzo > WZZO_MAX: ', subrsurf(1,isr)%aslrr*0.3,' > ', WZZO_MAX
        end if

      else if (input_tag(SCI_RidgeOrientation)%in_tag) then
        call read_param(input_tag(SCI_RidgeOrientation)%name, param_value, subrsurf(1,isr)%asargo)
        input_tag(SCI_RidgeOrientation)%acquired = .true.

      else if (input_tag(SCI_RidgeHeight)%in_tag) then
        call read_param(input_tag(SCI_RidgeHeight)%name, param_value, subrsurf(1,isr)%aszrgh)
        input_tag(SCI_RidgeHeight)%acquired = .true.

      else if (input_tag(SCI_RidgeSpacing)%in_tag) then
        call read_param(input_tag(SCI_RidgeSpacing)%name, param_value, subrsurf(1,isr)%asxrgs)
        input_tag(SCI_RidgeSpacing)%acquired = .true.

      else if (input_tag(SCI_RidgeWidth)%in_tag) then
        call read_param(input_tag(SCI_RidgeWidth)%name, param_value, subrsurf(1,isr)%asxrgw)
        input_tag(SCI_RidgeWidth)%acquired = .true.

      else if (input_tag(SCI_DikeSpacing)%in_tag) then
        call read_param(input_tag(SCI_DikeSpacing)%name, param_value, subrsurf(1,isr)%asxdks)
        input_tag(SCI_DikeSpacing)%acquired = .true.

      else if (input_tag(SCI_SnowDepth)%in_tag) then
        call read_param(input_tag(SCI_SnowDepth)%name, param_value, subrsurf(1,isr)%ahzsnd)
        input_tag(SCI_SnowDepth)%acquired = .true.

      else if (input_tag(SCI_SurfaceSubDayWaters)%in_tag) then
        if (input_tag(SCI_SurfaceSubDayWater)%in_tag) then
          call read_param(input_tag(SCI_SurfaceSubDayWater)%name, param_value, subrsurf(1,isr)%ahrwc0(isurfwat))
          surfwat_complete(isurfwat) = .true.
        end if
      end if

    end if

  end subroutine pcdata_treatment_chunk_handler

end module sweep_io_xml_mod
