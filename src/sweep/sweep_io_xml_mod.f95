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
  use erosion_data_struct_defs, only: subregionsurfacestate, create_subregionsoillayers, create_subregionsurfacewet, &
                                          awzypt, awdair, anemht, awzzo, wzoflg, &
                                          ntstep, awadir, awudmx, subday, am0eif, subrsurf
  use p1erode_def, only: SLRR_MIN, SLRR_MAX, WZZO_MIN, WZZO_MAX
  use barriers_mod, only: create_barrier, barrier, barseas
  use barriers_mod, only: barrier_day_state, barrier_params, barrier_climate
  use grid_mod, only: amasim, amxsim, xgdpt, ygdpt
  use sae_in_out_mod, only: saeinp
 
  interface read_param
    module procedure read_param_real_1
    module procedure read_param_real_2
    module procedure read_param_int_1
    module procedure read_param_int_2
    module procedure read_param_int_3
  end interface

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
  logical :: accounts_present = .false.
  logical, dimension(:), allocatable :: account_complete
  logical, dimension(:), allocatable :: subregion_complete
  logical, dimension(:), allocatable :: coord_complete
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

contains

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

    do idx = 1, size(input_tag)
      if( input_tag(idx)%name .eq. name ) then
        input_tag(idx)%in_tag = .true.
        ! write(*,*) 'In tag ', trim(name)
        exit  ! found tag, no need to look further
      end if
    end do

    if (   (idx .eq. SCI_Subregions) &
      .or. (idx .eq. SCI_coords) &
      .or. (idx .eq. SCI_SoilLays) &
      .or. (idx .eq. SCI_SurfaceSubDayWaters) &
      .or. (idx .eq. SCI_Accounts) &
      .or. (idx .eq. SCI_Barriers) &
      .or. (idx .eq. SCI_WindSpeeds) ) then
      if ( has_key(attributes, input_tag(SCI_number)%name) ) then
        call get_value(attributes, input_tag(SCI_number)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregions)
          call read_param(SCI_number, param_value, nsubr)
          !write(*,*) 'Number of Subregions: ', nsubr
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

        case (SCI_coords)
          call read_param(SCI_number, param_value, poly_np)
          ! write(*,*) 'Number of Subregion Coordinate Points: ', poly_np
          if (poly_np .ge. 3) then
            if ( input_tag(SCI_Subregion)%in_tag ) then
              ! create polygon point storage
              subr_poly(isr) = create_polygon(poly_np)
            else if ( input_tag(SCI_Account)%in_tag ) then
              ! create polygon point storage
              acct_poly(iar) = create_polygon(poly_np)
            end if
            ! create storage for points index tracking
            allocate(coord_complete(poly_np), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              ! allocation failed
              write(*,*) "ERROR: unable to allocate memory for coord_complete array"
            end if
            ! initialize _complete arrays to false
            do idx = 1, poly_np
              coord_complete(idx) = .false.
            end do
          else
            write(*,*) 'Subregion or Account Coordinate polygons must have at least 3 points. Only has ', poly_np
            call exit(1)
          end if

        case (SCI_SoilLays)
          call read_param(SCI_number, param_value, subrsurf(isr)%nslay)
          !write(*,*) 'Number of Soil Layers: ', subrsurf(isr)%nslay
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

        case (SCI_SurfaceSubDayWaters)
          call read_param(SCI_number, param_value, subrsurf(isr)%nswet)
          !write(*,*) 'Number of Surface Water Sub Day values: ', subrsurf(isr)%nswet
            allocate(surfwat_complete(subrsurf(isr)%nswet), stat=alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory allocation, surfwat_complete'
              call exit(1)
            end if
            call create_subregionsurfacewet(subrsurf(isr)%nswet, subrsurf(isr))
            ! initialize _complete arrays to .false.
            do idx = 1, subrsurf(isr)%nswet
              surfwat_complete(idx) = .false.
            end do

        case (SCI_Accounts)
          call read_param(SCI_number, param_value, nacctr)
          !write(*,*) 'Number of Accounting Regions: ', nacctr
          ! allocate structure for accounting regions (nacctr .lt. 1 gives zero size array)
          sum_stat = 0
          allocate(acct_poly(nacctr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          allocate(account_complete(nacctr), stat = alloc_stat)
          sum_stat = sum_stat + alloc_stat
          if( sum_stat .gt. 0 ) then
            write(*,*) 'ERROR: memory alloc., accounting region arrays'
          end if
          ! initialize _complete arrays to .false.
          do idx = 1, nacctr
            account_complete(idx) = .false.
          end do
          accounts_present = .true.

        case (SCI_Barriers)
          call read_param(SCI_number, param_value, nbr)
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

        case (SCI_WindSpeeds)
          call read_param(SCI_number, param_value, ntstep)
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
      .or. (idx .eq. SCI_Account) &
      .or. (idx .eq. SCI_coord) &
      .or. (idx .eq. SCI_SoilLay) &
      .or. (idx .eq. SCI_SurfaceSubDayWater) &
      .or. (idx .eq. SCI_BarPoint) &
      .or. (idx .eq. SCI_WindSpeed) ) then
      if ( has_key(attributes, input_tag(SCI_index)%name) ) then
        call get_value(attributes, input_tag(SCI_index)%name, param_value, ret_stat)
        select case (idx)
        case (SCI_Subregion)
          call read_param(SCI_index, param_value, isr)
          ! adjust from base 0 to base 1 arrays
          isr = isr + 1
          !write(*,*) 'Subregion Index: ', isr
        case (SCI_coord)
          call read_param(SCI_index, param_value, ipol)
          ! adjust from base 0 to base 1 arrays
          ipol = ipol + 1
          !write(*,*) 'Subregion Coordinates Point Index: ', ipol
        case (SCI_SoilLay)
          call read_param(SCI_index, param_value, isl)
          ! adjust from base 0 to base 1 arrays
          isl = isl + 1
          !write(*,*) 'Soil Layer Index: ', isl
        case (SCI_SurfaceSubDayWater)
          call read_param(SCI_index, param_value, isurfwat)
          ! adjust from base 0 to base 1 arrays
          isurfwat = isurfwat + 1
          !write(*,*) 'Surface Water Index: ', isurfwat
        case (SCI_Account)
          call read_param(SCI_index, param_value, iar)
          ! adjust from base 0 to base 1 arrays
          iar = iar + 1
          !write(*,*) 'Accounting region Index: ', iar
        case (SCI_BarPoint)
          call read_param(SCI_index, param_value, ipol)
          ! adjust from base 0 to base 1 arrays
          ipol = ipol + 1
          !write(*,*) 'Barrier Point Index: ', ipol
        case (SCI_WindSpeed)
          call read_param(SCI_index, param_value, iwind)
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
        call read_param(SCI_index, param_value, ibr)
        ! adjust from base 0 to base 1 arrays
        ibr = ibr + 1
        !write(*,*) 'Barrier Index: ', ibr
      else
        write(*,*) 'SCI_index attribute required for each ', trim(input_tag(idx)%name), ' Tag.'
        call exit(1)
      end if
      if ( has_key(attributes, input_tag(SCI_number)%name) ) then
        call get_value(attributes, input_tag(SCI_number)%name, param_value, ret_stat)
        call read_param(SCI_number, param_value, poly_np)
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

  end subroutine begin_element_handler

  subroutine end_element_handler(name)
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
          if ( .not. accounts_present) then
            ! no SCI_Accounts tag found (not needed so set to true)
            input_tag(SCI_Accounts)%acquired = .true.
            ! allocate structure for accounting regions (zero size array allowed)
            allocate(acct_poly(0), stat = alloc_stat)
            if( alloc_stat .gt. 0 ) then
              write(*,*) 'ERROR: memory alloc., accounting region arrays'
            end if
          end if

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
            .and. input_tag(SCI_XGrid)%acquired &
            .and. input_tag(SCI_YGrid)%acquired &
            .and. input_tag(SCI_AirDensity)%acquired &
            .and. input_tag(SCI_WindDirection)%acquired &
            .and. input_tag(SCI_AnemometerHeight)%acquired &
            .and. input_tag(SCI_AerodynamicRoughness)%acquired &
            .and. input_tag(SCI_AnemometerFlag)%acquired &
            .and. input_tag(SCI_AverageAnnualPrecipitation)%acquired &
            .and. input_tag(SCI_Subregions)%acquired &
            .and. input_tag(SCI_WindSpeeds)%acquired &
            .and. input_tag(SCI_Accounts)%acquired &
            .and. input_tag(SCI_Barriers)%acquired &
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
                    SCI_XGrid, &
                    SCI_YGrid, &
                    SCI_AirDensity, &
                    SCI_WindDirection, &
                    SCI_AnemometerHeight, &
                    SCI_AerodynamicRoughness, &
                    SCI_AnemometerFlag, &
                    SCI_AverageAnnualPrecipitation)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_Subregions, &
                    SCI_WindSpeeds, &
                    SCI_Accounts, &
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
            write(*,*) "ERROR: unable to deallocate memory for tags and subregion_complete arrays"
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
            write(*,*) "ERROR: unable to deallocate memory for tags and wind_complete array"
          end if

        else if (idx .eq. SCI_Subregion) then
          ! check for acquisition of all required elements
          if (    input_tag(SCI_coords)%acquired &
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
            .and. input_tag(SCI_SoilLays)%acquired &
            .and. input_tag(SCI_SurfaceSubDayWaters)%acquired &
            ) then
            input_tag(SCI_coords)%acquired = .false.
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
            input_tag(SCI_SoilLays)%acquired = .false.
            input_tag(SCI_SurfaceSubDayWaters)%acquired = .false.
            subregion_complete(isr) = .true.

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
            !write(*,*) 'SCI_Subregion complete'
          else
            !write(*,*) 'SCI_Subregion NOT complete'
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_ResidueHeight, &      
                    SCI_CropHeight, &      
                    SCI_CropSAI, &      
                    SCI_CropLAI, &      
                    SCI_ResidueSAI, &      
                    SCI_ResidueLAI, &      
                    SCI_CropRowSpacing, &      
                    SCI_CropSeedPlace, &      
                    SCI_BiomassFlatCover, &      
                    SCI_CrustCover, &      
                    SCI_CrustThick, &      
                    SCI_CrustFracCoverLoose, &      
                    SCI_CrustMassCoverLoose, &      
                    SCI_CrustDensity, &      
                    SCI_CrustStability, &      
                    SCI_RandomRoughness, &      
                    SCI_RidgeHeight, &      
                    SCI_RidgeSpacing, &      
                    SCI_RidgeWidth, &      
                    SCI_RidgeOrientation, &      
                    SCI_DikeSpacing, &      
                    SCI_SnowDepth)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              case (SCI_SoilLays, &
                    SCI_SurfaceSubDayWaters)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(SCI_SoilLay)%name), ' is incomplete or missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_coords) then
          count_complete = 0
          do jdx = 1, poly_np
            if (coord_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. poly_np) then
            input_tag(SCI_coords)%acquired = .true.
            ! polygon complete, set area
            if ( input_tag(SCI_Subregion)%in_tag ) then
              call set_area_polygon(subr_poly(isr))
            else if ( input_tag(SCI_Account)%in_tag ) then
              call set_area_polygon(acct_poly(iar))
            end if
          else
            do jdx = 1, poly_np
              if ( .not. coord_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_coord)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(coord_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for tags and coord_complete array"
          end if

        else if (idx .eq. SCI_SoilLays) then
          count_complete = 0
          do jdx = 1, subrsurf(isr)%nslay
            if (soillay_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. subrsurf(isr)%nslay) then
            input_tag(SCI_SoilLays)%acquired = .true.
          else
            do jdx = 1, size(soillay_complete)
              if ( .not. soillay_complete(jdx)) then
                 write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_SoilLay)%name), &
                                      ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(soillay_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for tags and soillay_complete array"
          end if

        else if (idx .eq. SCI_SurfaceSubDayWaters) then
          count_complete = 0
          do jdx = 1, subrsurf(isr)%nswet
            if (surfwat_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. subrsurf(isr)%nswet) then
            input_tag(SCI_SurfaceSubDayWaters)%acquired = .true.
          else
            do jdx = 1, subrsurf(isr)%nswet
              if ( .not. surfwat_complete(jdx)) then
                 write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_SoilLay)%name), &
                                      ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete array
          deallocate(surfwat_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for tags and surfwat_complete array"
          end if

        else if (idx .eq. SCI_coord) then
          ! check for acquisition of all required elements
          if (    input_tag(SCI_x)%acquired &
            .and. input_tag(SCI_y)%acquired &
            ) then 
            input_tag(SCI_x)%acquired = .false.
            input_tag(SCI_y)%acquired = .false.
            coord_complete(ipol) = .true.
          else
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_x, &
                    SCI_y)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if
        else if (idx .eq. SCI_SoilLay) then
          ! check for acquisition of all required elements
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
          else
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_LayerThickness, &
                    SCI_BulkDensity, &
                    SCI_Sand, &
                    SCI_Silt, &
                    SCI_Clay, &
                    SCI_VeryFineSand, &
                    SCI_RockVolume, &
                    SCI_AggregateDensity, &
                    SCI_AggregateStability, &
                    SCI_AggregateGMD, &
                    SCI_AggregateGSD, &
                    SCI_AggregateMIN, &
                    SCI_AggregateMAX, &
                    SCI_WiltingPoint, &
                    SCI_WaterContent)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,*) 'Tag ', trim(input_tag(jdx)%name), ' is missing from input file.'
                end if
              end select
            end do
          end if

        else if (idx .eq. SCI_Accounts) then
          count_complete = 0
          do jdx = 1, nacctr
            if (account_complete(jdx)) then
              count_complete = count_complete + 1
            end if
          end do
          if (count_complete .ge. nacctr) then
            input_tag(SCI_Accounts)%acquired = .true.
          else
            do jdx = 1, size(account_complete)
              if ( .not. account_complete(jdx)) then
                write(*,'(3a,i0,a)') 'Tag ', trim(input_tag(SCI_Account)%name), &
                                     ' SCI_index="', jdx-1, '" is incomplete or missing from input file.'
              end if
            end do
          end if
          ! deallocate _complete arrays
          deallocate(account_complete, stat=dealloc_stat)
          if( dealloc_stat .gt. 0 ) then
            ! deallocation failed
            write(*,*) "ERROR: unable to deallocate memory for account_complete array"
          end if

        else if (idx .eq. SCI_Account) then
          ! check for acquisition of all required elements
          if ( input_tag(SCI_coords)%acquired ) then 
            input_tag(SCI_coords)%acquired = .false.
            account_complete(iar) = .true.
          else
            do jdx = 1, size(input_tag)
              select case (jdx)
              case (SCI_coords)
                if( .not. input_tag(jdx)%acquired ) then
                  write(*,'(3a)') 'Tag ', trim(input_tag(jdx)%name), ' is incomplete or missing from input file.'
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

  end subroutine end_element_handler

  subroutine pcdata_chunk_handler(chunk)
    character(len=*), intent(in) :: chunk

    character(len=80) :: param_value

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

      else if (input_tag(SCI_Accounts)%in_tag) then
        if (input_tag(SCI_Account)%in_tag) then
          if (input_tag(SCI_coords)%in_tag) then
            if (input_tag(SCI_coord)%in_tag) then
              !SCI_coord
              if (input_tag(SCI_x)%in_tag) then
                call read_param(SCI_x, param_value, acct_poly(iar)%points(ipol)%x)
                input_tag(SCI_x)%acquired = .true.
              else if (input_tag(SCI_y)%in_tag) then
                call read_param(SCI_y, param_value, acct_poly(iar)%points(ipol)%y)
                input_tag(SCI_y)%acquired = .true.
              end if
            end if
          end if
        end if

      else if (input_tag(SCI_Subregions)%in_tag) then
        if (input_tag(SCI_Subregion)%in_tag) then
          ! SCI_Subregion
          if (input_tag(SCI_coords)%in_tag) then
            if (input_tag(SCI_coord)%in_tag) then
              !SCI_coord
              if (input_tag(SCI_x)%in_tag) then
                call read_param(SCI_x, param_value, subr_poly(isr)%points(ipol)%x)
                input_tag(SCI_x)%acquired = .true.
              else if (input_tag(SCI_y)%in_tag) then
                call read_param(SCI_y, param_value, subr_poly(isr)%points(ipol)%y)
                input_tag(SCI_y)%acquired = .true.
              end if
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

          else if (input_tag(SCI_SoilLays)%in_tag) then
            if (input_tag(SCI_SoilLay)%in_tag) then
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
            end if

          else if (input_tag(SCI_SurfaceSubDayWaters)%in_tag) then
            if (input_tag(SCI_SurfaceSubDayWater)%in_tag) then
              call read_param(SCI_SurfaceSubDayWater, param_value, subrsurf(isr)%ahrwc0(isurfwat))
              surfwat_complete(isurfwat) = .true.
            end if
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
      else if (input_tag(SCI_WindSpeeds)%in_tag) then
        if (input_tag(SCI_WindSpeed)%in_tag) then
          call read_param(SCI_WindSpeed, param_value, subday(iwind)%awu)
          wind_complete(iwind) = .true.
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
