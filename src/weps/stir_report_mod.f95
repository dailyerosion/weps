!$Author$
!$Date$
!$Revision$
!$HeadURL$

module stir_report_mod

   type stir_operation_vars
      integer phopday            ! day of the operation
      integer phopmon            ! month of the operation
      integer phopyr             ! year of the operation
      character*80 stir_opname   ! operation name from operation input
      character*80 stir_cropname ! crop name associated with this operation if a planting, residue set/add or harvest 
      character*80 stir_fuelname ! fuel name associated with this operation
                                 ! may be blank to indicate use of the default fuel as defined by the interface
      logical phop_skip          ! skip operation flag
                                 !   .false. = do every rotation
                                 !   .true.  = skip all but first instance (also skippped in the stir report)
      integer phop_type          ! operation type flag
                                 !  -1 = not yet initialized
                                 !   0 = not related to setting crop interval
                                 !   1 = planting operation
                                 !   2 = harvest operation
                                 !   3 = termination operation
                                 !   4 = set/add residue operation
                                 !   5 = harvest and terminaton
      real phop_stir             ! STIR value for that operation
      real phop_energy           ! energy value for that operation
      integer crop_num           ! number of crop in the crop rotation cycle, 0 = not yet initialized, 1-n = number of crop
      logical crop_growing       ! .true. if crop planted but not terminated, .false. for operation after termination operation
      integer last_harv          ! is this the last harvest in a crop rotation cycle
                                 ! 0 = not the last harvest
                                 ! 1 = this is the last harvest (ie. end of the cycle)
                                 ! For rotations without any harvests (just consecutive plantings)
                                 ! set this to 1 when next planting occurs indicating termination
   end type stir_operation_vars

   type stir_accumulators
      logical header_not_printed  ! flag to keep from printing multiple headers to stir report file
      integer oper_cnt       ! stir count of operations, used to trigger accumulation
      integer proc_cnt       ! stir count of processes, used in averaging
      real    stir_op_sum    ! stir_op_sum - accumulated stir values, divide by proc count to get average
      real    stir_op_energy ! operation energy value from operation input
      integer phopcnt        ! actual number of (p)lanting and (h)arvest (op)erations tabulated
      integer phopidx        ! operation index, location in the array
      type(stir_operation_vars), dimension(:), allocatable :: phop ! individual values for each planting or harvest operation
   end type stir_accumulators

   type(stir_accumulators), dimension(:), allocatable :: stircum

 contains

    subroutine alloc_stir_accumulators(nsubr)
        integer, intent(in) :: nsubr

        ! local variable
        integer :: alloc_stat  ! allocation status return
        integer :: isr         ! variable to loop through subregions

        ! allocate main array for subregion
        allocate(stircum(nsubr), stat=alloc_stat)
        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: unable to allocate memory for stir accumulators'
           call exit(1)
        else
           do isr = 1, nsubr
              stircum(isr)%header_not_printed = .TRUE.
           end do
        end if
    end subroutine alloc_stir_accumulators

    subroutine create_stir_accumulator(isr, mxphops)
        integer, intent(in) :: isr
        integer, intent(in) :: mxphops  ! maximum number of planting and harvest operations that can be tracked with these arrays

        ! local variable
        integer :: alloc_stat  ! allocation status return

        ! allocate operation arrays for this subregion
        allocate(stircum(isr)%phop(mxphops), stat=alloc_stat)

        if( alloc_stat .gt. 0 ) then
           write(*,*) 'ERROR: unable to allocate memory for stir accumulator, subregion ', isr
        end if
    end subroutine create_stir_accumulator

    subroutine destroy_stir_accumulators(nsubr)
        integer, intent(in) :: nsubr

        ! local variable
        integer :: dealloc_stat  ! allocation status return
        integer :: sum_stat    ! accumulates allocation status results so only one write/exit statement needed
        integer :: isr         ! variable to loop through subregions

        sum_stat = 0
        ! for each subregion, deallocate operation arrays
        do isr = 1, nsubr
           deallocate(stircum(isr)%phop, stat=dealloc_stat)
           sum_stat = sum_stat + dealloc_stat
        end do
        ! deallocate main array
        deallocate(stircum, stat=dealloc_stat)
        sum_stat = sum_stat + dealloc_stat

        if( sum_stat .gt. 0 ) then
           write(*,*) 'ERROR: unable to deallocate memory for stir accumulators'
        end if
    end subroutine destroy_stir_accumulators

    subroutine stir_report( mandate, manFile )

!     + + + MODULES + + +
      use weps_cmdline_parms, only: report_debug, soil_cond
      use stir_soil_texture_mod, only : get_stir_soil_multiplier
      use file_io_mod, only: luostir
      use sci_report_mod, only: scisum
      use manage_data_struct_defs, only: man_file_struct
      use manage_data_struct_mod, only: getManVal
      use mandate_mod, only: opercrop_date, create_mandate    ! Load shared mandate() array

!     + + + ARGUMENT DECLARATIONS + + +
      type (opercrop_date), dimension(:), allocatable :: mandate
      type(man_file_struct) :: manFile

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     end_of_file - flag indicating reaching end of managment file
!     ostir - STIR value assigned as operation level parameter
!     oenergyarea - 

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +
      integer isr
      real ostir, oenergyarea
      real stir_op_avg
      integer idx, jdx
      real :: ospeed, tdepth, fracarea
      integer :: burydistflg
      character*80     cropname
      integer :: croptype
      integer :: killflag
      integer temp_num
      integer temp_idx
      real :: plantpop
      real :: pyieldf
      real :: pstalkf
      real :: rstandf
      real :: storef
      real :: leaff
      real :: stemf
      real :: rootstoref
      real :: rootfiberf
      logical :: pass_2
      integer :: prev_day
      integer :: prev_mon
      integer :: prev_yr

!     + + + LOCAL DEFINITIONS + + +
!     stir_op_avg - average over all burial processes of stir value
!     idx         - index used to search for crop_num

      isr = manFile%isub

      ! count number of management operations
      manFile%oper => manFile%operFirst
      idx = 0
      do while( associated(manFile%oper) )
        idx = idx + 1
        manFile%oper => manFile%oper%operNext
      end do

      ! create mandate array
      call create_mandate( idx, mandate )

      ! allocate tracking array for stir report info
      call create_stir_accumulator(isr, idx)
      call sci_stir_init(isr)

      ! go through management file and populate STIR operation array with operation name,
      ! types, stir and energy values and operation specified crop name.
      cropname = ''
      croptype = 0
      pass_2 = .false.
      prev_day = 0
      prev_mon = 0
      prev_yr = 0
      manFile%oper => manFile%operFirst
      do while( associated(manFile%oper) )

        ! increment index for planting harvest accounting
        stircum(isr)%phopidx = stircum(isr)%phopidx + 1

        if( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .lt. 0 ) then

          ! check for total number, make maximum
          stircum(isr)%phopcnt = max( stircum(isr)%phopcnt, stircum(isr)%phopidx )
          stircum(isr)%oper_cnt = stircum(isr)%oper_cnt + 1
          stircum(isr)%phop(stircum(isr)%phopidx)%phopday = manFile%oper%operDate%day
          stircum(isr)%phop(stircum(isr)%phopidx)%phopmon = manFile%oper%operDate%month
          stircum(isr)%phop(stircum(isr)%phopidx)%phopyr = manFile%oper%operDate%year
          stircum(isr)%phop(stircum(isr)%phopidx)%stir_opname = manFile%oper%operName
          if ( manFile%oper%operType .eq. 0 ) then
            stircum(isr)%phop(stircum(isr)%phopidx)%phop_skip = .true.
          else
            stircum(isr)%phop(stircum(isr)%phopidx)%phop_skip = .false.
          end if

          stircum(isr)%phop(stircum(isr)%phopidx)%stir_fuelname = ''
          oenergyarea = -1
          ostir = -1
          select case ( manFile%oper%operType )
          case (1)
            call getManVal(manFile%oper, 'ospeed', ospeed)
          case (3)
            call getManVal(manFile%oper, 'ospeed', ospeed)
            call getManVal(manFile%oper, 'ofuel', stircum(isr)%phop(stircum(isr)%phopidx)%stir_fuelname)
            call getManVal(manFile%oper, 'oenergyarea', oenergyarea)
            call getManVal(manFile%oper, 'ostir', ostir)
          case (4)
            call getManVal(manFile%oper, 'ofuel', stircum(isr)%phop(stircum(isr)%phopidx)%stir_fuelname)
            call getManVal(manFile%oper, 'oenergyarea', oenergyarea)
            call getManVal(manFile%oper, 'ostir', ostir)
          end select
          ! do groups
          manFile%grp => manFile%oper%grpFirst
          do while ( associated(manFile%grp) )
            select case( manFile%grp%grpType )
            case (1)
              call getManVal(manFile%grp, 'gtdepth', tdepth)
              call getManVal(manFile%grp, 'gtilArea', fracarea)
            case (3)
              call getManVal(manFile%grp, 'gcropname', cropname)
            end select
            ! do processes
            manFile%proc => manFile%grp%procFirst
            do while ( associated(manFile%proc) )
              select case( manFile%proc%procType )
              case (25)
                call getManVal(manFile%proc, 'burydist', burydistflg)
                ! accumulation of STIR values
                call stir_cum(isr, ospeed, tdepth, burydistflg, fracarea)
              case (31)
                call getManVal(manFile%proc, 'kilflag', killflag)
                if (  ( (killflag .eq. 1) &
                   .and. ( (croptype .eq. 1) .or. (croptype .eq. 2) &
                      .or. (croptype .eq. 4) .or. (croptype .eq. 5) &
                         ) &
                       ) &
                   .or. (killflag .eq. 2) &
                  ) then
                   if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .eq. 2 ) then
                     stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 5
                   else
                     stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 3
                   end if
                end if
              case (32, 42)
                call getManVal(manFile%proc, 'cyldrmh', pyieldf)
                call getManVal(manFile%proc, 'cplrmh', pstalkf)
                call getManVal(manFile%proc, 'cstrmh', rstandf)
                if( pyieldf+pstalkf+rstandf.gt.0.0 ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                  else
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 5
                  end if
                end if
              case (33, 43)
                call getManVal(manFile%proc, 'cyldrmf', pyieldf)
                call getManVal(manFile%proc, 'cplrmf', pstalkf)
                call getManVal(manFile%proc, 'cstrmf', rstandf)
                if( pyieldf+pstalkf+rstandf.gt.0.0 ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                  else
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 5
                  end if
                end if
              case (37, 47)
                call getManVal(manFile%proc, 'tyldrmp', pyieldf)
                call getManVal(manFile%proc, 'tplrmp', pstalkf)
                call getManVal(manFile%proc, 'tstrmp', rstandf)
                if( pyieldf+pstalkf+rstandf.gt.0.0 ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                  else
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 5
                  end if
                end if
              case (38, 48)
                call getManVal(manFile%proc, 'tyldrmf', pyieldf)
                call getManVal(manFile%proc, 'tplrmf', pstalkf)
                call getManVal(manFile%proc, 'tstrmf', rstandf)
                if( pyieldf+pstalkf+rstandf.gt.0.0 ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                  else
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 5
                  end if
                end if
              case (51)
                call getManVal(manFile%proc, 'idc', croptype)
                call getManVal(manFile%proc, 'plantpop', plantpop)
                if ( plantpop .gt. 0.0 ) then
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 1
                  stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (100)
                call getManVal(manFile%proc, 'idc', croptype)
                call getManVal(manFile%proc, 'plantpop', plantpop)
                if ( plantpop .gt. 0.0 ) then
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 1
                  stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (61, 62)
                call getManVal(manFile%proc, 'rstore', storef)
                call getManVal(manFile%proc, 'rleaf', leaff)
                call getManVal(manFile%proc, 'rstem', stemf)
                call getManVal(manFile%proc, 'rrootstore', rootstoref)
                call getManVal(manFile%proc, 'rrootfiber', rootfiberf)
                if( storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0 ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                  else
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 5
                  end if
                end if
              case (50, 65, 66)
                stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 4
                stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
              end select
              ! next process
              manFile%proc => manFile%proc%procNext
            end do
            ! next group
            manFile%grp => manFile%grp%grpNext
          end do

          if( .not. pass_2 ) then
            ! STIR accumulation
            if( stircum(isr)%proc_cnt .gt. 0 ) then
              stir_op_avg = stircum(isr)%stir_op_sum/stircum(isr)%proc_cnt
            else
              stir_op_avg = 0.0
            end if

            if( ostir .ge. 0.0 ) then
              scisum(isr)%stir = scisum(isr)%stir + ostir
              ! set stir value for each operation
              stircum(isr)%phop(stircum(isr)%phopidx)%phop_stir = ostir
            else
              scisum(isr)%stir = scisum(isr)%stir + stir_op_avg
              stircum(isr)%phop(stircum(isr)%phopidx)%phop_stir = stir_op_avg
            end if

            if ( (oenergyarea .ge. 0.0) .and. (manFile%oper%operType .eq. 3) ) then
              stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy = oenergyarea * get_stir_soil_multiplier(isr)
            else
              stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy = oenergyarea
            end if 
            if( oenergyarea .ge. 0.0 ) then
              ! set energy value for each operation
              scisum(isr)%energy = scisum(isr)%energy + stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy
            end if
          end if

        end if

        ! reset values for next operation
        prev_day = manFile%oper%operDate%day
        prev_mon = manFile%oper%operDate%month
        prev_yr = manFile%oper%operDate%year

        stircum(isr)%stir_op_sum = 0
        stircum(isr)%oper_cnt = 0
        stircum(isr)%proc_cnt = 0

        manFile%oper => manFile%oper%operNext

        ! second pass required to take care planting/harvest combination
        ! wrapped from end to beginning of rotation
        if( .not. pass_2 ) then
          if( .not. associated(manFile%oper) ) then
            manFile%oper => manFile%operFirst
            stircum(isr)%phopidx = 0
            pass_2 = .true.
          end if
        end if
      end do

      ! go through phop array, setting crop_growing flag
      idx = 1
      do while (.true.)
        ! search forward for planting operation
        if ( stircum(isr)%phop(idx)%phop_type .eq. 1 ) then
          ! planted so crop is growing
          stircum(isr)%phop(idx)%crop_growing = .true.
          ! search forward for termination operation
          jdx = idx + 1
          if ( jdx .gt. stircum(isr)%phopcnt ) then
            jdx = 1
          end if
          do while (stircum(isr)%phop(jdx)%phop_type .ne. 1)
            if ( (stircum(isr)%phop(jdx)%phop_type .eq. 3 ) &    ! termination operation
              .or. (stircum(isr)%phop(jdx)%phop_type .eq. 5) &   ! harvest operation with termination
               ) then
              stircum(isr)%phop(jdx)%crop_growing = .true.
              exit
            else
              ! operation after planting and before tremination
              stircum(isr)%phop(jdx)%crop_growing = .true.
            end if
            jdx = jdx + 1
            if ( jdx .gt. stircum(isr)%phopcnt ) then
              jdx = 1
            end if
          end do
        end if
        idx = idx + 1
        if ( idx .gt. stircum(isr)%phopcnt ) then
          exit
        end if
      end do

      ! go through phop array, setting last harvest flag
      idx = 1
      do while (.true.)
        ! search forward for planting operation
        if ( stircum(isr)%phop(idx)%phop_type .eq. 1 ) then
          ! search backward for last harvest operation
          jdx = idx - 1
          if ( jdx .lt. 1 ) then
            jdx = stircum(isr)%phopcnt
          end if
          do while (stircum(isr)%phop(jdx)%phop_type .ne. 1)
            if ( (stircum(isr)%phop(jdx)%phop_type .eq. 2 ) &    ! harvest operation
              .or. (stircum(isr)%phop(jdx)%phop_type .eq. 5) &   ! harvest operation with termination, last harvest
               ) then
              stircum(isr)%phop(jdx)%last_harv = 1           
              exit
            end if
            jdx = jdx - 1
            if ( jdx .lt. 1 ) then
              jdx = stircum(isr)%phopcnt
            end if
          end do
        end if
        idx = idx + 1
        if ( idx .gt. stircum(isr)%phopcnt ) then
          exit
        end if
      end do

      ! go through phop array, setting crop name for harvest and first termination
      idx = 1
      do while (.true.)
        ! search forward for planting operation
        if ( stircum(isr)%phop(idx)%phop_type .eq. 1 ) then
          ! search forward for end of crop growth
          jdx = idx + 1
          if ( jdx .gt. stircum(isr)%phopcnt ) then
            jdx = 1
          end if
          do while (stircum(isr)%phop(jdx)%phop_type .ne. 1)
            if (    (stircum(isr)%phop(jdx)%phop_type .eq. 3) &
              .and. (stircum(isr)%phop(jdx)%crop_growing) ) then
              ! termination, add name
              stircum(isr)%phop(jdx)%stir_cropname = stircum(isr)%phop(idx)%stir_cropname
            else if ( stircum(isr)%phop(jdx)%phop_type .eq. 2 ) then
              ! harvest operation, add name
              stircum(isr)%phop(jdx)%stir_cropname = stircum(isr)%phop(idx)%stir_cropname
            else if ( stircum(isr)%phop(jdx)%phop_type .eq. 5 ) then
              ! harvest operation with termination, add name
              stircum(isr)%phop(jdx)%stir_cropname = stircum(isr)%phop(idx)%stir_cropname
            end if
            jdx = jdx + 1
            if ( jdx .gt. stircum(isr)%phopcnt ) then
              jdx = 1
            end if
          end do
        end if
        idx = idx + 1
        if ( idx .gt. stircum(isr)%phopcnt ) then
          exit
        end if
      end do

      ! go through phop array, setting crop number for each "last Harvest"
      idx = 1
      temp_num = 0
      do while (.true.)
        ! search forward for first last harvest flag
        if ( stircum(isr)%phop(idx)%last_harv .eq. 1 ) then
          ! search backward for first planting operation
          jdx = idx - 1
          if ( jdx .lt. 1 ) then
            jdx = stircum(isr)%phopcnt
          end if
          do while (jdx .ne. idx)
            if ( stircum(isr)%phop(jdx)%phop_type .eq. 1 ) then
              ! found planting operation
              ! increment crop number
              temp_num = temp_num + 1
              ! set crop number for last harvest operation
              stircum(isr)%phop(idx)%crop_num = temp_num
              exit
            else
              jdx = jdx - 1
              if ( jdx .lt. 1 ) then
                jdx = stircum(isr)%phopcnt
              end if
            end if
          end do
        end if
        ! increment start index
        idx = idx + 1
        if ( idx .gt. stircum(isr)%phopcnt ) then
          ! no last harvest operation
          exit
        end if
      end do

      ! go through phop array, setting crop numbers from first 'last harvest' number
      idx = 1
      temp_idx = 0
      do while ( idx .le. stircum(isr)%phopcnt )
        ! search forward for first last harvest crop number
        if ( (stircum(isr)%phop(idx)%last_harv .eq. 1) &
          .and. (stircum(isr)%phop(idx)%crop_num .eq. 1) &
          ) then
          ! set idx for loop exit
          temp_idx = idx
          ! set current crop number
          temp_num = 1
        end if
        idx = idx + 1
      end do
      if( temp_idx .eq. 0 ) then
        ! no last harvest, set all crop number values to 1
        do idx = 1, stircum(isr)%phopcnt
          stircum(isr)%phop(idx)%crop_num =  1
        end do
      else  
        ! search forward for all last harvests and set operation crop interval number
        idx = temp_idx + 1
        do while ( idx .ne. temp_idx )
          if ( idx .gt. stircum(isr)%phopcnt ) then
            idx = 1
          end if
          if( stircum(isr)%phop(idx)%last_harv .eq. 1 ) then
            temp_num = temp_num + 1
            stircum(isr)%phop(idx)%crop_num = temp_num
          end if
          idx = idx + 1
          if ( idx .gt. stircum(isr)%phopcnt ) then
            idx = 1
          end if
        end do
        ! index through all last harvest ops and set crop numbers backward
        do idx = 1, stircum(isr)%phopcnt
          if( stircum(isr)%phop(idx)%last_harv .eq. 1 ) then
            temp_num = stircum(isr)%phop(idx)%crop_num
            jdx = idx - 1
            if ( jdx .lt. 1 ) then
              jdx = stircum(isr)%phopcnt
            end if
            do while ( stircum(isr)%phop(jdx)%last_harv .ne. 1 )
              stircum(isr)%phop(jdx)%crop_num = temp_num
              jdx = jdx - 1
              if ( jdx .lt. 1 ) then
                jdx = stircum(isr)%phopcnt
              end if
            end do
          end if
        end do

      end if

      ! create and print STIR report (2nd time through complete, info complete)
      do idx = 1, stircum(isr)%phopcnt

        if( (soil_cond .gt. 0) .and. (.not. stircum(isr)%phop(idx)%phop_skip ) ) then
          ! print this line
          write(luostir(isr),"(i2,'/',i2,'/',i4,3(' | ',a),2(' | ',f8.2),2(' | ',i0) )") &
                        stircum(isr)%phop(idx)%phopday, stircum(isr)%phop(idx)%phopmon, &
                        stircum(isr)%phop(idx)%phopyr, &
                        trim(stircum(isr)%phop(idx)%stir_opname), &
                        trim(stircum(isr)%phop(idx)%stir_cropname), &
                        trim(stircum(isr)%phop(idx)%stir_fuelname), &
                        stircum(isr)%phop(idx)%phop_stir, stircum(isr)%phop(idx)%phop_energy, &
                        stircum(isr)%phop(idx)%crop_num, stircum(isr)%phop(idx)%last_harv
        end if

        ! populate mandate arrays
        mandate(idx)%sr = manFile%isub
        ! assign operation dates
        mandate(idx)%d = stircum(isr)%phop(idx)%phopday
        mandate(idx)%m = stircum(isr)%phop(idx)%phopmon
        if( stircum(isr)%phop(idx)%phop_skip ) then
          ! set year for operation O 0 to year minus number of years in rotation. Works in all use of mandates array in reports
          mandate(idx)%y = stircum(isr)%phop(idx)%phopyr - manFile%mperod
        else
          mandate(idx)%y = stircum(isr)%phop(idx)%phopyr
        end if
        mandate(idx)%opname = stircum(isr)%phop(idx)%stir_opname
        mandate(idx)%cropname = stircum(isr)%phop(idx)%stir_cropname

        if( report_debug >= 1 ) then
          print *, idx, mandate(idx)%d, mandate(idx)%m, mandate(idx)%y, &
            trim(mandate(idx)%opname)," | ",trim(mandate(idx)%cropname)
        end if

      end do

      if( report_debug >= 1 ) then
        print *, 'size of mandate', size(mandate)
      end if

      ! close file when STIR output is enabled
      if( soil_cond .gt. 0 ) then
        close(luostir(isr))
      end if

      ! reset management file to beginnning
      manFile%oper => manFile%operFirst

      return

    end subroutine stir_report

    subroutine sci_stir_init(isr)

      use weps_cmdline_parms, only: soil_cond
      use file_io_mod, only: luostir
      use sci_report_mod, only: scisum
      use manage_data_struct_defs, only: manFile

!     + + + ARGUMENT VARIABLES + + +
      integer isr

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index

!     + + + PURPOSE + + +
!     each time it is called, it adds a value to the total biomass increments
!     the counter for number of values added together.

!     + + + LOCAL VARIABLES + + +
      integer idx

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      ! initialize sci accumulator values
      scisum(isr)%allbiomass = 0.0
      scisum(isr)%bdays = 0
      scisum(isr)%allerosion = 0.0
      scisum(isr)%edays = 0
      scisum(isr)%stir = 0.0
      scisum(isr)%energy = 0.0

      ! initialize stir accumulator values
      stircum(isr)%oper_cnt = 0
      stircum(isr)%proc_cnt = 0
      stircum(isr)%stir_op_sum = 0.0
      stircum(isr)%stir_op_energy = 0.0

      ! initialize counters and arrays for planting and harvest operation tracking
      stircum(isr)%phopcnt = 0
      stircum(isr)%phopidx = 0
      do idx = 1, size(stircum(isr)%phop)
          stircum(isr)%phop(idx)%stir_opname = ''
          stircum(isr)%phop(idx)%stir_cropname = ''
          stircum(isr)%phop(idx)%stir_fuelname = ''
          stircum(isr)%phop(idx)%phop_type = -1
          stircum(isr)%phop(idx)%phop_stir = 0.0
          stircum(isr)%phop(idx)%phop_energy = 0.0
          stircum(isr)%phop(idx)%crop_num = 0
          stircum(isr)%phop(idx)%crop_growing = .false.
          stircum(isr)%phop(idx)%last_harv = 0
      end do 

      if (stircum(isr)%header_not_printed) then
          ! write header to stir_energy.out file
          write(luostir(isr), '(5A)') '#dd/mm/yyyy | operation name',   &
     &       ' | crop name (optional) | fuel display name (optional)',  &
     &       ' | stir | energy (L diesel/ha) (soil texture adjusted)',  &
     &       ' | crop sequence number',                                 &
     &       ' | 1 if last harvest/termination of crop'
          ! write number of years in management rotation
          write(luostir(isr),'(i4,(A))') manFile(isr)%mperod,           &
     &              '  Number of years in WEPS management rotation file'
         stircum(isr)%header_not_printed = .FALSE.
      end if

      return
    end subroutine sci_stir_init

    subroutine stir_cum(isr, speed, depth, tilltype, fracarea)

      use weps_cmdline_parms, only: soil_cond

!     + + + ARGUMENT DECLARATIONS + + +
      integer isr
      real speed, depth
      integer tilltype
      real fracarea

!     + + + ARGUMENT DEFINITIONS + + +
!     isr - subregion index
!     speed - operation speed (m/s)
!     depth - tillage depth (mm)
!     tilltype - tillage burial distribution type (0-5)
!              0    o uniform distribution
!              1    o Mixing+Inversion Burial Distribution
!              2    o Mixing Burial Distribution
!              3    o Inversion Burial Distribution
!              4    o Lifting, Fracturing Burial Distribution
!              5    o Compression
!     fracarea - fraction of area affected (fraction)

!     + + + PURPOSE + + +
!     each time it is called, it calculates the Soil Tillage Intensity Rating
!     for the current operation and adds it to the total.

!     + + + LOCAL VARIABLES + + +
      real stir_val
      real mstomph, mmtoin
      real tilltype_coef

!     + + + LOCAL DEFINITIONS + + +
!     stir_val - soil tilage intensity rating value for this residue burial
!     mstomph - conversion constant from meters per second to miles per hour
!     mmtoin  - conversion constant from millimeters to inches
!     tilltype_coef - multiplier value assigned to each tillage type

      parameter (mstomph = 2.237)
      parameter (mmtoin = 0.03937)

      ! only do if flag is set 
      if( soil_cond .eq. 0 ) return

      select case (tilltype)
      case (1)  ! Mixing, some inversion
          tilltype_coef = 0.8
      case (2)  ! Mixing
          tilltype_coef = 0.7
      case (3)  ! Inversion + some mixing
          tilltype_coef = 1.0
      case (4)  ! Lifting, Fracturing
          tilltype_coef = 0.4
      case (5)  ! Compression
          tilltype_coef = 0.15
      case default
          tilltype_coef = 0.4
      end select

      stir_val = speed * mstomph * 0.5                                  &
     &         * tilltype_coef * 3.25                                   &
     &         * depth * mmtoin                                         &
     &         * fracarea

      stircum(isr)%stir_op_sum = stircum(isr)%stir_op_sum + stir_val
      stircum(isr)%proc_cnt = stircum(isr)%proc_cnt + 1

      return
    end subroutine stir_cum

end module stir_report_mod

