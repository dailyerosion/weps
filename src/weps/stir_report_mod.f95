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
      character*80 stir_cropname ! crop name associated with this operation if a harvest
      character*80 stir_fuelname ! fuel name associated with this operation, may be blank to indicate use of the default fuel as defined by the interface
      integer phop_skip          ! skip operation flag
                                 !   0 = do every rotation
                                 !   1 = skip all but first instance (also skippped in the stir report)
      integer phop_type          ! operation type flag
                                 !   0 = not yet initialized
                                 !   1 = planting operation
                                 !   2 = harvest operation
                                 !   3 = termination operation
      real phop_stir             ! STIR value for that operation
      real phop_energy           ! energy value for that operation
      integer crop_num           ! number of crop in the crop rotation cycle, 0 = not yet initialized, 1-n = number of crop
      integer last_harv          ! is this the last harvest in a crop rotation cycle, 0 = not the last harvest, 1 = this is the last harvest (ie. end of the cycle)
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
      use weps_main_mod, only: report_debug
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
      integer idx, jdx, kdx
      real :: ospeed, tdepth, fracarea
      integer :: burydistflg
      character*80     cropname
      character*80     prevcropname
      integer :: lastoperskip
      integer :: killflag
      integer :: croptype
      real :: plantpop
      real :: pyieldf
      real :: pstalkf
      real :: rstandf
      real :: storef
      real :: leaff
      real :: stemf
      real :: rootstoref
      real :: rootfiberf
      logical :: crop_present
      logical :: temp_present

!     + + + LOCAL DEFINITIONS + + +
!     stir_op_avg - average over all burial processes of stir value
!     idx         - index used to search for crop_num

      isr = manFile%isub

      ! count number of management operations
      manFile%oper => manFile%operFirst
      idx = 0
      do while( associated(manFile%oper) )
        if ( manFile%oper%operType .ne. 0 ) then
          idx = idx + 1
        end if
        manFile%oper => manFile%oper%operNext
      end do

      ! create mandate array
      call create_mandate( idx, mandate )

      ! allocate tracking array for stir report info
      call create_stir_accumulator(isr, idx)
      call sci_stir_init(isr)

      ! go through management file and populate STIR operation array with names, types, stir and energy values
      cropname = ''
      lastoperskip = 0
      croptype = 0
      crop_present = .false.
      temp_present = .false.
      manFile%oper => manFile%operFirst
      do while( associated(manFile%oper) )
        if ( manFile%oper%operType .eq. 0 ) then
          lastoperskip = 1
        else if ( manFile%oper%operType .ne. 0 ) then
          ! increment index for planting harvest accounting
          stircum(isr)%phopidx = stircum(isr)%phopidx + 1
          ! check for total number, make maximum
          stircum(isr)%phopcnt = max( stircum(isr)%phopcnt, stircum(isr)%phopidx )
          stircum(isr)%oper_cnt = stircum(isr)%oper_cnt + 1
          stircum(isr)%phop(stircum(isr)%phopidx)%phopday = manFile%oper%operDate%day
          stircum(isr)%phop(stircum(isr)%phopidx)%phopmon = manFile%oper%operDate%month
          stircum(isr)%phop(stircum(isr)%phopidx)%phopyr = manFile%oper%operDate%year
          stircum(isr)%phop(stircum(isr)%phopidx)%stir_opname = manFile%oper%operName
          stircum(isr)%phop(stircum(isr)%phopidx)%phop_skip = lastoperskip
          if ( stircum(isr)%phopidx .eq. 1 ) then
            stircum(isr)%phop(stircum(isr)%phopidx)%crop_num = stircum(isr)%phop(stircum(isr)%phopcnt)%crop_num
          else
            stircum(isr)%phop(stircum(isr)%phopidx)%crop_num = stircum(isr)%phop(stircum(isr)%phopidx-1)%crop_num
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
                   stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 3
                   if ( crop_present ) then
                     crop_present = .false.
                     temp_present = .true.
                   end if
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (40)
                temp_present = .false.
              case (32, 42)
                call getManVal(manFile%proc, 'cyldrmh', pyieldf)
                call getManVal(manFile%proc, 'cplrmh', pstalkf)
                call getManVal(manFile%proc, 'cstrmh', rstandf)
                if(     (pyieldf+pstalkf+rstandf.gt.0.0) &
                  .and. ((crop_present) .or. (temp_present)) ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                    stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                  end if
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (33, 43)
                call getManVal(manFile%proc, 'cyldrmf', pyieldf)
                call getManVal(manFile%proc, 'cplrmf', pstalkf)
                call getManVal(manFile%proc, 'cstrmf', rstandf)
                if(     (pyieldf+pstalkf+rstandf.gt.0.0) &
                  .and. ((crop_present) .or. (temp_present)) ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                    stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                  end if
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (37, 38, 47, 48)
                call getManVal(manFile%proc, 'tyldrmp', pyieldf)
                call getManVal(manFile%proc, 'tplrmp', pstalkf)
                call getManVal(manFile%proc, 'tstrmp', rstandf)
                if(     (pyieldf+pstalkf+rstandf.gt.0.0) &
                  .and. ((crop_present) .or. (temp_present)) ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                    stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                  end if
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (61, 62)
                call getManVal(manFile%proc, 'rstore', storef)
                call getManVal(manFile%proc, 'rleaf', leaff)
                call getManVal(manFile%proc, 'rstem', stemf)
                call getManVal(manFile%proc, 'rrootstore', rootstoref)
                call getManVal(manFile%proc, 'rrootfiber', rootfiberf)
                if(     (storef + leaff + stemf + rootstoref + rootfiberf .gt. 0.0) &
                  .and. ((crop_present) .or. (temp_present)) ) then
                  if ( stircum(isr)%phop(stircum(isr)%phopidx)%phop_type .ne. 3 ) then
                    stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 2
                    stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                  end if
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              case (51)
                call getManVal(manFile%proc, 'idc', croptype)
                call getManVal(manFile%proc, 'plantpop', plantpop)
                if ( plantpop .gt. 0.0 ) then
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 1
                  stircum(isr)%phop(stircum(isr)%phopidx)%stir_cropname = cropname
                  crop_present = .true.
                  stircum(isr)%phop(stircum(isr)%phopidx)%crop_num = stircum(isr)%phop(stircum(isr)%phopidx)%crop_num + 1
                else
                  stircum(isr)%phop(stircum(isr)%phopidx)%phop_type = 0
                end if
              end select
              ! next process
              manFile%proc => manFile%proc%procNext
            end do
            ! next group
            manFile%grp => manFile%grp%grpNext
          end do
          lastoperskip = 0
        end if
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

        if ( oenergyarea .ge. 0.0 ) then
          stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy = oenergyarea * get_stir_soil_multiplier(isr)
        else
          stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy = oenergyarea
        end if 
        if( oenergyarea .ge. 0.0 ) then
          ! set energy value for each operation
          scisum(isr)%energy = scisum(isr)%energy + stircum(isr)%phop(stircum(isr)%phopidx)%phop_energy
        end if

        ! reset values for next operation
        stircum(isr)%stir_op_sum = 0
        stircum(isr)%oper_cnt = 0
        stircum(isr)%proc_cnt = 0

        prevcropname = cropname
        manFile%oper => manFile%oper%operNext
      end do

      ! go through phop array, setting last harvest flag and crop harvest name
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
            stircum(isr)%phop(jdx)%stir_cropname = stircum(isr)%phop(idx)%stir_cropname
            if ( stircum(isr)%phop(jdx)%phop_type .eq. 3 ) then
              stircum(isr)%phop(jdx)%last_harv = 1           
              exit
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

      ! go through phop array, setting crop numbers backward until "last Harvest/Termination" is reached
      idx = 1
      do while (.true.)
        ! search forward for planting operation
        if ( stircum(isr)%phop(idx)%phop_type .eq. 1 ) then
          ! search backward for "last Harvest"
          jdx = idx - 1
          if ( jdx .lt. 1 ) then
            jdx = stircum(isr)%phopcnt
          end if
          do while (stircum(isr)%phop(jdx)%last_harv .ne. 1)
            stircum(isr)%phop(jdx)%crop_num = stircum(isr)%phop(idx)%crop_num
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


      ! create and print STIR report (2nd time through complete, info complete)
      do idx = 1, stircum(isr)%phopcnt
        ! assign crop number to non planting or harvest operations
        if( stircum(isr)%phop(idx)%phop_type .eq. 0 ) then
          ! assign crop number from next planting or harvest operation
          do jdx = idx, stircum(isr)%phopcnt
             ! index ahead in file to end
             if( stircum(isr)%phop(jdx)%phop_type .gt. 0 ) then
                ! planting or harvest operation, use assigned crop number
                stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(jdx)%crop_num
                ! we don't need any more
                exit
             else if( jdx .eq. stircum(isr)%phopcnt ) then
                ! didn't find number, continue from start of file
                do kdx = 1, idx-1
                   if( stircum(isr)%phop(kdx)%phop_type .gt. 0 ) then
                      ! planting or harvest operation, use assigned crop number
                      stircum(isr)%phop(idx)%crop_num = stircum(isr)%phop(kdx)%crop_num
                      ! we don't need any more
                      exit
                   end if
                end do
             end if
          end do
        end if
             
        if( stircum(isr)%phop(idx)%phop_skip .eq. 0 ) then
          ! print this line
          write(luostir(isr),"(i2,'/',i2,'/',i4,3(' | ',a),2(' | ',f8.2),2(' | ',i1) )") &
                        stircum(isr)%phop(idx)%phopday, stircum(isr)%phop(idx)%phopmon, &
                        stircum(isr)%phop(idx)%phopyr, &
                        trim(stircum(isr)%phop(idx)%stir_opname), &
                        trim(stircum(isr)%phop(idx)%stir_cropname), &
                        trim(stircum(isr)%phop(idx)%stir_fuelname), &
                        stircum(isr)%phop(idx)%phop_stir, stircum(isr)%phop(idx)%phop_energy, &
                        stircum(isr)%phop(idx)%crop_num, stircum(isr)%phop(idx)%last_harv
          ! populate mandate arrays
          mandate(idx)%sr = manFile%isub
          ! assign operation dates
          mandate(idx)%d = stircum(isr)%phop(idx)%phopday
          mandate(idx)%m = stircum(isr)%phop(idx)%phopmon
          mandate(idx)%y = stircum(isr)%phop(idx)%phopyr
          mandate(idx)%opname = stircum(isr)%phop(idx)%stir_opname
          mandate(idx)%cropname = stircum(isr)%phop(idx)%stir_cropname

          if( report_debug >= 1 ) then
              print *, idx, mandate(idx)%d, mandate(idx)%m, mandate(idx)%y, &
                trim(mandate(idx)%opname)," | ",trim(mandate(idx)%cropname)
          end if

        end if
      end do

      if( report_debug >= 1 ) then
        print *, 'size of mandate', size(mandate)
      end if

      ! close file
      close(luostir(isr))

      ! reset management file to beginnning
      manFile%oper => manFile%operFirst

      return

    end subroutine stir_report

end module stir_report_mod

